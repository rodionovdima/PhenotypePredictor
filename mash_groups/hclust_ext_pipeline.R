library(tidyverse)
library(optparse)

# Command line parameters
parser <- OptionParser()
parser <- add_option(parser, c("-d", "--dist"), action="store", default='', type='character', help="MASH distance file. Query - MAGs, Reference - mcSEED.")
parser <- add_option(parser, c("-r", "--ref_taxonomy"), action="store", default='', type='character', help="Table with genomes taxonomy for reference genomes (mcSEED). Require columnes: id, name, species, genus.")
parser <- add_option(parser, c("-m", "--mag_taxonomy"), action="store", default='', type='character', help="Table with genomes taxonomy for MAG genomes. Require columnes: id, species, genus.")
parser <- add_option(parser, c("-t", "--mgn_thr"), action="store", default='20', type='integer', help="Minimum number of genomes. Default: 20")
parser <- add_option(parser, c("-l", "--lth_thr"), action="store", default='0.9', type='double', help="Threshold on genome similarity when to stop algorithm. Default: 0.9 (90%)")
parser <- add_option(parser, c("-s", "--step"), action="store", default='0.025', type='double', help="Similarity decrease step. Default 0.025 (2.5%)")
parser <- add_option(parser, c("-o", "--out"), action="store", default='./mash_groups', type='character', help="Output folder. Default: ./mash_groups")
# args = parse_args(parser, args = c('-d', './in/PC181vsmcSEED_results', '-r', './in/taxonomy/ref_taxonomy.txt', '-m', './in/taxonomy/query_taxonomy.txt'))
args = parse_args(parser)

# Create output directory
print('Create outut directory')
dir.create(args$out)

step = args$step
min_genomes_in_group = args$mgn_thr
min_thr = 1 - args$lth_thr

# Collect taxonomy
print('Read reference taxonomy')
mcseed_taxonomy = read_tsv(
  args$ref_taxonomy,
  col_types = cols(.default = col_character())
) %>% select(id, genus, species, name)
names(mcseed_taxonomy) = c('id', 'mcSEED_genus', 'mcSEED_species', 'mcSEED_name')
mcseed_taxonomy = mcseed_taxonomy %>% distinct() %>% arrange(id)

print('Read MAG taxonomy')
mag_taxonomy = read_tsv(
  args$mag_taxonomy,
  col_types = cols(.default = col_character())
) %>% select(id, species, genus)
# mag_taxonomy = mag_taxonomy %>% mutate(id = str_c('NZ_', id))
names(mag_taxonomy) = c('id', 'MAG_species', 'MAG_genus')


# Read distance file
print('Read MASH distance file')
mags = vroom::vroom(args$dist, col_names = c("query", "ref", "dist", "pvalue", "match_hash"))
mags = mags %>% mutate(
  query = str_remove(basename(query), '\\.fna'),
  ref = str_remove(basename(ref), '\\.fna'),
  #query = str_match(query, "([^/]+)\\.fna")[,2], 
  #ref = str_match(ref, "([^/]+)\\.fna")[,2], 
  match_hash = as.integer(str_sub(match_hash, 1, -7))
)

# print('Remove suffizes and prefixes from files')
# print('Remove _b[number] suffix')
# mags = mags %>% mutate(query = str_remove(query, '_b\\d$'))
# print('Remove NZ_ prefix')
# mags = mags %>% mutate(query = str_remove(query, '^NZ_'))

print(str_c(c('Example of MAG id from taxonomy file: ', mag_taxonomy$id[1])))
print(str_c(c('Example of MAG id from MASH file: ', mags$query[1])))

mags = mags %>% select(query, ref, dist, match_hash)

# Make matrix
print('Make a distance matrix')
print('Make wide table')
mags_wide = mags %>% select(-match_hash) %>% pivot_wider(names_from = 'ref', values_from = 'dist')

print('Convert to matrix')
mags_dist = as.matrix(mags_wide[-1])
rownames(mags_dist) = mags_wide[1]$query

# Collect columns with MAGs in mags_dist and convert to list
mags_dist = t(mags_dist)
mags_dist_mag = mags_dist[rownames(mags_dist) %in% mcseed_taxonomy$id, colnames(mags_dist) %in% mag_taxonomy$id]

if(dim(mags_dist_mag)[2] == 0){
  stop('No MAGs were selected. Check if IDs in taxonomy and MASH files are the same.')
}

mags_columns = colnames(mags_dist_mag)
mags_dist_mag = lapply(seq_len(dim(mags_dist_mag)[2]), function(i) mags_dist_mag[, i])
names(mags_dist_mag) = mags_columns

# make algorithm as function
collect_group = function(dist_list){
  thr = step
  n_genomes = 0
  while(n_genomes < min_genomes_in_group && thr <= min_thr + 10^-5){
    dist_list_selected = dist_list[dist_list <= thr]
    n_genomes = length(dist_list_selected)
    thr = thr + step
  }
  return(c(dist_list_selected, thr - step))
}

print('Collect groups')
groups = lapply(mags_dist_mag, collect_group)
thresholds = lapply(groups, function(group) group[length(group)])
groups = lapply(groups, function(group) if (length(group) > 1) head(group, -1) else c('X' = -1))
group_lengths = as_vector(map(groups, length))

# Create vector of MAG Names
print('Create group table.')
mag_names = vector(sum(group_lengths), mode = "character")
mag_thresholds = vector(sum(group_lengths), mode = "numeric")
mcseed_group = vector(sum(group_lengths), mode = "character")
mcseed_dist = vector(sum(group_lengths), mode = "numeric")
size = vector(sum(group_lengths), mode = "integer")
offset = 1

for(i in seq_along(group_lengths)){
  mag_names[c(offset:(offset + group_lengths[i] - 1))] = rep(names(group_lengths[i]), group_lengths[i])
  mag_thresholds[c(offset:(offset + group_lengths[i] - 1))] = rep(thresholds[[names(group_lengths[i])]], group_lengths[i])
  mcseed_dist[c(offset:(offset + group_lengths[i] - 1))] = groups[[names(group_lengths[i])]]
  mcseed_group[c(offset:(offset + group_lengths[i] - 1))] = names(groups[[names(group_lengths[i])]])
  size[c(offset:(offset + group_lengths[i] - 1))] =  group_lengths[[names(group_lengths[i])]]
  offset = offset + group_lengths[i]
}

group_table = tibble(
  MAG = mag_names,
  mcSEED_id = mcseed_group,
  Similarity = 1 - mcseed_dist,
  Threshold = mag_thresholds,
  Group_Size = size
)

group_table = group_table %>% mutate(
  Similarity = case_when(
    Similarity == 2 ~ -1,
    TRUE ~ Similarity
  )
)

group_table[group_table$Similarity == 2, 'Similarity'] = -1
group_table[group_table$Similarity == -1, 'Group_Size'] = 0

print('Add number of kmers.')
group_table = left_join(group_table, mags %>% select(-dist), by = c('mcSEED_id' = 'ref', 'MAG' = 'query'))
print('Add reference taxonomy.')
group_table = left_join(group_table, mcseed_taxonomy, by = c('mcSEED_id' = 'id'))
print('Add MAG taxonomy.')
group_table = left_join(group_table, mag_taxonomy, by = c('MAG' = 'id'))
group_table = group_table %>% mutate(Threshold = str_c(100 * (1 - Threshold), '%'))

print('Write groups table.')
group_table %>% 
  write_tsv(file.path(args$out, 'group_table.txt'))

# Make list of species in groups
list_of_species = group_table %>% 
  select(MAG, mcSEED_species) %>% 
  group_by(MAG) %>%
  nest()
list_of_species = list_of_species %>% mutate(Species = lapply(data, function(tbl) unname(
    unlist(
      distinct(
        tbl['mcSEED_species']
        )
      )
    )
  )
)

list_of_species$Species = sapply(list_of_species$Species, paste, collapse = ', ')
list_of_species = list_of_species %>% select(-data)

group_table_summary = group_table %>% 
  select(-c('Similarity', 'mcSEED_id', 'mcSEED_name', 'mcSEED_species', 'mcSEED_genus', 'match_hash')) %>% 
  distinct()

group_table_summary = left_join(group_table_summary, list_of_species, by = 'MAG')

print('Write summary table')
group_table_summary %>%
  write_tsv(file.path(args$out, 'summary_table.txt'))
