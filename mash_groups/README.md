# mash_groups
Mash groups is a script designed to collect taxonomically-annotated groups of mcSEED reference genomes based on their corresponding mash distances to each target genome/MAG (FNA)

## Prerequisites:
- MASH
- R 
  - tidyverse
  - optparse

## Usage

### 1. Calculate MASH distances

**a. Make sketch for FNA files using 10000 k-mers**

```bash
mash sketch -p [number of cpu] -s [number of k-mers] -o [out_file] [Path to FNA files]

# Example:
mash sketch -p 16 -s 10000 -o sample_sketch ../fna/*.fna
```

`-p` - number of parallel processes [use 1-16]
`-s` - number of k-mers [use 10000]
`-o` - output files

**b. Calculate distance against mcSEED reference**

```bash
mash dist -p [number of cpu] [sample sketch] [mcSEED reference sketch] > [results file]

# Example:
mash dist -p 16 sample_sketch.msh mcSEED_mash_20210125.msh > sample_dist
```
Note, that this step will require 'mcSEED_mash_20210125.msh' - MASH-generated sketch file for 2856 reference mcSEED genomes used to calculate mash distance for target genomes.
This file can be downloaded from the reference database archive at https://doi.org/10.5281/zenodo.10041396

### 2. Make groups

**a. Make taxonomy tables**

**a.1. Reference taxonomy table** 

It could be found in the `mash_groups/taxonomy/ref_taxonomy.txt` file.

The table design is:

| id        | name                            | genus     | species               |
|-----------|---------------------------------|-----------|-----------------------|
| 1120974.3 | Alistipes onderdonkii DSM 19147 | Alistipes | Alistipes onderdonkii |

**a.2. Sample taxonomy table**

The taxonomy table for the analyzed sample has the following structure:

| id       | genus       | species          |
|----------|-------------|------------------|
| EC_21453 | Escherichia | Escherichia coli |

ID should be the same as file names.
If taxonomy is unknown populate `genus` and `species` with dashes `-`.

**b. Run a script**

```bash
Rscript --vanilla hclust_ext_pipeline.R \
                      -d [distance file] \
                      -r [reference taxonomy table] \
                      -m [query taxonomy table]
                      
# Example:
Rscript --vanilla hclust_ext_pipeline.R -d sample_dist -r ./taxonomy/ref_table.txt -m ./taxonomy/query_table.txt
```

List of options that could be modified:
```
Options:
        -h, --help
                Show this help message and exit

        -d DIST, --dist=DIST
                MASH distance file. Query - MAGs, Reference - mcSEED.

        -r REF_TAXONOMY, --ref_taxonomy=REF_TAXONOMY
                Table with genomes taxonomy for reference genomes (mcSEED). Require columnes: id, name, species, genus.

        -m MAG_TAXONOMY, --mag_taxonomy=MAG_TAXONOMY
                Table with genomes taxonomy for MAG genomes. Require columnes: id, species, genus.

        -t MGN_THR, --mgn_thr=MGN_THR
                Minimum number of genomes. Default: 20

        -l LTH_THR, --lth_thr=LTH_THR
                Threshold on genome similarity when to stop algorithm. Default: 0.9 (90%)

        -s STEP, --step=STEP
                Similarity decrease step. Default 0.025 (2.5%)

        -o OUT, --out=OUT
                Output folder. Default: ./mash_groups
```

## Outputs

Script outputs two tables:

- Table `group_table.txt` with columns:
  - **MAG** - Query genome id.
  - **mcSEED_id** - Reference mcSEED genome id.
  - **Similarity** - Similarity measure (1 - MASH_distance).
  - **Threshold** - Threshold that was used to populate a group.
  - **Group_Size** - Size of the group.
  - **match_hash** - Number of matched k-mers.
  - **mcSEED_genus** - Genus from the mcSEED taxonomy.
  - **mcSEED_species** - Species from the mcSEED taxonomy.
  - **mcSEED_name** - Genome name from the mcSEED taxonomy.
  - **MAG_species** - Species from the query taxonomy.
  - **MAG_genus** - Genus from the query taxonomy.

- Table `summary_table.txt` with columns:
  - **MAG** - Query genome id.
  - **Threshold** - Threshold that was used to populate a group.
  - **Group_Size** - Number of reference genomes in group.
  - **MAG_species** - Species from the query taxonomy.
  - **MAG_genus** - Genus from the query taxonomy.
  - **Species** - List of species from mcSEED taxonomy that are in the group.
