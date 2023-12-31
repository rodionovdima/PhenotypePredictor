# Phenotype Predictor
Workflow for executing functional annotation and metabolic phenotype assignement to microbial genomes.

The functional annotation workflow utilizes a ‘subsystems’ approach adapted from the SEED genome annotation platform to identify genes that comprise curated metabolic pathways in mcSEED, a microbial community-centered implementation of SEED. The current version of the workflow (January 2021) uses the mcSEED database comprised of 2,856 reference human gut bacterial genomes and 80 curated metabolic subsystems, available to download from [Zenodo](https://doi.org/10.5281/zenodo.10041396).

Each mcSEED subsystem includes a set of functional roles (e.g., enzymes, transporters, transcriptional regulators) that contribute to the prediction of functional metabolic pathways and pathway variants involved in utilization and catabolism carbohydrates and amino acids, biosynthesis of vitamins/cofactors and amino acids, and generation of fermentation end-products such as short-chain fatty acids. 

The pipeline produces two major outputs: (i) a complete set of functionally annotated proteins contributing to 80 reconstructed metabolic subsystems identified in target genomes; and (ii) a Binary Phenotype Matrix (BPM) reflecting the inferred presence or absence of 106 functional metabolic pathways in each target genome. 

The annotation workflow can be applied to any set of isolate genomes or Metagenome Assembled Genomes (MAGs) and requires as an input: i) nucleotide genomic sequences (FNA), ii) amino acid proteomes (FAA), and iii) simple taxonomic names provided for each target genome (species and genus). 

The workflow is composed of three major steps that need to be run independently in the current version:
1. Danatello is a DIAMOND-based annotation pipeline (Python) used to propagate mcSEED annotations to the proteomes (FAA) of each target genome.
2. Mash groups is an R-script designed to collect taxonomically-annotated neighbor groups (NG) of mcSEED reference genomes based on their corresponding mash distances to each target genome (FNA).
3. Phenotype Propagator is a R-script based package that uses the results of gene-level functional annotation (step 1) and phylogentic neighbor groups (step 2) to *in silico* predict the presence or absence (denoted as binary “1” or “0” ) of 106 functional metabolic pathways.