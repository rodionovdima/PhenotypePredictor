# Phenotype Propagator
Phenotype Propagator uses gene-level mcSEED functional annotations for in silico predictions of the presence or absence (denoted as binary: “1” or “0”) of 106 functional metabolic pathways using a semi-automated process based on a combination of the following three approaches:
## Pathway Rules (PR)-based phenotype predictions.
This approach uses explicit logic-based “pathway rules” to assign binary phenotypes. These rules combine (i) expert curators knowledge regarding the gene composition of various metabolic pathway variants contained in the mcSEED database with (ii) a decision tree method to identify patterns of gene representation in reference genomes corresponding to an intact functional pathway variant (and a respective binary phenotype value denoted as “1”). A total of 106 functional pathway-specific decision trees were generated, where the presence or absence of a particular phenotype was the response variable, and the presence or absence of functional roles (encoded by genes) in each reference pathway were predictor variables. The resulting pathway rules were formally encoded into an R-script 'get_binary_phenotype.R'  that allowed us to process the genome annotations and assign values (1 or 0) for each of the 106 functional metabolic pathways.

## Machine Learning (ML)-based phenotype predictions.
We compared >30 ML methods (Caret, v6.0.86), using a ‘leave one out’ cross-validation approach in which we removed a single reference genome from the set of 2,856 reference genomes, trained ML models on the remaining genomes, then applied the models to the “test” genome to predict phenotypes. This procedure was then repeated for each genome and each metabolic phenotype. The results of this analysis identified Random Forest as the best-performing method (i.e., it produced the greatest number of correctly predicted phenotypes in our reference training dataset). We then built Random Forest models for each phenotype based on the reference dataset, optimized model parameters using a grid search, and used these models to predict binary (1/0) values for the same set of 106 phenotypes for all target genomes/MAGs.

## Neighbor Group (NG)-based phenotype predictions.
This approach identifies reference bacteria that are closely related to the target genomes/MAGs in this study and uses these high-quality reference genomes for phenotype predications that are robust to variation in taregt genome quality. Examination of groups of closely related reference organisms suggested that close phylogenetic neighbor genomes tend to either possess or lack an entire pathway variant, whereas more distant neighbors (e.g., other neighbor groups) often carry more divergent pathway variants that specify the same phenotype. We used this observation to develop heuristics that minimize false negative phenotype assignments emerging from the other two prediction strategies. We compiled a set of NGs comprised of target genomes/MAGs and closely related reference genomes (Mash/MinHash distance ≤ 0.1, corresponding to ANI ≥90%). Within each NG and for each metabolic pathway, we tentatively assigned a binary phenotype value for a given MAG based on the NG genome with the closest matching gene annotation pattern (based on Hamming distance), even if some of the genes were absent in the query MAG. We limited comparisons to genes required for the function of each respective pathway

## Consensus phenotype prediction.
We established a procedure to reconcile inconsistent phenotype predictions between the three strategies described above, based on observing discordant gene patterns and/or discordant predicted phenotypes within a given group of neighbor genomes. In the rare case of irreconcilable disagreement between the prediction methods, assignment of a consensus phenotype defaulted to that produced by the ML method. We assigned consensus confidence scores to each prediction based on the degree of concordance between the three techniques and our confidence in the accuracy of each.
The complete phenotype prediction process was validated using the 2,856 reference genomes in the mcSEED database, their functionally annotated genes and the accompanying patterns of presence/absence of functional metabolic pathways (curator-inferred binary phenotypes). 
The consensus phenotype predictions are combined into a binary phenotype matrix (BPM) containing the complete set of target genomes/MAGs and 106 phenotypes (output file `consensusBPM.txt`). The obatined for each predicted phenotype confidence codees are also provided (output file `confidenceBPM.txt`).
### Methods used for Consensus Phenotype assignment:
PR	Pathway Rule-based phenotypes

ML	Machine Learning (ML) model-based phenotypes 

NG	Neighbor Group-based phenotypes
 
PRc	NG-corrected PR phenotypes

MLc	NG-corrected ML phenotypes

### Confidence codes of Consensus Phenotype assignments:
c	consistent phenotypes (with NG)

c3	consistent phenotypes (without NG)

n0	Inconsistencies between PR/ML & NG (resolved for PR/ML)

n1	Inconsistencies between PR/ML & NG (resolved for NG)

n2	Inconsistencies between either PR or ML & NG (resolved for NG)

n3	Conflict between PR & ML when neighbors are absent

n4	Other inconsistences between any of methods

## Prerequisites

- R
  - data.table
  - openxlsx
  - caret
  - yaml

## Running Phenotype Propagator
To run the pipeline first prepare the input files by placing them into `input/annotation` (danatello outputs with mcSEED functional annotations for each target genome/MAG) and `input/mash_groups` (neighbor groups obtained by mash_group script).
Then update `PhenotypePropagator.yaml` config file (including `root_dir` and other directories)

Finally, run `Rscript PhenotypePropagator.R` 