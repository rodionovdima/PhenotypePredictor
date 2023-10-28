library(data.table)                                                     
library(openxlsx)
library(caret)
library(yaml)

config <- yaml.load_file("PhenotypePropagator.yaml")

noStyle <- createStyle(fontColour = "#9A0511", bgFill = "#FEC7CE")
yesStyle <- createStyle(fontColour = "#09600B", bgFill = "#C7EECF")

# Function to return number of missing genes in a target genome and a target metabolic subsystem as compared to the closely-related reference genome from the neighbor group
getMissing <- function(query, ref)
{
  cnt <- 0
  for(name in names(query)){
   if(query[,get(name)] == 0 && ref[,get(name)] == 1){
        cnt <- cnt + 1
      }
  }
  return(cnt)
}

# Function to return number of excessive genes in a target genome and a target metabolic subsystem as compared to the closely-related reference genome from the neighbor group
getExcessive <- function(query, ref)
{
  cnt <- 0
  for(name in names(query)){
    if(query[,get(name)] == 1 && ref[,get(name)] == 0){
      cnt <- cnt + 1
    }
  }
  return(cnt)
}

#Function to return neighbor group-bases corrected phenotype that take as input either the Pathway Rules (PR)-based or MAchine Learning (ML)-based phenotype predictions 
phen_correction <- function(phenRules, phenNB, missingCnt, excessCnt, minGenesPhen1, maxGenesPhen0, phen0Cnt, phen1Cnt, CPNGsize) {
  flag <- 0
  if(phenNB == 1){
    if(phenRules == 1) {
      cons <- 1
    } else if(phenRules == 0){
      if(missingCnt > 0){
        if(phen0Cnt > 0){
          if(missingCnt > 1 && missingCnt > (minGenesPhen1-maxGenesPhen0)){
            cons <- 0
            flag <- 1
            if(CPNGsize <= 3){
              flag <- 0
            }
          } else {
            cons <- 1
          }
        } else {
          if(missingCnt > ((1/3)*minGenesPhen1)){
            cons <- 0
            flag <- 1
            if(CPNGsize <= 3){
              flag <- 0
            }
          } else {
            cons <- 1
          }
        }
      } else {
        cons <- 1
      }
    }
  } else if(phenNB == 0) {
    if(phenRules == 1) {
      if(excessCnt > 0){
        if(phen1Cnt > 0){
          if(excessCnt > 1 && excessCnt > (minGenesPhen1-maxGenesPhen0)) {
            flag <- 1
            cons <- 1
            if(CPNGsize <= 3){
              flag <- 0
            }
          } else {
            cons <- 0
          }
        } else {
          cons <- 1
        }
      } else {
        cons <- 0
      }
    } else {
      if(excessCnt > 0){
        if(phen1Cnt > 0){
          if(excessCnt > 1 && excessCnt > (minGenesPhen1-maxGenesPhen0)) {
            flag <- 1
            cons <- 0
          } else {
            cons <- 0
          }
        } else {
          flag <- 1
          cons <- 0
        }
      } else {
        cons <- 0
      }
    }
  }
  res <- list()
  res$cons <- cons
  res$flag <- flag
  return(res)
}

# External function to return the Pathway Rules (PR)-based phenotype predictions
source("get_binary_phenotype.R")

#Define directories for input, output and reference data
PREDICTIONS_DIR <- file.path(config$root_dir,config$annotation_dir)
MODEL_DIR <- file.path(config$root_dir,config$model_dir)
TRAINING_DIR <- file.path(config$root_dir,config$training_dir)
TRAINING_DIR_GENOMEID <- file.path(config$root_dir,config$training_dir_genomeid)
OUTPUT_DIR <- file.path(config$root_dir,config$output_dir)

# Read reference metabolic subsystem data
subsRefPATH <- file.path(config$root_dir,config$subs_ref)
subsRef <- read.csv(subsRefPATH,sep='\t')
subsRef <- data.table(subsRef)
subsRef <- subsRef[isDone == 1]
subsRef[,isDone := NULL]

#Read reference functional role data
releaseFuncRolesPATH <- file.path(config$root_dir,config$functional_roles)
releaseFuncRoles <- read.csv(releaseFuncRolesPATH,sep='\t',header=FALSE)
releaseFuncRoles <- data.table(releaseFuncRoles)
releaseFuncRoles[,V4 := NULL]
releaseFuncRoles[,V5 := NULL]
releaseFuncRoles[,V6 := NULL]
setnames(releaseFuncRoles,c("subsystem","geneShort","geneFull"))
funcRoles <- copy(releaseFuncRoles)
setnames(funcRoles,"geneShort","geneShortRelease")

#Read reference data of functional roles that are involved in the metabolic pathway rules
patternRolesPATH <- file.path(config$root_dir,config$pattern_roles)
patternRoles <- read.csv(patternRolesPATH,sep=';')
patternRoles <- data.table(patternRoles)

#Read neighbor groups of reference genomes for each target genome
refGroupsPATH <- file.path(config$root_dir,config$groups_dir,"group_table.txt")
refGroups <- read.csv(refGroupsPATH,sep='\t',colClasses = "character")
refGroups <- data.table(refGroups)

# Main script start here
wbFull <- createWorkbook()
results <- data.table()

prevSubsystem <- '-1'
subsSheetNo <- 1
tableConsensusMat <- data.table()
tableConfidenceMat <- data.table()
for(p in 1:nrow(subsRef)){
  curPhenotype <- subsRef[p]$Phenotype
  curSubsystem <- subsRef[p]$Subsystem
  curFilename <- subsRef[p]$Filename
  patternRolesSubsystem <- patternRoles[Subsystem == curSubsystem & FlagDR == 0]

  rolesSubsystem <- funcRoles[subsystem == curSubsystem]
  
  model <- readRDS(file.path(MODEL_DIR,curPhenotype,paste0("model_",curPhenotype,"_ranger.rds")))
#  modelXGB <- readRDS(file.path(MODEL_DIR,curPhenotype,paste0("model_",curPhenotype,"_xgbTree.rds")))
  trainset <- read.csv(file.path(TRAINING_DIR,curFilename),colClasses = "factor",check.names = FALSE)
  trainset <- data.table(trainset)
  trainset <- trainset[1]
  trainset[,Y:=NULL]
  for(n in names(trainset)){
    trainset[,(n):="0"]
  }
  
  trainsetGID <- read.csv(file.path(TRAINING_DIR_GENOMEID,curFilename),colClasses = "factor",check.names = FALSE)
  trainsetGID <- data.table(trainsetGID)
 
  files <- list.files(PREDICTIONS_DIR)
  
  data4write <- data.table()
  resultsConsensus <- c()
  resultsConsensusDetails <- c()
  resultsConsensusMat <- c()
  resultsConfidenceMat <- c()
  for(f in files){
    print(paste0("Genome: ",f,", Subsystem: ", curSubsystem,", Phenotype: ",curPhenotype))
    
    genome <- gsub(".faa","",f)

    data <- read.csv(file.path(PREDICTIONS_DIR,f),sep='\t')
    data <- data.table(data)
               

    genePresence <- merge(rolesSubsystem,data,by.x="geneFull",by.y="Winner")
    genes <- genePresence$geneShortRelease
    
# Prediction of metabolic phenotypes by Pathway Rules(PR)
    phenByRules <- getBinaryPhenotype(curSubsystem,curPhenotype,genes)

# Prediction of metabolic phenotypes by ML models (Random Forest)
    curset <- copy(trainset)
    for(g in genes){
      if(g %in% names(curset)){
       curset[,(g):="1"]
      }
    }
    set.seed(1)
    phenByMLranger <- predict(model,curset)
    data4write <- rbind(data4write,cbind("ID"=genome,curset))
    
# Prediction of metabolic phenotypes by Neighbor Groups (NG) of reference genomes
    nbgroup <- refGroups[MAG == genome]
    MAG_species <- nbgroup[1]$mcSEED_species
    MAG_genus <- nbgroup[1]$mcSEED_genus
    if(nrow(nbgroup) == 0){
      print("NB group is empty!")
      stop()
    }
    CPNG <- trainsetGID[id %in% nbgroup$mcSEED_id]
    nbIDs <- CPNG$id
    CPNG[, id:=NULL]
    
#Size of neighbor group of reference genomes
    CPNGsize <- nrow(CPNG)
    
    if(CPNGsize < 1){
      phenByNB <- -1
      closestID <- -1
      missing <- -1
      excessive <- -1
      phen0Count <- -1
      phen1Count <- -1
      maxGenesPhen0 <- -1
      minGenesPhen1 <- -1
    } else {
      
     phen0Count <- nrow(CPNG[Y == 0])
     phen1Count <- nrow(CPNG[Y == 1])
     CPNGint <- CPNG[,lapply(.SD,as.character)]
     CPNGint <- CPNGint[,lapply(.SD,as.integer)]
     CPNGphen0 <- CPNGint[Y==0]
     CPNGphen1 <- CPNGint[Y==1]
     CPNGphen0[, genesCount := rowSums(.SD)]
     CPNGphen1[, genesCount := rowSums(.SD)]
     if(phen0Count == 0){
       maxGenesPhen0 <- -1
     } else {
       maxGenesPhen0 <- CPNGphen0[,max(genesCount)]
     }
     if(phen1Count == 0){
       minGenesPhen1 <- -1
     } else {
       minGenesPhen1 <- CPNGphen1[,min(genesCount)]
     }
     
     nbYs <- CPNG$Y
     CPNG[, Y:=NULL]

     mindist <- 999
     minind <- 0
     for(g in 1:nrow(CPNG)){
      tmp <- rbind(curset,CPNG[g])
      for(colname in patternRolesSubsystem$Name){
        if(colname %in% names(tmp)){
         tmp[,(colname) := NULL]
        }
      }
      d <- dist(tmp,method="manhattan")
      if(d < mindist){
        mindist <- d
        minind <- g
      }
     }
     closestID <- nbIDs[minind]
     phenByNB <- nbYs[minind]
     
     tmp <- rbind(curset,CPNG[minind])
     for(colname in patternRolesSubsystem$Name){
       if(colname %in% names(tmp)){
         tmp[,(colname) := NULL]
       }
     }
     missing <- getMissing(tmp[1],tmp[2])
     excessive <- getExcessive(tmp[1],tmp[2])
    } 
    
# Correction of Pathway Rules (PR)-based and Machine Learnine (ML)-based phenotype predictions using closely-related reference genomes from Neighbor Group
    if(phenByNB != -1) {
     PRcorrected <- phen_correction(phenByRules,phenByNB,missing,excessive,minGenesPhen1,maxGenesPhen0,phen0Count,phen1Count,CPNGsize)
     phenPRc <- PRcorrected$cons
     flagPRc <- PRcorrected$flag
     MLcorrected <- phen_correction(phenByMLranger,phenByNB,missing,excessive,minGenesPhen1,maxGenesPhen0,phen0Count,phen1Count,CPNGsize)
     phenMLc <- MLcorrected$cons
     flagMLc <- MLcorrected$flag
     if(flagPRc == 1 | flagMLc == 1){
       correction_flag <- 1
     } else {
       correction_flag <- 0
     }
    } else {
      phenPRc <- -1
      phenMLc <- -1
      correction_flag <- -1
    }

    phenByMLranger <- as.integer(as.character(phenByMLranger))
    phenByNB <- as.integer(as.character(phenByNB))
  
   
# Assignment of Consensus phenotypes and Confidence codes using both the PR, ML, and NG-based phenotypes as well as the neighbor-corrected PR and ML phenotypes 
    ConsensusMat <- "-1"
    ConfidenceMat <- "-1"
    if(phenByRules == 1 && phenByMLranger == 1 && phenByNB == 1 && phenPRc == 1 && phenMLc == 1){
      ConsensusMat <- "1"
      ConfidenceMat <- "c"
    } else if(phenByRules == 1 && phenByMLranger == 0 && phenByNB == 1 && phenPRc == 1 && phenMLc == 1){
      ConsensusMat <- "1"
      ConfidenceMat <- "n2"
    } else if(phenByRules == 1 && phenByMLranger == 0 && phenByNB == 1 && phenPRc == 1 && phenMLc == 0){
      ConsensusMat <- "0"
      ConfidenceMat <- "n4"
    } else if(phenByRules == 0 && phenByMLranger == 1 && phenByNB == 1 && phenPRc == 1 && phenMLc == 1){
      ConsensusMat <- "1"
      ConfidenceMat <- "n2"
    } else if(phenByRules == 0 && phenByMLranger == 1 && phenByNB == 1 && phenPRc == 0 && phenMLc == 1){
      ConsensusMat <- "1"
      ConfidenceMat <- "n4"
    } else if(phenByRules == 0 && phenByMLranger == 0 && phenByNB == 1 && phenPRc == 1 && phenMLc == 1){
      ConsensusMat <- "1"
      ConfidenceMat <- "n1"
    } else if(phenByRules == 0 && phenByMLranger == 0 && phenByNB == 1 && phenPRc == 0 && phenMLc == 1){
      ConsensusMat <- "0"
      ConfidenceMat <- "n4"
    } else if(phenByRules == 0 && phenByMLranger == 0 && phenByNB == 1 && phenPRc == 1 && phenMLc == 0){
      ConsensusMat <- "0"
      ConfidenceMat <- "n4"
    } else if(phenByRules == 0 && phenByMLranger == 0 && phenByNB == 1 && phenPRc == 0 && phenMLc == 0){
      ConsensusMat <- "0"
      ConfidenceMat <- "n0"
    } else if(phenByRules == 1 && phenByMLranger == 1 && phenByNB == 0 && phenPRc == 1 && phenMLc == 1){
      ConsensusMat <- "1"
      ConfidenceMat <- "n0"
    } else if(phenByRules == 1 && phenByMLranger == 1 && phenByNB == 0 && phenPRc == 1 && phenMLc == 0){
      ConsensusMat <- "1"
      ConfidenceMat <- "n4"
    } else if(phenByRules == 1 && phenByMLranger == 1 && phenByNB == 0 && phenPRc == 0 && phenMLc == 1){
      ConsensusMat <- "1"
      ConfidenceMat <- "n4"
    } else if(phenByRules == 1 && phenByMLranger == 1 && phenByNB == 0 && phenPRc == 0 && phenMLc == 0){
      ConsensusMat <- "0"
      ConfidenceMat <- "n1"
     } else if(phenByRules == 1 && phenByMLranger == 0 && phenByNB == 0 && phenPRc == 0 && phenMLc == 0){
      ConsensusMat <- "0"
      ConfidenceMat <- "n2"
    } else if(phenByRules == 1 && phenByMLranger == 0 && phenByNB == 0 && phenPRc == 1 && phenMLc == 0){
      ConsensusMat <- "0"
      ConfidenceMat <- "n4"
    } else if(phenByRules == 0 && phenByMLranger == 1 && phenByNB == 0 && phenPRc == 0 && phenMLc == 0){
      ConsensusMat <- "0"
      ConfidenceMat <- "n2"
     } else if(phenByRules == 0 && phenByMLranger == 1 && phenByNB == 0 && phenPRc == 0 && phenMLc == 1){
      ConsensusMat <- "1"
      ConfidenceMat <- "n4"
    } else if(phenByRules == 0 && phenByMLranger == 0 && phenByNB == 0 && phenPRc == 0 && phenMLc == 0){
      ConsensusMat <- "0"
      ConfidenceMat <- "c"
    } else if(phenByRules == 0 && phenByMLranger == 0 && phenByNB == -1){
      ConsensusMat <- "0"
      ConfidenceMat <- "c3"
    } else if(phenByRules == 1 && phenByMLranger == 1 && phenByNB == -1){
      ConsensusMat <- "1"
      ConfidenceMat <- "c3"
    } else if(phenByRules == 1 && phenByMLranger == 0 && phenByNB == -1){
      ConsensusMat <- "0"
      ConfidenceMat <- "n3"
    } else if(phenByRules == 0 && phenByMLranger == 1 && phenByNB == -1){
      ConsensusMat <- "1"
      ConfidenceMat <- "n3"
    } else {
      stop("Unknown consensus!")
    }
      
    results <- rbind(results,data.table("species"=MAG_species,
                                        "genus"=MAG_genus,
                                        "genome"=genome,
                                        "group_size"=CPNGsize,
                                        "subsystem"=curSubsystem,
                                        "phenotype"=curPhenotype,
                                        "phen_PR"=phenByRules,
                                        "phen_ML"=phenByMLranger,
                                        "phen_NG"=phenByNB,
                                        "phen_PR_corrected"=phenPRc,
                                        "phen_ML_corrected"=phenMLc,
                                        "phen_Consensus"=ConsensusMat,
                                        "phen_Confidence"=ConfidenceMat
    ))
    
    resultsConsensusMat <- c(resultsConsensusMat,ConsensusMat)
    resultsConfidenceMat <- c(resultsConfidenceMat,ConfidenceMat)
  }    

  prevSubsystem <- curSubsystem

# Detailed output for each metabolic subsystem and phenotype
  if(nrow(results) > 0){ 
   subsResults <- results[phenotype == curPhenotype]
   subsResults[,subsystem:=NULL]
   subsResults[,phenotype:=NULL]
   addWorksheet(wbFull,curPhenotype)
   data4write[,ID:=NULL]
   writeData(wbFull,sheet=p,cbind(subsResults,data4write),startRow=1)
   conditionalFormatting(wbFull,sheet=p,rows=2:(nrow(subsResults)+1),cols=(ncol(subsResults)+1):(ncol(subsResults)+ncol(data4write)),type="contains",rule="1",yesStyle)
   conditionalFormatting(wbFull,sheet=p,rows=2:(nrow(subsResults)+1),cols=(ncol(subsResults)+1):(ncol(subsResults)+ncol(data4write)),type="contains",rule="0",noStyle)
  }
saveWorkbook(wbFull,file.path(OUTPUT_DIR,"Phenotype_prediction_detailed.xlsx"), overwrite = TRUE)
  
# Short output of Consensus Phenotypes and Confidence Codes
  if(nrow(results) > 0){ 
   tableConsensusMat <- cbind(tableConsensusMat,"tmp"=resultsConsensusMat)
   setnames(tableConsensusMat,"tmp",curPhenotype)
   tableConfidenceMat <- cbind(tableConfidenceMat,"tmp"=resultsConfidenceMat)
   setnames(tableConfidenceMat,"tmp",curPhenotype)
  }
}

tableConsensusMat <- cbind("species"=subsResults$species,
                               "genus"=subsResults$genus,
                               "genome"=subsResults$genome,
                                tableConsensusMat)
write.table(tableConsensusMat,file.path(OUTPUT_DIR,"consensusBPM.txt"),sep='\t',row.names = FALSE,quote = FALSE)

tableConfidenceMat <- cbind("species"=subsResults$species,
                               "genus"=subsResults$genus,
                               "genome"=subsResults$genome,
                                tableConfidenceMat)
write.table(tableConfidenceMat,file.path(OUTPUT_DIR,"confidenceBPM.txt"),sep='\t',row.names = FALSE,quote = FALSE)
