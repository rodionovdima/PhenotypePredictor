library(data.table)
library(openxlsx)
library(caret)
library(yaml)

config <- yaml.load_file("PhenotypePropagator.yaml")

#projectName <- "752_MAG"

warningNoNB_Style <- createStyle(fgFill = "#48C1F1") # blue
errorNoNB_Style <- createStyle(fgFill = "#CC7BB1") #magenta
warningFlag_Style <- createStyle(fgFill = "#F6EA5C") #yellow
errorIsFlag_Style <- createStyle(fgFill = "#E9487E") #red
errorNoFlag_Style <- createStyle(fgFill = "#F49B49") #orange
good_Style <- createStyle(fgFill = "#6BB53A")
noStyle <- createStyle(fontColour = "#9A0511", bgFill = "#FEC7CE")
yesStyle <- createStyle(fontColour = "#09600B", bgFill = "#C7EECF")
redStyle <- createStyle(fgFill = "#E9487E")
orangeStyle <- createStyle(fgFill = "#F49B49")


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

consensusAO <- function(phenRules, phenNB, missingCnt, excessCnt, minGenesPhen1, maxGenesPhen0, phen0Cnt, phen1Cnt, CPNGsize) {
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

#source("/Users/mar/BIO/PROJECTS/MICROBIOME/DIMA/R/get_binary_phenotype.R")
source("get_binary_phenotype.R")

#PREDICTIONS_DIR <- paste0("/Users/mar/BIO/PROJECTS/MICROBIOME/DIMA/GORDON/input/",projectName,"/annotation/")
PREDICTIONS_DIR <- file.path(config$root_dir,config$annotation_dir)
#PARTIAL_DIR <- "/Users/mar/BIO/PROJECTS/MICROBIOME/DIMA/GORDON/input/MAG5/output_partial/"
PARTIAL_DIR <- ""
#MODEL_DIR <- "/Users/mar/BIO/PROJECTS/MICROBIOME/DIMA/ML/Models/"
MODEL_DIR <- file.path(config$root_dir,config$model_dir)
#TRAINING_DIR <- "/Users/mar/BIO/PROJECTS/MICROBIOME/DIMA/Data/trainingExcel25122020/noGenomeID/"
TRAINING_DIR <- file.path(config$root_dir,config$training_dir)
#TRAINING_DIR_GENOMEID <- "/Users/mar/BIO/PROJECTS/MICROBIOME/DIMA/Data/trainingExcel25122020/withGenomeID/"
TRAINING_DIR_GENOMEID <- file.path(config$root_dir,config$training_dir_genomeid)
#OUTPUT_DIR <- paste0("/Users/mar/BIO/PROJECTS/MICROBIOME/DIMA/GORDON/output/",projectName,"/")
OUTPUT_DIR <- file.path(config$root_dir,config$output_dir)

subsRefPATH <- file.path(config$root_dir,config$subs_ref)
subsRef <- read.csv(subsRefPATH,sep='\t')
subsRef <- data.table(subsRef)
subsRef <- subsRef[isDone == 1]
subsRef[,isDone := NULL]

releaseFuncRolesPATH <- file.path(config$root_dir,config$functional_roles)
releaseFuncRoles <- read.csv(releaseFuncRolesPATH,sep='\t',header=FALSE)
releaseFuncRoles <- data.table(releaseFuncRoles)
releaseFuncRoles[,V4 := NULL]
releaseFuncRoles[,V5 := NULL]
releaseFuncRoles[,V6 := NULL]
setnames(releaseFuncRoles,c("subsystem","geneShort","geneFull"))
funcRoles <- copy(releaseFuncRoles)
setnames(funcRoles,"geneShort","geneShortRelease")

patternRolesPATH <- file.path(config$root_dir,config$pattern_roles)
patternRoles <- read.csv(patternRolesPATH,sep=';')
patternRoles <- data.table(patternRoles)

refGroupsPATH <- file.path(config$root_dir,config$groups_dir,"group_table.txt")
refGroups <- read.csv(refGroupsPATH,sep='\t',colClasses = "character")
refGroups <- data.table(refGroups)

wbMAG <- createWorkbook()
wbFull <- createWorkbook()
wbShort <- createWorkbook()

results <- data.table()

prevSubsystem <- '-1'
subsSheetNo <- 1
tableConsensus <- data.table()
tableConsensusDetails <- data.table()
tableConsensusMat <- data.table()
tableConfidenceMat <- data.table()
for(p in 1:nrow(subsRef)){
  curPhenotype <- subsRef[p]$Phenotype
  curSubsystem <- subsRef[p]$Subsystem
  curFilename <- subsRef[p]$Filename
  patternRolesSubsystem <- patternRoles[Subsystem == curSubsystem & FlagDR == 0]

  rolesSubsystem <- funcRoles[subsystem == curSubsystem]
  
  #model <- readRDS(paste0(MODEL_DIR,curPhenotype,"/model_",curPhenotype,"_ranger.rds"))
  model <- readRDS(file.path(MODEL_DIR,curPhenotype,paste0("model_",curPhenotype,"_ranger.rds")))
  #modelXGB <- readRDS(paste0(MODEL_DIR,curPhenotype,"/model_",curPhenotype,"_xgbTree.rds"))
  modelXGB <- readRDS(file.path(MODEL_DIR,curPhenotype,paste0("model_",curPhenotype,"_xgbTree.rds")))
  #trainset <- read.csv(paste0(TRAINING_DIR,curFilename),colClasses = "factor",check.names = FALSE)
  trainset <- read.csv(file.path(TRAINING_DIR,curFilename),colClasses = "factor",check.names = FALSE)
  trainset <- data.table(trainset)
  trainset <- trainset[1]
  trainset[,Y:=NULL]
  for(n in names(trainset)){
    trainset[,(n):="0"]
  }
  
  #trainsetGID <- read.csv(paste0(TRAINING_DIR_GENOMEID,curFilename),colClasses = "factor",check.names = FALSE)
  trainsetGID <- read.csv(file.path(TRAINING_DIR_GENOMEID,curFilename),colClasses = "factor",check.names = FALSE)
  trainsetGID <- data.table(trainsetGID)
 
  files <- list.files(PREDICTIONS_DIR)
  #proteins <- read.csv(paste0(PREDICTIONS_DIR,"CGR_outputs.txt"),sep='\t')
  #proteins <- data.table(proteins)
  #files <- unique(proteins$Organism)
  
  data4write <- data.table()
  resultsConsensus <- c()
  resultsConsensusDetails <- c()
  resultsConsensusMat <- c()
  resultsConfidenceMat <- c()
  for(f in files){
    print(paste0("Genome: ",f,", Subsystem: ", curSubsystem,", Phenotype: ",curPhenotype))
    
    #genome <- f
    genome <- gsub(".faa","",f)
    #genome <- gsub("_b2.out","",f)
    #genome <- gsub("NZ_","",genome)
    
    #if(curPhenotype != "(GlcA)n" || genome != "P10C777_maxbin.105"){
    #  next
    #}
    
    #data <- read.csv(paste0(PREDICTIONS_DIR,f),sep='\t')
    data <- read.csv(file.path(PREDICTIONS_DIR,f),sep='\t')
    data <- data.table(data)
    #data <- proteins[Organism == genome]
    
    if(dir.exists(PARTIAL_DIR)){
      data_partial <- read.csv(paste0(PARTIAL_DIR,genome),sep='\t')
      data_partial <- data.table(data_partial)
      if(nrow(data_partial) > 0){
       data <- rbind(data,data_partial)
      }
    }
    

    genePresence <- merge(rolesSubsystem,data,by.x="geneFull",by.y="Winner")
    genes <- genePresence$geneShortRelease
    
    # Prediction by rules
    phenByRules <- getBinaryPhenotype(curSubsystem,curPhenotype,genes)
    #print(paste0("Status: ",status))
    
    # Prediction by ML ranger
    curset <- copy(trainset)
    for(g in genes){
      if(g %in% names(curset)){
       curset[,(g):="1"]
      }
    }
    set.seed(1)
    phenByMLranger <- predict(model,curset)
    data4write <- rbind(data4write,cbind("ID"=genome,curset))
    
    # Prediction bt ML xgBoost
#    phenByMLxgboost <- predict(modelXGB,curset)
    
    # Prediction by neighbours
    #nbgroup <- refGroups[MAG == paste0(genome,".mod2")]
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
      # { Deleting non-informative genes
      for(colname in patternRolesSubsystem$Name){
        if(colname %in% names(tmp)){
         tmp[,(colname) := NULL]
        }
      }
      # }
      d <- dist(tmp,method="manhattan")
      if(d < mindist){
        mindist <- d
        minind <- g
      }
     }
     closestID <- nbIDs[minind]
     phenByNB <- nbYs[minind]
     
     tmp <- rbind(curset,CPNG[minind])
     # { Deleting non-informative genes
     for(colname in patternRolesSubsystem$Name){
       if(colname %in% names(tmp)){
         tmp[,(colname) := NULL]
       }
     }
     # }
     #missing <- getMissing(curset,CPNG[minind])
     #excessive <- getExcessive(curset,CPNG[minind])
     missing <- getMissing(tmp[1],tmp[2])
     excessive <- getExcessive(tmp[1],tmp[2])
    } 
    
    # Prediction by AO tree
    if(phenByNB != -1) {
     phenAOrules <- consensusAO(phenByRules,phenByNB,missing,excessive,minGenesPhen1,maxGenesPhen0,phen0Count,phen1Count,CPNGsize)
     phenByAOrules <- phenAOrules$cons
     flagAOrules <- phenAOrules$flag
     phenAOml <- consensusAO(phenByMLranger,phenByNB,missing,excessive,minGenesPhen1,maxGenesPhen0,phen0Count,phen1Count,CPNGsize)
     phenByAOml <- phenAOml$cons
     flagAOml <- phenAOml$flag
     if(flagAOrules == 1 | flagAOml == 1){
       AOflag <- 1
     } else {
       AOflag <- 0
     }
    } else {
      phenByAOrules <- -1
      phenByAOml <- -1
      AOflag <- -1
    }
    
    # Filter, RAST

    phenByMLranger <- as.integer(as.character(phenByMLranger))
    phenByNB <- as.integer(as.character(phenByNB))
    if(phenByRules == phenByMLranger && phenByMLranger == phenByNB && phenByNB == phenByAOrules && phenByAOrules == phenByAOml && AOflag == 0){
      errorType <- 'OK'
      errorType2 <- paste0('OK-',phenByRules)
    } else if(phenByRules == phenByMLranger && phenByMLranger == phenByNB && phenByNB == phenByAOrules && phenByAOrules == phenByAOml && AOflag == 1) {
      errorType <- 'warningFlag'
      errorType2 <- 'WF-0'
    } else if(phenByNB==-1 && phenByRules == phenByMLranger) {
      errorType <- 'warningNoNB'
      errorType2 <- paste0('NNB-',phenByRules)
    } else if(phenByNB==-1 && (phenByRules != phenByMLranger)) {
      errorType <- 'errorNoNB'
      errorType2 <- 'ENNB'
    } else if(AOflag==1) {
      errorType <- 'errorIsFlag'
      errorType2 <- paste0('EIF-',phenByRules,phenByMLranger,phenByNB)
    } else if(AOflag==0) {
      errorType <- 'errorNoFlag' 
      errorType2 <- paste0('ENF-',phenByRules,phenByMLranger,phenByNB)
    } else {
      errorType <- 'Unknown'
    }
    
    # Consensus
    Consensus <- "-1"
    ConsensusDetails <- "-1"
    ConsensusMat <- "-1"
    ConfidenceMat <- "-1"
    if(phenByRules == 1 && phenByMLranger == 1 && phenByNB == 1 && phenByAOrules == 1 && phenByAOml == 1){
      Consensus <- "1"
      ConsensusDetails <- "1"
      ConsensusMat <- "1"
      ConfidenceMat <- "c"
      ConsensusFlag <- "0"
    } else if(phenByRules == 1 && phenByMLranger == 0 && phenByNB == 1 && phenByAOrules == 1 && phenByAOml == 1){
      Consensus <- "1"
      ConsensusDetails <- "10111"
      ConsensusMat <- "1"
      ConfidenceMat <- "n2"
      ConsensusFlag <- "2"
    } else if(phenByRules == 1 && phenByMLranger == 0 && phenByNB == 1 && phenByAOrules == 1 && phenByAOml == 0){
      Consensus <- "0"
      ConsensusDetails <- "10110"
      ConsensusMat <- "0"
      ConfidenceMat <- "n4"
      ConsensusFlag <- "4"
    } else if(phenByRules == 0 && phenByMLranger == 1 && phenByNB == 1 && phenByAOrules == 1 && phenByAOml == 1){
      Consensus <- "1"  
      ConsensusDetails <- "01111"
      ConsensusMat <- "1"
      ConfidenceMat <- "n2"
      ConsensusFlag <- "2"
    } else if(phenByRules == 0 && phenByMLranger == 1 && phenByNB == 1 && phenByAOrules == 0 && phenByAOml == 1){
      Consensus <- "1"  
      ConsensusDetails <- "01101"
      ConsensusMat <- "1"
      ConfidenceMat <- "n4"
      ConsensusFlag <- "4"
    } else if(phenByRules == 0 && phenByMLranger == 0 && phenByNB == 1 && phenByAOrules == 1 && phenByAOml == 1){
      Consensus <- "1"  
      ConsensusDetails <- "00111"
      ConsensusMat <- "1"
      ConfidenceMat <- "n1"
      ConsensusFlag <- "1"
    } else if(phenByRules == 0 && phenByMLranger == 0 && phenByNB == 1 && phenByAOrules == 0 && phenByAOml == 1){
      Consensus <- "0"  
      ConsensusDetails <- "00101"
      ConsensusMat <- "0"
      ConfidenceMat <- "n4"
      ConsensusFlag <- "4"
    } else if(phenByRules == 0 && phenByMLranger == 0 && phenByNB == 1 && phenByAOrules == 1 && phenByAOml == 0){
      Consensus <- "0"  
      ConsensusDetails <- "00110"
      ConsensusMat <- "0"
      ConfidenceMat <- "n4"
      ConsensusFlag <- "4"
    } else if(phenByRules == 0 && phenByMLranger == 0 && phenByNB == 1 && phenByAOrules == 0 && phenByAOml == 0){
      Consensus <- "0"  
      ConsensusDetails <- "00100"
      ConsensusMat <- "0"
      ConfidenceMat <- "n0"
      ConsensusFlag <- "0"
    } else if(phenByRules == 1 && phenByMLranger == 1 && phenByNB == 0 && phenByAOrules == 1 && phenByAOml == 1){
      Consensus <- "1"  
      ConsensusDetails <- "11011"
      ConsensusMat <- "1"
      ConfidenceMat <- "n0"
      ConsensusFlag <- "0"
    } else if(phenByRules == 1 && phenByMLranger == 1 && phenByNB == 0 && phenByAOrules == 1 && phenByAOml == 0){
      Consensus <- "1"  
      ConsensusDetails <- "11010"
      ConsensusMat <- "1"
      ConfidenceMat <- "n4"
      ConsensusFlag <- "4"
    } else if(phenByRules == 1 && phenByMLranger == 1 && phenByNB == 0 && phenByAOrules == 0 && phenByAOml == 1){
      Consensus <- "1"  
      ConsensusDetails <- "11001"
      ConsensusMat <- "1"
      ConfidenceMat <- "n4"
      ConsensusFlag <- "4"
    } else if(phenByRules == 1 && phenByMLranger == 1 && phenByNB == 0 && phenByAOrules == 0 && phenByAOml == 0){
      Consensus <- "0"  
      ConsensusDetails <- "11000"
      ConsensusMat <- "0"
      ConfidenceMat <- "n1"
      ConsensusFlag <- "1"
     } else if(phenByRules == 1 && phenByMLranger == 0 && phenByNB == 0 && phenByAOrules == 0 && phenByAOml == 0){
      Consensus <- "0"  
      ConsensusDetails <- "10000"
      ConsensusMat <- "0"
      ConfidenceMat <- "n2"
      ConsensusFlag <- "2"
    } else if(phenByRules == 1 && phenByMLranger == 0 && phenByNB == 0 && phenByAOrules == 1 && phenByAOml == 0){
      Consensus <- "0"  
      ConsensusDetails <- "10010"
      ConsensusMat <- "0"
      ConfidenceMat <- "n4"
      ConsensusFlag <- "4"
    } else if(phenByRules == 0 && phenByMLranger == 1 && phenByNB == 0 && phenByAOrules == 0 && phenByAOml == 0){
      Consensus <- "0"  
      ConsensusDetails <- "01000"
      ConsensusMat <- "0"
      ConfidenceMat <- "n2"
      ConsensusFlag <- "2"
     } else if(phenByRules == 0 && phenByMLranger == 1 && phenByNB == 0 && phenByAOrules == 0 && phenByAOml == 1){
      Consensus <- "1"  
      ConsensusDetails <- "01001"
      ConsensusMat <- "1"
      ConfidenceMat <- "n4"
      ConsensusFlag <- "4"
    } else if(phenByRules == 0 && phenByMLranger == 0 && phenByNB == 0 && phenByAOrules == 0 && phenByAOml == 0){
      Consensus <- "0" 
      ConsensusDetails <- "0"
      ConsensusMat <- "0"
      ConfidenceMat <- "c"
      ConsensusFlag <- "0"
    } else if(phenByRules == 0 && phenByMLranger == 0 && phenByNB == -1){
      Consensus <- "0" 
      ConsensusDetails <- "0"
      ConsensusMat <- "0"
      ConfidenceMat <- "c3"
      ConsensusFlag <- "3"
    } else if(phenByRules == 1 && phenByMLranger == 1 && phenByNB == -1){
      Consensus <- "1" 
      ConsensusDetails <- "1"
      ConsensusMat <- "1"
      ConfidenceMat <- "c3"
      ConsensusFlag <- "3"
    } else if(phenByRules == 1 && phenByMLranger == 0 && phenByNB == -1){
      Consensus <- "0" 
      ConsensusDetails <- "10"
      ConsensusMat <- "0"
      ConfidenceMat <- "n3"
      ConsensusFlag <- "3"
    } else if(phenByRules == 0 && phenByMLranger == 1 && phenByNB == -1){
      Consensus <- "1" 
      ConsensusDetails <- "01"
      ConsensusMat <- "1"
      ConfidenceMat <- "n3"
      ConsensusFlag <- "3"
    } else {
      stop("Unknown consensus!")
    }
      
    results <- rbind(results,data.table("MAG_species"=MAG_species,
                                        "MAG_genus"=MAG_genus,
                                        "genome"=genome,
                                        "CPNGsize"=CPNGsize,
                                        "subsystem"=curSubsystem,
                                        "phenotype"=curPhenotype,
                                        "phenByRules"=phenByRules,
                                        "phenByMLranger"=phenByMLranger,
                                        "phenByNB"=phenByNB,
                                        "phenByAOrules"=phenByAOrules,
                                        "phenByAOml"=phenByAOml,
                                        #"AOflag"=AOflag,
                                        "phenConsensus"=Consensus,
                                        "phenFlag"=ConsensusFlag,
                                        #"errorType"=errorType,
                                        #"errorType2"=errorType2,
                                        "closestID"=closestID,
                                        "missing"=missing,
                                        "excessive"=excessive,
                                        "phen0Count"=phen0Count,
                                        "phen1Count"=phen1Count,
                                        "maxGenesPhen0"=maxGenesPhen0,
                                        "minGenesPhen1"=minGenesPhen1
    ))
    
    resultsConsensus <- c(resultsConsensus,Consensus)
    resultsConsensusDetails <- c(resultsConsensusDetails,ConsensusDetails)
    resultsConsensusMat <- c(resultsConsensusMat,ConsensusMat)
    resultsConfidenceMat <- c(resultsConfidenceMat,ConfidenceMat)
  }    
  curSubsystem30char <- substr(curSubsystem,1,30)
  if(prevSubsystem != curSubsystem){
    addWorksheet(wbMAG,curSubsystem30char)
    writeData(wbMAG,sheet=subsSheetNo,curSubsystem)
    writeData(wbMAG,sheet=subsSheetNo,data4write,startRow=2)
    subsSheetNo <- subsSheetNo + 1
  }
  prevSubsystem <- curSubsystem
  
  # Short output
  if(nrow(results) > 0){ 
   subsResults <- results[phenotype == curPhenotype]
   subsResults[,subsystem:=NULL]
   subsResults[,phenotype:=NULL]
   subsResults <- subsResults[errorType != 'OK']
   addWorksheet(wbShort,curPhenotype)
   #writeData(wbShort,sheet=p,paste(curSubsystem,' - ',curPhenotype))
   writeData(wbShort,sheet=p,subsResults,startRow=1)
   # styleCell <- good_Style
   # for(i in 1:nrow(subsResults)){
   #  et <- subsResults[i]$errorType
   #  if(et == "warningNoNB"){
   #    styleCell <- warningNoNB_Style
   #  } else if (et == "errorNoNB") {
   #    styleCell <- errorNoNB_Style
   #  } else if (et == "warningFlag") {
   #    styleCell <- warningFlag_Style
   #  } else if (et == "errorIsFlag") {
   #    styleCell <- errorIsFlag_Style
   #  } else if (et == "errorNoFlag") {
   #    styleCell <- errorNoFlag_Style
   #  }
   #  addStyle(wbShort,sheet=p,styleCell,rows=(i+2),cols=14:15)
   # }
  }
   
  # Full output
  if(nrow(results) > 0){ 
   subsResults <- results[phenotype == curPhenotype]
   subsResults[,subsystem:=NULL]
   subsResults[,phenotype:=NULL]
   addWorksheet(wbFull,curPhenotype)
   #writeData(wbFull,sheet=p,paste(curSubsystem,' - ',curPhenotype))
   data4write[,ID:=NULL]
   writeData(wbFull,sheet=p,cbind(subsResults,data4write),startRow=1)
   conditionalFormatting(wbFull,sheet=p,rows=2:(nrow(subsResults)+1),cols=(ncol(subsResults)+1):(ncol(subsResults)+ncol(data4write)),type="contains",rule="1",yesStyle)
   conditionalFormatting(wbFull,sheet=p,rows=2:(nrow(subsResults)+1),cols=(ncol(subsResults)+1):(ncol(subsResults)+ncol(data4write)),type="contains",rule="0",noStyle)
   # for(i in 1:nrow(subsResults)){
   #   et <- subsResults[i]$errorType
   #   if(et == "warningNoNB"){
   #     styleCell <- warningNoNB_Style
   #   } else if (et == "errorNoNB") {
   #     styleCell <- errorNoNB_Style
   #   } else if (et == "warningFlag") {
   #     styleCell <- warningFlag_Style
   #   } else if (et == "errorIsFlag") {
   #     styleCell <- errorIsFlag_Style
   #   } else if (et == "errorNoFlag") {
   #     styleCell <- errorNoFlag_Style
   #   } else if (et == "OK"){
   #     styleCell <- good_Style
   #   }
   #   addStyle(wbFull,sheet=p,styleCell,rows=(i+1),cols=14:15)
   # }  
  }
  
  # Consensus
  if(nrow(results) > 0){ 
   tableConsensus <- cbind(tableConsensus, "tmp"=resultsConsensus)
   setnames(tableConsensus,"tmp",curPhenotype)
   tableConsensusDetails <- cbind(tableConsensusDetails, "tmp"=resultsConsensusDetails)
   setnames(tableConsensusDetails,"tmp",curPhenotype)
   tableConsensusMat <- cbind(tableConsensusMat,"tmp"=resultsConsensusMat)
   setnames(tableConsensusMat,"tmp",curPhenotype)
   tableConfidenceMat <- cbind(tableConfidenceMat,"tmp"=resultsConfidenceMat)
   setnames(tableConfidenceMat,"tmp",curPhenotype)
  }
}

#saveWorkbook(wbMAG,paste0(OUTPUT_DIR,"MAGgenes.xlsx"), overwrite = TRUE)
saveWorkbook(wbMAG,file.path(OUTPUT_DIR,"MAGgenes.xlsx"), overwrite = TRUE)
#saveWorkbook(wbShort,paste0(OUTPUT_DIR,"MAGpredictionsShort.xlsx"), overwrite = TRUE)
saveWorkbook(wbShort,file.path(OUTPUT_DIR,"MAGpredictionsShort.xlsx"), overwrite = TRUE)
#saveWorkbook(wbFull,paste0(OUTPUT_DIR,"MAGpredictionsFull.xlsx"), overwrite = TRUE)
saveWorkbook(wbFull,file.path(OUTPUT_DIR,"MAGpredictionsFull.xlsx"), overwrite = TRUE)

statTableRef <- data.table()
statTableRef <- rbind(statTableRef,data.table("No"=1,"phenByRules"=0,"phenByMLranger"=0,"phenByNB"=0,"phenByAOrules"=0,"phenByAOml"=0,"Consensus"=0,"Flag"=0))
statTableRef <- rbind(statTableRef,data.table("No"=2,"phenByRules"=1,"phenByMLranger"=1,"phenByNB"=1,"phenByAOrules"=1,"phenByAOml"=1,"Consensus"=1,"Flag"=0))
statTableRef <- rbind(statTableRef,data.table("No"=3,"phenByRules"=0,"phenByMLranger"=0,"phenByNB"=1,"phenByAOrules"=0,"phenByAOml"=0,"Consensus"=0,"Flag"=0))
statTableRef <- rbind(statTableRef,data.table("No"=4,"phenByRules"=1,"phenByMLranger"=1,"phenByNB"=0,"phenByAOrules"=1,"phenByAOml"=1,"Consensus"=1,"Flag"=0))
statTableRef <- rbind(statTableRef,data.table("No"=5,"phenByRules"=0,"phenByMLranger"=0,"phenByNB"=1,"phenByAOrules"=1,"phenByAOml"=1,"Consensus"=1,"Flag"=1))
statTableRef <- rbind(statTableRef,data.table("No"=6,"phenByRules"=1,"phenByMLranger"=0,"phenByNB"=1,"phenByAOrules"=1,"phenByAOml"=1,"Consensus"=1,"Flag"=2))
statTableRef <- rbind(statTableRef,data.table("No"=7,"phenByRules"=0,"phenByMLranger"=1,"phenByNB"=1,"phenByAOrules"=1,"phenByAOml"=1,"Consensus"=1,"Flag"=2))
statTableRef <- rbind(statTableRef,data.table("No"=8,"phenByRules"=1,"phenByMLranger"=1,"phenByNB"=0,"phenByAOrules"=0,"phenByAOml"=0,"Consensus"=0,"Flag"=1))
statTableRef <- rbind(statTableRef,data.table("No"=9,"phenByRules"=1,"phenByMLranger"=0,"phenByNB"=0,"phenByAOrules"=1,"phenByAOml"=0,"Consensus"=0,"Flag"=4))
statTableRef <- rbind(statTableRef,data.table("No"=10,"phenByRules"=1,"phenByMLranger"=0,"phenByNB"=0,"phenByAOrules"=0,"phenByAOml"=0,"Consensus"=0,"Flag"=2))
statTableRef <- rbind(statTableRef,data.table("No"=11,"phenByRules"=0,"phenByMLranger"=1,"phenByNB"=1,"phenByAOrules"=0,"phenByAOml"=1,"Consensus"=1,"Flag"=4))
statTableRef <- rbind(statTableRef,data.table("No"=12,"phenByRules"=1,"phenByMLranger"=0,"phenByNB"=1,"phenByAOrules"=1,"phenByAOml"=0,"Consensus"=0,"Flag"=4))
statTableRef <- rbind(statTableRef,data.table("No"=13,"phenByRules"=0,"phenByMLranger"=1,"phenByNB"=0,"phenByAOrules"=0,"phenByAOml"=1,"Consensus"=1,"Flag"=4))
statTableRef <- rbind(statTableRef,data.table("No"=14,"phenByRules"=0,"phenByMLranger"=1,"phenByNB"=0,"phenByAOrules"=0,"phenByAOml"=0,"Consensus"=0,"Flag"=2))
statTableRef <- rbind(statTableRef,data.table("No"=15,"phenByRules"=0,"phenByMLranger"=0,"phenByNB"=1,"phenByAOrules"=0,"phenByAOml"=1,"Consensus"=0,"Flag"=4))
statTableRef <- rbind(statTableRef,data.table("No"=16,"phenByRules"=0,"phenByMLranger"=0,"phenByNB"=1,"phenByAOrules"=1,"phenByAOml"=0,"Consensus"=0,"Flag"=4))
statTableRef <- rbind(statTableRef,data.table("No"=17,"phenByRules"=1,"phenByMLranger"=1,"phenByNB"=0,"phenByAOrules"=1,"phenByAOml"=0,"Consensus"=1,"Flag"=4))
statTableRef <- rbind(statTableRef,data.table("No"=18,"phenByRules"=1,"phenByMLranger"=1,"phenByNB"=0,"phenByAOrules"=0,"phenByAOml"=1,"Consensus"=1,"Flag"=4))
statTableRef <- rbind(statTableRef,data.table("No"=19,"phenByRules"=0,"phenByMLranger"=0,"phenByNB"=-1,"phenByAOrules"=-1,"phenByAOml"=-1,"Consensus"=0,"Flag"=3))
statTableRef <- rbind(statTableRef,data.table("No"=20,"phenByRules"=1,"phenByMLranger"=1,"phenByNB"=-1,"phenByAOrules"=-1,"phenByAOml"=-1,"Consensus"=1,"Flag"=3))
statTableRef <- rbind(statTableRef,data.table("No"=21,"phenByRules"=1,"phenByMLranger"=0,"phenByNB"=-1,"phenByAOrules"=-1,"phenByAOml"=-1,"Consensus"=0,"Flag"=3))
statTableRef <- rbind(statTableRef,data.table("No"=22,"phenByRules"=0,"phenByMLranger"=1,"phenByNB"=-1,"phenByAOrules"=-1,"phenByAOml"=-1,"Consensus"=1,"Flag"=3))

resStat <- results[,.("Count"=.N),by=.(phenByRules,phenByMLranger,phenByNB,phenByAOrules,phenByAOml,phenotype)]
phenotypeList <- unique(resStat$phenotype)
statTable <- dcast(resStat, phenByRules+phenByMLranger+phenByNB+phenByAOrules+phenByAOml ~ phenotype)
statTable <- merge(statTableRef,statTable,by=c("phenByRules","phenByMLranger","phenByNB","phenByAOrules","phenByAOml"))
statTable[is.na(statTable)] <- 0
statTable[, Total := rowSums(.SD, na.rm=T), .SDcols=phenotypeList]
statTable <- statTable[order(No)]
#write.table(statTable,paste0(OUTPUT_DIR,"stat_table.txt"),sep='\t',row.names = FALSE,quote = FALSE)
write.table(statTable,file.path(OUTPUT_DIR,"stat_table.txt"),sep='\t',row.names = FALSE,quote = FALSE)

tableConsensus <- cbind("MAG_species"=subsResults$MAG_species,
                        "MAG_genus"=subsResults$MAG_genus,
                        "genome"=subsResults$genome,
                        "filter"=subsResults$filter,
                        "RAST"=subsResults$RAST,
                        tableConsensus)
#write.table(tableConsensus,paste0(OUTPUT_DIR,"consensus.txt"),sep='\t',row.names = FALSE,quote = FALSE)
write.table(tableConsensus,file.path(OUTPUT_DIR,"consensus.txt"),sep='\t',row.names = FALSE,quote = FALSE)

tableConsensusDetails <- cbind("MAG_species"=subsResults$MAG_species,
                               "MAG_genus"=subsResults$MAG_genus,
                               "genome"=subsResults$genome,
                               "filter"=subsResults$filter,
                               "RAST"=subsResults$RAST,
                               tableConsensusDetails)
#write.table(tableConsensusDetails,"/Users/mar/BIO/PROJECTS/MICROBIOME/DIMA/GORDON/output/MAG2/consensusDetails.txt",sep='\t',row.names = FALSE,quote = FALSE)
wbConsensusDetails <- createWorkbook()
addWorksheet(wbConsensusDetails,"ConsensusDetails")
writeData(wbConsensusDetails,sheet=1,tableConsensusDetails,startRow=1)
#saveWorkbook(wbConsensusDetails,paste0(OUTPUT_DIR,"consensusDetails.xlsx"),overwrite = TRUE)
saveWorkbook(wbConsensusDetails,file.path(OUTPUT_DIR,"consensusDetails.xlsx"),overwrite = TRUE)

tableConsensusMat <- cbind("MAG_species"=subsResults$MAG_species,
                               "MAG_genus"=subsResults$MAG_genus,
                               "genome"=subsResults$genome,
                               "filter"=subsResults$filter,
                               "RAST"=subsResults$RAST,
                               tableConsensusMat)
#write.table(tableConsensusMat,paste0(OUTPUT_DIR,"consensusBPM.txt"),sep='\t',row.names = FALSE,quote = FALSE)
write.table(tableConsensusMat,file.path(OUTPUT_DIR,"consensusBPM.txt"),sep='\t',row.names = FALSE,quote = FALSE)

tableConfidenceMat <- cbind("MAG_species"=subsResults$MAG_species,
                               "MAG_genus"=subsResults$MAG_genus,
                               "genome"=subsResults$genome,
                               "filter"=subsResults$filter,
                               "RAST"=subsResults$RAST,
                               tableConfidenceMat)
#write.table(tableConfidenceMat,paste0(OUTPUT_DIR,"confidenceBPM.txt"),sep='\t',row.names = FALSE,quote = FALSE)
write.table(tableConfidenceMat,file.path(OUTPUT_DIR,"confidenceBPM.txt"),sep='\t',row.names = FALSE,quote = FALSE)

wbMat <- createWorkbook()
addWorksheet(wbMat,"Consensus")
addWorksheet(wbMat,"Confidence")
writeData(wbMat,sheet=1,tableConsensusMat,startRow=1)
writeData(wbMat,sheet=2,tableConfidenceMat,startRow=1)
for(i in 1:nrow(tableConfidenceMat)){
  for(j in 6:ncol(tableConfidenceMat)){
    if(tableConfidenceMat[i,..j] %in% c('n1','n2','n3')){
     addStyle(wbMat,sheet=1,redStyle,rows=(i+1),cols=j)
    } else if(tableConfidenceMat[i,..j]=='n0') {
      addStyle(wbMat,sheet=1,orangeStyle,rows=(i+1),cols=j)
    }
  }
}
#saveWorkbook(wbMat,paste0(OUTPUT_DIR,"consensusBPM.xlsx"), overwrite = TRUE)
saveWorkbook(wbMat,file.path(OUTPUT_DIR,"consensusBPM.xlsx"), overwrite = TRUE)

#write.table(results,paste0(OUTPUT_DIR,"results.txt"),sep='\t',row.names = FALSE,quote = FALSE)
write.table(results,file.path(OUTPUT_DIR,"results.txt"),sep='\t',row.names = FALSE,quote = FALSE)
