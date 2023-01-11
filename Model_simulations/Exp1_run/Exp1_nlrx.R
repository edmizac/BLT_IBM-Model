## Header --------------------------
# Script name: Exp1_nlrx
# Script purpose: Run the v1.1 Model procedure with the imported build-forest 
# landscapes (Experiment 1)
#
# Date created: 2023-01-10d
# Author: Eduardo Zanette

## Notes --------------------------- 

## Packages -------------------------
library("nlrx")
# library("here")
# library("dplyr")
library("stringr")
library("progressr")
library("future")
library("tictoc")


## Config cores
ncores <- parallel::detectCores()

## Java memory:
if(Sys.getenv("JAVA_HOME") == "") {
  if(Sys.info()[["sysname"]] == "Linux") {
    Sys.setenv(JAVA_HOME = "/usr/lib/jvm/java-11-openjdk-amd64")
    unixtools::set.tempdir(".")
  } else {
    Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk1.8.0_321")
  }
}

## ---------------------------

# Step 1: Create nl object
if(Sys.info()[["nodename"]] == "DESKTOP-R12V3D6") {
  path <- "D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest/"
  netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
  modelpath <- paste0(path, "build_forest_EMZ_2023-01-07d.nlogo")
  outpath <- paste0(path, "Experiment1/")
  user_scp = "\"Eduardo\""
}
if(Sys.info()[["nodename"]] == "PC9") { # LEEC
  # path <- "D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest/"
  netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
  # modelpath <- here("Model_development", "BLT_model_v1.1.nlogo")
  # outpath <- here("Model_analysis", "Genetic_analysis", "temp")
  Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8.0_351")
  user_scp = "\"LEEC\""
}


nl <- nl(nlversion = "6.2.2",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

nlogo_model_param <- report_model_parameters(nl)
nlogo_model_param


## List files patch generated csv files ------
pathfiles <- "D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest/Experiment1/" #batch1/

files_forests <- list.files(pathfiles, pattern = ".csv")
files_forests <- paste0(pathfiles, "/", files_forests)

length(files_forests) # number of patches * home ranges (10 reps) * number of clumped/random/ordered resourcers

#### Simple design experiment ( = one go button, no varibles) ####

## Step 2: Attach an experiment
expname <- "Exp1_Batch2"

set.seed(1234)
