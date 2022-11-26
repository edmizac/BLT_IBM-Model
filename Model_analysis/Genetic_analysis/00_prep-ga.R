# Script name: 00_prep-ga.R
# Script purpose: 

# Date created: 2022-11-25d
# Author: Eduardo Zanette

## Notes --------------------------- 
#
#

## Options -------------------------
# (plotting, memory limit, decimal digits)
# 

## Packages -------------------------
library("here")
library("dplyr")
library("nlrx")
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


# Empirical data for criteval function:
variable.table <- read.csv(here("Data", "Validation-table.csv"),
                           sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  dplyr::mutate(group = recode(group, "Guarei" = "Guareí")) # only to match those of the NetLogo model



# Step 1: Create nl object
if(Sys.info()[["nodename"]] == "simul02") {
  netlogopath <- file.path("/home/rbialozyt/Software/NetLogo 6.2.0")
  modelpath <- "/home/rbialozyt/BLT_IBM-Model/Model_development/BLT_model_v1.1.nlogo"
  outpath <- "/home/rbialozyt/BLT_IBM-Model/Model_analysis/Sensitivity-analysis"
}
if(Sys.info()[["nodename"]] == "PC146") {
  netlogopath <- file.path("/opt/netlogo_620")
  modelpath <- paste0("/home/rbialozyt/ownCloud-Forst/Projektideen/FAPESP_Project_Eduardo/"
                      , "BLT_IBM-Model/Model_development/BLT_model_v1.1.nlogo")
  outpath <-  paste0("/home/rbialozyt/ownCloud-Forst/Projektideen/FAPESP_Project_Eduardo/"
                     , "BLT_IBM-Model/Model_analysis/Sensitivity-analysis")
}
if(Sys.info()[["nodename"]] == "DESKTOP-R12V3D6") {
  netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
  modelpath <- here("Model_development", "BLT_model_v1.1.nlogo")
  outpath <- here("Model_analysis", "Sensitivity-analysis", "v1.1_November2022", "temp")
}


nl <- nl(nlversion = "6.2.2",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

nlogo_model_param <- report_model_parameters(nl)
nlogo_model_param


## Step 2: Attach an experiment   -------------------------
expname <- "GA_algorithm"

step_model_param <- "true" # velocity parameters are setted inside the model. Change this when velocity is summarized and inclued in the parameter table
gtt_param <- "true" # gtt parameters are setted inside the model. Change this when velocity is summarized and inclued in the parameter table
p_forage_param <- "true" # p_foraging parameter is setted inside the model. Change this when velocity is summarized and inclued in the parameter table 
feedingbout <- "false" # previous sensitivity analysis showed that this does not matter, at least for Guareí


# Attach to nl object
nl@experiment <- experiment(expname = expname,
                            outpath = outpath,
                            repetition = 1, # number of repetitions with the same seed (use repetition = 1)
                            tickmetrics = "false", # "true" for every tick, "false" for metrics only in the end of the simulation
                            idsetup = "setup",
                            idgo = "go",
                            runtime = 2000, #(if = 0 or NA_integer_, define stopcond_)
                            stopcond= "day > no_days", # reporter that returns TRUE
                            evalticks = NA_integer_, # NA_integer_ = measures each tick. Only applied if tickmetrics = TRUE
                            idfinal = "r:stop", # for making r NetLogo extension to work: https://cran.r-project.org/web/packages/nlrx/vignettes/furthernotes.html
                            # reporters:
                            metrics = c(
                              "count feeding-trees with [visitations > 0] / count feeding trees" # the best set of parameters should make tamarins visit all the observed trees in the period
                            ),
                            metrics.turtles = list("monkeys" = c(
                              "energy",      # final energy                # the best set of parameters should make tamarins viable in energetic terms
                              "DPL_d",       # DPL is set to 0 everyday    # the best set of parameters should reproduce the observed DPL
                              "KDE_95",      # final value                 # the best set of parameters should reproduce the observed home range
                              "KDE_50",      # final value                 # the best set of parameters should reproduce the observed core area
                              "p_feeding",   # final value                 # the best set of parameters should optimize the activity budget
                              "p_foraging",  # final value                 # the best set of parameters should optimize the activity budget
                              "p_traveling", # final value                 # the best set of parameters should optimize the activity budget
                              "p_resting"    # final value                 # the best set of parameters should optimize the activity budget
                            ),

                            ), # "who" "color"
                            variables = list(
                              
                              ### DO NOT SPECIFY STEPS:
                              
                              # energy
                              "energy-from-fruits" = list(min=1, max = 20),
                              'energy-from-prey' = list(min=1, max=20),
                              "energy-loss-traveling" = list(min=-10, max = 0), #, step = 2
                              "energy-loss-foraging" = list(min=-10, max = 0),
                              "energy-loss-resting" = list(min=-10, max = 0),
                              
                              "start-energy" = list(min=100, max=2000),
                              "energy_level_1" = list(min=100, max=2000),
                              "energy_level_2" = list(min=100, max=2000),
                              
                              "energy-from-seeds" = 4,# ?
                              "energy-from-prey" = 4,
                              "energy-loss-traveling" = -1.6,
                              "energy-loss-foraging" = -2,
                              "energy-loss-resting" = -1.9
                              
                              
                              # Seed dispersal
                              # "gut_transit_time_val" = 15,
                              
                              
                              # movement
                              # "travel_speed_val" = list(values=seq(0.3, 1, by = 0.1))  # only when step-model-param? = 'false'
                              # "foraging_speed_val" = list(min= 0, max = 1, step = 2)   # only when step-model-param? = 'false'
                              # "duration" = list(min=0, max = 10, step = 2),
                              
                              # memory
                              # "step_forget" = list(min=0, max = 150, step = 10, qfun="qunif")
                              # "visual" = list(min=0, max = 3, step = 1)
                              
                              # others
                              # 'species_time_val' = list(min = 1, max = 6, step = 2)
                            ),
                            
                            constants = list(
                              
                              ### "true" for output related stuff
                              # "output-files?" = "false", #THIS IS VERY IMPORTANT (csv files)
                              # "output-print?" = "false", #true to output in the console
                              "USER" = "\"Eduardo\"",
                              'feedingbout-on?' = feedingbout,        # uses empirical values of time spent feeding on each tree species or a random one
                              "step-model-param?" = step_model_param, # uses observed mean step length and 75q turning angles
                              "gtt-param?"= gtt_param,                # uses mean + sd of GTT for seed dispersal
                              "p-forage-param?" = p_forage_param,     # uses empirical probabilities of foraging while traveling (p_foraging = p_foraging + p_traveling)
                              
                              # "print-step?" = "false",
                              # 'export-png'= "false",
                              # "show-energy?" = "false",
                              # "show-path?" = "false",
                              # "all-slp-trees?" = "false",
                              # "path-color-by-day?" = "false",
                              
                              ### resource scenario
                              "study_area" = area_run, #"\"Guareí\"",
                              'feeding-trees-scenario' = month_run, #"\"May\"",
                              'no_days' = no_days_run, # DON'T TRY no_days = 1
                              'simulation-time' = simultime_run
                              # 'feeding-trees?' = "true",
                              # 'sleeping-trees?' = "true",
                              # 'sleeping-trees-scenario' = "\"empirical\"",
                              # 'empirical-trees-choice' = "\"closest\"",
                              
                              ### memory
                              # 'duration' = 3,
                              # 'visual' = 2,
                              # "step_forget" = 130,
                              
                              ### energy
                              # 'start-energy' = 70,
                              # "energy_level_1" = 80,
                              # "energy_level_2" = 150,
                              # "energy-from-seeds" = 4,# ?
                              # "energy-from-prey" = 4,
                              # "energy-loss-traveling" = -1.6,
                              # "energy-loss-foraging" = -2,
                              # "energy-loss-resting" = -1.9,
                              # "gut_transit_time_val" = 15,
                              # "n_seeds_hatched" = 1,
                              
                              ### movement
                              # travel_speed = 1
                              
                            )
)


## Create evaluation criteria function -------------------------

# Example nl object:
nl <- readRDS(here("Model_analysis", "Sensitivity-analysis",
                   "v1.1_November2022", "temp", "v1.1_Taquara_Jan_simple1199731059_tempRDS.Rdata"))

critfun <- function(nl) {
  # extract values
  db <- unnest_simoutput(nl)
  
  db <- db %>% 
    dplyr::filter(breed == "monkeys")
  
  energy <- db %>%  dplyr::select("energy") %>% unlist() %>% as.vector()
  
  KDE95 <- db %>%  dplyr::select("KDE95") %>% unlist() %>% as.vector()
  KDE50 <- db %>%  dplyr::select("KDE50") %>% unlist() %>% as.vector()
  p_feeding <- db %>%  dplyr::select("p_feeding") %>% unlist() %>% as.vector()
  p_foraging <- db %>%  dplyr::select("p_foraging") %>% unlist() %>% as.vector()
  p_traveling <- db %>%  dplyr::select("p_traveling") %>% unlist() %>% as.vector()
  p_resting <- db %>%  dplyr::select("p_resting") %>% unlist() %>% as.vector()
  
  DPL_mean <- db$DPL_d
  DPL_mean <- DPL_mean %>%
    str_replace_all(., c("\\[" = "", "\\]" = "")) %>%
    str_split(pattern = "_") %>%  #, simplify = TRUE) %>%
    purrr::map(as.numeric, na.rm = TRUE) %>% 
    # purrr::map(round, 2) %>%
    map_dbl(mean, na.rm=TRUE)
  db$DPL_mean <- DPL_mean
  
  
  visit_count <- db$visits
  
  crit <- lsm$value
  return(crit)
}

  


## Step 3: Attach a simulation design   -------------------------

nl@simdesign <- simdesign_GenSA(nl, 
                                evalcrit = 1, 
                                nseeds = 1, 
                                control=list(maxit = 20))


