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
                              "p_resting",    # final value                 # the best set of parameters should optimize the activity budget
                              "step_length_mean",    # besides the parameterization, agents interactions make the observed step length and turning angles change
                              "step_length_sd",      # besides the parameterization, agents interactions make the observed step length and turning angles change
                              # "turn_ang_mean",     # this one is quite consistent, so I don't think this one is necessary
                              "turn_ang_sd",          # this one might be interesting though
                              
                              # additional movement variables
                              "MR",               # movement rate (MR) is used to predict SDD by primates: http://doi.wiley.com/10.1002/ajp.22659
                              # "MSD",              # other modelling studies have used this one (https://doi.org/10.3390/ani12182412.), but I believe it is very similar to MR
                              # "intensity_use",    # bether than MSD in my oppinion: read about it in: https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en
                              "PT",               # path twisting is used by Fuzessy et al 2017 to predict SDD among primates: http://doi.wiley.com/10.1002/ajp.22659
                              # "straightness",   # straightness and sinuosity are slightlty different in terms of properties (https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en) and they were not tested as predictors of SDD, so i'm not using them
                              # "sinuosity"       # straightness and sinuosity are slightlty different in terms of properties (https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en) and they were not tested as predictors of SDD, so i'm not using them
                            ), 

                            ), # "who" "color"
                            variables = list(
                              
                              ### DO NOT SPECIFY STEPS:
                              
                              # energy
                              "energy-from-fruits" = list(min=1, max = 300),
                              'energy-from-prey' = list(min=1, max=300),
                              "energy-loss-traveling" = list(min=-100, max = 0), #, step = 2
                              "energy-loss-foraging" = list(min=-100, max = 0),
                              "energy-loss-resting" = list(min=-100, max = 0),
                              
                              "start-energy" = list(min=100, max=2000),
                              "energy_level_1" = list(min=100, max=2000),
                              "energy_level_2" = list(min=100, max=2000),
                              
                              
                              # 4. Movement
                              # "step_len_travel" = list(min= 0, max = 100)         # only when step-model-param? = 'false'
                              # "step_len_forage" = list(min= 0, max = 100)         # only when step-model-param? = 'false'
                              # "max_rel_ang_travel_75q" = list(min= 0, max= 180)   # only when step-model-param? = 'false'
                              # "max_rel_ang_forage_75q" = list(min= 0, max= 180)   # only when step-model-param? = 'false'
                              
                              # "p_foraging_while_traveling" = list(min= 0, max= 1) # only when p-forage-param? = 'false'
                              
                              # memory
                              "step_forget" = list(min=3, max = 150),
                              "visual" = list(min=0, max = 3)
                              # 'prop_trees_to_reset_memory" = list(min=2, max=5)   # I don't think this one is needed
                              
                              # 5. Feeding bout
                              # 'species_time' = list(min = 1, max = 10)        # only when "feedingbout-on?" = 'true'
                              # 'duration' = list(min = 1, max = 10)            # only when "feedingbout-on?" = 'true'
                              
                              # 6. Seed dispersal
                              # "gut_transit_time_val" = 15,    # this won't be optimized as it is an emerging pattern
                              # "n_seeds_hatched" = 1           # this won't be optimized as it is an emerging pattern
                            ),
                            
                            constants = list(
                              
                              "study_area" = "\"Taquara\"",           # we are optimizing with Taquara as it is the most natural condition
                              "feeding-trees-scenario" = "\"Jan\"",   # we are optimizing with Taquara as it is the most natural condition
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



# Empirical data for criteval function:
values_ga <- read.csv(here("Data", "Validation-table.csv"),
                      sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  dplyr::mutate(group = recode(group, "Guarei" = "Guareí")) # only to match those of the NetLogo model

# Movement variables:
values_ga_mv <-  read.csv(here("Data", "Movement", "Curated", "Validation", "Siminputrow_MR-PT_by-day.csv"),
                          sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  dplyr::mutate(group = recode(group, "Guarei" = "Guareí")) # only to match those of the NetLogo model

## Create evaluation criteria function -------------------------

# get optimized (observed) values
KDE95_obs <- values_ga %>% dplyr::filter(group == "Taquara" | "id_month" == "Jan") %>%
  dplyr::select(KDE95) %>% unlist() %>% as.vector()
KDE50_obs <- values_ga %>% dplyr::filter(group == "Taquara" | "id_month" == "Jan") %>%
  dplyr::select(KDE50) %>% unlist() %>% as.vector()

p_feeding_obs <- values_ga %>%  dplyr::filter(group == "Taquara" | "id_month" == "Jan") %>%
  dplyr::select("Frugivory_perc_behavior_mean") %>% unlist() %>% as.vector()
p_foraging_obs <- values_ga %>%  dplyr::filter(group == "Taquara" | "id_month" == "Jan") %>%
  dplyr::select("Foraging_perc_behavior_mean") %>% unlist() %>% as.vector()
p_traveling_obs <- values_ga %>%  dplyr::filter(group == "Taquara" | "id_month" == "Jan") %>%
  dplyr::select("Travel_perc_behavior_mean") %>% unlist() %>% as.vector()
p_resting_obs <- values_ga %>%  dplyr::filter(group == "Taquara" | "id_month" == "Jan") %>%
  dplyr::select("Resting_perc_behavior_mean") %>% unlist() %>% as.vector()
# obs: they don't sum up to 1
p_feeding_obs + p_foraging_obs + p_traveling_obs + p_resting_obs


MR_obs <- values_ga_mv %>% dplyr::filter(group == "Taquara" | "id_month" == "Jan") %>%
  dplyr::select("MR") %>% unlist() %>% as.vector()
PT_obs <- values_ga_mv %>% dplyr::filter(group == "Taquara" | "id_month" == "Jan") %>%
  dplyr::select("PT") %>% unlist() %>% as.vector()

# tamarins visit all the observed trees, but they don't usually visit all of them in the model (might be related to memory)
param_table <- read.csv(here("Data", "Parameter_table.csv"),
                       sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  dplyr::mutate(group = recode(group, "Guarei" = "Guareí")) # only to match those of the NetLogo model

visits_obs <- param_table %>% 
  dplyr::filter(group == "Taquara" | "id_month" == "Jan") %>%
  dplyr::select(n_trees_Frugivory) %>% unlist() %>% as.vector()

# # Still have to calculate them from empirical data
# step_length_mean_obs <- param_table %>% 
#   dplyr::filter(group == "Taquara" | "id_month" == "Jan") %>%
#   dplyr::select(step_length_mean_obs) %>% unlist() %>% as.vector()
# 
# step_length_sd <- param_table %>% 
#   dplyr::filter(group == "Taquara" | "id_month" == "Jan") %>%
#   dplyr::select(step_length_sd) %>% unlist() %>% as.vector()
# 
# turn_ang_sd <- param_table %>% 
#   dplyr::filter(group == "Taquara" | "id_month" == "Jan") %>%
#   dplyr::select(turn_ang_sd) %>% unlist() %>% as.vector()


# Create critfun (fitness function)
critfun <- function(nl) {
  
  # extract values from run from example nl object:
  # nl <- readRDS(here("Model_analysis", "Genetic_analysis", "v1.1_Taquara_Jan_simple-609482361_tempRDS.Rdata"))
  nl <- readRDS(here("Model_analysis", "Sensitivity-analysis", "v1.1_November2022", "temp",
                     "v1.1_Guareí_Aug_simple-866006203_tempRDS.Rdata"))
  
  db <- unnest_simoutput(nl)
  
  db <- db %>% 
    dplyr::filter(breed == "monkeys") %>% 
    mutate(
      KDE_50 = KDE_50 / 10000,
      KDE_95 = KDE_95 / 10000
    )
  
  visits <- db$n_visited_trees
  
  energy <- db %>%  dplyr::select("energy") %>% unlist() %>% as.vector()
  energy_obs <- db %>%  dplyr::select(`start-energy`) %>% unlist() %>% as.vector() # we don't have observed energy values so we will use the start energy
  
  KDE95 <- db %>%  dplyr::select("KDE_95") %>% unlist() %>% as.vector()
  KDE50 <- db %>%  dplyr::select("KDE_50") %>% unlist() %>% as.vector()
  
  
  p_feeding <- db %>%  dplyr::select("p_feeding") %>% unlist() %>% as.vector()
  p_foraging <- db %>%  dplyr::select("p_foraging") %>% unlist() %>% as.vector()
  p_traveling <- db %>%  dplyr::select("p_traveling") %>% unlist() %>% as.vector()
  p_resting <- db %>%  dplyr::select("p_resting") %>% unlist() %>% as.vector()
  
  DPL_mean <- db$DPL
  # DPL_sd <- db$DPL_sd # check it
  
  # DPL_mean <- db$DPL_d
  # DPL_mean <- DPL_mean %>%
  #   str_replace_all(., c("\\[" = "", "\\]" = "")) %>%
  #   str_split(pattern = "_") %>%  #, simplify = TRUE) %>%
  #   purrr::map(as.numeric, na.rm = TRUE) %>% 
  #   # purrr::map(round, 2) %>%
  #   map_dbl(mean, na.rm=TRUE)
  # db$DPL_mean <- DPL_mean
  
  MR_mean <- db$MR
  MR_sd <- db$MR_sd
  PT_mean <- db$PT
  PT_sd <- db$PT_sd
  
  
  
  # calc differences
  en <- abs(energy - energy_obs)
  hr <- abs(KDE95 - KDE95_obs)
  ca <- abs(KDE50 - KDE50_obs)
  dp <- abs(DPL_mean - DPL_obs)
  
  # normalize them (min-max)
  en_min <- min(en) 
  en_max <- max(en) 
  hr <- min(hr) 
  hr <- max(hr)
  # ca <- 
  # dp <- 
  
  # or put all into a dataframe and use custom_fitness() function
  
  
  crit <- lsm$value
  return(crit)
}

  


## Step 3: Attach a simulation design   -------------------------

nl@simdesign <- simdesign_GenSA(nl, 
                                evalcrit = 1, 
                                nseeds = 1, 
                                control=list(maxit = 20))


