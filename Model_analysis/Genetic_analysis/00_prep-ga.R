# Script name: 00_prep-ga.R
# Script purpose: identify the best set of energetic parameters that better predict 
# tamarins movement patterns/range behavior of Taquara group, the group which inhabits
# the most 'natural' condition

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
library("magrittr")
# library("GenSA")
library("genalg")

## Config cores
# ncores <- parallel::detectCores() # ga is not paralelized

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
  outpath <- here("Model_analysis", "Genetic_analysis", "temp")
}


nl <- nl(nlversion = "6.2.2",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

nlogo_model_param <- report_model_parameters(nl)
nlogo_model_param

# Decide which area/month to run the optimization on:
area_run <- "Taquara"
month_run <- "Jan"


# Empirical data for parameterisation:
param_table <- read.csv(here("Data", "Parameter_table.csv"),
                        sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  dplyr::mutate(group = recode(group, "Guarei" = "Guareí")) # only to match those of the NetLogo model


# Empirical data for criteval function:
values_ga <- read.csv(here("Data", "Validation-table.csv"),
                      sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  dplyr::mutate(group = recode(group, "Guarei" = "Guareí")) # only to match those of the NetLogo model

# Movement variables:
values_ga_mv <-  read.csv(here("Data", "Movement", "Curated", "Validation", "Siminputrow_MR-PT_by-day.csv"),
                          sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  dplyr::mutate(group = recode(group, "Guarei" = "Guareí")) # only to match those of the NetLogo model



## Create evaluation criteria function -------------------------

# 1) Get min and max values for each parameter -----------------

energy_min <- 0
# tamarins should not spend too much energy above level 2, thus I'm defining the max energy as 50% more than energy_level_2. If the values are above this, the run is dropped:
# energy_max <- nlogo_model_param$energy_level_2$value + ( 0.5 * nlogo_model_param$energy_level_2$value )
energy_max <- 3000 # or the max possible number used as variable in the experiment

# KDE95_max <- values_ga %>% dplyr::select(KDE95) %>% unlist() %>% max() %>% as.vector()
KDE95_max <- 400 # (400 is the year-round home range of Taquara group)
KDE50_max <- values_ga %>% dplyr::select(KDE50) %>% unlist() %>% max() %>% as.vector()
KDE95_min <- values_ga %>% dplyr::select(KDE95) %>% unlist() %>% min() %>% as.vector()
KDE50_min <- values_ga %>% dplyr::select(KDE50) %>% unlist() %>% min() %>% as.vector()

# p_feeding_max <- values_ga %>% dplyr::select("Frugivory_perc_behavior_mean") %>% unlist() %>% max() %>% as.vector() %>% '/' (100)
p_feeding_max <- 0.7
# p_feeding_min <- values_ga %>% dplyr::select("Frugivory_perc_behavior_mean") %>% unlist() %>% min() %>% as.vector() %>% '/' (100)
p_feeding_min <- 0.1
# p_foraging_max <- values_ga %>%  dplyr::select("Foraging_perc_behavior_mean") %>% unlist() %>% max() %>% as.vector() %>% '/' (100)
p_foraging_max <- 0.7
# p_foraging_min <- values_ga %>%  dplyr::select("Foraging_perc_behavior_mean") %>% unlist() %>% min() %>% as.vector() %>% '/' (100)
p_foraging_min <- 0
# p_traveling_max <- values_ga %>% dplyr::select("Travel_perc_behavior_mean") %>% unlist() %>% max() %>% as.vector() %>% '/' (100)
p_traveling_max <- 0.8
p_traveling_min <- values_ga %>% dplyr::select("Travel_perc_behavior_mean") %>% unlist() %>% min() %>% as.vector() %>% '/' (100)
# p_traveling_min <- 0.1
# p_resting_max <- values_ga %>% dplyr::select("Resting_perc_behavior_mean") %>% 
#   dplyr::filter(!is.na(.)) %>%  unlist() %>% max() %>% as.vector() %>% '/' (100)
p_resting_max <- 0.8
# p_resting_min <- values_ga %>% dplyr::select("Resting_perc_behavior_mean") %>% 
#   dplyr::filter(!is.na(.)) %>%  unlist() %>% min() %>% as.vector() %>% '/' (100)
p_resting_min <- 0 # (Guareí NA = 0, i.e., no resting)
# obs: p_feeding_obs + p_foraging_obs + p_traveling_obs + p_resting_obs don't sum up to 1 (these are values filtered for the four behaviors in the model)


# # Still have to calculate them from empirical data
# step_length_mean_obs <- param_table %>% 
#   dplyr::filter(group == area_run | "id_month" == month_run) %>%
#   dplyr::select(step_length_mean_obs) %>% unlist() %>% as.vector()
# 
# step_length_sd <- param_table %>% 
#   dplyr::filter(group == area_run | "id_month" == month_run) %>%
#   dplyr::select(step_length_sd) %>% unlist() %>% as.vector()
# 
# turn_ang_sd <- param_table %>% 
#   dplyr::filter(group == area_run | "id_month" == month_run) %>%
#   dplyr::select(turn_ang_sd) %>% unlist() %>% as.vector()

max_angle_75_travel_max <- param_table %>% 
  dplyr::select(max_random_angle_75q_Travel) %>% max() %>% unlist() %>% as.vector()
max_angle_75_travel_min <- param_table %>% 
  dplyr::select(max_random_angle_75q_Travel) %>% min() %>% unlist() %>% as.vector()

max_angle_75_forage_max <- param_table %>% 
  dplyr::select(max_random_angle_75q_Foraging) %>% max() %>% unlist() %>% as.vector()
max_angle_75_forage_min <- param_table %>% 
  dplyr::select(max_random_angle_75q_Foraging) %>% min() %>% unlist() %>% as.vector()


values_ga_mv_summary <- values_ga_mv %>% 
  group_by(group, id_month) %>% 
  summarise(
    DPL_mean_obs = mean(DPL),
    DPL_sd_obs = sd(DPL),
    MR_mean_obs = mean(MR), 
    MR_sd_obs = sd(MR),
    PT_mean_obs = mean(PT),
    PT_sd_obs = sd(PT)
  ) %>% 
  ungroup()
  #dplyr::filter(group == area_run | "id_month" == month_run)

dpl_mean_min <- values_ga_mv_summary %>%
  dplyr::select(DPL_mean_obs) %>% min() %>%  unlist() %>% as.vector()
dpl_mean_max <- values_ga_mv_summary %>%
  dplyr::select(DPL_mean_obs) %>% max() %>%  unlist() %>% as.vector()
dpl_sd_min <- values_ga_mv_summary %>%
  dplyr::select(DPL_sd_obs) %>% min() %>%  unlist() %>% as.vector()
dpl_sd_max <- values_ga_mv_summary %>%
  dplyr::select(DPL_sd_obs) %>% max() %>%  unlist() %>% as.vector()


mr_min <- values_ga_mv_summary %>%
  dplyr::select(MR_mean_obs) %>% min() %>%  unlist() %>% as.vector()
mr_max <- values_ga_mv_summary %>%
  dplyr::select(MR_mean_obs) %>% max() %>% unlist() %>% as.vector()
mr_sd_min <- values_ga_mv_summary %>%
  dplyr::select(MR_sd_obs) %>% min() %>%  unlist() %>% as.vector()
mr_sd_max <- values_ga_mv_summary %>%
  dplyr::select(MR_sd_obs) %>%max() %>% unlist() %>% as.vector()

pt_min <- values_ga_mv_summary %>%
  dplyr::select(PT_mean_obs) %>% min() %>% unlist() %>% as.vector()
pt_max <- values_ga_mv_summary %>%
  dplyr::select(PT_mean_obs) %>% max() %>% unlist() %>% as.vector()
# pt_sd_min <- values_ga_mv_summary %>%
#   dplyr::select(PT_sd_obs) %>% min() %>% unlist() %>% as.vector()
pt_sd_min <- 0.1 # check which value is the minimum theoritically possible
pt_sd_max <- values_ga_mv_summary %>%
  dplyr::select(PT_sd_obs) %>% max() %>% unlist() %>% as.vector()


# tamarins visit all the observed trees, but they don't usually visit all of them in the model (might be related to memory)
visits_max <- param_table %>% 
  dplyr::select(n_trees_Frugivory) %>% max() %>% unlist() %>% as.vector()
visits_min <- param_table %>% 
  dplyr::select(n_trees_Frugivory) %>% min() %>% unlist() %>% as.vector()


# 2) Normalizing (min-max; 0-1) observed parameters --------------

# normalized = (x-min(x))/(max(x)-min(x))
# normalize <- function(x) {
#   x.norm <- (x-min(x))/(max(x)-min(x))
#   return(x.norm)
# }
# values_ga_mv_summary.norm <- values_ga_mv_summary %>% 
#   mutate_if(is.numeric, normalize)
# 
# values_ga_mv.norm <- values_ga_mv %>% 
#   mutate_if(is.numeric, normalize)

normalize <- function(x, min, max) {
  x.norm <- ((x-min)/(max-min))
  return(x.norm)
}
# normalize(20, min=2.5, max=100)

KDE95_obs <- values_ga %>% dplyr::filter(group == area_run | "id_month" == month_run) %>%
  dplyr::select(KDE95) %>% unlist() %>% as.vector() %>% normalize(min = KDE95_min, max = KDE95_max)
KDE50_obs <- values_ga %>% dplyr::filter(group == area_run | "id_month" == month_run) %>%
  dplyr::select(KDE50) %>% unlist() %>% as.vector() %>% normalize(min = KDE50_min, max = KDE50_max)

MR_obs <- values_ga_mv_summary %>% dplyr::filter(group == area_run | "id_month" == month_run) %>%
  dplyr::select("MR_mean_obs") %>% unlist() %>% as.vector() %>% normalize(min = mr_min, max = mr_max)
PT_obs <- values_ga_mv_summary %>% dplyr::filter(group == area_run | "id_month" == month_run) %>%
  dplyr::select("PT_mean_obs") %>% unlist() %>% as.vector() %>% normalize(min = pt_min, max = pt_max)

p_feeding_obs <- values_ga %>%  dplyr::filter(group == area_run | "id_month" == month_run) %>%
  dplyr::select("Frugivory_perc_behavior_mean") %>% unlist() %>% as.vector() %>%  '/' (100) %>% 
  normalize(min = p_feeding_min, max = p_feeding_max)
p_foraging_obs <- values_ga %>%  dplyr::filter(group == area_run | "id_month" == month_run) %>%
  dplyr::select("Foraging_perc_behavior_mean") %>% unlist() %>% as.vector() %>%  '/' (100) %>% 
  normalize(min = p_foraging_min, max = p_foraging_max)
p_traveling_obs <- values_ga %>%  dplyr::filter(group == area_run | "id_month" == month_run) %>%
  dplyr::select("Travel_perc_behavior_mean") %>% unlist() %>% as.vector() %>%  '/' (100) %>% 
  normalize(min = p_traveling_min, max = p_traveling_max)
p_resting_obs <- values_ga %>%  dplyr::filter(group == area_run | "id_month" == month_run) %>%
  dplyr::select("Resting_perc_behavior_mean") %>% unlist() %>% as.vector() %>% '/' (100) %>% 
  normalize(min = p_resting_min, max = p_resting_max)


visits_obs <- param_table %>% 
  dplyr::filter(group == area_run | "id_month" == month_run) %>%
  dplyr::select(n_trees_Frugivory) %>% unlist() %>% as.vector() %>% 
  normalize(min = visits_min, max = visits_max)


# Por enquanto não - trampo demais:
# step_length_mean_min <- param_table %>%
#   dplyr::select(step) %>% min() %>% unlist() %>% as.vector()
# step_length_mean_max <- param_table %>%
#   dplyr::select(step) %>% max() %>% unlist() %>% as.vector()
# 
# step_length_sd
# turn_ang_sd
# energy



## Step 2: Attach an experiment   -------------------------
expname <- paste0("GA_", area_run, "_", month_run)

# define how much each run should take based on empirical activity periods
no_days_run <- param_table %>% 
  dplyr::filter(group == area_run,
                id_month == month_run) %>% 
  dplyr::select(ndays) %>% 
  pull() #+ 1 # one day more for "initializing" the model (take this first day out when analyzing data?)

simultime_run <- param_table %>% 
  dplyr::filter(group == area_run,
                id_month == month_run) %>% 
  dplyr::select(mean_timesteps) %>% 
  pull()
simultime_run <- round(simultime_run * 0.95) # that's the timestep when tamarins should start looking for the sleeping site

# choose which parameterizations should be on:
step_model_param <- "true" # velocity parameters are setted inside the model. Change this when velocity is summarized and inclued in the parameter table
gtt_param <- "true" # gtt parameters are setted inside the model. Change this when velocity is summarized and inclued in the parameter table
p_forage_param <- "true" # p_foraging parameter is setted inside the model. Change this when velocity is summarized and inclued in the parameter table 
feedingbout <- "false" # previous sensitivity analysis showed that this does not matter, at least for Guareí

# escape strings to nlrx experiment
area_run_scp <- paste0('"', area_run, '"')   # scaped
month_run_scp <- paste0('"', month_run, '"') # scaped

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
                              # "count feeding-trees with [visitations > 0] / count feeding trees" # the best set of parameters should make tamarins visit all the observed trees in the period
                              # "count feeding-trees"
                              "p-visited-trees"
                            ),
                            metrics.turtles = list(
                              
                              "monkeys" = c(
                                "energy",      # final energy                # the best set of parameters should make tamarins viable in energetic terms
                                "enlvl1",         # value of energy_level_1 used at the start of the simulation
                                "enlvl2",         # value of energy_level_2 used at the start of the simulation 
                                "enstart",          # start-energy value at the start of the similuation
                                "DPL",         # mean DPL (in case we don't want all the values -> good for bar and pointrange plots)
                                # "DPL_d",       # DPL is set to 0 everyday    # the best set of parameters should reproduce the observed DPL
                                "DPL_sd",      # sd DPL  (in case we don't want all the values -> good for bar and pointrange plots)
                                "KDE_95",      # final value                 # the best set of parameters should reproduce the observed home range
                                "KDE_50",      # final value                 # the best set of parameters should reproduce the observed core area
                                "p_feeding",   # final value                 # the best set of parameters should optimize the activity budget
                                "p_foraging",  # final value                 # the best set of parameters should optimize the activity budget
                                "p_traveling", # final value                 # the best set of parameters should optimize the activity budget
                                "p_resting",    # final value                 # the best set of parameters should optimize the activity budget
                                # "step_length_mean",    # besides the parameterization, agents interactions make the observed step length and turning angles change
                                # "step_length_sd",      # besides the parameterization, agents interactions make the observed step length and turning angles change
                                # "turn_ang_mean",     # this one is quite consistent, so I don't think this one is necessary
                                # "turn_ang_sd",          # this one might be interesting though (but I haven't estimated the empirical ones yet)
                                
                                # additional movement variables
                                "MR",               # movement rate (MR) is used to predict SDD by primates: http://doi.wiley.com/10.1002/ajp.22659
                                "MR_sd",
                                # "MSD",              # other modelling studies have used this one (https://doi.org/10.3390/ani12182412.), but I believe it is very similar to MR
                                # "intensity_use",    # bether than MSD in my oppinion: read about it in: https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en
                                "PT",               # path twisting is used by Fuzessy et al 2017 to predict SDD among primates: http://doi.wiley.com/10.1002/ajp.22659
                                "PT_sd",
                                # "straightness",   # straightness and sinuosity are slightlty different in terms of properties (https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en) and they were not tested as predictors of SDD, so i'm not using them
                                # "sinuosity"       # straightness and sinuosity are slightlty different in terms of properties (https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en) and they were not tested as predictors of SDD, so i'm not using them
                                
                                "n_visited_trees",
                                "n_unvisited_trees"
                                
                              ) 
                              
                            ), # "who" "color"
                            variables = list(
                              
                              ### DO NOT SPECIFY STEPS FOR GA:
                              
                              # energy
                              # "energy-from-fruits" = list(min=1, max = 300),
                              # 'energy-from-prey' = list(min=1, max=300),
                              # "energy-loss-traveling" = list(min=-100, max = -1), #, step = 2
                              # "energy-loss-foraging" = list(min=-100, max = -1),
                              # "energy-loss-resting" = list(min=-100, max = -1),
                              
                              # "start-energy" = list(min=100, max=2000),
                              "energy_level_1" = list(min=100, max=2000),
                              "energy_level_2" = list(min=100, max=2000)
                              
                              
                              # 4. Movement
                              # "step_len_travel" = list(min= 0, max = 100)         # only when step-model-param? = 'false'
                              # "step_len_forage" = list(min= 0, max = 100)         # only when step-model-param? = 'false'
                              # "max_rel_ang_travel_75q" = list(min= 0, max= 180)   # only when step-model-param? = 'false'
                              # "max_rel_ang_forage_75q" = list(min= 0, max= 180)   # only when step-model-param? = 'false'
                              
                              # "p_foraging_while_traveling" = list(min= 0, max= 1) # only when p-forage-param? = 'false'
                              
                              # memory
                              # "step_forget" = list(min=3, max = 150),
                              # "visual" = list(min=0, max = 3),
                              # 'prop_trees_to_reset_memory' = list(min=2, max=5),   # Initially I didn't think this one is needed (mainly because of the first sensitivity analysis in Guareí), but this might help (as step_forget) making some regions of the home range to not be targeted
                              
                              # 5. Feeding bout (only when "feedingbout-on?" = 'false')
                              # 'species_time' = list(min = 1, max = 10), #
                              # 'duration' = list(min = 1, max = 10)      #
                              
                              # 6. Seed dispersal
                              # "gut_transit_time_val" = 15,    # this won't be optimized as it is an emerging pattern
                              # "n_seeds_hatched" = 1           # this won't be optimized as it is an emerging pattern
                            ),
                            
                            constants = list(
                              
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
                              "study_area" = area_run_scp,               # "\"Taquara\"",   # we are optimizing with Taquara as it is the most natural condition
                              "feeding-trees-scenario" = month_run_scp,  #"\"Jan\"",        # we are optimizing with Taquara as it is the most natural condition
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
                              # 'start-energy' = 980,
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





## Step 3: Attach a simulation design   -------------------------


# 3) Define critfun (fitness function) --------------
critfun <- function(nl) {
  
  # extract values from run from example nl object:
  # db2 <- nlrx::getexp(nl, "variables") # or nl@experiment@variables
  db <- unnest_simoutput(nl)
  # db <- nlrx::getexp(nl, "metrics.turtles")
  # db3 <- report_model_parameters(nl)
  
  # nl@experiment
  
  # nl.x@simdesign@siminput %>% as.data.frame
  # nl@experiment
  # nl@experiment@variables
  # nl@experiment@metrics.turtles
  # str(db)
  # class(db3)

  # for criteria:
  # db[[1]][1]
  
  # en1 <- db %>% magrittr::extract("en1") %>%  unlist() %>% na.omit() %>% as.vector() %>% 
  en1 <- db %>% dplyr::select("enlvl1") %>%  unlist() %>% na.omit() %>% as.vector() %>% 
    normalize(min = energy_min, max = energy_max)
  # en2 <- db %>% magrittr::extract("en2") %>%  unlist() %>% na.omit() %>% as.vector() %>% 
  en2 <- db %>% dplyr::select("enlvl2") %>%  unlist() %>% na.omit() %>% as.vector() %>%
    normalize(min = energy_min, max = energy_max)
  # # energy_level2 <- db %>% magrittr::extract2("energy_level_2") %>% magrittr::use_series(value) %>%  unlist() %>% as.vector()
  # energy_obs <- 980
  energy_obs <- db %>% dplyr::select("enstart") %>%  unlist() %>% na.omit() %>% as.vector()  %>% 
    normalize(min = energy_min, max = energy_max) # = start-energy. does it make sense to use the start value as 'observed' (we don't have this number estimated)
  
  # # for fitness:
  # energy_obs <- db %>%  magrittr::extract("ens") %>% unlist() %>% as.vector() %>% 
  #   normalize(min = energy_min, max = energy_max)
  # energy <- db %>%  magrittr::extract("energy") %>% unlist() %>% na.omit() %>% as.vector() %>% 
  energy <- db %>%  dplyr::select("energy") %>% unlist() %>% na.omit() %>% as.vector() %>% 
    normalize(min = energy_min, max = energy_max)

  # KDE95 <- db %>%  magrittr::extract("KDE_95") %>% unlist() %>%  na.omit() %>% as.vector() %>%
  KDE95 <- db %>%  dplyr::select("KDE_95") %>% unlist() %>%  na.omit() %>% as.vector() %>%
    `/` (10000) %>%  #convert to hectares
    normalize(min = KDE95_min, max = KDE95_max)
  # KDE50 <- db %>%  magrittr::extract("KDE_50") %>% unlist() %>%  na.omit() %>% as.vector() %>% 
  KDE50 <- db %>%  dplyr::select("KDE_50") %>% unlist() %>%  na.omit() %>% as.vector() %>% 
    `/` (10000) %>%  #convert to hectares
    normalize(min = KDE50_min, max = KDE50_max)
  
  # visits <- db %>% magrittr::extract("n_visited_trees") %>% unlist() %>% unique() %>% as.vector() %>% 
  visits <- db %>% dplyr::select("n_visited_trees") %>% unlist() %>% unique() %>% as.vector() %>% 
    normalize(min = visits_min, max = visits_max)
  
  # p_feeding <- db %>%  magrittr::extract("p_feeding") %>% unlist()  %>% na.omit() %>% as.vector() %>% 
  p_feeding <- db %>%  dplyr::select("p_feeding") %>% unlist()  %>% na.omit() %>% as.vector() %>% 
    normalize(min = p_feeding_min, max = p_feeding_max)
  # p_foraging <- db %>%  magrittr::extract("p_foraging") %>% unlist() %>%  na.omit() %>% as.vector() %>% 
  p_foraging <- db %>%  dplyr::select("p_foraging") %>% unlist() %>%  na.omit() %>% as.vector() %>% 
    normalize(min = p_foraging_min, max = p_foraging_max)
  # p_traveling <- db %>%  magrittr::extract("p_traveling") %>% unlist() %>%  na.omit() %>% as.vector() %>% 
  p_traveling <- db %>%  dplyr::select("p_traveling") %>% unlist() %>%  na.omit() %>% as.vector() %>% 
    normalize(min = p_traveling_min, max = p_traveling_max)
  # p_resting <- db %>%  magrittr::extract("p_resting") %>% unlist() %>%  na.omit() %>% as.vector() %>% 
  p_resting <- db %>%  dplyr::select("p_resting") %>% unlist() %>%  na.omit() %>% as.vector() %>% 
    normalize(min = p_resting_min, max = p_resting_max)
  
  # DPL_sd <- db$DPL_sd # check it
  
  # DPL_mean <- db$DPL_d
  # DPL_mean <- DPL_mean %>%
  #   str_replace_all(., c("\\[" = "", "\\]" = "")) %>%
  #   str_split(pattern = "_") %>%  #, simplify = TRUE) %>%
  #   purrr::map(as.numeric, na.rm = TRUE) %>% 
  #   # purrr::map(round, 2) %>%
  #   map_dbl(mean, na.rm=TRUE)
  # db$DPL_mean <- DPL_mean
  
  DPL_mean <- db$DPL %>%  na.omit() %>% as.vector() %>% normalize(min = dpl_mean_min, max = dpl_mean_max)
  DPL_sd <- db$DPL_sd %>% na.omit() %>% as.vector() %>%normalize(min = dpl_sd_min, max = dpl_sd_max)
  MR_mean <- db$MR %>% na.omit() %>% as.vector() %>% normalize(min = mr_min, max = mr_max)
  MR_sd <- db$MR_sd  %>% na.omit() %>% as.vector() %>% normalize(min = mr_sd_min, max = mr_sd_max)
  PT_mean <- db$PT  %>% na.omit() %>% as.vector() %>% normalize(min = pt_min, max = pt_max)
  PT_sd <- db$PT_sd  %>% na.omit() %>% as.vector() %>% normalize(min = pt_sd_min, max = pt_sd_max)
  
  
  # Get observed values (normalized)
  DPL_mean_obs <- values_ga_mv_summary %>% 
    dplyr::filter(group == area_run | "id_month" == month_run) %>%
    dplyr::select(DPL_mean_obs)  %>% unlist() %>% as.vector() %>% 
    normalize(min = dpl_mean_min, max = dpl_mean_max)
  DPL_sd_obs <- values_ga_mv_summary %>% 
    dplyr::filter(group == area_run | "id_month" == month_run) %>%
    dplyr::select(DPL_sd_obs)  %>% unlist() %>% as.vector() %>% 
    normalize(min = dpl_sd_min, max = dpl_sd_max)
  
  MR_mean_obs <- values_ga_mv_summary %>% 
    dplyr::filter(group == area_run | "id_month" == month_run) %>%
    dplyr::select(MR_mean_obs)  %>% unlist() %>% as.vector() %>% 
    normalize(min = mr_min, max = mr_max)
  MR_sd_obs <- values_ga_mv_summary %>% 
    dplyr::filter(group == area_run | "id_month" == month_run) %>%
    dplyr::select(MR_sd_obs)  %>% unlist() %>% as.vector() %>% 
    normalize(min = mr_sd_min, max = mr_sd_max)
  
  PT_mean_obs <- values_ga_mv_summary %>%
    dplyr::filter(group == area_run | "id_month" == month_run) %>%
    dplyr::select(PT_mean_obs)  %>% unlist() %>% as.vector() %>% 
    normalize(min = pt_min, max = pt_max)
  PT_sd_obs <- values_ga_mv_summary %>% 
    dplyr::filter(group == area_run | "id_month" == month_run) %>%
    dplyr::select(PT_sd_obs)  %>% unlist() %>% as.vector() %>% 
    normalize(min = pt_sd_min, max = pt_sd_max)
  
  
  # calc differences (or put all into a dataframe and use custom_fitness() function)
  en <- abs(energy - energy_obs)
  vi <- abs(visits - visits_obs)
  hr95 <- abs(KDE95 - KDE95_obs)
  hr50 <- abs(KDE50 - KDE50_obs)
  dp <- abs(DPL_mean - DPL_mean_obs)
  dpsd <- abs(DPL_sd - DPL_sd_obs)
  mr <- abs(MR_mean - MR_obs)
  mrsd <- abs(MR_sd - MR_sd_obs)
  pt <- abs(PT_mean - PT_obs)
  ptsd <- abs(PT_sd - PT_sd_obs)
  pfee <- abs(p_feeding - p_feeding_obs)
  pfor <- abs(p_foraging - p_foraging_obs)
  ptra <- abs(p_traveling - p_traveling_obs)
  pres <- abs(p_resting - p_resting_obs)
  
  # calc the fitness function
  # more important ones:
  w <- 1 # weight 
  crit <- 
    1/sum(vi) * w + 1/sum(hr95) * w + 1/sum(hr50) * w + 
    1/sum(dp) * w + 1/sum(mr) * w + 1/sum(pt) * w +
    1/sum(pfee) * w + 1/sum(pfor) * w + 1/sum(ptra) * w + 1/sum(pres) * w
  # less important ones:
  w <- 0.5
  crit <- crit +
    1/sum(en) * w + # energy varies too much (min = 0, max = 3000) and we can't parameterize it (yet)
    1/sum(dpsd) * w + 1/sum(mrsd) * w + 1/sum(ptsd) # variation of the output variables is not as important as the variables
  
  # print(en1)
  # print(en2)
  
  print(paste("Fitness value of run: ", crit))
  # print(paste("energy_lvl_1", en1))
  # print(paste("energy_lvl_2", en2))
  
  if ( en1 > en2 ) {
    crit <- 99999
    print("energy_lvl_1 is bigger than energy_lvl 2, dropping simulation")
    return(crit)
  } else {
    return(crit)
  }
  
}

# Test the eval function:
# # extract values from run from example objects for testing:
# nl<- readRDS(here("Model_analysis", "Sensitivity-analysis", "v1.1_November2022", "temp",
#                      "v1.1_Taquara_Jan_simple1671135962_tempRDS.Rdata"))
# # # params_run <- nl.x %>% nlrx::report_model_parameters()
# # critfun(nl.x)
# db2 <- nlrx::getexp(nl.x, "variables")
# db <- nlrx::getexp(nl.x, "metrics.turtles")
# db3 <- report_model_parameters(nl.x)
# db4 <- nl.x@experiment@variables

# Monitor
# monitor <- function(obj) {
#   # plot the population
#   xlim = c(obj$stringMin[1], obj$stringMax[1]);
#   ylim = c(obj$stringMin[2], obj$stringMax[2]);
#   plot(obj$population, xlim=xlim, ylim=ylim, 
#        xlab="pi", ylab="sqrt(50)");
# }

# For differences between genetic algorithm (simdesign_GenAlg) and genetic anealing (simdesign_GenSA):
# https://stackoverflow.com/questions/4092774/what-are-the-differences-between-simulated-annealing-and-genetic-algorithms
nl@simdesign <- simdesign_GenAlg(nl, 
                                evalcrit = critfun, # 1, # "e.g. 1 would use the first defined metric of the experiment to evaluate each iteration)"
                                
                                popSize = 10, # or chromosomes
                                iters = 5,
                                elitism = 2, # from stackoverflow link above: "New members of the population are created in essentially one of three ways. The first is usually referred to as 'elitism' and in practice usually refers to just taking the highest ranked candidate solutions and passing them straight through--unmodified--to the next generation. The other two ways that new members of the population are usually referred to as 'mutation' and 'crossover'."
                                mutationChance = 0.01,
                                nseeds = 1
                                )

## Step 4: run -------------------------
set.seed(1234)

tictoc::tic()
progressr::handlers("progress")

results <- with_progress(
  run_nl_dyn(
    nl, 
    seed = nl@simdesign@simseeds
  )
)
tictoc::toc()


## Step 5: Investiga output
# a <- noquote(area_run) # or gsub(area_run_scp, pattern = ('\"'), replacement = '', fixed = T)
# m <- noquote(month_run) # or gsub(month_run_scp, pattern = ('\"'), replacement = '', fixed = T)

# 00_rep-ga_resuls results
cat(summary(results))

saveRDS(results, file.path(nl@experiment@outpath, paste0(expname, "_results_test.rds")))
# save.image(file=paste0(outpath, '/GA_Taquara_Jan_Environment_test.RData'))

# setsim(nl, "simoutput") <- tibble::enframe(results)
# saveRDS(nl, file.path(nl@experiment@outpath, paste0(expname, ".rds")))



## --- ##
# ?genalg::rbga
resultsrbga <- readRDS(paste0(outpath, "/GA_Taquara_Jan_results_test.rds"))
cat(summary(resultsrbga))
# summary.rbga(resultsrbga)
# plot(resultsrbga, type = "vars")
# plot(resultsrbga, type = "hist")
# 
# 
# summary(rbga.results)
