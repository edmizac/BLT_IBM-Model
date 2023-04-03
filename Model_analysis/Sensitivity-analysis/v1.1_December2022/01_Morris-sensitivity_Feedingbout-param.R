# Script name: 01_Morris-sensitivity_Feedingbout-param.R
# Script purpose: Define if parameterizing the feedingbouts per species (species_time) 
# is important in the model and contrast it with the sensitivity with the same aim 
# done for Guareí (/v1.1_August2022). However, we can only check the  the effect of
# feedingbout parameterization on the patterns that are turned as global variables: 
# visitation to trees (proportion of visited trees) and seed aggregation(R-index and NN)

# Date created: 2022-12-23d
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
# library("magrittr")
library("ggplot2")

# ggplot theme
theme_set(theme_bw(base_size = 15))
                   # theme(axis.text.x = element_text(size = 11))))

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


## Step 1: Create nl object ------------------------ 

if(Sys.info()[["nodename"]] == "simul02") {
  netlogopath <- file.path("/home/rbialozyt/Software/NetLogo 6.2.0")
  modelpath <- "/home/rbialozyt/BLT_IBM-Model/Model_development/BLT_model_v1.1.nlogo"
  outpath <- "/home/rbialozyt/BLT_IBM-Model/Model_analysis/Sensitivity-analysis/v1.1_December2022/temp"
}
if(Sys.info()[["nodename"]] == "PC146") {
  netlogopath <- file.path("/opt/netlogo_620")
  modelpath <- paste0("/home/rbialozyt/ownCloud-Forst/Projektideen/FAPESP_Project_Eduardo/"
                      , "BLT_IBM-Model/Model_development/BLT_model_v1.1.nlogo")
  outpath <-  paste0("/home/rbialozyt/ownCloud-Forst/Projektideen/FAPESP_Project_Eduardo/"
                     , "BLT_IBM-Model/Model_analysis/Sensitivity-analysis/v1.1_December2022/temp")
}
if(Sys.info()[["nodename"]] == "DESKTOP-R12V3D6") {
  netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
  modelpath <- here("Model_development", "BLT_model_v1.1.nlogo")
  outpath <- here("Model_analysis", "Sensitivity-analysis", "v1.1_December2022", "temp")
  user_scp = "\"Eduardo\""
}
if(Sys.info()[["nodename"]] == "PC9") { # LEEC
  netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
  modelpath <- here("Model_development", "BLT_model_v1.1.nlogo")
  outpath <- here("Model_analysis", "Sensitivity-analysis", "v1.1_December2022", "temp")
  Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8.0_351")
  user_scp = "\"LEEC\""
}


nl <- nl(nlversion = "6.2.2",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

nlogo_model_param <- report_model_parameters(nl)
nlogo_model_param

# Decide which area/month to run the sensitivity on:
area_run <- "Guareí" #"Taquara"
month_run <- "Jun"  # because this months has the greates diversity of consumed feeding trees (n = 6) "Jan"


# Define feedinbout chooser to iterate through
# feedingbout <- "true"
feedingbout <- "true"

if (feedingbout == "false") { 
  print("RUNNING WITH FEEDING BOUT PARAMETERIZATION OFF, COMMENT IN 'species_time' FROM EXPERIMENT")
} else {
  print("RUNNING WITH FEEDING BOUT PARAMETERIZATION ON, COMMENT OUT 'species_time' FROM EXPERIMENT")
}


## Step 2: Attach an experiment ------------------------

### Define expname ----
expname = paste0("v1.1_Morris_Feedingbout_", feedingbout, "_2022-12-23d")


### Empirical data for parameterisation:
param_table <- read.csv(here("Data", "Parameter_table.csv"),
                        sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  dplyr::mutate(group = recode(group, "Guarei" = "Guareí")) # only to match those of the NetLogo model

### define how much each run should take based on empirical activity periods ----
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


### choose which parameterizations should be on ----
step_model_param <- "true" # velocity parameters are setted inside the model. Change this when velocity is summarized and inclued in the parameter table
gtt_param <- "true" # gtt parameters are setted inside the model. Change this when velocity is summarized and inclued in the parameter table
p_forage_param <- "true" # p_foraging parameter is setted inside the model. Change this when velocity is summarized and inclued in the parameter table 


### escape strings to nlrx experiment ----
area_run_scp <- paste0('"', area_run, '"')   # scaped
month_run_scp <- paste0('"', month_run, '"') # scaped


### Attach to nl object ----
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
                              "p-visited-trees",
                              "R_seeds",
                              "R_seeds_p",
                              "NN_seeds"
                            ),
                            metrics.turtles = list(
                              
                              "monkeys" = c(
                                # "energy",      # final energy                # the best set of parameters should make tamarins viable in energetic terms
                                # "enlvl1",         # value of energy_level_1 used at the start of the simulation
                                # "enlvl2",         # value of energy_level_2 used at the start of the simulation 
                                # "enstart",          # start-energy value at the start of the similuation
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
                                # "MR",               # movement rate (MR) is used to predict SDD by primates: http://doi.wiley.com/10.1002/ajp.22659
                                # "MR_sd",
                                # "MSD",              # other modelling studies have used this one (https://doi.org/10.3390/ani12182412.), but I believe it is very similar to MR
                                # "intensity_use",    # bether than MSD in my oppinion: read about it in: https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en
                                # "PT",               # path twisting is used by Fuzessy et al 2017 to predict SDD among primates: http://doi.wiley.com/10.1002/ajp.22659
                                # "PT_sd",
                                # "straightness",   # straightness and sinuosity are slightlty different in terms of properties (https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en) and they were not tested as predictors of SDD, so i'm not using them
                                # "sinuosity"       # straightness and sinuosity are slightlty different in terms of properties (https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en) and they were not tested as predictors of SDD, so i'm not using them
                                
                                "n_visited_trees",
                                "n_unvisited_trees"
                                
                              ) 
                              
                            ), # "who" "color"
                            variables = list(
                              
                              ### DO NOT SPECIFY STEPS FOR GA:
                              
                              # energy
                              "energy-from-fruits" = list(min=1, max = 300, qfun="qunif"),
                              'energy-from-prey' = list(min=1, max=300, qfun="qunif"),
                              "energy-loss-traveling" = list(min=-100, max = -1, qfun="qunif"), #, step = 2
                              "energy-loss-foraging" = list(min=-100, max = -1, qfun="qunif"),
                              "energy-loss-resting" = list(min=-100, max = -1, qfun="qunif"),
                              
                              "start-energy" = list(min=100, max=2000, qfun="qunif"),
                              "energy_level_1" = list(min=100, max=2000, qfun="qunif"),
                              "energy_level_2" = list(min=100, max=2000, qfun="qunif"),
                              
                              
                              # 4. Movement
                              # "step_len_travel" = list(min= 0, max = 100)         # only when step-model-param? = 'false'
                              # "step_len_forage" = list(min= 0, max = 100)         # only when step-model-param? = 'false'
                              # "max_rel_ang_travel_75q" = list(min= 0, max= 180)   # only when step-model-param? = 'false'
                              # "max_rel_ang_forage_75q" = list(min= 0, max= 180)   # only when step-model-param? = 'false'
                              
                              # "p_foraging_while_traveling" = list(min= 0, max= 1) # only when p-forage-param? = 'false'
                              
                              # memory
                              "step_forget" = list(min=3, max = 150, qfun="qunif"),
                              # "visual" = list(min=0, max = 3),
                              'prop_trees_to_reset_memory' = list(min=2, max=5, qfun="qunif"),   # Initially I didn't think this one is needed (mainly because of the first sensitivity analysis in Guareí), but this might help (as step_forget) making some regions of the home range to not be targeted
                              
                              # 5. Feeding bout (only when "feedingbout-on?" = 'false')
                              'species_time' = list(min = 1, max = 10, qfun="qunif"),
                                
                              'duration' = list(min = 1, max = 10, qfun="qunif")      #
                              
                              # 6. Seed dispersal
                              # "gut_transit_time_val" = 15,    # this won't be optimized as it is an emerging pattern
                              # "n_seeds_hatched" = 1           # this won't be optimized as it is an emerging pattern
                            ),
                            
                            constants = list(
                              
                              "USER" = user_scp, # "\"Eduardo\"",
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
                              "study_area" = area_run_scp,               # "\"Taquara\"",   
                              "feeding-trees-scenario" = month_run_scp,  #"\"Jan\"",        
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

# Step 3: Attach a simulation design.
# install.packages("morris")
# library("morris")

nseeds <- 4 # (= num cores)

nl@simdesign <- simdesign_morris(nl = nl,
                                 morristype = "oat",
                                 morrislevels = 8, # sets the number of different values for each parameter (sampling density)
                                 morrisr = 10, # sets the number of repeated samplings (sampling size)
                                 morrisgridjump = 8 / 2, # sets the number of levels that are increased/decreased for computing the elementary effects. . Morris recommendation is to set this value to levels / 2.
                                 nseeds = nseeds)


# More information on the Morris specific parameters can be found in the description of the morris function in the sensitivity package (?morris).
# ?morris
report_model_parameters(nl)


# Step 4: Run simulations
# Evaluate nl object:
eval_variables_constants(nl)

print(nl)

# Run all simulations (loop over all siminputrows and simseeds)

library(future)
library(tictoc)

# plan(multisession)
## plan(list(sequential, multiseprocess))
## plan(list(sequential, multisession))
plan(list(sequential, multicore))
# split_param <- min(nrow(nl@simdesign@siminput), ((ncores - 2)/nseeds))
tictoc::tic()
progressr::handlers("progress")
results %<-% progressr::with_progress(
  run_nl_all(nl = nl
             , split = 2)
)
tictoc::toc()
print("================ Finished! =========================")
results$USER <- NULL
# results$'feeding-trees-scenario' <- NULL
results$'start-energy' <- NULL
results$energy_level_1  <- NULL
results$energy_level_2 <- NULL
results$'energy-from-fruits' <- NULL
results$'energy-from-prey' <- NULL
results$'energy-loss-traveling' <- NULL
results$'energy-loss-foraging' <- NULL
results$'energy-loss-resting' <- NULL
# results$gut_transit_time_val <- NULL
# results$n_seeds_hatched <- NULL

### movement
# results$travel_speed_val <- NULL

### others
# results$'simulation-time' <- NULL 

# Step 5: Attach results to nl and run analysis In order to run the
# analyze_nl function, the simulation output has to be attached to the
# nl object first. The simdesign class within the nl object provides a
# slot for attaching output results (simoutput). An output results
# tibble can be attached to this slot by using the simdesign setter
# function setsim(nl, "simoutput"). After attaching the simulation
# results, these can also be written to the defined outpath of the
# experiment object.  Attach results to nl object:
setsim(nl, "simoutput") <- results

print(nl)

print("================== save nl! ==========================")
# save(nl, file = paste0(nl@experiment@outpath, "/"
#      , "morris_2022-07-20_feedingbout-off.RData"))
# load(file = paste0(nl@experiment@outpath, "/"
#                         , "morris_2022-07-23d_feedingbout-off.RData"))
saveRDS(nl, file.path(paste0(nl@experiment@outpath, "/"
                             , "morris_2022-12-23d_feedingbout-"
                             , if(feedingbout) {"on"} else {"off"}
                             , ".rds")))
# readRDS(file.path(paste0(nl@experiment@outpath, "/"
#                          , "morris_2022-12-23d_feedingbout-"
#                          , if(feedingbout) {"on"} else {"off"}
#                          , ".rds")))

# print("================ save unnest =========================")
# results_unnest <- unnest_simoutput(nl)
# save(results_unnest,file = paste0(nl@experiment@outpath, "/"
#                                , "results_unnest_2022-12-23_feedingbout"
#                                , if(feedingbout) {"-on"} else {"-off"}
#                                , ".RData"))


# Morris analysis -----
## Load -----
nl_on <- readRDS(file.path(paste0(outpath, "/"
                             , "morris_2022-12-23d_feedingbout-on.rds")))
nl_off <- readRDS(file.path(paste0(outpath, "/"
                             , "morris_2022-12-23d_feedingbout-off.rds")))

## Merge -----
morris_on <- analyze_nl(nl_on) ; rm(nl_on)
morris_on$feedingbout <- "feedingbout on"
morris_off <- analyze_nl(nl_off) ; rm(nl_off)
morris_off$feedingbout <- "feedingbout off"
morris <- dplyr::bind_rows(morris_on, morris_off)
rm(morris_on); rm(morris_off)

## Plot -----
morris %>% 
  dplyr::filter(metric=="p-visited-trees_mean") %>% 
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on proportion of visited trees")

# Save plot
ggsave(paste0(outpath, "/",
            '01_Morris_n-visited-trees.png'), height = 7, width = 10)


morris %>% 
  dplyr::filter(metric =="R_seeds_mean" |
                metric == "R_seeds_p_mean") %>%
  # tidyr::pivot_wider(names_from = "metric", values_from = c(value) ) %>% 
  # dplyr::filter(R_seeds_p_mean <= 0.05) %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on seed aggreation (R index)")
  
# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_R-index.png'), height = 7, width = 10)

morris %>% 
  dplyr::filter(metric =="NN_seeds_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on seed aggreation (Nearest neighbor distance)")

# Save plot
ggsave(paste0(outpath, "/",
              '01_Morris_NN-distance.png'), height = 7, width = 10)

