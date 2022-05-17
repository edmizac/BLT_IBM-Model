## ---------------------------
## Script name: 00_start-nlrx
## Script purpose: Start nlrx workflow
## Date created: 2022-04-22d
## Author: Eduardo Zanette

## ---------------------------
## Notes:
##   
## ---------------------------

## ---------------------------
## Options (plotting, memory limit, decimal digits)
# Packages from github:
# install.packages('devtools')
# devtools::install_github('thomasp85/gganimate')
# remotes::install_github("ropensci/nlrx")

# Java memory:
if(Sys.getenv("JAVA_HOME") == "") {
        Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk1.8.0_321")
}

## ---------------------------
## Packages:
library("nlrx")
library("here")
library("progressr")
library("tidyverse")
## ---------------------------

# vignette(nlrx) # not available. Try:
# https://docs.ropensci.org/nlrx/articles/getstarted.html


# Step 1: Create nl object
## netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
netlogopath <- file.path("/opt/netlogo_622")
modelpath <- here("Model_development", "Model-cleaning", "BLT_model_v1.nlogo")
# modelpath <- here("C:/Program Files/NetLogo 6.2.2", "app/models", "BLT_model_v1.nlogo")

# C:\Program Files\NetLogo 6.2.2\app\models
file.info(modelpath)

outpath <- here("Model_development", "Model-cleaning", "runtime")


nl <- nl(nlversion = "6.2.2",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 8192)

report_model_parameters(nl)






#### Simple design experiment ( = one go button, no varibles) ####

# Step 2: Attach an experiment
expname = "v1-model-speed"
# no_days = 10

nl@experiment <- experiment(expname = expname,
                            outpath = outpath,
                            repetition = 1, # number of repetitions with the same seed
                            tickmetrics = "true", # "false" for metrics only in the end of the simulation
                            idsetup = "setup",
                            idgo = "go",
                            # idfinal = "",
                            # idrunnum = "nlrx_id",
                            runtime = 1500, #(if = 0 or NA_integer_, define stopcond_)
                            stopcond= "day > no_days", # reporter that returns TRUE
                            # stopcond= "ifelse day = no_days AND all? monkeys [ behavior = "sleeping" ] ", # reporter that returns TRUE
                            # stopcond= "not any? monkeys", # reporter that returns TRUE
                            evalticks = NA_integer_, # NA_integer_ = measures each tick. Only applied if tickmetrics = TRUE
                            # reporters:
                            metrics = c("timestep", "day"), # e.g. "count sheep" or "count patches with [pcolor = green]"
                            metrics.turtles = list("monkeys" = c("x_UTM", "y_UTM",
                                                                 "energy", "behavior")
                                                   
                                                                 # , "steps-moved"
                                                                 
                            ), # "who" "color"
                            # metrics.patches = list(),
                            # metrics.patches = list(
                            #   # c("pxcor", "pycor", "pcolor")
                            #                        ),
                            variables = list(
                              
                              # energy
                              # "energy-from-seeds" = list(min=1, max = 10, step = 2),
                              # 'energy-from-prey' = list(min=1, max=16, step = 3),
                              # "energy-loss-traveling" = list(min=-10, max = 0, step = 2),
                              # "energy-loss-foraging" = list(min=-10, max = 0, step = 2),
                              # "energy-loss-resting" = list(min=-10, max = 0, step = 2),
                              
                              # memory
                              # "step_forget" = list(min=0, max = 150, step = 10, qfun="qunif")
                              # "visual" = list(min=0, max = 3, step = 1)
                              
                              # movement
                              "travel_speed_val" = list(values=seq(0.3, 1, by = 0.1))
                              # "foraging_speed_val" = list(min= 0, max = 1, step = 2)
                              # "duration" = list(min=0, max = 10, step = 2),
                              
                              # others
                              # 'species_time_val' = list(min = 1, max = 6, step = 2)
                            ),
                            
                            # (
                            #   'start-energy' = list(min=10, max=170, step = 40, qfun="qunif")
                            #                ),
                            constants = list(
                              
                              ### "true" for output related stuff
                              # "output-files?" = "false", #THIS IS VERY IMPORTANT (csv files)
                              # "output-print?" = "false", #true to output in the console
                              "USER" = "\"Ronald\"",
                              # "print-step?" = "false",
                              # 'export-png'= "false",
                              # "show-energy?" = "false",
                              # "show-path?" = "false",
                              # "all-slp-trees?" = "false",
                              # "display-hatched-trees?" = "false",
                              # "path-color-by-day?" = "false",
                              
                              ### resource scenario
                              "no_days" = 10 # DON'T TRY no_days = 1
                              # 'feeding-trees-scenario' = "\"trees_all\"",
                              # 'feeding-trees?' = "true",
                              # 'sleeping-trees?' = "true",
                              # 'resting-trees?' = "false",
                              # 'sleeping-trees-scenario' = "\"empirical\"",
                              # 'empirical-trees-choice' = "\"closest\"",
                              # 'species_time_val' = 6,
                              
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
                              # "travel_speed_val" = 0.7,
                              # "foraging_speed_val" = 0.7,
                              
                              ### others
                              # "simulation-time" = 108
                              
                            )
)


# Step 3: Attach a simulation design.
nl@simdesign <- simdesign_distinct(nl, nseeds = 10)

# Step 4: Run simulations
# Evaluate nl object:
eval_variables_constants(nl)

print(nl)

nl@simdesign

# Run all simulations (loop over all siminputrows and simseeds)

library(future)
library(tictoc)

# With run_nl_one (with only the first seed)
# tictoc::tic()
# progressr::handlers("progress")
# results <- progressr::with_progress(
#   run_nl_one(nl,
#              seed = getsim(nl, "simseeds")[1],
#              siminputrow = 1
#              )
# )
# tictoc::toc()


# With run_nl_all (all 17 seeds)
tictoc::tic()
progressr::handlers("progress")
results <- progressr::with_progress(
  run_nl_all(nl,
             split = 4
             )
)
tictoc::toc()


## Step 5:
#' Attach results to nl and run analysis In order to run the
#' analyze_nl function, the simulation output has to be attached to the
#' nl object first. The simdesign class within the nl object provides a
#' slot for attaching output results (simoutput). An output results
#' tibble can be attached to this slot by using the simdesign setter
#' function setsim(nl, "simoutput"). After attaching the simulation
#' results, these can also be written to the defined outpath of the
#' experiment object.  Attach results to nl object:
setsim(nl, "simoutput") <- results

nl@experiment@metrics.turtles
nl@experiment@metrics.patches
nl@experiment@variables

#' Save RDS to avoid losing it by R abortion:
filename <- here("Model_development", "Model-cleaning", "runtime", "tempRDS.Rdata")
saveRDS(nl, file = filename) ; rm(results)
nl <- readRDS(filename)

rm(results)
gc()

#' Go to file 00_Validation_patterns_v1






#### Full fact experiment design ####

#' Step 2: Attach an experiment
expname = "v1-model-fullfact"

nl@experiment <- experiment(expname = expname,
                            outpath = outpath,
                            repetition = 1,
                            tickmetrics = "true", # "false" for metrics only in the end of the simulation
                            idsetup = "setup",
                            idgo = "run_days",
                            # idfinal = "",
                            # idrunnum = "nlrx_id",
                            runtime = 0, #(if = 0 or NA_integer_, define stopcond_)
                            stopcond= "simulation-time-end", # reporter that returns TRUE
                            evalticks = NA_integer_, # NA_integer_ = measures each tick
                            # reporters:
                            metrics = c("count sleeping-trees"), # e.g. "count sheep" or "count patches with [pcolor = green]"
                            metrics.turtles = list("monkeys" = c("xcor", "ycor",
                                                                 "energy", "steps-moved")
                                                   ), # "who" "color"
                            metrics.patches = c("pxcor", "pycor", "pcolor"),
                            variables = list(
                              
                              # energy
                              # "energy-from-seeds" = list(min=1, max = 10, step = 2),
                              # 'energy-from-prey' = list(min=1, max=16, step = 3),
                              # "energy-loss-traveling" = list(min=-10, max = 0, step = 2),
                              # "energy-loss-foraging" = list(min=-10, max = 0, step = 2),
                              # "energy-loss-resting" = list(min=-10, max = 0, step = 2),
                              
                              # memory
                              "step_forget" = list(min=0, max = 150, step = 15),
                              # "visual" = list(min=0, max = 3, step = 1)
                              
                              # movement
                              "travel_speed_val" = list(min=0.5, max = 1, step = 0.1),
                              "foraging_speed_val" = list(min= 0.5, max = 1, step = 0.1)
                              # "duration" = list(min=0, max = 10, step = 2),
                              
                              # others
                              # 'species_time_val' = list(min = 1, max = 6, step = 2)
                              ),
                            
                              # (
                              #   'start-energy' = list(min=10, max=170, step = 40, qfun="qunif")
                              #                ),
                            constants = list(
                              
                              ### "true" for output related stuff
                              "output-files?" = "false", #THIS IS VERY IMPORTANT
                              "print-step?" = "false",
                              'export-png'= "false",
                              "show-energy?" = "false",
                              "show-path?" = "false",
                              "all-slp-trees?" = "false",
                              "display-hatched-trees?" = "false",
                              "path-color-by-day?" = "false",
                              
                              ### resource scenario
                              "no_days" = 20,
                              'feeding-trees-scenario' = "\"trees_all\"",
                              'feeding-trees?' = "true",
                              'sleeping-trees?' = "true",
                              'resting-trees?' = "false",
                              'sleeping-trees-scenario' = "\"empirical\"",
                              'empirical-trees-choice' = "\"closest\"",
                              'species_time_val' = 6,
                              
                              ### memory
                              'duration' = 3,
                              'visual' = 2,
                              
                              ### energy
                              'start-energy' = 70,
                              "energy_level_1" = 80,
                              "energy_level_2" = 150,
                              "energy-from-seeds" = 4,# ?
                              "energy-from-prey" = 4,
                              "energy-loss-traveling" = -1.6,
                              "energy-loss-foraging" = -2,
                              "energy-loss-resting" = -1.9,
                              "gut_transit_time_val" = 15,
                              "n_seeds_hatched" = 1,
                              
                              ### movement
                              # "travel_speed_val" = 0.7,
                              # "foraging_speed_val" = 0.7,
                              
                              ### others
                              "simulation-time" = 108
                              
                            )
)


# Step 3: Attach a simulation design.
nl@simdesign <- simdesign_ff(nl,
                             nseeds = 10)

# Step 4: Run simulations
# Evaluate nl object:
eval_variables_constants(nl)

print(nl)

# Run all simulations (loop over all siminputrows and simseeds)

library(future)
library(tictoc)

plan(multisession)
tictoc::tic()
progressr::handlers("progress")
results <- progressr::with_progress(
  run_nl_all(nl,
             split = 4)
)
tictoc::toc()

# Step 5: Attach results to nl and run analysis
# In order to run the analyze_nl function, the simulation output has to be attached to the nl object first. The simdesign class within the nl object provides a slot for attaching output results (simoutput). An output results tibble can be attached to this slot by using the simdesign setter function setsim(nl, "simoutput"). After attaching the simulation results, these can also be written to the defined outpath of the experiment object.
# Attach results to nl object:
setsim(nl, "simoutput") <- results

nl@experiment@metrics.turtles
nl@experiment@variables




##### Screening data #####
results_unnest <- unnest_simoutput(nl)
results.sf <- nl_to_points(nl, coords = "px")

# Split tibble into turtles and patches tibbles and select each 10th step:
results_unnest_turtles <- results_unnest %>% 
  dplyr::filter(agent=="monkeys")

nl
eval_simoutput(nl)


















#### LHS experiment design ####

# Step 2: Attach an experiment
expname = "v1-model-fullfact"

nl@experiment <- experiment(expname = expname,
                            outpath = outpath,
                            repetition = 1,
                            tickmetrics = "true", # "false" for metrics only in the end of the simulation
                            idsetup = "setup",
                            idgo = "run_days",
                            # idfinal = "",
                            # idrunnum = "nlrx_id",
                            runtime = 0, #(if = 0 or NA_integer_, define stopcond_)
                            stopcond= "simulation-time-end", # reporter that returns TRUE
                            evalticks = NA_integer_, # NA_integer_ = measures each tick
                            # reporters:
                            metrics = c("count sleeping-trees"), # e.g. "count sheep" or "count patches with [pcolor = green]"
                            metrics.turtles = list("monkeys" = c("xcor", "ycor",
                                                                 "energy", "steps-moved")
                            ), # "who" "color"
                            metrics.patches = c("pxcor", "pycor", "pcolor"),
                            variables = list(
                              
                              # energy
                              # "energy-from-seeds" = list(min=1, max = 10, step = 2),
                              # 'energy-from-prey' = list(min=1, max=16, step = 3),
                              # "energy-loss-traveling" = list(min=-10, max = 0, step = 2),
                              # "energy-loss-foraging" = list(min=-10, max = 0, step = 2),
                              # "energy-loss-resting" = list(min=-10, max = 0, step = 2),
                              
                              # memory
                              "step_forget" = list(min=0, max = 150, qfun="qunif"),
                              # "visual" = list(min=0, max = 3, step = 1)
                              
                              # movement
                              "travel_speed_val" = list(min=0, max = 1, qfun="qunif"),
                              "foraging_speed_val" = list(min= 0, max = 1, qfun="qunif")
                              # "duration" = list(min=0, max = 10, step = 2),
                              
                              # others
                              # 'species_time_val' = list(min = 1, max = 6, step = 2)
                            ),
                            
                            # (
                            #   'start-energy' = list(min=10, max=170, step = 40, qfun="qunif")
                            #                ),
                            constants = list(
                              
                              ### "true" for output related stuff
                              "output-files?" = "false", #THIS IS VERY IMPORTANT
                              "print-step?" = "false",
                              'export-png'= "false",
                              "show-energy?" = "false",
                              "show-path?" = "false",
                              "all-slp-trees?" = "false",
                              "display-hatched-trees?" = "false",
                              "path-color-by-day?" = "false",
                              
                              ### resource scenario
                              "no_days" = 20,
                              'feeding-trees-scenario' = "\"trees_all\"",
                              'feeding-trees?' = "true",
                              'sleeping-trees?' = "true",
                              'resting-trees?' = "false",
                              'sleeping-trees-scenario' = "\"empirical\"",
                              'empirical-trees-choice' = "\"closest\"",
                              'species_time_val' = 6,
                              
                              ### memory
                              'duration' = 3,
                              'visual' = 2,
                              
                              ### energy
                              'start-energy' = 70,
                              "energy_level_1" = 80,
                              "energy_level_2" = 150,
                              "energy-from-seeds" = 4,# ?
                              "energy-from-prey" = 4,
                              "energy-loss-traveling" = -1.6,
                              "energy-loss-foraging" = -2,
                              "energy-loss-resting" = -1.9,
                              "gut_transit_time_val" = 15,
                              "n_seeds_hatched" = 1,
                              
                              ### movement
                              # "travel_speed_val" = 0.7,
                              # "foraging_speed_val" = 0.7,
                              
                              ### others
                              "simulation-time" = 108
                              
                            )
)


# Step 3: Attach a simulation design.
nl@simdesign <- simdesign_lhs(nl=nl,
                              samples=100,
                              nseeds=10,
                              precision=3)
# Step 4: Run simulations
# Evaluate nl object:
eval_variables_constants(nl)

print(nl)

# Run all simulations (loop over all siminputrows and simseeds)

library(future)
library(tictoc)

plan(multisession)
tictoc::tic()
progressr::handlers("progress")
results <- progressr::with_progress(
  run_nl_all(nl,
             split = 1)
)
tictoc::toc()

# Step 5: Attach results to nl and run analysis
# In order to run the analyze_nl function, the simulation output has to be attached to the nl object first. The simdesign class within the nl object provides a slot for attaching output results (simoutput). An output results tibble can be attached to this slot by using the simdesign setter function setsim(nl, "simoutput"). After attaching the simulation results, these can also be written to the defined outpath of the experiment object.
# Attach results to nl object:
setsim(nl, "simoutput") <- results

nl@experiment@metrics.turtles
nl@experiment@variables





