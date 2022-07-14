# Script name: Model v1.1 Sensitivity Analysis

# Date created:
Sys.time()
# Author: Eduardo Zanette

## Notes --------------------------- 
#
#

## Options -------------------------
# (plotting, memory limit, decimal digits)
# 

## Packages -------------------------
library("here")
library("nlrx")
library("tidyverse")
library("dplyr")
library("ggplot2")
## Packages -------------------------


# vignette: https://docs.ropensci.org/nlrx/articles/sensitivity.html

# Step 1: Create nl object
netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
modelpath <- here("Model_development", "BLT_model_v1.1.nlogo")
# Ronald:
# netlogopath <- file.path("/opt/netlogo_622")
# modelpath <- here("C:/Program Files/NetLogo 6.2.2", "app/models", "BLT_model_v1.nlogo")
# C:\Program Files\NetLogo 6.2.2\app\models
file.info(modelpath)

outpath <- here("Model_analysis", "Sensitivity-analysis")


nl <- nl(nlversion = "6.2.2",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 8192)

report_model_parameters(nl)


# Step 2: Attach an experiment
expname = "v1.1_sensitivity_2022-07-14d"
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
                              "step_forget" = list(min=0, max = 150, step = 10, qfun="qunif"),
                              "visual" = list(min=0, max = 3, step = 1, qfun="qunif"),
                              "prop_trees_to_reset_memory" = list(min = 1, max = 8, step = 1, qfun="qunif"),
                              
                              # movement
                              "travel_speed_val" = list(min = 0.3, max = 1, step = 0.1, qfun="qunif"),
                              "p-foraging-while-traveling" = list(min = 0.05, max = 0.5, step = 0.05, qfun="qunif")
                              # "foraging_speed_val" = list(min= 0, max = 1, step = 2)
                              # "duration" = list(min=0, max = 10, step = 2),
                              
                              # phenology
                              
                              
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
                              # "USER" = "\"Ronald\"",
                              "USER" = "\"Eduardo\"",
                              # "print-step?" = "false",
                              # 'export-png'= "false",
                              # "show-energy?" = "false",
                              # "show-path?" = "false",
                              # "all-slp-trees?" = "false",
                              # "display-hatched-trees?" = "false",
                              # "path-color-by-day?" = "false",
                              
                              ### resource scenario
                              "no_days" = 10, # DON'T TRY no_days = 1
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
                              
                              ## phenology
                              "phenology-on?" = "true" # does not work as variable
                              
                              ### others
                              # "simulation-time" = 108
                              
                            )
)



# Step 3: Attach a simulation design.
# install.packages("morris")
# library("morris")
nl@simdesign <- simdesign_morris(nl = nl,
                                 morristype = "oat",
                                 morrislevels = 10, # sets the number of different values for each parameter (sampling density)
                                 morrisr = 100, # sets the number of repeated samplings (sampling size)
                                 morrisgridjump = 5, # sets the number of levels that are increased/decreased for computing the elementary effects. . Morris recommendation is to set this value to levels / 2.
                                 nseeds = 5)


# More information on the Morris specific parameters can be found in the description of the morris function in the sensitivity package (?morris).
?morris


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
             split = 3)
)
tictoc::toc()












#### Model parameters ####
# > report_model_parameters(nl)
# $`start-energy`
# $`start-energy`$type
# [1] "SLIDER"
# 
# $`start-energy`$value
# [1] 107
# 
# $`start-energy`$min
# [1] NA
# 
# $`start-energy`$max
# [1] 170
# 
# $`start-energy`$incr
# [1] 1
# 
# 
# $`show-energy?`
# $`show-energy?`$type
# [1] "SWITCH"
# 
# $`show-energy?`$value
# [1] TRUE
# 
# 
# $`show-path?`
# $`show-path?`$type
# [1] "SWITCH"
# 
# $`show-path?`$value
# [1] TRUE
# 
# 
# $`simulation-time`
# $`simulation-time`$type
# [1] "SLIDER"
# 
# $`simulation-time`$value
# [1] 108
# 
# $`simulation-time`$min
# [1] 0
# 
# $`simulation-time`$max
# [1] 170
# 
# $`simulation-time`$incr
# [1] 1
# 
# 
# $`energy-from-fruits`
# $`energy-from-fruits`$type
# [1] "SLIDER"
# 
# $`energy-from-fruits`$value
# [1] 10
# 
# $`energy-from-fruits`$min
# [1] 0
# 
# $`energy-from-fruits`$max
# [1] 15
# 
# $`energy-from-fruits`$incr
# [1] 1
# 
# 
# $no_days
# $no_days$type
# [1] "INPUTBOX"
# 
# $no_days$value
# [1] "6.0"
# 
# $no_days$entrytype
# [1] "Number"
# 
# 
# $`energy-from-prey`
# $`energy-from-prey`$type
# [1] "SLIDER"
# 
# $`energy-from-prey`$value
# [1] 1.8
# 
# $`energy-from-prey`$min
# [1] 0
# 
# $`energy-from-prey`$max
# [1] 15
# 
# $`energy-from-prey`$incr
# [1] 0.1
# 
# 
# $`energy-loss-traveling`
# $`energy-loss-traveling`$type
# [1] "SLIDER"
# 
# $`energy-loss-traveling`$value
# [1] -1.8
# 
# $`energy-loss-traveling`$min
# [1] -10
# 
# $`energy-loss-traveling`$max
# [1] 0
# 
# $`energy-loss-traveling`$incr
# [1] 0.1
# 
# 
# $`energy-loss-foraging`
# $`energy-loss-foraging`$type
# [1] "SLIDER"
# 
# $`energy-loss-foraging`$value
# [1] -2.2
# 
# $`energy-loss-foraging`$min
# [1] -10
# 
# $`energy-loss-foraging`$max
# [1] 0
# 
# $`energy-loss-foraging`$incr
# [1] 0.1
# 
# 
# $`energy-loss-resting`
# $`energy-loss-resting`$type
# [1] "SLIDER"
# 
# $`energy-loss-resting`$value
# [1] -1.9
# 
# $`energy-loss-resting`$min
# [1] -10
# 
# $`energy-loss-resting`$max
# [1] 0
# 
# $`energy-loss-resting`$incr
# [1] 0.1
# 
# 
# $runtime
# $runtime$type
# [1] "INPUTBOX"
# 
# $runtime$value
# [1] "./runtime/"
# 
# $runtime$entrytype
# [1] "String"
# 
# 
# $`feeding-trees-scenario`
# $`feeding-trees-scenario`$type
# [1] "CHOOSER"
# 
# $`feeding-trees-scenario`$value
# [1] "trees_may"
# 
# $`feeding-trees-scenario`$validvalues
# [1] "trees_all" "trees_may" "trees_jun" "trees_jul" "trees_aug"
# 
# 
# $`sleeping-trees-scenario`
# $`sleeping-trees-scenario`$type
# [1] "CHOOSER"
# 
# $`sleeping-trees-scenario`$value
# [1] "empirical"
# 
# $`sleeping-trees-scenario`$validvalues
# [1] "empirical" "simulated"
# 
# 
# $`export-png`
# $`export-png`$type
# [1] "SWITCH"
# 
# $`export-png`$value
# [1] TRUE
# 
# 
# $step_forget
# $step_forget$type
# [1] "SLIDER"
# 
# $step_forget$value
# [1] 69
# 
# $step_forget$min
# [1] 0
# 
# $step_forget$max
# [1] 1000
# 
# $step_forget$incr
# [1] 1
# 
# 
# $gut_transit_time_val
# $gut_transit_time_val$type
# [1] "SLIDER"
# 
# $gut_transit_time_val$value
# [1] 13
# 
# $gut_transit_time_val$min
# [1] 0
# 
# $gut_transit_time_val$max
# [1] 100
# 
# $gut_transit_time_val$incr
# [1] 1
# 
# 
# $travel_speed_val
# $travel_speed_val$type
# [1] "SLIDER"
# 
# $travel_speed_val$value
# [1] 0.7
# 
# $travel_speed_val$min
# [1] 0
# 
# $travel_speed_val$max
# [1] 1
# 
# $travel_speed_val$incr
# [1] 0.1
# 
# 
# $species_time_val
# $species_time_val$type
# [1] "SLIDER"
# 
# $species_time_val$value
# [1] 2
# 
# $species_time_val$min
# [1] 0
# 
# $species_time_val$max
# [1] 100
# 
# $species_time_val$incr
# [1] 1
# 
# 
# $energy_level_1
# $energy_level_1$type
# [1] "SLIDER"
# 
# $energy_level_1$value
# [1] 97
# 
# $energy_level_1$min
# [1] 0
# 
# $energy_level_1$max
# [1] 200
# 
# $energy_level_1$incr
# [1] 1
# 
# 
# $energy_level_2
# $energy_level_2$type
# [1] "SLIDER"
# 
# $energy_level_2$value
# [1] 117
# 
# $energy_level_2$min
# [1] NA
# 
# $energy_level_2$max
# [1] 1000
# 
# $energy_level_2$incr
# [1] 1
# 
# 
# $n_seeds_hatched
# $n_seeds_hatched$type
# [1] "SLIDER"
# 
# $n_seeds_hatched$value
# [1] 0
# 
# $n_seeds_hatched$min
# [1] 0
# 
# $n_seeds_hatched$max
# [1] 100
# 
# $n_seeds_hatched$incr
# [1] 1
# 
# 
# $`empirical-trees-choice`
# $`empirical-trees-choice`$type
# [1] "CHOOSER"
# 
# $`empirical-trees-choice`$value
# [1] "closest"
# 
# $`empirical-trees-choice`$validvalues
# [1] "closest" "random" 
# 
# 
# $`sleeping-trees?`
# $`sleeping-trees?`$type
# [1] "SWITCH"
# 
# $`sleeping-trees?`$value
# [1] TRUE
# 
# 
# $`resting-trees?`
# $`resting-trees?`$type
# [1] "SWITCH"
# 
# $`resting-trees?`$value
# [1] TRUE
# 
# 
# $`feeding-trees?`
# $`feeding-trees?`$type
# [1] "SWITCH"
# 
# $`feeding-trees?`$value
# [1] TRUE
# 
# 
# $`all-slp-trees?`
# $`all-slp-trees?`$type
# [1] "SWITCH"
# 
# $`all-slp-trees?`$value
# [1] TRUE
# 
# 
# $`print-step?`
# $`print-step?`$type
# [1] "SWITCH"
# 
# $`print-step?`$value
# [1] TRUE
# 
# 
# $duration
# $duration$type
# [1] "SLIDER"
# 
# $duration$value
# [1] 1
# 
# $duration$min
# [1] 0
# 
# $duration$max
# [1] 20
# 
# $duration$incr
# [1] 1
# 
# 
# $visual
# $visual$type
# [1] "SLIDER"
# 
# $visual$value
# [1] 2
# 
# $visual$min
# [1] 0
# 
# $visual$max
# [1] 10
# 
# $visual$incr
# [1] 1
# 
# 
# $`display-hatched-trees?`
# $`display-hatched-trees?`$type
# [1] "SWITCH"
# 
# $`display-hatched-trees?`$value
# [1] TRUE
# 
# 
# $`path-color-by-day?`
# $`path-color-by-day?`$type
# [1] "SWITCH"
# 
# $`path-color-by-day?`$value
# [1] TRUE
# 
# 
# $`output-files?`
# $`output-files?`$type
# [1] "SWITCH"
# 
# $`output-files?`$value
# [1] TRUE
# 
# 
# $USER
# $USER$type
# [1] "CHOOSER"
# 
# $USER$value
# [1] "Eduardo"
# 
# $USER$validvalues
# [1] "Ronald"  "Eduardo" "Others" 
# 
# 
# $`output-print?`
# $`output-print?`$type
# [1] "SWITCH"
# 
# $`output-print?`$value
# [1] TRUE
# 
# 
# $`p-foraging-while-traveling`
# $`p-foraging-while-traveling`$type
# [1] "SLIDER"
# 
# $`p-foraging-while-traveling`$value
# [1] 0.3
# 
# $`p-foraging-while-traveling`$min
# [1] 0
# 
# $`p-foraging-while-traveling`$max
# [1] 1
# 
# $`p-foraging-while-traveling`$incr
# [1] 0.05
# 
# 
# $`random-angle?`
# $`random-angle?`$type
# [1] "SWITCH"
# 
# $`random-angle?`$value
# [1] TRUE
# 
# 
# $`phenology-on?`
# $`phenology-on?`$type
# [1] "SWITCH"
# 
# $`phenology-on?`$value
# [1] TRUE
# 
# 
# $prop_trees_to_reset_memory
# $prop_trees_to_reset_memory$type
# [1] "SLIDER"
# 
# $prop_trees_to_reset_memory$value
# [1] 8
# 
# $prop_trees_to_reset_memory$min
# [1] 1
# 
# $prop_trees_to_reset_memory$max
# [1] 8
# 
# $prop_trees_to_reset_memory$incr
# [1] 1
