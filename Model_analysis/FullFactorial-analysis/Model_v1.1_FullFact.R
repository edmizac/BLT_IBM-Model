# Script name: Model v1.1 Sensitivity Analysis

# Date created:
# 2022-08-02d
# Author: Eduardo Zanette and Ronald Bialozyt

## Notes --------------------------- 
#
#

## Options -------------------------
# (plotting, memory limit, decimal digits)
# 
library(tictoc)
tic("The whole script")
expname = "v1p1_FullFact_30days"
nseeds <- 5 # please use at least 5 seeds
reps <- 1 # number of repetitions with the same seed
with_phenology <- "false"

# Java memory:
if(Sys.info()[["sysname"]] == "Linux") {
    if(Sys.info()[["nodename"]] == "simul02") {
        Sys.setenv(JAVA_HOME = "/usr/lib/jvm/java-11-openjdk-amd64")
        ## unixtools::set.tempdir(".")
    }
    if(Sys.info()[["nodename"]] == "Simul01") {
        Sys.setenv(JAVA_HOME = "/usr/lib/jvm/java-8-oracle")
    }
} else {
    Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk1.8.0_321")
}


## Packages -------------------------
library("here")
library("nlrx")
library("tidyverse")
library("dplyr")
library("ggplot2")
## Packages -------------------------

ncores <- parallel::detectCores()
ncrores <- round(ncores * 0.9)

# vignette: https://docs.ropensci.org/nlrx/articles/sensitivity.html

# Step 1: Create nl object
## netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
if(Sys.info()[["nodename"]] == "simul02") {
    netlogopath <- file.path("/home/rbialozyt/Software/NetLogo 6.2.0")
    modelpath <- "/home/rbialozyt/BLT_IBM-Model/Model_development/BLT_model_v1.1.nlogo"
}
if(Sys.info()[["nodename"]] == "Simul01") {
    netlogopath <- file.path("/home/rbialozyt/Software/netlogo_622")
    modelpath <- "/home/rbialozyt/BLT_IBM-Model/Model_development/BLT_model_v1.1.nlogo"
}
if(Sys.info()[["nodename"]] == "PC146") {
    netlogopath <- file.path("/opt/netlogo_620")
    modelpath <- paste0("/home/rbialozyt/ownCloud-Forst/Projektideen/FAPESP_Project_Eduardo/"
                      , "BLT_IBM-Model/Model_development/BLT_model_v1.1.nlogo")
}
## netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
## modelpath <- here("Model_development", "BLT_model_v1.1.nlogo")
# C:\Program Files\NetLogo 6.2.2\app\models
file.info(modelpath)

if(Sys.info()[["nodename"]] == "simul02") {
    outpath <- ("/home/rbialozyt/BLT_IBM-Model/Model_analysis/FullFactorial-analysis")
}
if(Sys.info()[["nodename"]] == "Simul01") {
    outpath <- ("/home/rbialozyt/BLT_IBM-Model/Model_analysis/FullFactorial-analysis")
}
if(Sys.info()[["nodename"]] == "PC146") {
    outpath <-  paste0("/home/rbialozyt/ownCloud-Forst/Projektideen/FAPESP_Project_Eduardo/"
                      , "BLT_IBM-Model/Model_analysis/FullFactorial-analysis")
}    

nl <- nl(nlversion = "6.2.0",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

report_model_parameters(nl)


# Step 2: Attach an experiment

# no_days = 10

nl@experiment <- experiment(expname = expname,
                            outpath = outpath,
                            repetition = reps, # number of repetitions with the same seed
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
                                                                 # "xcor", "ycor",
                                                                 "energy", "behavior",
                                                                 "dist-traveled",
                                                                 "travel_mode")
                                                   
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
                              "step_forget" = list(min=1, max = 151, step = 30, qfun="qunif"),
                              "visual" = list(min=1, max = 3, step = 1, qfun="qunif"),
                              # "prop_trees_to_reset_memory" = list(min = 2, max = 5, step = 1, qfun="qunif"), ## > 1
                              
                              # movement
                              # "travel_speed_val" = list(min = 0.3, max = 1, step = 0.1, qfun="qunif"),
                              "p-foraging-while-traveling" = list(min = 0.1, max = 0.6, step = 0.1, qfun="qunif"), 
                              "duration" = list(min=1, max = 6, step = 2, qfun="qunif")
                              
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
                              # "USER" = "\"Eduardo\"",
                              # "print-step?" = "false",
                              # 'export-png'= "false",
                              # "show-energy?" = "false",
                              # "show-path?" = "false",
                              # "all-slp-trees?" = "false",
                              # "display-hatched-trees?" = "false",
                              # "path-color-by-day?" = "false",
                              
                              ### resource scenario
                              # "no_days" = 10, # DON'T TRY no_days = 1
                              # 'feeding-trees-scenario' = "\"trees_jul\"",
                              # 'feeding-trees?' = "true",
                              # 'sleeping-trees?' = "true",
                              # 'resting-trees?' = "false",
                              # 'sleeping-trees-scenario' = "\"empirical\"",
                              # 'empirical-trees-choice' = "\"closest\"",
                              
                              ### memory
                              # 'duration' = 3,
                              # 'visual' = 2,
                              # "step_forget" = 130,
                              
                              ### energy
                              # 'start-energy' = 124,
                              # "energy_level_1" = 99,
                              # "energy_level_2" = 143,
                              # "energy-from-fruits" = 10,# ?
                              # "energy-from-prey" = 1.8,
                              # "energy-loss-traveling" = -1.8,
                              # "energy-loss-foraging" = -1.7,
                              # "energy-loss-resting" = -1.8,
                              # "gut_transit_time_val" = 13,
                              # "n_seeds_hatched" = 1,
                              
                              ### movement
                               # "travel_speed_val" = 0.7,
                              
                              ## phenology
                              "phenology-on?" = with_phenology# , # does not work as variable
                              
                              ### others
                              # "simulation-time" = 108
                              
                            )
)



# Step 3: Attach a simulation design.
nl@simdesign <- simdesign_ff(nl,
                             nseeds = nseeds)

# Step 4: Run simulations
# Evaluate nl object:
eval_variables_constants(nl)

print(nl)

# Run all simulations (loop over all siminputrows and simseeds)

library(future)

# plan(multisession)
## plan(list(sequential, multiseprocess))
# plan(list(sequential, multisession))  ## On Simul01
plan(list(sequential, cluster))  ## On Simul01
## plan(list(sequential, multicore))
# split_param <- min(nrow(nl@simdesign@siminput), ((ncores - 2)/nseeds))
tictoc::tic()
progressr::handlers("progress")
results %<-% progressr::with_progress(
                            run_nl_all(nl = nl
                                     , split = 9)
                            )
tictoc::toc()
plan(sequential)
print("================ Finished! =========================")

# Check if it is needed:
results$USER <- NULL
results$'feeding-trees-scenario' <- NULL
results$'start-energy' <- NULL
results$energy_level_1  <- NULL
results$energy_level_2 <- NULL
results$'energy-from-fruits' <- NULL
results$'energy-from-prey' <- NULL
results$'energy-loss-traveling' <- NULL
results$'energy-loss-foraging' <- NULL
results$'energy-loss-resting' <- NULL
results$gut_transit_time_val <- NULL
results$n_seeds_hatched <- NULL

### movement
results$travel_speed_val <- NULL

### others
results$'simulation-time' <- NULL 

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
today <- strsplit(as.character(Sys.time()), "\ ")[[1]][1]
# save(nl, file = paste0(nl@experiment@outpath, "/"
#      , "nl_object_2022-07-20_phenology-off.RData"))
saveRDS(nl, file.path(
    paste0(nl@experiment@outpath, "/"
           , expname, "_", today, "_phenology-"
           , if(with_phenology) {"on"} else {"off"}
           , ".rds")
    )
)
#print("================ save unnest =========================")
#results_unnest <- unnest_simoutput(nl)
#save(results_unnest,file = paste0(nl@experiment@outpath, "/"
#                                , "results_unnest_2022-07-20_phenology-"
#                                , if(with_phenology) {"on"} else {"off"}
#                                , ".RData"))

# morris <- analyze_nl(nl)


#### Check Model parameters ####
# > report_model_parameters(nl)
toc()
