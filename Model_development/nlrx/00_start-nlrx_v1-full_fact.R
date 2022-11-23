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
    if(Sys.info()[["sysname"]] == "Linux") {
        Sys.setenv(JAVA_HOME = "/usr/lib/jvm/java-11-openjdk-amd64")
        unixtools::set.tempdir(".")
    } else {
        Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk1.8.0_321")
    }
}

## ---------------------------
## Packages:
library("nlrx")
library("here")
library("progressr")
library("tidyverse")
## ---------------------------

ncores <- parallel::detectCores()
nseeds <- 2

# vignette(nlrx) # not available. Try:
# https://docs.ropensci.org/nlrx/articles/getstarted.html


# Step 1: Create nl object
## netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
if(Sys.info()[["nodename"]] == "simul02") {
    netlogopath <- file.path("/home/rbialozyt/Software/NetLogo 6.2.0")
    modelpath <- "/home/rbialozyt/BLT_IBM-Model/Model_development/BLT_model_v1.11.nlogo"
}
if(Sys.info()[["nodename"]] == "PC146") {
    netlogopath <- file.path("/opt/netlogo_620")
    modelpath <- paste0("/home/rbialozyt/ownCloud-Forst/Projektideen/FAPESP_Project_Eduardo/"
                      , "BLT_IBM-Model/Model_development/BLT_model_v1.11.nlogo")
}

## modelpath <- here("C:/Program Files/NetLogo 6.2.2", "app/models", "BLT_model_v1.nlogo")

# C:\Program Files\NetLogo 6.2.2\app\models
file.info(modelpath)

if(Sys.info()[["nodename"]] == "simul02") {
    outpath <- ("/home/rbialozyt/BLT_IBM-Model/Model_development/Model-cleaning/runtime")
}
if(Sys.info()[["nodename"]] == "PC146") {
    outpath <-  paste0("/home/rbialozyt/ownCloud-Forst/Projektideen/FAPESP_Project_Eduardo/"
                      , "BLT_IBM-Model/Model_development/Model-cleaning/runtime")
}    

nl <- nl(nlversion = "6.2.0",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 8192)

report_model_parameters(nl)

#### Full fact experiment design ####

#' Step 2: Attach an experiment
expname = "v1-model-fullfact"

nl@experiment <- experiment(
    expname = expname,
    outpath = outpath,
    repetition = 1,
    tickmetrics = "true", # "false" for metrics only in the end of the
                          # simulation
    idsetup = "setup",
    idgo = "go",
                                        # idfinal = "",
                                        # idrunnum = "nlrx_id",
    runtime = 0, #(if = 0 or NA_integer_, define stopcond_)
    stopcond= "day > no_days", # "simulation-time-end", # reporter
                               # that returns TRUE
    evalticks = NA_integer_, # NA_integer_ = measures each tick
### reporters:
    metrics = c("count sleeping-trees"), # e.g. "count sheep" or
                                         # "count patches with [pcolor
                                         # = green]"
    metrics.turtles = list("monkeys" = c("xcor", "ycor"
                                       , "x_utm", "y_utm"
                                       , "energy", "steps-moved"
                                       , "action", "travel_mode")
                           ), # "who" "color"
    ## metrics.patches = c("pxcor", "pycor", "pcolor"),
    variables = list(
        ## energy
        ## "energy-from-seeds" = list(min=1, max = 10, step = 2),
        ## 'energy-from-prey' = list(min=1, max=16, step = 3),
        ## "energy-loss-traveling" = list(min=-10, max = 0, step = 2),
        ## "energy-loss-foraging" = list(min=-10, max = 0, step = 2),
        ## "energy-loss-resting" = list(min=-10, max = 0, step = 2),
### memory
        "step_forget" = list(min=50, max = 150, step = 50),
        ## "visual" = list(min=0, max = 3, step = 1)
### movement
        "travel_speed_val" = list(min=0.5, max = 1.1, step = 0.2)
        ## "foraging_speed_val" = list(min= 0.5, max = 1, step = 0.1)
        ## "duration" = list(min=0, max = 10, step = 2),
### others
        ## 'species_time_val' = list(min = 1, max = 6, step = 2)
    ),
    
    ## (
    ##   'start-energy' = list(min=10, max=170, step = 40, qfun="qunif")
    ##                ),
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
        "no_days" = 25,
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
        "energy-from-fruits" = 4,# ?
        "energy-from-prey" = 4,
        "energy-loss-traveling" = -1.6,
        "energy-loss-foraging" = -2,
        "energy-loss-resting" = -1.9,
        "gut_transit_time_val" = 15,
        "n_seeds_hatched" = 1,
        
### movement
        ## "travel_speed_val" = 0.7,
        ## "foraging_speed_val" = 0.7,
        
### others
        "simulation-time" = 108
        
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
library(tictoc)

# plan(multisession)
plan(list(sequential, multiprocess))

tictoc::tic()
split_param <- min(nrow(nl@simdesign@siminput), ((ncores - 2)/nseeds))
progressr::handlers("progress")
# results <- progressr::with_progress(
results %<-% progressr::with_progress(
                            run_nl_all(nl = nl
                                     , split = split_param)
                            )
tictoc::toc()

# Step 5: Attach results to nl and run analysis In order to run the
# analyze_nl function, the simulation output has to be attached to the
# nl object first. The simdesign class within the nl object provides a
# slot for attaching output results (simoutput). An output results
# tibble can be attached to this slot by using the simdesign setter
# function setsim(nl, "simoutput"). After attaching the simulation
# results, these can also be written to the defined outpath of the
# experiment object.  Attach results to nl object:
setsim(nl, "simoutput") <- results

# nl@experiment@metrics.turtles
# nl@experiment@variables

print(nl)


##### Screening data #####
results_unnest <- unnest_simoutput(nl)
## not running
## results.sf <- nl_to_points(nl, coords = "px") 
fragemento <- sf::st_read("../shape_files/polig_fragmento.shp")
monkeys <- results_unnest[ , c("agent", "breed", "x_utm", "y_utm"
                             , "energy", "action", "travel_mode"
                             , "step_forget", "travel_speed_val") ]
sp::coordinates(monkeys) <- c("x_utm","y_utm")
## sp::proj4string(monkeys) <- sf::st_crs(fragemento)
monkeys.sf <- sf::st_as_sf(monkeys)
sf::st_crs(monkeys.sf) <- sf::st_crs(fragemento)
par(mar=c(3,3,1,1))
plot(sf::st_geometry(fragemento), axes=TRUE, col="lightgreen", border="red")
plot(sf::st_geometry(monkeys.sf), pch=19, cex=0.6, add=TRUE)

print(nl)
eval_simoutput(nl)

