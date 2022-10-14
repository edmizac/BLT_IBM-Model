## Header --------------------------
# Script name: 01_nlrx_all-areas
# Script purpose: Test nlrx within all areas and with avoid-matrix procedures
# Date created: 2022-10-14d
# Author: Eduardo Zanette

## Notes --------------------------- 
#
#

## Options -------------------------
# (plotting, memory limit, decimal digits)
# Packages from github:
# install.packages('devtools')
# devtools::install_github('thomasp85/gganimate')
# remotes::install_github("ropensci/nlrx")

# Java memory:
if(Sys.getenv("JAVA_HOME") == "") {
  Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk1.8.0_321")
}

## Packages -------------------------
library(nlrx)
library(here)
library(progressr)
library(ggplot2)
# library(gganimate)


# Step 1: Create nl object
netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")

modelpath <- here("Model_development", "BLT_model_v1.1.nlogo")
file.info(modelpath)

outpath <- here("Model_development", "runtime")

nl <- nl(nlversion = "6.2.2",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 8192)

report_model_parameters(nl)


## Packages:
library("nlrx")
library("here")
library("progressr")
library("ggplot2")
library("ggspatial")
library("gganimate")
## ---------------------------

## Options (plotting, memory limit, decimal digits)
theme_set(theme_bw())

# GIS
our_crs <- "+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
gua_xlim_all = c(780000, 782350)
gua_ylim_all = c(7407080, 7408250)
gua.x.min = 781587
gua.x.max = 782361.5
gua_xlim_set = c(781200, 782350)
gua_ylim_set = c(7407250, 7408250) 
sma_xlim_set = c(364400, 366000)
sma_ylim_set = c(7540500, 7541700)
taq_xlim_set = c(370500, 373200)
taq_ylim_set = c(7498750, 7500550)
suz_xlim_set = c(705300, 706200)
suz_ylim_set = c(7480800, 7481600)

## sf objects
gua.polyg <- sf::st_read(here("Data", "shapefiles", "Fragment polygon.shp"))

## sf plots 
gua.sf <- ggplot2::ggplot() +
  geom_sf(data = gua.polyg,
          color = "black",
          fill = "#A9DFBF") +
  coord_sf(
    datum = sf::st_crs(our_crs),
    xlim = gua_xlim_set,    
    ylim = gua_ylim_set) +  
  theme_bw() +
  annotation_scale(location = "bl", width_hint = .35) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0, "cm"), pad_y = unit(.8, "cm"),
                         style = north_arrow_fancy_orienteering)

# World colors:
pal <- data.frame(breed = c("forest", "matrix", "sleeping-tree", "feeding-tree", "seeds"),
                  color = c("#F6F9BF", "#BFF8B7", "#E3A9F7", "#1FCD2B", "black"),
                  shape = c( NA, NA, 23, 23, 15),
                  size = c(1, 1, 2, 1.5, 0.3)
)

# Tamarin behavior colors:
simulated_shapes <- c("foraging" = 1,
                      "frugivory" = 19, 
                      "resting" = 1, 
                      "sleeping" = 17, 
                      "travel" = 1)

simulated_colors <- c("foraging" = "magenta",
                      "frugivory" = "#1E8449", 
                      "resting" = "grey", 
                      "sleeping" = "#E74C3C", 
                      "travel" = "orange")

simulated_sizes <- c("foraging" = 1.5,
                     "frugivory" = 2.5, 
                     "resting" = 1.5, 
                     "sleeping" = 3, 
                     "travel" = 1.5)

simulated_colors_noforage <- c("grey", "#1E8449", "grey", "#E74C3C", "grey")

empirical_shapes <- c(1, 19, 1, 1, 1, 1, 1, 1, 1, 17, 1, 1)
empirical_colors <- c("magenta", "#1E8449", "grey", "grey", "grey", "grey", "grey", 
                      "grey", "grey", "#E74C3C", "grey", "grey")
## ---------------------------
# Packages from github:
# install.packages('devtools')
# devtools::install_github('thomasp85/gganimate')
# remotes::install_github("ropensci/nlrx")

# Java memory:
if(Sys.getenv("JAVA_HOME") == "") {
  Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk1.8.0_321")
}



# Step 1: Create nl object
## netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")

modelpath <- here("Model_development", "BLT_model_v1.1.nlogo")
# file.info(modelpath)
outpath <- here("Model_development", "runtime")
# modelpath <- here("C:/Program Files/NetLogo 6.2.2", "app/models", "BLT_model_v1.nlogo")
# C:\Program Files\NetLogo 6.2.2\app\models

nl <- nl(nlversion = "6.2.2",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

report_model_parameters(nl)




#### Simple design experiment ( = one go button, no varibles) ####

# Step 2: Attach an experiment
expname = "v1.1_simplerun"
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
                            evalticks = NA_integer_, # NA_integer_ = measures each tick. Only applied if tickmetrics = TRUE
                            # reporters:
                            metrics = c("timestep", "day"), # e.g. "count sheep" or "count patches with [pcolor = green]"
                            metrics.turtles = list("monkeys" = c("x_UTM", "y_UTM",
                                                                 # "xcor", "ycor",
                                                                 "energy", "behavior",
                                                                 "dist-traveled",
                                                                 "travel_mode ")
                                                   
                                                   # , "steps-moved"
                                                   
                            ), # "who" "color"
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
                              # "travel_speed_val" = list(values=seq(0.3, 1, by = 0.1))
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
                              "USER" = "\"Eduardo\"",
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
# nl@simdesign <- simdesign_distinct(nl, nseeds = 17)
nl@simdesign <- simdesign_simple(nl, nseeds = 17)

# Step 4: Run simulations
# Evaluate nl object:
eval_variables_constants(nl)

print(nl)

nl@simdesign

# Run all simulations (loop over all siminputrows and simseeds)

library(future)
library(tictoc)

# With run_nl_one (with only the first seed)
tictoc::tic()
progressr::handlers("progress")
results <- progressr::with_progress(
  run_nl_one(nl,
             seed = getsim(nl, "simseeds")[1], # only first seed (simple run)
             siminputrow = 1
  )
)
tictoc::toc()


# With run_nl_all (all 17 seeds)
# tictoc::tic()
# progressr::handlers("progress")
# results <- progressr::with_progress(
#   run_nl_all(nl,
#              split = 4
#              )
# )
# tictoc::toc()


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
filename <- here("Model_development", "Model-cleaning", "runtime", "v1.1_simple_tempRDS.Rdata")
saveRDS(nl, file = filename) ; rm(results)
nl <- readRDS(filename)

gc()

#' Go to file 00_Validation_patterns_v1 or ##### Screening data #####