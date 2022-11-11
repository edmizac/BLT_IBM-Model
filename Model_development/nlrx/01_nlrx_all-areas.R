## Header --------------------------
# Script name: 01_nlrx_all-areas
# Script purpose: Test nlrx within all areas and with avoid-matrix procedures
# Date created: 2022-10-14d
# Author: Eduardo Zanette

## Notes --------------------------- 
#
#

## Packages -------------------------
library("nlrx")
library("here")
library("progressr")
library("future")
library("tictoc")
library("ggplot2")
library("ggspatial")
library("gganimate")
library("tidyverse")

## ---------------------------

# Packages from github:
# install.packages('devtools')
# devtools::install_github('thomasp85/gganimate')
# remotes::install_github("ropensci/nlrx")

## ---------------------------

# Spatial plots
theme_set(theme_bw())

simulated_shapes <- c("foraging" = 1,
                      "frugivory" = 19, 
                      "resting" = 1, 
                      "sleeping" = 17, 
                      "travel" = 1)

simulated_colors <- c("foraging" = "magenta",
                      "frugivory" = "#1E8449", 
                      "resting" = "grey", 
                      "sleeping" = "#E74C3C", 
                      "travel" = "grey")

## ---------------------------


# Empirical data for parameterisation:
dat.summary <- read.csv(here("Data", "Movement", "Curated", "BLT_groups_data_summary_aftercleaning.csv"),
                                          sep = ";", dec = ".", stringsAsFactors = TRUE) %>% 
  dplyr::mutate(group = recode(group, "Guarei" = "Guareí")) # only to match those of the NetLogo model


# Options (plotting, memory limit, decimal digits)

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
  outpath <- here("Model_development", "runtime")
}


nl <- nl(nlversion = "6.2.2",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

report_model_parameters(nl)



#### Simple design experiment ( = one go button, no varibles) ####

## Step 2: Attach an experiment
exptype <- "simple"

i <- 1
for (i in i:nrow(dat.summary)) {
  
  # Define run and parameterisation: (all strings must use escaped quotes)
  area_run <- paste0('\"', dat.summary$group[i], '\"')
  month_run <- paste0('\"', dat.summary$id_month[i], '\"')
  
  # area_run <- '"Guareí"'
  # month_run <- '"Jun"'
  expname <-  paste0("v1.1_", 
                     gsub(area_run, pattern = ('\"'), replacement = '', fixed = T), 
                     "_", 
                     gsub(month_run, pattern = ('\"'), replacement = '', fixed = T), 
                     "_", exptype)
  expname <- expname %>% 
    str_replace_all(., c("-" = "_", "\\s+" = "_")) # Remove - and space characters.
  # print(expname)
  # i <- i + 1
#} # loop testing
  
  no_days_run <- dat.summary %>% 
    dplyr::filter(group == gsub(area_run, pattern = ('\"'), replacement = '', fixed = T),
                  id_month == gsub(month_run, pattern = ('\"'), replacement = '', fixed = T)) %>% 
    dplyr::select(ndays) %>% 
    pull() + 1 # one day more for "initializing" the model (take this first day out when analyzing data?)
  
  simultime_run <- dat.summary %>% 
    dplyr::filter(group == gsub(area_run, pattern = ('\"'), replacement = '', fixed = T),
                  id_month == gsub(month_run, pattern = ('\"'), replacement = '', fixed = T)) %>% 
    dplyr::select(mean_timesteps) %>% 
    pull()
  simultime_run <- round(simultime_run * 0.9) # that's the timestep when tamarins should start looking for the sleeping site
  
  empiricalvelocities <- "true" # velocity parameters are setted inside the model. Change this when velocity is summarized and inclued in dat.summary
  feedingbout <- "false" # previous sensitivity analysis showed that this does not matter for Guareí
  

# # Attach to nl experiment
nl@experiment <- experiment(expname = expname,
                            outpath = outpath,
                            repetition = 1, # number of repetitions with the same seed (use repetition = 1)
                            tickmetrics = "true", # "false" for metrics only in the end of the simulation
                            idsetup = "setup",
                            idgo = "go",
                            runtime = 2000, #(if = 0 or NA_integer_, define stopcond_)
                            stopcond= "day > no_days", # reporter that returns TRUE
                            evalticks = NA_integer_, # NA_integer_ = measures each tick. Only applied if tickmetrics = TRUE
                            # idfinal = "r:stop", # for making r NetLogo extension to work: https://cran.r-project.org/web/packages/nlrx/vignettes/furthernotes.html
                            # reporters:
                            metrics = c("timestep", "day"), # e.g. "count sheep" or "count patches with [pcolor = green]"
                            metrics.turtles = list("monkeys" = c("x_UTM", "y_UTM",
                                                                 # "xcor", "ycor",
                                                                 "energy", "behavior",
                                                                 "dist-traveled"#,
                                                                 #"travel_mode "
                                                                 )

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
                              "empirical-velocities?" = empiricalvelocities, #"true",
                              'feedingbout-on?' = feedingbout,
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
  
  
  
  
  # report_model_parameters(nl)



  nseeds <- 10 # repetitions (ideally n = 5)
  
  # Step 3: Attach a simulation design.
  # nl@simdesign <- simdesign_distinct(nl, nseeds = 17)
  nl@simdesign <- simdesign_simple(nl, nseeds = nseeds)
  
  # Step 4: Run simulations
  # Evaluate nl object:
  # eval_variables_constants(nl)
  
  # print(nl)
  
  # nl@simdesign
  
  
  # Run all simulations (loop over all siminputrows and simseeds)
  

  
k <- 1
for (seed in unique(nl@simdesign@simseeds)) {
  
  # seed <- getsim(nl, "simseeds")[k]
  # print(seed)
  # k <- k + 1
  # print(k)
# }
  
  seed <- getsim(nl, "simseeds")[k] 
  paste0("running seed ", seed[k])
  ## With run_nl_one (with only the first seed)
  tictoc::tic()
  progressr::handlers("progress")
  results <- progressr::with_progress(run_nl_one(nl,
                                                 seed = seed, #[1], # only first seed (simple run)
                                                 siminputrow = 1))
  tictoc::toc()
  
  
  # ## With run_nl_all (all 17 seeds)
  # # Check number of simimputrows:
  # siminput_nrow <- nrow(getsim(nl, "siminput"))
  # # siminput_nrow %%
  # 
  # tictoc::tic()
  # plan(multisession)
  # progressr::handlers("progress")
  # results <- progressr::with_progress(
  #   run_nl_all(nl,
  #              split = 1 # with simdesign = simple it is only possible to run one core?
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
  
  # rm(results)
  
  # nl@experiment@metrics.turtles
  # nl@experiment@metrics.patches
  # nl@experiment@variables
  
  #' Save RDS to avoid losing it by R abortion:
  filename <-
    here("Model_development",
         "runtime",
         "v1.1",
         paste0(expname, seed, "_tempRDS.Rdata"))
  saveRDS(nl, file = filename)
  # rm(nl)
  
  # nl <- readRDS(filename)
  
  gc()
  
  print(paste0("seed finished: ", seed))
  k <- k + 1
  
  }
  
  print(paste0("finishing ", expname))
  i <- i + 1
}




#' Go to file 00_Validation_patterns_v1 or ##### Screening data #####


##### Screening data #####
results_unnest <- unnest_simoutput(nl)
results_unnest <- results_unnest %>% 
  rename(x = x_UTM, y = y_UTM)

# Select random run by selecting random seed (run of x days = seed)
aux_run <- results_unnest$`random-seed` %>%
  na.exclude() %>%
  sample(size = 1)

results_unnest_turtles <- results_unnest %>%
  dplyr::filter(agent=="turtles") %>%               # by agent
  dplyr::filter(`random-seed` == aux_run)       # by random run (~= number of the agent = who)

# Check if is only one run (seed):
results_unnest_turtles$`random-seed` %>% unique()

# Plot
ggplot() +
  geom_path(data = results_unnest_turtles,
            aes(x = x, y = y),
            size = 0.15) +
  geom_point(data = results_unnest_turtles,
             aes(x = x, y = y
                 , group = behavior,
                 color = behavior,
                 shape = behavior
             ),
             size = 1.4) +
  scale_color_manual(values = simulated_colors) +
  scale_shape_manual(values = simulated_shapes) +
  ggtitle(paste0("Simulated data"), expname)





#### Factorial design experiment ( = various go buttons, with input matrix for varying parameters) ####

## Step 2: Attach an experiment
# exptype <- "fullfactorial"
