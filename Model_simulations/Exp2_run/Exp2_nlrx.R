## Header --------------------------
# Script name: Exp2_nlrx
# Script purpose: Run the v1.1 Model procedure with the imported build-forest 
# landscapes (Experiment 2, previously experiment 3, see Batch_design.xlsx)

# Most parameterizations are within the model already (velocity, angles, gtt, etc)
# As we simulated general resources, we set the species time to a random value every
# time tamarins select a new feeding tree (i.e. each tree will yield a random amount of
# energy from 1 to 6 times the energy-from-fruit value)

# Building from earlier model runs and the genetic analysis, we use the parameters obtained from
# Santa Maria to all the following params 

# 'duration' = 3,
# 'visual' = 2,
# "step_forget" = 130,

# 'start-energy' = 980,
# "energy_level_1" = 80,
# "energy_level_2" = 150,
# "energy-from-fruits" = 4,#c ?
# "energy-from-prey" = 4,
# "energy-loss-traveling" = -1.6,
# "energy-loss-foraging" = -2,
# "energy-loss-resting" = -1.9,

# Here we also control some parameters we have been parameterizing so far for model development:
# - simulation time  -> mean of empirical values
# - day time -> still the same as for Guareí
# - p_foraging_while_traveling -> mean of empirical values



# Finally, we analyze (to decide to filter out) tamarin runs that ended up with tamarins dying 
# based on the global survived?
#
# Date created: 2023-01-10d
# Author: Eduardo Zanette

## Notes --------------------------- 

## Packages -------------------------
library("nlrx")
library("here")
library("dplyr")
library("stringr")
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
if(Sys.info()[["nodename"]] == "DESKTOP-R12V3D6") {
  path <- "D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest/"
  netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
  # modelpath <- here("Model_simulations", "Model_v1.1.nlogo")
  modelpath <- "D:/Data/Documentos/github/BLT_IBM-Model/Model_simulations/BLT_model_v1.1.nlogo"
  outpath <- paste0(path, "Experiment2/")
  user_scp = "\"Eduardo\"" # scaped
}
if(Sys.info()[["nodename"]] == "PC9") { # LEEC
  # path <- "D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest/"
  netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
  # modelpath <- here("Model_development", "BLT_model_v1.1.nlogo")
  # outpath <- here("Model_analysis", "Genetic_analysis", "temp")
  Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8.0_351")
  user_scp = "\"LEEC\"" # scaped
}


nl <- nl(nlversion = "6.2.2",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 2048)

nlogo_model_param <- report_model_parameters(nl)
nlogo_model_param


## List files patch generated csv files ------
pathfiles <- paste0(path, "Experiment3/batch_all/") # Exp 1 and 2 turned into Exp 1 and Exp 3 turned into Exp 2

files_forests <- list.files(pathfiles, pattern = ".csv")
# files_forests <- paste0(pathfiles, "/", files_forests)

# n files should be 2700 but it is 1069:
length(files_forests) # number of patches * home ranges (10 reps) * number of clumped/random/ordered resourcers

#### Simple design experiment ( = one go button, no varibles) ####

## Step 2: Attach an experiment
expname <- "Exp2_v1.1"

set.seed(1234)


### Empirical data for parameterisation ----
param_table <- read.csv(here("Data", "Parameter_table.csv"),
                        sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  dplyr::mutate(group = recode(group, "Guarei" = "Guareí")) # only to match those of the NetLogo model

pathga <- here("Model_analysis", "Genetic_analysis", "temp")

filesga <- list.files(pathga, pattern = "feedingbouton.csv")
filesga <- paste0(pathga, "/", filesga)

dfga <- data.frame("expname" = character(), "parameter" = character(), "value" = double())
for (i in filesga) {
  # i <- filesga[2]
  filei <- read.csv(i)
  dfga <- dplyr::bind_rows(dfga, filei)
} 

dfga <- dfga %>% dplyr::filter(expname == "SantaMaria_Mar") # We will use Santa Maria values

### memory
duration <- dfga %>% dplyr::filter(parameter=="duration") %>% pull("value")
# visual = 2,
step_forget <- dfga %>% dplyr::filter(parameter=="step_forget") %>% pull("value")

### energy
start_energy <- dfga %>% dplyr::filter(parameter=="start-energy") %>% pull("value")
energy_level_1 <- dfga %>% dplyr::filter(parameter=="energy_level_1") %>% pull("value")
energy_level_2 <- dfga %>% dplyr::filter(parameter=="energy_level_2") %>% pull("value")
energy_from_fruits <- dfga %>% dplyr::filter(parameter=="energy-from-fruits") %>% pull("value")
energy_from_prey <- dfga %>% dplyr::filter(parameter=="energy-from-prey") %>% pull("value")
energy_loss_traveling <- dfga %>% dplyr::filter(parameter=="energy-loss-traveling") %>% pull("value")
energy_loss_foraging <- dfga %>% dplyr::filter(parameter=="energy-loss-foraging") %>% pull("value")
energy_loss_resting <- dfga %>% dplyr::filter(parameter=="energy-loss-resting") %>% pull("value")


### define how much each run should take based on empirical activity periods
no_days_run <- 10
simultime_run <- param_table %>%  dplyr::select(mean_timesteps) %>% pull() %>% mean()
simultime_run <- round(simultime_run * 0.95) # that's the timestep when tamarins should start looking for the sleeping site


### choose which parameterizations should be on
step_model_param <- "true" # velocity parameters are setted inside the model. up to 100 ha = Guareí data, above 200 and up to 2000 = Santa Maria data, after = Taquara data
gtt_param <- "true" # gtt parameters are setted inside the model. Is is a rough estimate of gtt of all groups (although Suzano seems to have significantly smaller SDDs)
p_forage_param <- "false" # we want to take some variation out of the runs, so we are using a specific value
p_forage_val <- param_table %>% dplyr::select(p_foraging_while_traveling) %>% pull() %>% mean()
feedingbout <- "true" # we are running general trees, and these values are specified inside the model (rough estimate, hardwired). A value is drawn from a normal distribution with 2.5 mean and 3 of sd. 


### escape strings to nlrx experiment ----
# month_run_scp <- paste0('"', month_run, '"') # scaped
pathfiles <- paste0('"', pathfiles, '"'); noquote(pathfiles) # scaped
files_forests <- paste0('"', files_forests, '"'); noquote(files_forests) # scaped
files_forests[1]



### Define expname ---- 
expname = paste0("Exp2_2023-01-10d")



# # Loop through all forest files in .csv (import-world ---

# for (i in files_forests) {
  
  # Test loop:
  # i <- files_forests[sample(length(files_forests), 1)]
  i <- files_forests[999]
  
  
  ### Attach to nl object ----
  nl@experiment <- experiment(expname = expname,
                              outpath = outpath,
                              repetition = 1, # number of repetitions with the same seed (tamarins can spawn in distinct sleeping sites and acquire distinct species_time)
                              tickmetrics = "false", # "true" for every tick, "false" for metrics only in the end of the simulation
                              idsetup = "setup",
                              idgo = "go",
                              runtime = 2000, #(if = 0 or NA_integer_, define stopcond_)
                              stopcond= "day > no_days", # reporter that returns TRUE
                              evalticks = NA_integer_, # NA_integer_ = measures each tick. Only applied if tickmetrics = TRUE
                              idfinal = "r:stop", # for making r NetLogo extension to work: https://cran.r-project.org/web/packages/nlrx/vignettes/furthernotes.html
                              # reporters:
                              metrics = c(
                                "survived?", # if tamarins are alive
                                
                                # patch generation variables
                                "patch-size-ha",
                                "field.shape.factor",
                                "density",
                                # "FRAC2", # not used in this epxeriment
                                
                                # home range generation variables
                                "hr-size-final", # final hr_size
                                # "hr_x", # id of run per home range (= replicates)
                                
                                # resource generation variables
                                # "n-clusters", # n-clusters were held constant (=10)
                                # "n-sleeping-trees", # also constant (n=5)
                                "n",
                                "p-visited-trees",
                                "R_seeds",        
                                "R_seeds_p",      
                                "NN_seeds",       
                                "NN_feeding_trees", # this is calculated again by the end of the run although it was calculated priorly within the build_forest process
                                "NN_sleeping_trees", # might be a more important factor affecting SDD than the NN of feeding trees 
                                
                                # turtle variables as globals:
                                  "g_energy_stored", # energy is reset to energy-start everyday, thus we take
                                  # "g_energy",      # final energy                # the best set of parameters should make tamarins viable in energetic terms
                                  # "g_enlvl1",         # value of energy_level_1 used at the start of the simulation
                                  # "g_enlvl2",         # value of energy_level_2 used at the start of the simulation 
                                  # "g_enstart",          # start-energy value at the start of the similuation
                                  "g_DPL",         # mean DPL (in case we don't want all the values -> good for bar and pointrange plots)
                                  # "g_DPL_d",       # DPL is set to 0 everyday    # the best set of parameters should reproduce the observed DPL
                                  "g_DPL_sd",      # sd DPL  (in case we don't want all the values -> good for bar and pointrange plots)
                                  "g_KDE_95",      # final value                 # the best set of parameters should reproduce the observed home range
                                  "g_KDE_50",      # final value                 # the best set of parameters should reproduce the observed core area
                                  "g_p_feeding",   # final value                 # the best set of parameters should optimize the activity budget
                                  "g_p_foraging",  # final value                 # the best set of parameters should optimize the activity budget
                                  "g_p_traveling", # final value                 # the best set of parameters should optimize the activity budget
                                  "g_p_resting",    # final value                 # the best set of parameters should optimize the activity budget
                                  # "g_step_length_mean",    # besides the parameterization, agents interactions make the observed step length and turning angles change
                                  # "g_step_length_sd",      # besides the parameterization, agents interactions make the observed step length and turning angles change
                                  # "g_turn_ang_mean",     # this one is quite consistent, so I don't think this one is necessary
                                  # "g_turn_ang_sd",          # this one might be interesting though (but I haven't estimated the empirical ones yet)
                                  
                                  # additional movement variables
                                  "g_MR",               # movement rate (MR) is used to predict SDD by primates: http://doi.wiley.com/10.1002/ajp.22659
                                  "g_MR_sd",
                                  # "g_MSD",              # other modelling studies have used this one (https://doi.org/10.3390/ani12182412.), but I believe it is very similar to MR
                                  # "g_intensity_use",    # bether than MSD in my oppinion: read about it in: https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en
                                  "g_PT",               # Highly recommended as it is the only unbiased estimator of tortuosity. Almost the same thing as intensity of use (IU). path twisting is used by Fuzessy et al 2017 to predict SDD among primates: http://doi.wiley.com/10.1002/ajp.22659
                                  "g_PT_sd",
                                  # "g_straightness",   # straightness is being wrongly estimated. DON'T USE IT NOW (first make it to be calculated in daily basis). straightness and sinuosity are slightlty different in terms of properties (https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en) and they were not tested as predictors of SDD, so i'm not using them
                                  # "g_sinuosity"       # sinuosity can't be compared across scales. DON'T USE IT straightness and sinuosity are slightlty different in terms of properties (https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en) and they were not tested as predictors of SDD, so i'm not using them
                                  
                                  "g_n_visited_trees",
                                  "g_n_unvisited_trees"
                                  
                              # metrics.turtles = list(
                                
                                # empty because I'm outputing everything as globals
                                
                                # "monkeys" = c(
                                #   "energy_stored", # energy is reset to energy-start everyday, thus we take the surplus energy as indicator if the energy variables are good enough
                                #   # "energy",      # final energy                # the best set of parameters should make tamarins viable in energetic terms
                                #   # "enlvl1",         # value of energy_level_1 used at the start of the simulation
                                #   # "enlvl2",         # value of energy_level_2 used at the start of the simulation 
                                #   # "enstart",          # start-energy value at the start of the similuation
                                #   "DPL",         # mean DPL (in case we don't want all the values -> good for bar and pointrange plots)
                                #   # "DPL_d",       # DPL is set to 0 everyday    # the best set of parameters should reproduce the observed DPL
                                #   "DPL_sd",      # sd DPL  (in case we don't want all the values -> good for bar and pointrange plots)
                                #   "KDE_95",      # final value                 # the best set of parameters should reproduce the observed home range
                                #   "KDE_50",      # final value                 # the best set of parameters should reproduce the observed core area
                                #   "p_feeding",   # final value                 # the best set of parameters should optimize the activity budget
                                #   "p_foraging",  # final value                 # the best set of parameters should optimize the activity budget
                                #   "p_traveling", # final value                 # the best set of parameters should optimize the activity budget
                                #   "p_resting",    # final value                 # the best set of parameters should optimize the activity budget
                                #   # "step_length_mean",    # besides the parameterization, agents interactions make the observed step length and turning angles change
                                #   # "step_length_sd",      # besides the parameterization, agents interactions make the observed step length and turning angles change
                                #   # "turn_ang_mean",     # this one is quite consistent, so I don't think this one is necessary
                                #   # "turn_ang_sd",          # this one might be interesting though (but I haven't estimated the empirical ones yet)
                                #   
                                #   # additional movement variables
                                #   "MR",               # movement rate (MR) is used to predict SDD by primates: http://doi.wiley.com/10.1002/ajp.22659
                                #   "MR_sd",
                                #   # "MSD",              # other modelling studies have used this one (https://doi.org/10.3390/ani12182412.), but I believe it is very similar to MR
                                #   # "intensity_use",    # bether than MSD in my oppinion: read about it in: https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en
                                #   "PT",               # Highly recommended as it is the only unbiased estimator of tortuosity. Almost the same thing as intensity of use (IU). path twisting is used by Fuzessy et al 2017 to predict SDD among primates: http://doi.wiley.com/10.1002/ajp.22659
                                #   "PT_sd",
                                #   # "straightness",   # straightness is being wrongly estimated. DON'T USE IT NOW (first make it to be calculated in daily basis). straightness and sinuosity are slightlty different in terms of properties (https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en) and they were not tested as predictors of SDD, so i'm not using them
                                #   # "sinuosity"       # sinuosity can't be compared across scales. DON'T USE IT straightness and sinuosity are slightlty different in terms of properties (https://www.scielo.br/j/zool/a/8F9QpD7mRFttmkY9QdxZTmm/?format=pdf&lang=en) and they were not tested as predictors of SDD, so i'm not using them
                                #   
                                #   "n_visited_trees",
                                #   "n_unvisited_trees"
                                #   
                                # ) 
                                # 
                              ), # "who" "color"
                              variables = list(
                                
                                ### we are running simples parameterized runs, so it is not needed
                                
                                # energy
                                # "energy-from-fruits" = list(values=c(5, 10, 20, 30, 40, 50, 100, 175, 250)),
                                # 'energy-from-prey' = list(values=c(5, 10, 20, 30, 40, 50, 100, 175, 250)),
                                # "energy-loss-traveling" = list(values=c(-5, -10, -20, -30, -40, -50, -100, -175, -250)),
                                # "energy-loss-foraging" = list(values=c(-5, -10, -20, -30, -40, -50, -100, -175, -250)),
                                # "energy-loss-resting" = list(values=c(-5, -10, -20, -30, -40, -50, -100, -175, -250)),
                                # 
                                # "start-energy" = list(min=100, max=1900, step = 300),
                                # "energy_level_1" = list(min=100, max=1900, step = 300),
                                # "energy_level_2" = list(min=100, max=1900, step = 300),
                                # 
                                
                                # 4. Movement
                                # "step_len_travel" = list(min= 0, max = 100)         # only when step-model-param? = 'false'
                                # "step_len_forage" = list(min= 0, max = 100)         # only when step-model-param? = 'false'
                                # "max_rel_ang_travel_75q" = list(min= 0, max= 180)   # only when step-model-param? = 'false'
                                # "max_rel_ang_forage_75q" = list(min= 0, max= 180)   # only when step-model-param? = 'false'
                                
                                # "p_foraging_while_traveling" = list(min= 0, max= 1) # only when p-forage-param? = 'false'
                                
                                # memory
                                # "step_forget" = list(min=0, max = 150, step = 30),
                                # "visual" = list(min=0, max = 3),
                                # 'prop_trees_to_reset_memory' = list(min=2, max=5, step = 1),   # Initially I didn't think this one is needed (mainly because of the first sensitivity analysis in Guareí), but this might help (as step_forget) making some regions of the home range to not be targeted
                                
                                # 5. Feeding bout (only when "feedingbout-on?" = 'false')
                                # 'species_time' = list(min = 1, max = 10, step = 1),
                                
                                # 'duration' = list(min = 1, max = 10, step = 2)      #
                                
                                # 6. Seed dispersal
                                # "gut_transit_time_val" = 15,    # this won't be optimized as it is an emerging pattern
                                # "n_seeds_hatched" = 1           # this won't be optimized as it is an emerging pattern
                              ),
                              
                              constants = list(
                                
                                "path" = pathfiles,
                                "generated_patch" = i,
                                
                                # "monkey_runs" = monkey_runs, # for know we are running only once (or use repetition in nlrx slot)
                                
                                "USER" = user_scp, # "\"Eduardo\"",
                                'feedingbout-on?' = feedingbout,        # uses empirical values of time spent feeding on each tree species or a random one (specified in species_time_val)
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
                                # "study_area" = area_run_scp,               # "\"Taquara\"",   # we are optimizing with Taquara as it is the most natural condition
                                # "feeding-trees-scenario" = month_run_scp,  #"\"Jan\"",        # we are optimizing with Taquara as it is the most natural condition
                                'no_days' = no_days_run, # DON'T TRY no_days = 1
                                'simulation-time' = simultime_run,
                                "p_foraging_while_traveling" = p_forage_val,
                                # 'feeding-trees?' = "true",
                                # 'sleeping-trees?' = "true",
                                # 'sleeping-trees-scenario' = "\"empirical\"",
                                # 'empirical-trees-choice' = "\"closest\"",
                                
                                ### memory
                                'duration' = duration,
                                # 'visual' = 2,
                                "step_forget" = step_forget,
                                
                                ### energy
                                'start-energy' = start_energy,
                                "energy_level_1" = energy_level_1,
                                "energy_level_2" = energy_level_2,
                                "energy-from-fruits" = energy_from_fruits,# ?
                                "energy-from-prey" = energy_from_prey,
                                "energy-loss-traveling" = energy_loss_traveling,
                                "energy-loss-foraging" = energy_loss_foraging,
                                "energy-loss-resting" = energy_loss_resting
                                
                                
                                # seed dispersal
                                # "gut_transit_time_val" = 15,
                                # "n_seeds_hatched" = 1,
                                
                                
                                ### movement
                                # travel_speed = 1
                                
                              )
  )


  
  
  # report_model_parameters(nl)
  
  
  
  nseeds <- 1 # repetitions are specified in n-reps slider
  
  # Step 3: Attach a simulation design.
  # nl@simdesign <- simdesign_distinct(nl, nseeds = 17)
  nl@simdesign <- simdesign_simple(nl, nseeds = nseeds)
  
  # Step 4: Run simulations
  # Evaluate nl object:
  # eval_variables_constants(nl)
  
  # print(nl)
  
  # nl@simdesign
  
  
  # Run all simulations (loop over all siminputrows and simseeds)
  
  
  ## With run_nl_one (with only the first seed)
  tictoc::tic()
  progressr::handlers("progress")
  # results <- progressr::with_progress(run_nl_one(nl,
  #                                                seed = seed, #[1], # only first seed (simple run)
  #                                                siminputrow = 1))
  results <- progressr::with_progress(run_nl_one(nl,
                                                 seed = getsim(nl, "simseeds")[1],
                                                 siminputrow = 1
                                                 ))
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
  i <- files_forests[sample(length(files_forests), 1)]
  i <- i %>% str_remove(".csv") %>% str_remove_all("\"") # unquote
  # i <- i %>% noquote()
  
  filename <-
         paste0(outpath, 
                i, 
                ".rds"
                )
  filename
  
  
  saveRDS(nl, file = filename)
  # rm(nl)
  
  # nl <- readRDS(filename)
  
  gc()
  
  # i <- i + 1
  
  
# }




##### Screening data #####
# results_unnest <- unnest_simoutput(nl) # for some reason unnest_simoutput does not work
results_unnest <- getsim(nl, "simoutput")
  
  # nl2 <- readRDS(filename)

# # "Study/Mestrado/Model_Documentation/build_forest/"
#   nl <- readRDS(paste0("D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest/",  
#                           "generate-R-aggreg-values.rds"))
#   
#   # results <- unnest_simoutput(nl)
#   results <- nl@simdesign@simoutput
# 
# results <- results %>%
#     rename(
#       R_value = `bs-R_feeding_trees`,
#       p_value = `bs-R_feeding_trees_p`,
#       NN_dist = `bs-NN_feeding_trees`
#     ) %>% 
#   mutate(
#     point_pattern = case_when(
#       R_value > 1 & p_value <= 0.05 ~ "ordered",
#       R_value < 1 & p_value <= 0.05 ~ "clustered",
#       TRUE ~ "random"
#     )
#   )
# 
# results$point_pattern %>% table()
# 
# results_p <- results %>% 
#   dplyr::filter(p_value <= 0.05)
# 
# results_p$point_pattern %>% as.factor() %>% levels()
# 