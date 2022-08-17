## ---------------------------
## Script name: Model v1.1 simple
## Script purpose: Start nlrx workflow
## Date created: 2022-08-17d
## Author: Eduardo Zanette

## ---------------------------
## Notes: Fiz esse script só pra fazer single runs do mundo com os patches e 
## arvores de alimntação pra fazer o gif pra apresentação do CBPr
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
## ---------------------------


# Step 1: Create nl object
## netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
modelpath <- here("Model_development", "BLT_model_v1.1.nlogo")
# modelpath <- here("C:/Program Files/NetLogo 6.2.2", "app/models", "BLT_model_v1.nlogo")
# C:\Program Files\NetLogo 6.2.2\app\models

# file.info(modelpath)

outpath <- here("Model_development", "Model-cleaning", "runtime")


nl <- nl(nlversion = "6.2.2",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

report_model_parameters(nl)




#### Simple design experiment ( = one go button, no varibles) ####

# Step 2: Attach an experiment
expname = "v1.1_patches-and-trees"
# no_days = 10

nl@experiment <- experiment(expname = expname,
                            outpath = outpath,
                            repetition = 1, # number of repetitions with the same seed
                            tickmetrics = "false", # "false" for metrics only in the end of the simulation
                            idsetup = "setup",
                            idgo = "go",
                            # idfinal = "",
                            # idrunnum = "nlrx_id",
                            runtime = 1500, #(if = 0 or NA_integer_, define stopcond_)
                            stopcond= "day > no_days", # reporter that returns TRUE
                            # evalticks = NA_integer_, # NA_integer_ = measures each tick. Only applied if tickmetrics = TRUE
                            # reporters:
                            metrics = c("timestep", "day"), # e.g. "count sheep" or "count patches with [pcolor = green]"
                            metrics.turtles = list("monkeys" = c("x_UTM", "y_UTM",
                                                                 # "xcor", "ycor",
                                                                 "energy", "behavior",
                                                                 "dist-traveled",
                                                                 "travel_mode "),
                                                   "feeding-trees" = c("x_UTM", "y_UTM",
                                                                      "species", "id-tree"),
                                                   "sleeping-trees" = c("x_UTM", "y_UTM",
                                                                        "id-tree")
                                                   
                                                   # , "steps-moved"
                                                   
                            ), # "who" "color"
                            metrics.patches = c("pxcor", "pycor", "habitat"),
                            variables = list(),
                            constants = list(
                              "USER" = "\"Eduardo\"",
                              "no_days" = 10 # DON'T TRY no_days = 1
                              # 'feeding-trees-scenario' = "\"trees_all\"",
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
filename <- here("Model_development", "Model-cleaning", "runtime", "v1.1_simple_worldRDS.Rdata")
saveRDS(nl, file = filename) ; rm(results)
nl <- readRDS(filename)

rm(results)
gc()
