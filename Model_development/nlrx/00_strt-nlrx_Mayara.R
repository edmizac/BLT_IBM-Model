# 00_strt-nlrx.R but with Mayara's model

if(Sys.getenv("JAVA_HOME") == "") {
  Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk1.8.0_321")
}

library(nlrx)
library(here)
library(progressr)

# Step 1: Create nl object
# netlogopath <- file.path("C:/Program Files/NetLogo 6.0.3")
netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
modelpath <- here("Model_development", "Model-cleaning",  "BLT_model_2020.nlogo")
outpath <- here("Model_development", "Model-cleaning", "runtime")

nl <- nl(nlversion = "6.2.2",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 4096)

report_model_parameters(nl)

# Step 2: Attach an experiment

# Simple design
expname = "setup-for-workflow"

nl@experiment <- experiment(expname = expname,
                            outpath = outpath,
                            repetition = 1,
                            tickmetrics = "true", # I want tick_counts and no_days of the end of simulations
                            idsetup = "setup",
                            idgo = "run_days",
                            # idfinal = "",
                            # idrunnum = "nlrx_id",
                            runtime = 108, #(if = NA_integer_, define stopcond_)
                            # stopcond= x, # reporter that returns TRUE
                            evalticks = 108,
                            # reporters:
                            metrics = c("count sleeping-trees", "count resting-trees"), # e.g. "count sheep" or "count patches with [pcolor = green]"
                            metrics.turtles = list("monkeys" = c("who", "pxcor", "pycor",
                                                                 "energy", "steps-moved")), # "who" "color"
                            # metrics.patches = c(c("pxcor", "pycor", "pcolor"))),
                            # global variables:
                            # variables = list('start-energy' = list(min=10, max=170, step = 10, qfun="qunif")
                            #                  ),
                            variables = list(),
                            # ,
                            # 'energy-from-seeds' = list(min=0, max=15, qfun="qunif"),
                            # 'energy-from-prey' = list(min=0, max=15, qfun="qunif"),
                            # 'energy-loss-traveling' = list(min=0, max=15, qfun="qunif"),
                            # 'energy-loss-foraging' = list(min=0, max=15, qfun="qunif"),
                            # 'energy-loss-resting' = list(min=0, max=15, qfun="qunif")
                            #
                            constants = list("no_days" = 4,
                                             'start-energy' = 70,
                                             "simulation-time" = 108,
                                             "show-energy?" = "false",
                                             "show-path?" = "false",
                                             # "travel_speed",
                                             # "foraging_speed",
                                             # "foraging_time",
                                             # "species_time",
                                             # "energy_species",
                                             "energy-from-seeds" = 2,# ?
                                             "energy-from-prey" = 5.4,
                                             "energy-loss-traveling" = -1.6,
                                             "energy-loss-foraging" = -1.3,
                                             "energy-loss-resting" = -1.0
                            )
)

# Step 3: Attach a simulation design.
# Simple
nl@simdesign <- simdesign_simple(nl,
                                 nseeds = 1)

# LHS
# nl@simdesign <- simdesign_lhs(nl=nl,
#                               samples=1,
#                               nseeds=1,
#                               precision=3)

# Step 4: Run simulations
# Evaluate nl object:
eval_variables_constants(nl) # energy variables not recognized a constants?

print(nl)

# Run all simulations (loop over all siminputrows and simseeds)

# Run all
# progressr::handlers("progress")
# results <- run_nl_all(nl)
# This gives n_days = 1552

# Run one
progressr::handlers("progress")
results <- progressr::with_progress(
  run_nl_one(nl,
             seed = getsim(nl, "simseeds")[1],
             siminputrow = 1,
             writeRDS = TRUE)
)
# This has prouced 432 runs with Mayara original model (simple design)

# Step 5: Attach results to nl and run analysis
# In order to run the analyze_nl function, the simulation output has to be attached to the nl object first. The simdesign class within the nl object provides a slot for attaching output results (simoutput). An output results tibble can be attached to this slot by using the simdesign setter function setsim(nl, "simoutput"). After attaching the simulation results, these can also be written to the defined outpath of the experiment object.
# Attach results to nl object:
setsim(nl, "simoutput") <- results

nl@experiment@metrics.turtles
nl@experiment@variables

nl@experiment@repetition


#### Screening data ####
results_unnest <- unnest_simoutput(nl)
results_points_x <- nl_to_points(nl, coords = "px")
results_points_y <- nl_to_points(nl, coords = "py")

# Split tibble into turtles and patches tibbles and select each 10th step:
results_unnest_turtles <- results_unnest %>% 
  dplyr::filter(agent=="monkeys")

nl
eval_simoutput(nl)


