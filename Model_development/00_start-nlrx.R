## ---------------------------
## Script name: 00_start-nlrx
## Script purpose: Start nlrx workflow
## Date created: 2022-01-22d
## Author: Eduardo Zanette

## ---------------------------
## Notes:
##   
## ---------------------------

## Working directory:
getwd()

## ---------------------------
## Options (plotting, memory limit, decimal digits)
install.packages('unixtools', repos = 'http://www.rforge.net/')
unixtools::set.tempdir("<path-to-temp-dir>")


if(Sys.getenv("JAVA_HOME") == "") {
        Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk1.8.0_321")
}

## ---------------------------
## Packages:
library(nlrx)
library(here)
library(progressr)
## ---------------------------

vignette(nlrx) # not available. Try:
# https://docs.ropensci.org/nlrx/articles/getstarted.html

# Step 1: Create nl object
netlogopath <- file.path("C:/Program Files/NetLogo 6.2.0")
modelpath <- here("Model_Mayara2020", "BLT_model_2020.nlogo")
outpath <- here("Model_development", "runtime")

nl <- nl(nlversion = "6.2.0",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 4096)

report_model_parameters(nl)

# Step 2: Attach an experiment
# Let's say I want to test the energy related varibles from the model. That includes:
# start_energy and energy from specific food items
nl@experiment <- experiment(expname = "Guarei-Mayara_setup-experiment",
                            outpath = outpath,
                            repetition = 1,
                            tickmetrics = "false", # I want tick_counts and no_days of the end of simulations
                            idsetup = "setup",
                            idgo = "run_days",
                            # idfinal = "",
                            runtime = 110, #(if = NA_integer_, define stopcond_)
                            # stopcond= x, # reporter that returns TRUE
                            evalticks = 110,
                            # reporters:
                            metrics = c("count sleeping-trees", "count resting-trees"), # e.g. "count sheep" or "count patches with [pcolor = green]"
                            metrics.turtles = list("monkeys" = c("who", "pxcor", "pycor", 
                                                                 "energy", "steps-moved")), # "who" "color"
                            # metrics.patches = c(c("pxcor", "pycor", "pcolor"))),
                            # global variables:
                            variables = list('start-energy' = list(min=10, max=170, step = 1, qfun="qunif")
                                             ),
                            # ,
                            # 'energy-from-seeds' = list(min=0, max=15, qfun="qunif"),
                            # 'energy-from-prey' = list(min=0, max=15, qfun="qunif"),
                            # 'energy-loss-traveling' = list(min=0, max=15, qfun="qunif"),
                            # 'energy-loss-foraging' = list(min=0, max=15, qfun="qunif"),
                            # 'energy-loss-resting' = list(min=0, max=15, qfun="qunif")
                            # 
                            constants = list("no_days" = 31,
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


# 
# # OR
# nl@experiment <- experiment(expname = "Guarei-Mayara_setup-experiment",
#                             outpath = outpath,
#                             repetition = 1,
#                             tickmetrics = "false",
#                             idsetup = "setup",
#                             idgo = "run_days",
#                             runtime = (31*110),
#                             evalticks = 110,
#                             # metrics = c(list("turtles" = c("who", "pxcor", "pycor", "color",
#                             #                                "energy", "steps-moved")
#                             # )),
#                             # variables = list('start-energy' = list(min=10, max=170, qfun="qunif")
#                             # ),
#                             # constants = list()
#                             )

# Step 3: Attach a simulation design.
nl@simdesign <- simdesign_lhs(nl=nl,
                              samples=30,
                              nseeds=3,
                              precision=3)

# nl@simdesign <- simdesign_simple(nl=nl,
#                                  nseeds=3,
#                                  )


# Step 4: Run simulations
# Evaluate nl object:
eval_variables_constants(nl) # energy variables not recognized a constants?
print(nl)

# Run all simulations (loop over all siminputrows and simseeds)
progressr::handlers("progress")
results <- run_nl_all(nl)

progressr::handlers("progress")
results <- run_nl_one(nl,
                      seed = getsim(nl, "simseeds")[1],
                      siminputrow = 1)







# Step 5: Attach results to nl and run analysis
# In order to run the analyze_nl function, the simulation output has to be attached to the nl object first. The simdesign class within the nl object provides a slot for attaching output results (simoutput). An output results tibble can be attached to this slot by using the simdesign setter function setsim(nl, "simoutput"). After attaching the simulation results, these can also be written to the defined outpath of the experiment object.
# Attach results to nl object:
setsim(nl, "simoutput") <- results

# Write output to outpath of experiment within nl
write_simoutput(nl)

# Do further analysis:
analyze_nl(nl)






