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
# Packages from github:
# install.packages('devtools')
# devtools::install_github('thomasp85/gganimate')

# Java memory:
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
# netlogopath <- file.path("C:/Program Files/NetLogo 6.0.3")
netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
modelpath <- here("Model_development", "Model-cleaning", "BLT_model_2022-developm.nlogo")
# modelpath <- here("Model_development", "Model-cleaning",  "BLT_model_2020_Ronald.nlogo")
# outpath <- here("Model_development", "Model-cleaning", "runtime")
outpath <- here("Model_development")

nl <- nl(nlversion = "6.2.2",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 8192)

report_model_parameters(nl)


#### Simple design experiment ( = one go button) ####

# Step 2: Attach an experiment
expname = "setup-for-workflow-simple"
  
nl@experiment <- experiment(expname = expname,
                            outpath = outpath,
                            repetition = 1,
                            tickmetrics = "true", # "false" for metrics only in the end of the simulation
                            idsetup = "setup",
                            idgo = "run_days",
                            # idfinal = "",
                            # idrunnum = "nlrx_id",
                            runtime = NA_integer_, #(if = NA_integer_, define stopcond_)
                            stopcond= 'simulation-time-end', # reporter that returns TRUE.
                            evalticks = NA_integer_, # only applied when tickmetrics = 'true'. NA_integer_ measures each tick
                            # reporters:
                            metrics = c("count sleeping-trees", "count resting-trees"), # e.g. "count sheep" or "count patches with [pcolor = green]"
                            metrics.turtles = list("turtles" = c("who", "xcor", "ycor"
                                                                 # "energy", "steps-moved"
                                                                 )), # "who" "color"
                            metrics.patches = c("pxcor", "pycor", "pcolor"),
                            # global variables:
                            # variables = list('start-energy' = list(min=10, max=170, step = 10, qfun="qunif")
                            #                  ),
                            variables = list(),
                            constants = list("no_days" = 4,
                                             'tree-scenario' = "\"trees_all_2\"",
                                             'sleeping-trees-scenario' = "\"simulated\"",
                                             'start-energy' = 70,
                                             "simulation-time" = 108,
                                             
                                             "energy-from-seeds" = 2,# ?
                                             "energy-from-prey" = 5.4,
                                             "energy-loss-traveling" = -1.6,
                                             "energy-loss-foraging" = -1.3,
                                             "energy-loss-resting" = -1.0,
                                             
                                             # "true" when you want export-view in the end of the run:
                                             'export-png'= "true",
                                             "show-energy?" = "true",
                                             "show-path?" = "true"
                                             )
                            )


# Step 3: Attach a simulation design.
# Simple
nl@simdesign <- simdesign_simple(nl,
                                 nseeds = 1)

# Step 4: Run simulations
# Evaluate nl object:
eval_variables_constants(nl) # energy variables not recognized a constants?

print(nl)

# Run one
results <- run_nl_one(nl,
                      seed = getsim(nl, "simseeds")[1],
                      siminputrow = 1)

# Step 5: Attach results to nl and run analysis
# In order to run the analyze_nl function, the simulation output has to be attached to the nl object first. The simdesign class within the nl object provides a slot for attaching output results (simoutput). An output results tibble can be attached to this slot by using the simdesign setter function setsim(nl, "simoutput"). After attaching the simulation results, these can also be written to the defined outpath of the experiment object.
# Attach results to nl object:
setsim(nl, "simoutput") <- results

# nl@experiment@metrics.turtles
# nl@experiment@variables

# saveRDS(nl, file = "nl_simple_2022-02-16d.rds")
nl <- readRDS(file = "nl_simple_2022-02-16d.rds")

### Screening data ###

## Method 1: unnest_simoutput()
nl@simdesign@simoutput$metrics.turtles[[109]] # TURTLE X AND Y:

results_unnest <- unnest_simoutput(nl)
str(results_unnest)
results_unnest$agent %>% as.factor() %>% levels() # no 'monkeys'
results_unnest$breed %>% as.factor() %>% levels() # ya 'monkeys'!

# Split tibble into turtles and patches tibbles and select each 10th step:

# by random seed
aux <- results_unnest$`random-seed` %>% 
  na.exclude() %>% 
  sample(size = 1)

# patches
results_unnest_patches <- results_unnest %>% 
  dplyr::filter(agent=="patches") %>% 
  dplyr::filter(`random-seed` == aux) %>% 
  dplyr::filter(`[step]` %in% seq(8, 108, by=10))

# turtles
results_unnest_turtles <- results_unnest %>% 
  dplyr::filter(agent=="turtles") %>%            # by agent
  dplyr::filter(`random-seed` == aux) %>%         # by day (=number of the agent = who)
  dplyr::filter(`[step]` %in% seq(8, 108, by=10))  # by steps

# Take unwanted breeds out
results_unnest <- results_unnest %>% filter(breed != "legend-trees")
results_unnest <- results_unnest %>% filter(breed != "resting-trees")
results_unnest$breed %>% as.factor() %>% levels()
results_unnest$agent %>% as.factor() %>% levels()

# monkey locations
results_unnest_monkeys <- results_unnest_turtles %>%
  dplyr::filter(breed=="monkeys")

# feeding-trees
results_unnest_trees <- results_unnest_turtles %>%
  dplyr::filter(breed=="feeding-trees")

# sleeping-trees
results_unnest_sleep <- results_unnest_turtles %>%
  dplyr::filter(breed=="sleeping-trees")

# seeds
results_unnest_seeds <- results_unnest_turtles %>%
  dplyr::filter(breed=="seeds")

rm(results_unnest)
# results_unnest_ok <- rbind(results_unnest_patches,
#                            results_unnest_turtles)

# Create normal plot
ggplot() +
  coord_equal() +
  geom_tile(data=results_unnest_patches, aes(x=pxcor, y=pycor, fill = factor(pcolor))) +
  geom_point()
  
  
# Create facet plot with many dfs
  library(ggplot2)
ggplot() +
  facet_wrap(~`[step]`, ncol=6, nrow = 2) +
  coord_equal() +
  
  geom_tile(data=results_unnest_patches, 
            aes(x=pxcor, y=pycor, fill=factor(pcolor))) +
  scale_fill_manual(breaks=c("49", "68"),
                    values = c("49" = "#F6F9BF", "68" = "#BFF8B7")) +
  
  geom_point(data=results_unnest_seeds, 
             aes(x = xcor, y = ycor, group = breed), 
             size = 0.5, color = "black", shape = 4) +
  
  geom_point(data=results_unnest_sleep, 
             aes(x = xcor, y = ycor, group = breed), 
             size = 1.5, color = "#E3A9F7", shape = 15) +

  geom_point(data=results_unnest_trees, 
             aes(x = xcor, y = ycor, group = breed), 
             size = 1, color = "black", shape = 23, fill = "#1FCD2B") +
  
  geom_point(data=results_unnest_monkeys, 
             aes(x = xcor, y = ycor, group = breed), 
             size = 4, color = "red", shape = 10) +
  
  guides(fill=guide_legend(title="Patch color")) +
  theme_minimal() +
  ggtitle("Output maps of each 10th simulation tick")

ggsave(filename = "setup-for-workflow-simple_multiple-layers.png",
       dpi = 300, width = 30, height = 20, units = "cm")
  
  

# # Create facet plot with different dfs (patches and turtles)
# library(ggplot2)
# ggplot() +
#   facet_wrap(~`[step]`, ncol=6, nrow = 2) +
#   coord_equal() +
#   geom_tile(data=results_unnest_patches, aes(x=pxcor, y=pycor, fill=factor(pcolor))) +
#   geom_point(data=results_unnest_turtles, aes(x = xcor, y = ycor, 
#                                               group = breed, color = breed,
#                                               size = breed, shape = breed), 
#              size = 1) +
#   scale_fill_manual(breaks=c("49", "68"),
#                     values = c("49" = "#F6F9BF", "68" = "#BFF8B7")) +
#   
#   scale_color_manual(breaks=c("seeds", "feeding-trees", 
#                               "sleeping-trees", "monkeys"
#                               # "resting-trees", "seeds"
#                               ), 
#                      values = c("seeds" = "grey", "feeding-trees" = "green",
#                                 "sleeping-trees" = "magenta", "monkeys" = "black"
#                                 # "seeds" = "red"
#                                 )) + 
#   
#   scale_size_manual(breaks=c("seeds", "feeding-trees", 
#                               "sleeping-trees", "monkeys"
#                               ),
#                      values = c("seeds" = 0.3, "feeding-trees" = 1,
#                                 "sleeping-trees" = 1, "monkeys" = 1.5)) +
#   
#   scale_shape_manual(breaks=c("seeds", "feeding-trees", 
#                               "sleeping-trees", "monkeys"
#                               ),
#                     values = c("seeds" = 1, "feeding-trees" = 1,
#                                "sleeping-trees" = 1, "monkeys" = 8)) +
#   
#   guides(fill=guide_legend(title="Patch color")) +
#   theme_minimal() +
#   ggtitle("Output maps of each 10th simulation tick")


# Create animated plot (GIF)
require(gganimate)
p1 <- ggplot() +
  
  geom_tile(data=results_unnest_patches, 
            aes(x=pxcor, y=pycor, fill=factor(pcolor))) +
  scale_fill_manual(breaks=c("49", "68"),
                    values = c("49" = "#F6F9BF", "68" = "#BFF8B7")) +
  
  geom_point(data=results_unnest_seeds, 
             aes(x = xcor, y = ycor, group = breed), 
             size = 0.5, color = "black", shape = 4) +
  
  geom_point(data=results_unnest_sleep, 
             aes(x = xcor, y = ycor, group = breed), 
             size = 1.5, color = "#E3A9F7", shape = 15) +
  
  geom_point(data=results_unnest_trees, 
             aes(x = xcor, y = ycor, group = breed), 
             size = 1, color = "black", shape = 23, fill = "#1FCD2B") +
  
  geom_point(data=results_unnest_monkeys, 
             aes(x = xcor, y = ycor, group = breed), 
             size = 4, color = "red", shape = 10) +
  
  guides(fill=guide_legend(title="Patch color")) +
  transition_time(`[step]`) +
  coord_equal() +
  labs(title = 'Step: {frame_time}') +
  theme_minimal() +
  ggtitle("Output maps of each 10th simulation tick")

# Animate the plot and use 1 frame for each step of the model simulations
gganimate::animate(p1, "setup-for-workflow-simple_multiple-layers.gif")
# gganimate::animate(p1, nframes = length(unique(results_unnest_patches$`[step]`)), 
#                    width=400, height=400, fps=4)

## Method 2: nl_to_points()
results_points_x <- nl_to_points(nl, coords = "px")
results_points_y <- nl_to_points(nl, coords = "py")



# Other
nl
eval_simoutput(nl)





#### Full fact experiment design ####

# Step 2: Attach an experiment
expname = "setup-for-workflow-fullfact"

nl@experiment <- experiment(expname = expname,
                            outpath = outpath,
                            repetition = 1,
                            tickmetrics = "false", # "false" for metrics only in the end of the simulation
                            idsetup = "setup",
                            idgo = "run_days",
                            # idfinal = "",
                            # idrunnum = "nlrx_id",
                            runtime = 108, #(if = NA_integer_, define stopcond_)
                            # stopcond= x, # reporter that returns TRUE
                            evalticks = NA_integer_, #measure each tick
                            # reporters:
                            metrics = c("count sleeping-trees", "count resting-trees"), # e.g. "count sheep" or "count patches with [pcolor = green]"
                            metrics.turtles = list("monkeys" = c("who", "xcor", "ycor",
                                                                 "energy", "steps-moved")
                                                   ), # "who" "color"
                            metrics.patches = c("pxcor", "pycor", "pcolor"),
                            # global variables:
                            variables = list('start-energy' = list(min=10, max=170, step = 40, qfun="qunif")
                                             ),
                            constants = list("no_days" = 4,
                                             'tree-scenario' = "\"trees_all_2\"",
                                             'sleeping-trees-scenario' = "\"simulated\"",
                                             # 'start-energy' = 70,
                                             "simulation-time" = 108,
                                             "energy-from-seeds" = 2,# ?
                                             "energy-from-prey" = 5.4,
                                             "energy-loss-traveling" = -1.6,
                                             "energy-loss-foraging" = -1.3,
                                             "energy-loss-resting" = -1,
                                             # "true" when you want export-view in the end of the run:
                                             'export-png'= "true",
                                             "show-energy?" = "true",
                                             "show-path?" = "true"
                            )
)


# Step 3: Attach a simulation design.
nl@simdesign <- simdesign_ff(nl,
                             nseeds = 1)

# Step 4: Run simulations
# Evaluate nl object:
eval_variables_constants(nl) # energy variables not recognized a constants?

print(nl)

# Run all simulations (loop over all siminputrows and simseeds)

progressr::handlers("progress")
results <- progressr::with_progress(
  run_nl_all(nl,
             split = 1)
)

# Step 5: Attach results to nl and run analysis
# In order to run the analyze_nl function, the simulation output has to be attached to the nl object first. The simdesign class within the nl object provides a slot for attaching output results (simoutput). An output results tibble can be attached to this slot by using the simdesign setter function setsim(nl, "simoutput"). After attaching the simulation results, these can also be written to the defined outpath of the experiment object.
# Attach results to nl object:
setsim(nl, "simoutput") <- results

nl@experiment@metrics.turtles
nl@experiment@variables




#### Screening data ####
results_unnest <- unnest_simoutput(nl)
results.sf <- nl_to_points(nl, coords = "px")

# Split tibble into turtles and patches tibbles and select each 10th step:
results_unnest_turtles <- results_unnest %>% 
  dplyr::filter(agent=="monkeys")

nl
eval_simoutput(nl)
