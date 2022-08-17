## ---------------------------
## Script name: Model v1.1 simple
## Script purpose: Start nlrx workflow
## Date created: 2022-08-17d
## Author: Eduardo Zanette

## ---------------------------
## Notes: Fiz esse script só pra fazer o gif pra apresentação do CBPr
##   
## ---------------------------
##
## ---------------------------
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
expname = "v1.1_simplerun_gif"
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

# Read RDS
filename <- here("Model_development", "Model-cleaning", "runtime", "v1.1_simple_tempRDS.Rdata")
nl <- readRDS(filename)

gc()

#' Go to file 00_Validation_patterns_v1 or ##### Screening data #####

#### Screening data ####

# Unnest simoutput
nl@simdesign@simoutput$metrics.monkeys[[1]] # TURTLE X AND Y:
nl@experiment@metrics.turtles
nl@experiment@metrics.patches

results_unnest <- unnest_simoutput(nl) ; rm(nl)
str(results_unnest)
results_unnest$agent %>% as.factor() %>% levels() # no 'monkeys'
results_unnest$breed %>% as.factor() %>% levels() # ya 'monkeys'!

# Clean dataset
results_unnest <- results_unnest %>% 
  rename(x = x_UTM, y = y_UTM)


# Split tibble into turtles and patches tibbles and select each 10th step:

# by random seed
results_unnest$`random-seed` %>% unique()

aux <- results_unnest$`random-seed` %>%
  na.exclude() %>%
  sample(size = 1)

# # patches
# results_unnest_patches <- results_unnest %>%
#   dplyr::filter(agent=="patches") %>%
#   dplyr::filter(`random-seed` == aux) %>% 
#   dplyr::filter(timestep == 1) # %>% # only 1 timestep needed for plotting the world landscape
#   # dplyr::filter(`[step]` %in% seq(8, 108, by=10))

# # turtles
# results_unnest_turtles <- results_unnest %>%
#   dplyr::filter(agent=="turtles") %>%            # by agent
#   dplyr::filter(`random-seed` == aux) # %>%         # by day (=number of the agent = who)
#   # dplyr::filter(`[step]` %in% seq(8, 108, by=10))  # by steps

# Take unwanted breeds out
# results_unnest <- results_unnest %>% filter(breed != "legend-trees")
# results_unnest <- results_unnest %>% filter(breed != "resting-trees")
results_unnest$breed %>% as.factor() %>% levels()
results_unnest$agent %>% as.factor() %>% levels()

# # monkey locations
# results_unnest_monkeys <- results_unnest_turtles %>%
#   dplyr::filter(breed=="monkeys")
# 
# # feeding-trees
# results_unnest_trees <- results_unnest_turtles %>%
#   dplyr::filter(breed=="feeding-trees")
# 
# # sleeping-trees
# results_unnest_sleep <- results_unnest_turtles %>%
#   dplyr::filter(breed=="sleeping-trees")
# 
# # seeds
# results_unnest_seeds <- results_unnest_turtles %>%
#   dplyr::filter(breed=="seeds")

# rm(results_unnest)
# results_unnest_ok <- rbind(results_unnest_patches,
#                            results_unnest_turtles)


#### Load world data ####
worldfile <- here("Model_development", "Model-cleaning", "runtime", "v1.1_simple_worldRDS.Rdata")
nl_world <- readRDS(worldfile)
results_unnest_world <- unnest_simoutput(nl_world) ; rm(nl_world)


# patches
results_unnest_patches <- results_unnest_world %>%
  dplyr::filter(agent=="patches") #%>%
# dplyr::filter(`random-seed` == aux) %>%
# dplyr::filter(timestep == 1) # %>% # only 1 timestep needed for plotting the world landscape
# dplyr::filter(`[step]` %in% seq(8, 108, by=10))

# other agents (sleeping and feeding trees)
results_unnest_world <- results_unnest_world %>% 
  mutate_if(is.character, as.factor) %>% 
  dplyr::filter(breed != "monkeys") # as patches have breed = NA, they are dropped by dplyr::filter
# dplyr::filter(breed != "monkeys" | is.na(breed)) # keep all but monkeys. This is the only way of keeping patches (breed = NA because dplyr::filter drops NAs)



# # feeding-trees
# results_unnest_trees <- results_unnest_world %>%
#   dplyr::filter(breed=="feeding-trees")
# 
# # sleeping-trees
# results_unnest_sleep <- results_unnest_world %>%
#   dplyr::filter(breed=="sleeping-trees")


#### Plotting ####

# Create normal plot
p1 <- 
  # real world shp
  gua.sf +
  # or netlogo world shp
  # ggplot() +
  # geom_tile(data=results_unnest_patches, aes(x=pxcor, y=pycor, fill = habitat)) +
  # scale_fill_manual(breaks=pal$breed, values = pal$color) + 
  geom_path(data = results_unnest,
            aes(x = x, y = y, color = "grey"),
            size = 0.15) +
  geom_point(data = results_unnest,
             aes(x = x, y = y
                 , group = behavior,
                 color = behavior,
                 shape = behavior,
                 size = behavior
             )#,
             #size = 1.8
  ) +
  scale_color_manual(values = simulated_colors) +
  scale_shape_manual(values = simulated_shapes) +
  scale_size_manual(values = simulated_sizes) +
  ggtitle("Guareí - simulated (1 run, 10 days - July resources)")

p1

# ggsave(filename = here("Model_development", "v1.1_simple_run.png"),
#        dpi = 300, width = 30, height = 20, units = "cm")



#### Create animated plot (GIF) ####
# Make plot

results_unnest <- results_unnest %>%
  dplyr::filter(day == 1) %>% 
  group_by(`[step]`)

p1 <- gua.sf +
  # geom_path(data = results_unnest,
  #           aes(x = x, y = y, color = "grey"),
  #           size = 0.15) +
  # transition_time(`[step]`) +
  geom_point(data = results_unnest,
             aes(x = x, y = y
                 , group = behavior,
                 color = behavior,
                 shape = behavior,
                 size = behavior
             )#,
             #size = 1.8
  ) +
  scale_color_manual(values = simulated_colors) +
  scale_fill_manual(values = simulated_colors) +
  scale_shape_manual(values = simulated_shapes) +
  scale_size_manual(values = simulated_sizes + 3) +
  theme_set(theme_bw(base_size = 20))
  # theme_minimal() +
  # ggtitle("Output maps of each day\n(108 + x timesteps)")
  # guides(color = guide_legend(override.aes = list(size = 5
  #                                                 # ,shape = simulated_shapes
  #                                                 ))) +
  
p1 <- p1 +
  # transition_states(`[step]`) + 
  transition_time(`[step]`) +
  # transition_reveal(`[step]`) +
  labs(title = 'Step: {frame_time}') #+
  # enter_fade()
  

# Animate the plots
library(gapminder)
library(gganimate)
library(gifski)
require(transformr)

n <- max(results_unnest$`[step]`)
ndays <- max(results_unnest$day)
# n <- length(unique(results_unnest$`[step]`)

# animate(p1,
#         width=400, height=400,
#         renderer = gifski_renderer())
# anim_save(here("Model_development", "v1.1_simple_run.gif"))


animate(p1, 
        nframes = n,
        width=800, height=800, 
        # fps= n/5,
        # duration = 30,
        renderer = gifski_renderer())
anim_save(here("Model_development", "v1.1_simple_run_perday.gif"))






# # Create facet plot with many dfs
#   library(ggplot2)
# ggplot() +
#   facet_wrap(~`[step]`, ncol=6, nrow = 2) +
#   coord_equal() +
#   
#   geom_tile(data=results_unnest_patches, 
#             aes(x=pxcor, y=pycor, fill=factor(pcolor))) +
#   scale_fill_manual(breaks=c("49", "68"),
#                     values = c("49" = "#F6F9BF", "68" = "#BFF8B7")) +
#   
#   geom_point(data=results_unnest_seeds, 
#              aes(x = xcor, y = ycor, group = breed), 
#              size = 0.5, color = "black", shape = 4) +
#   
#   geom_point(data=results_unnest_sleep, 
#              aes(x = xcor, y = ycor, group = breed), 
#              size = 1.5, color = "#E3A9F7", shape = 15) +
# 
#   geom_point(data=results_unnest_trees, 
#              aes(x = xcor, y = ycor, group = breed), 
#              size = 1, color = "black", shape = 23, fill = "#1FCD2B") +
#   
#   geom_point(data=results_unnest_monkeys, 
#              aes(x = xcor, y = ycor, group = breed), 
#              size = 4, color = "red", shape = 10) +
#   
#   guides(fill=guide_legend(title="Patch color")) +
#   theme_minimal() +
#   ggtitle("Output maps of each 10th simulation tick")
# 
# ggsave(filename = "setup-for-workflow-simple_multiple-layers.png",
#        dpi = 300, width = 30, height = 20, units = "cm")
#   
#   
# 
# # # Create facet plot with different dfs (patches and turtles)
# # library(ggplot2)
# # ggplot() +
# #   facet_wrap(~`[step]`, ncol=6, nrow = 2) +
# #   coord_equal() +
# #   geom_tile(data=results_unnest_patches, aes(x=pxcor, y=pycor, fill=factor(pcolor))) +
# #   geom_point(data=results_unnest_turtles, aes(x = xcor, y = ycor, 
# #                                               group = breed, color = breed,
# #                                               size = breed, shape = breed), 
# #              size = 1) +
# #   scale_fill_manual(breaks=c("49", "68"),
# #                     values = c("49" = "#F6F9BF", "68" = "#BFF8B7")) +
# #   
# #   scale_color_manual(breaks=c("seeds", "feeding-trees", 
# #                               "sleeping-trees", "monkeys"
# #                               # "resting-trees", "seeds"
# #                               ), 
# #                      values = c("seeds" = "grey", "feeding-trees" = "green",
# #                                 "sleeping-trees" = "magenta", "monkeys" = "black"
# #                                 # "seeds" = "red"
# #                                 )) + 
# #   
# #   scale_size_manual(breaks=c("seeds", "feeding-trees", 
# #                               "sleeping-trees", "monkeys"
# #                               ),
# #                      values = c("seeds" = 0.3, "feeding-trees" = 1,
# #                                 "sleeping-trees" = 1, "monkeys" = 1.5)) +
# #   
# #   scale_shape_manual(breaks=c("seeds", "feeding-trees", 
# #                               "sleeping-trees", "monkeys"
# #                               ),
# #                     values = c("seeds" = 1, "feeding-trees" = 1,
# #                                "sleeping-trees" = 1, "monkeys" = 8)) +
# #   
# #   guides(fill=guide_legend(title="Patch color")) +
# #   theme_minimal() +
# #   ggtitle("Output maps of each 10th simulation tick")
 





#### Full fact experiment design ####

#' Step 2: Attach an experiment
expname = "v1-model-fullfact"

nl@experiment <- experiment(expname = expname,
                            outpath = outpath,
                            repetition = 1,
                            tickmetrics = "true", # "false" for metrics only in the end of the simulation
                            idsetup = "setup",
                            idgo = "run_days",
                            # idfinal = "",
                            # idrunnum = "nlrx_id",
                            runtime = 0, #(if = 0 or NA_integer_, define stopcond_)
                            stopcond= "simulation-time-end", # reporter that returns TRUE
                            evalticks = NA_integer_, # NA_integer_ = measures each tick
                            # reporters:
                            metrics = c("count sleeping-trees"), # e.g. "count sheep" or "count patches with [pcolor = green]"
                            metrics.turtles = list("monkeys" = c("xcor", "ycor",
                                                                 "energy", "steps-moved")
                                                   ), # "who" "color"
                            metrics.patches = c("pxcor", "pycor", "pcolor"),
                            variables = list(
                              
                              # energy
                              # "energy-from-seeds" = list(min=1, max = 10, step = 2),
                              # 'energy-from-prey' = list(min=1, max=16, step = 3),
                              # "energy-loss-traveling" = list(min=-10, max = 0, step = 2),
                              # "energy-loss-foraging" = list(min=-10, max = 0, step = 2),
                              # "energy-loss-resting" = list(min=-10, max = 0, step = 2),
                              
                              # memory
                              "step_forget" = list(min=0, max = 150, step = 15),
                              # "visual" = list(min=0, max = 3, step = 1)
                              
                              # movement
                              "travel_speed_val" = list(min=0.5, max = 1, step = 0.1),
                              "foraging_speed_val" = list(min= 0.5, max = 1, step = 0.1)
                              # "duration" = list(min=0, max = 10, step = 2),
                              
                              # others
                              # 'species_time_val' = list(min = 1, max = 6, step = 2)
                              ),
                            
                              # (
                              #   'start-energy' = list(min=10, max=170, step = 40, qfun="qunif")
                              #                ),
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
                              "no_days" = 20,
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
                              "energy-from-seeds" = 4,# ?
                              "energy-from-prey" = 4,
                              "energy-loss-traveling" = -1.6,
                              "energy-loss-foraging" = -2,
                              "energy-loss-resting" = -1.9,
                              "gut_transit_time_val" = 15,
                              "n_seeds_hatched" = 1,
                              
                              ### movement
                              # "travel_speed_val" = 0.7,
                              # "foraging_speed_val" = 0.7,
                              
                              ### others
                              "simulation-time" = 108
                              
                            )
)


# Step 3: Attach a simulation design.
nl@simdesign <- simdesign_ff(nl,
                             nseeds = 10)

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
             split = 4)
)
tictoc::toc()

# Step 5: Attach results to nl and run analysis
# In order to run the analyze_nl function, the simulation output has to be attached to the nl object first. The simdesign class within the nl object provides a slot for attaching output results (simoutput). An output results tibble can be attached to this slot by using the simdesign setter function setsim(nl, "simoutput"). After attaching the simulation results, these can also be written to the defined outpath of the experiment object.
# Attach results to nl object:
setsim(nl, "simoutput") <- results

nl@experiment@metrics.turtles
nl@experiment@variables






#### LHS experiment design ####

# Step 2: Attach an experiment
expname = "v1-model-fullfact"

nl@experiment <- experiment(expname = expname,
                            outpath = outpath,
                            repetition = 1,
                            tickmetrics = "true", # "false" for metrics only in the end of the simulation
                            idsetup = "setup",
                            idgo = "run_days",
                            # idfinal = "",
                            # idrunnum = "nlrx_id",
                            runtime = 0, #(if = 0 or NA_integer_, define stopcond_)
                            stopcond= "simulation-time-end", # reporter that returns TRUE
                            evalticks = NA_integer_, # NA_integer_ = measures each tick
                            # reporters:
                            metrics = c("count sleeping-trees"), # e.g. "count sheep" or "count patches with [pcolor = green]"
                            metrics.turtles = list("monkeys" = c("xcor", "ycor",
                                                                 "energy", "steps-moved")
                            ), # "who" "color"
                            metrics.patches = c("pxcor", "pycor", "pcolor"),
                            variables = list(
                              
                              # energy
                              # "energy-from-seeds" = list(min=1, max = 10, step = 2),
                              # 'energy-from-prey' = list(min=1, max=16, step = 3),
                              # "energy-loss-traveling" = list(min=-10, max = 0, step = 2),
                              # "energy-loss-foraging" = list(min=-10, max = 0, step = 2),
                              # "energy-loss-resting" = list(min=-10, max = 0, step = 2),
                              
                              # memory
                              "step_forget" = list(min=0, max = 150, qfun="qunif"),
                              # "visual" = list(min=0, max = 3, step = 1)
                              
                              # movement
                              "travel_speed_val" = list(min=0, max = 1, qfun="qunif"),
                              "foraging_speed_val" = list(min= 0, max = 1, qfun="qunif")
                              # "duration" = list(min=0, max = 10, step = 2),
                              
                              # others
                              # 'species_time_val' = list(min = 1, max = 6, step = 2)
                            ),
                            
                            # (
                            #   'start-energy' = list(min=10, max=170, step = 40, qfun="qunif")
                            #                ),
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
                              "no_days" = 20,
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
                              "energy-from-seeds" = 4,# ?
                              "energy-from-prey" = 4,
                              "energy-loss-traveling" = -1.6,
                              "energy-loss-foraging" = -2,
                              "energy-loss-resting" = -1.9,
                              "gut_transit_time_val" = 15,
                              "n_seeds_hatched" = 1,
                              
                              ### movement
                              # "travel_speed_val" = 0.7,
                              # "foraging_speed_val" = 0.7,
                              
                              ### others
                              "simulation-time" = 108
                              
                            )
)


# Step 3: Attach a simulation design.
nl@simdesign <- simdesign_lhs(nl=nl,
                              samples=100,
                              nseeds=10,
                              precision=3)
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
             split = 1)
)
tictoc::toc()

# Step 5: Attach results to nl and run analysis
# In order to run the analyze_nl function, the simulation output has to be attached to the nl object first. The simdesign class within the nl object provides a slot for attaching output results (simoutput). An output results tibble can be attached to this slot by using the simdesign setter function setsim(nl, "simoutput"). After attaching the simulation results, these can also be written to the defined outpath of the experiment object.
# Attach results to nl object:
setsim(nl, "simoutput") <- results

nl@experiment@metrics.turtles
nl@experiment@variables












