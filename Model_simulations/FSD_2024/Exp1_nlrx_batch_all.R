## Header --------------------------
# Script name: Exp1_nlrx
# Script purpose: Run the batch_all procedure in the build-forest model to generate
# at least n-rep random and n-rep clumped distribution pattern of resource distribution
# for each homerange/forest size/shape combination (Experiment 1)
#
# The output is saved by code inside netlogo (import and export-world), generating
# one csv file and one png file with the world view
#
# Date created: 2023-07-22d
# Author: Eduardo Zanette

## Notes --------------------------- 

## Packages -------------------------
library("nlrx")
# library("here")
library("tidyverse")
library("stringr")
library("progressr")
library("future")
library("tictoc")

## ---------------------------

# Packages from github:
# install.packages('devtools')
# devtools::install_github('thomasp85/gganimate')
# remotes::install_github("ropensci/nlrx")

## ---------------------------

# Spatial plots
# theme_set(theme_bw())

## ---------------------------

# Options (plotting, memory limit, decimal digits)

## Config cores
ncores <- parallel::detectCores()

## Java memory:

## Config cores
# ncores <- parallel::detectCores() # ga is not paralelized

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
  path <- "D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest_2024/"
  netlogopath <- file.path("C:/Program Files/NetLogo 6.3.0")
  # modelpath <- here("Model_simulations", "Model_v1.1.nlogo")
  modelpath <- "D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest_2024/0_files/build_forest_EMZ_2024-07-23d.nlogo"
  outpath <- paste0(path, "Exp1/batch_all/")
  user_scp = "\"Eduardo\"" # scaped
}
if(Sys.info()[["nodename"]] == "PC9") { # LEEC
  path <- "D:/Eduardo_LaP/Model_Documentation/build_forest_2024/"
  netlogopath <- file.path("C:/Program Files/NetLogo 6.3.0")
  # modelpath <-  "D:/Eduardo_LaP/Model_simulations/BLT_model_v1.2.nlogo"
  outpath <- paste0(path, "Exp1/")
  Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8.0_351")
  user_scp = "\"LaP\"" # scaped
}



nl <- nl(nlversion = "6.3.0",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

nlogo_model_param <- report_model_parameters(nl)
nlogo_model_param


# ## List files patch generated csv files ------
# pathfiles <- "D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest/Experiment1/" #batch1/
# 
# files_forests <- list.files(pathfiles, pattern = ".csv")
# files_forests <- paste0(pathfiles, "/", files_forests)
# 
# length(files_forests) # number of patches * home ranges (10 reps) * number of clumped/random/ordered resourcers
# 
# #### Simple design experiment ( = one go button, no varibles) ####


## Step 2: Attach an experiment
expname <- "Exp1_Batch_all"

set.seed(1234)

# for (i in files_forests) {

# files_forests_i <- i

# # Attach to nl experiment
nl@experiment <- experiment(expname = expname,
                            outpath = outpath,
                            repetition = 1, # number of repetitions with the same seed (use repetition = 1)
                            tickmetrics = "false", # "true" for every tick, "false" for metrics only in the end of the simulation
                            idsetup = "dummy",
                            idgo = "batch_all",
                            runtime = 0, #(if = 0 or NA_integer_, define stopcond_)
                            stopcond= "n_reps_random > 6 * n-reps-resource", # reporter that returns TRUE
                            evalticks = NA_integer_, # NA_integer_ = measures each tick. Only applied if tickmetrics = TRUE
                            # idfinal = "r:stop", # for making r NetLogo extension to work: https://cran.r-project.org/web/packages/nlrx/vignettes/furthernotes.html
                            
                            # reporters:
                            metrics = c( # e.g. "count sheep" or "count patches with [pcolor = green]"
                              # "count turtles",
                              "density",
                              "patch-size-ha",
                              "field.shape.factor",
                              "hr_x",
                              "final-hr-size",
                              "R_feeding_trees",
                              "R_feeding_trees_p",
                              "NN_feeding_trees",
                              "aggregation_type", 
                              "sd-displacement"
                              
                            ),
                            # metrics.turtles = list("turtles" = c(
                            #   
                            # ),
                            # 
                            # 
                            # 
                            # ), # "who" "color"
                            variables = list(
                              # 'sd-displacement' = list(min=0, max= 1, step = 0.2), #step = 0.1, qfun="qunif"
                              # 'sd-displacement' = list(values = c(0.01, 0.03, 0.05, 0.10, 0.25, 0.5, 0.75, 1.00)), #step = 0.1, qfun="qunif"
                              #  sd-displacement is set within the model according to fragment size
                              # "n-clusters" = list(min=2, max=12, step=2),
                              
                              "density" = list(c(0.01, 0.35, 0.14, 0.20)),
                              "lc-patch-size-ha" = list(c(50, 100, 150, 200, 400, 600)),
                              "field.shape.factor" = list(c(1, 1.5, 2, 2.5)),
                              "n" = list(min=30, max=120, step=30)
                              
                              # batch2_input = files_forests_i
                              
                            ),
                            
                            constants = list(
                              # "output_folder" = "D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest_2024/Exp1/batch_all/",
                              # "path" = "D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest_2024/0_files/",
                              
                              "cluster-type" = "\"thomas\"",
                              
                              # "only-random?" = "true", # if you want to generate only (or mostly) random resource distributions
                              "n-reps-resource" = 1, # number of resource distribution repetitions. We don't want replicates of this in Experiment 1
                              "n-clusters" = 10,
                              # "n" = 75,
                              # "sd-displacement" = 0.02 # initial sd-displacement (the while loop drops it if clumped patterns are not generated)
                              "n-sleeping-trees" = 30,
                              
                              "n-reps-hr" = 1 # number of home ranges in each fragment
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
results <- progressr::with_progress(run_nl_all(nl))
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
# filename <-
#        paste0(outpath, "/", expname, ".rds")
# saveRDS(nl, file = filename)
# rm(nl)

# nl <- readRDS(filename)

gc()

# }



##### Screening data #####
# results_unnest <- unnest_simoutput(nl)

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
# results %>% 
# # results_p %>% 
#   ggplot(
#     aes(x = R_value, color = point_pattern, fill = point_pattern)
#   ) +
#   geom_density(alpha = 0.6) #+
#   # facet_grid(R_value ~ n)
# 
# results_p$point_pattern %>% table()
# # https://towardsdatascience.com/visualizing-trends-of-multivariate-data-in-r-using-ggplot2-1b85409afcfb
# 
# results$R_value %>% hist()
# results$R_value %>% shapiro.test() # non-normal
# p <- ggplot(results, aes(sample = R_value))
# p + stat_qq() + stat_qq_line()
# ggpubr::ggqqplot(results, x = "R_value")
# 
# 
# results$R_log <- log(results$R_value)
# results$R_log %>% hist()
# results$R_log %>% shapiro.test() # still non-normal
# 
# 
# # Finding the distribution:
# descdist(data = results$R_value)
# descdist(data = results$R_log)
# # It seems like it is a beta distribution that best fits it
# 
# # With gamma (not correct)
# lm1 <- glm(results$R_value ~ results$`n-clusters` + results$n + results$`sd-displacement`,
#            family = Gamma)
# summary(lm1)
# 
# # With beta
# library(betareg)
# lm2 <- betareg(results$R_value ~ results$`n-clusters` + results$n + results$`sd-displacement`,
#            link = "log" )
# # Nope
# 
