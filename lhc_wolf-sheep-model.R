if(Sys.getenv("JAVA_HOME") == "") {
  if(Sys.info()[["sysname"]] == "Linux") {
    Sys.setenv(JAVA_HOME = "/usr/lib/jvm/java-11-openjdk-amd64")
    unixtools::set.tempdir(".")
  } else {
    Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk1.8.0_321")
  }
}

## Packages -------------------------
library("here")
library("nlrx")
library("tidyverse")
library("dplyr")
library("ggplot2")
## Packages -------------------------

ncores <- parallel::detectCores()
nseeds <- 2

# vignette: https://docs.ropensci.org/nlrx/articles/sensitivity.html

# Step 1: Create nl object
## netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
if(Sys.info()[["nodename"]] == "simul02") {
  netlogopath <- file.path("/home/rbialozyt/Software/NetLogo 6.2.0")
  modelpath <- file.path(netlogopath, "app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo")
}
if(Sys.info()[["nodename"]] == "PC146") {
  netlogopath <- file.path("/opt/netlogo_620")
  modelpath <- file.path(netlogopath, "app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo")
}
## netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
## modelpath <- here("Model_development", "BLT_model_v1.1.nlogo")
# C:\Program Files\NetLogo 6.2.2\app\models
file.info(modelpath)

if(Sys.info()[["nodename"]] == "simul02") {
  outpath <- ("/home/rbialozyt/BLT_IBM-Model/Model_analysis/Sensitivity-analysis")
}
if(Sys.info()[["nodename"]] == "PC146") {
  outpath <-  paste0("/home/rbialozyt/ownCloud-Forst/Projektideen/FAPESP_Project_Eduardo/"
                     , "BLT_IBM-Model/Model_analysis/Sensitivity-analysis")
}    

nl <- nl(nlversion = "6.2.0",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

report_model_parameters(nl)

nl@experiment <- experiment(expname = "wolf-sheep-lhs",
                            outpath = outpath,
                            repetition = 1,   
                            tickmetrics = "true",
                            idsetup = "setup",  
                            idgo = "go",        
                            runtime = 500,
                            metrics=c("count sheep", "count wolves", "count patches with [pcolor = green]"),
                            variables = list("initial-number-sheep" = list(min=50, max=150, step=10, qfun="qunif"),
                                             "initial-number-wolves" = list(min=50, max=150, step=10, qfun="qunif"),
                                             "grass-regrowth-time" = list(min=0, max=100, step=10, qfun="qunif"),
                                             "sheep-gain-from-food" = list(min=0, max=50, step=10, qfun="qunif"),
                                             "wolf-gain-from-food" = list(min=0, max=100, step=10, qfun="qunif"),
                                             "sheep-reproduce" = list(min=0, max=20, step=5, qfun="qunif"),
                                             "wolf-reproduce" = list(min=0, max=20, step=5, qfun="qunif")),
                            constants = list("model-version" = "\"sheep-wolves-grass\"",
                                             "show-energy?" = "false"))
nl@simdesign <- simdesign_lhs(nl, samples=500, nseeds=1, precision=3)

library(future)
library(tictoc)

# plan(multisession)
## plan(list(sequential, multiseprocess))
## plan(list(sequential, multisession))
plan(list(sequential, multicore))
# split_param <- min(nrow(nl@simdesign@siminput), ((ncores - 2)/nseeds))
tictoc::tic()
progressr::handlers("progress")
results %<-% progressr::with_progress(
  run_nl_all(nl = nl
             , split = 2)
)
tictoc::toc()
print("================ Finished! =========================")
setsim(nl, "simoutput") <- results
saveRDS(nl, file.path(nl@experiment@outpath, "lhs.rds"))
library(tidyverse)
input <- getsim(nl, "siminput") %>%    # Take input parameter matrix
  dplyr::select(names(getexp(nl, "variables"))) %>%  # Select variable parameters only
  dplyr::rename_all(~str_replace_all(., c("-" = "_", "\\s+" = "_"))) # Remove - and space characters.

output <- getsim(nl, "simoutput") %>%   # Take simulation output
  dplyr::group_by(`random-seed`, siminputrow) %>% # Group by random seed and siminputrow
  dplyr::summarise_at(getexp(nl, "metrics"), list(mean=mean, sd=sd)) %>% # Aggregate output
  dplyr::ungroup() %>%  # Ungroup
  dplyr::select(-`random-seed`, -siminputrow) %>%  # Only select metrics
  dplyr::rename_all(~str_replace_all(., c("-" = "_", "\\s+" = "_", "\\[" = "_", "\\]" = "_", "=" = ""))) # Remove - and space characters.

# Perform pcc and src for each output separately (map)
pcc.result <- purrr::map(names(output), function(x) sensitivity::pcc(X=input, y=output[,x], nboot = 100, rank = FALSE)) 
src.result <- purrr::map(names(output), function(x) sensitivity::src(X=input, y=output[,x], nboot = 100, rank = FALSE)) 

plot(pcc.result[[1]])
pcc.result.tidy <- purrr::map_dfr(seq_along(pcc.result), function(x) {
  pcc.result[[x]]$PCC %>% 
    tibble::rownames_to_column(var="parameter") %>% 
    dplyr::mutate(metric = names(output)[x])
})

ggplot(pcc.result.tidy) +
  coord_flip() +
  facet_wrap(~metric) +
  geom_point(aes(x=parameter, y=original, color=metric)) +
  geom_errorbar(aes(x=parameter, ymin=`min. c.i.`, ymax=`max. c.i.`, color=metric), width=0.1)

src.result.tidy <- purrr::map_dfr(seq_along(src.result), function(x) {
  src.result[[x]]$SRC %>% 
    tibble::rownames_to_column(var="parameter") %>% 
    dplyr::mutate(metric = names(output)[x])
})

ggplot(src.result.tidy) +
  coord_flip() +
  facet_wrap(~metric) +
  geom_point(aes(x=parameter, y=original, color=metric)) +
  geom_errorbar(aes(x=parameter, ymin=`min. c.i.`, ymax=`max. c.i.`, color=metric), width=0.1)


