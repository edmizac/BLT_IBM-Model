library("nlrx")
library("here")
# Windows default NetLogo installation path (adjust to your needs!):
netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
modelpath <- file.path(netlogopath, "app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo")
outpath <- here("Model_analysis", "Sensitivity-analysis", "v1.1_November2022", "temp")

nlws <- nl(nlversion = "6.2.2",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)


nlws@experiment <- experiment(expname="wolf-sheep-GenSA2",
                            outpath=outpath,
                            repetition=1,
                            tickmetrics="false",
                            idsetup="setup",
                            idgo="go",
                            runtime=50,
                            metrics.patches = c("pxcor", "pycor", "pcolor"),
                            variables = list('initial-number-sheep' = list(min=50, max=150),
                                             'initial-number-wolves' = list(min=50, max=150)),
                            constants = list("model-version" = "\"sheep-wolves-grass\"",
                                             "grass-regrowth-time" = 30,
                                             "sheep-gain-from-food" = 4,
                                             "wolf-gain-from-food" = 20,
                                             "sheep-reproduce" = 4,
                                             "wolf-reproduce" = 5,
                                             "show-energy?" = "false"))


critfunws <- function(nlws) {
  library(landscapemetrics)
  res_spat <- nl_to_raster(nl)
  res_spat_raster <- res_spat$spatial.raster[[1]]
  lsm <- lsm_l_ed(res_spat_raster)
  crit <- lsm$value
  return(crit)
}


nlws@simdesign <- simdesign_GenSA(nlws, 
                                evalcrit = critfunws, 
                                nseeds = 1, 
                                control=list(maxit = 20))

resultswolfsheep <- run_nl_dyn(nlws, seed = nlws@simdesign@simseeds[1])

resultswolfsheep
