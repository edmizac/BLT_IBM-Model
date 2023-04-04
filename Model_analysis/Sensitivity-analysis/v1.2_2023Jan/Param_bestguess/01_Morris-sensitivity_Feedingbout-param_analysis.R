# Script name: 01_Morris-sensitivity_Feedingbout-param_analysis.R
# Script purpose: Plot Morris analysis from 00_Morris-sensitivity

# Date created: 2023-03-31d
# Author: Eduardo Zanette

## Notes --------------------------- 
# *** IMPORTANT NOTE: I STARTED USING THE MODEL VERSION IN MODEL_SIMULATIONS FOLDER FROM NOW ON
# *** THIS MEANS MY PREVISOU CALIBRATIONS WERE DONE WITH THE MODEL VERSION THAT DIDN'T HAVE THE STORED ENERGY THING IMPLEMENTED
#

## Options -------------------------
# (plotting, memory limit, decimal digits)
# 

## Packages -------------------------
library("here")
library("dplyr")
library("nlrx")
# library("magrittr")
library("ggplot2")

# ggplot theme
theme_set(theme_bw(base_size = 15))
theme_update(
          axis.text.x = element_text(size = 11)
)


## Step 1: Create nl object ------------------------ 

if(Sys.info()[["nodename"]] == "DESKTOP-R12V3D6") {
  netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
  # modelpath <- here("Model_development", "BLT_model_v1.2.nlogo")
  modelpath <- here("Model_simulations", "BLT_model_v1.2.nlogo") # Last version with stored-energy
  outpath <- here("Model_analysis", "Sensitivity-analysis", "v1.2_2023Jan", "Param_bestguess", "temp")
  user_scp = "\"Eduardo\""
}
if(Sys.info()[["nodename"]] == "PC9") { # LEEC
  netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
  # modelpath <- here("Model_development", "BLT_model_v1.2.nlogo")
  modelpath <- here("Model_simulations", "BLT_model_v1.2.nlogo") # Last version with stored-energy
  # outpath <- here("Model_analysis", "Sensitivity-analysis", "v1.2_December2022", "temp")
  Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8.0_351")
  user_scp = "\"LEEC\""
}


# Morris analysis for all areas -----

path <- paste0(outpath, "/")



### Grep files -----
# nls_to_df <- list.files(here("Model_analysis", "Sensitivity-analysis",
#                              "v1.2_2023Jan", "temp"), pattern = "[0-9].rds") #%>% # .RData does not work
nls_to_df <- list.files(path, pattern = "[0-9].rds") # .RData does not work

n <- 1 #counter
for (f in nls_to_df) {
  
  nl_file <- readRDS(paste0(path, "/", f))
  
  # test loop:
  # print(f)
  # }
  # nl_file <- readRDS(paste0(path, "/", "v1.2_Taquara_Jan_simple1671135962_tempRDS.Rdata"))
  # nl_file <- readRDS(paste0(path, "/", "v1.2_Suzano_Sep_simple453130432_tempRDS.Rdata"))
  # f <- nls_to_df[9]

  
  ### Remove runs where tamarins died (DPL or KDE = 0)
  out <- nl_file@simdesign@simoutput %>% 
    dplyr::filter(g_DPL == 0) %>% as_tibble()
  
  nl_file@simdesign@simoutput <- nl_file@simdesign@simoutput %>% 
    dplyr::filter(g_DPL != 0) %>% as_tibble()
  
    
  ### For initializing the first dbl
  if (n == 1) {
    morris_db <- analyze_nl(nl_file, "simoutput") #; rm(nl_file)
    
    # add identifiers
    morris_db$area_run <- nl_file@experiment@constants$study_area %>% gsub(., pattern = ('\"'), replacement = '', fixed = T)
    morris_db$month_run <- nl_file@experiment@constants$`feeding-trees-scenario` %>% gsub(., pattern = ('\"'), replacement = '', fixed = T)
    
    if (nl_file@experiment@constants$`feedingbout-on?` == "true") {
      morris_db$feedingbout <- "feedingbout on"
    } else {
      morris_db$feedingbout <- "feedingbout off"
    }
    
    # morris_db$area_run <- gsub(morris_db$area_run, pattern = ('\"'), replacement = '', fixed = T)
    # morris_db$month_run <- gsub(morris_db$month_run, pattern = ('\"'), replacement = '', fixed = T)
    
  } else {
    
    # f <- nls_to_df[2]
    # nl_file <- readRDS(paste0(path, "/", f))
    
    ### Attatch data from following  dbl
    morris_n <- analyze_nl(nl_file, "simoutput")
    
    # add identifiers
    morris_n$area_run <- nl_file@experiment@constants$study_area %>% gsub(., pattern = ('\"'), replacement = '', fixed = T)
    morris_n$month_run <- nl_file@experiment@constants$`feeding-trees-scenario` %>% gsub(., pattern = ('\"'), replacement = '', fixed = T)

    if (nl_file@experiment@constants$`feedingbout-on?` == "true") {
      morris_n$feedingbout <- "feedingbout on"
    } else {
      morris_n$feedingbout <- "feedingbout off"
    }
    
    
    
    morris_db <- dplyr::bind_rows(morris_db, morris_n)
    
    
  }
  
  # morris_db %>% str()
  
  n <- n + 1
}






### Plot -----
morris_db %>%
  dplyr::filter(metric=="g_energy_stored_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~area_run) +
  # facet_grid(~feedingbout) +
  ggtitle("Morris effects on stored energy")


morris_db %>%
  dplyr::filter(metric=="g_KDE_95_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~area_run) +
  ggtitle("Morris effects on KDE95")

morris_db %>%
  dplyr::filter(metric=="g_DPL_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~area_run) +
  ggtitle("Morris effects on DPL")

morris_db %>%
  dplyr::filter(metric=="g_p_feeding_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~area_run) +
  ggtitle("Morris effects on % frugivory")

morris_db %>%
  dplyr::filter(metric=="g_p_traveling_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~area_run) +
  ggtitle("Morris effects on % traveling")

morris_db %>%
  dplyr::filter(metric=="g_p_resting_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~area_run) +
  ggtitle("Morris effects on % resting")

morris_db %>%
  dplyr::filter(metric=="g_p_foraging_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~area_run) +
  ggtitle("Morris effects on % foraging")

morris_db %>%
  dplyr::filter(metric=="g_MR_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~area_run) +
  ggtitle("Morris effects on Movement Rate")

morris_db %>%
  dplyr::filter(metric=="g_PT_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~area_run) +
  ggtitle("Morris effects on Path Twisting") # wrong?

morris_db %>%
  dplyr::filter(metric=="NN_seeds_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~area_run) +
  ggtitle("Morris effects on seed aggregation")

morris %>%
  dplyr::filter(metric=="p-visited-trees_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(area_run = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on proportion of visited trees")

# # Save plot
# ggsave(paste0(outpath, "/",
#             '01_Morris_n-visited-trees.png'), height = 7, width = 10)


morris %>%
  dplyr::filter(metric =="R_seeds_mean" |
                  metric == "R_seeds_p_mean") %>%
  # tidyr::pivot_wider(names_from = "metric", values_from = c(value) ) %>%
  # dplyr::filter(R_seeds_p_mean <= 0.05) %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on seed aggreation (R index)")

# # Save plot
# ggsave(paste0(outpath, "/",
#               '01_Morris_R-index.png'), height = 7, width = 10)

morris %>%
  dplyr::filter(metric =="NN_seeds_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on seed aggreation (Nearest neighbor distance)")

# # Save plot
# ggsave(paste0(outpath, "/",
#               '01_Morris_NN-distance.png'), height = 7, width = 10)






































# Morris analysis of one area -----
## Load -----
nl_on <- readRDS(file.path(paste0(outpath, "/"
                                  , "v1.2_Morris_Guareí_Jun_Feedingbout_on_2023-03-31.rds")))
nl_off <- readRDS(file.path(paste0(outpath, "/"
                                   , "morris_2022-12-23d_feedingbout-off.rds")))


nl_on@simdesign@simoutput
nl_on@simdesign@simoutput %>% colnames



# nl <- readRDS(paste0(path, 
#                      "v1.2_Morris_Guareí_Aug_Feedingbout_on_2023-04-01.rds"))
# "v1.2_Taquara_Jan_simple-609482361_tempRDS.Rdata"))

# db1  <-  nl@simdesign@simoutput ; class(db1)
# db2 <- nl@simdesign@siminput

## Merge -----
morris_on <- analyze_nl(nl_on, "simoutput") ; rm(nl_on)
morris_on$feedingbout <- if (feedingbout) {"feedingbout on"} else {"feedinbout off"}
morris_off <- analyze_nl(nl_off) ; rm(nl_off)
morris_off$feedingbout <- "feedingbout off"
morris <- dplyr::bind_rows(morris_on, morris_off)
rm(morris_on); rm(morris_off)

## Plot -----
morris_on %>%
  dplyr::filter(metric=="g_energy_stored_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on stored energy")

morris_on %>%
  dplyr::filter(metric=="g_KDE_95_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on KDE95")

morris_on %>%
  dplyr::filter(metric=="g_DPL_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on DPL")

morris_on %>%
  dplyr::filter(metric=="g_p_feeding_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on % frugivory")

morris_on %>%
  dplyr::filter(metric=="g_p_traveling_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on % traveling")

morris_on %>%
  dplyr::filter(metric=="g_p_resting_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on % resting")

morris_on %>%
  dplyr::filter(metric=="g_p_foraging_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on % foraging")

morris_on %>%
  dplyr::filter(metric=="g_MR_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on Movement Rate")

morris_on %>%
  dplyr::filter(metric=="g_PT_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on Path Twisting") # deu errado

morris_on %>%
  dplyr::filter(metric=="NN_seeds_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on seed aggregation")

morris %>%
  dplyr::filter(metric=="p-visited-trees_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on proportion of visited trees")

# # Save plot
# ggsave(paste0(outpath, "/",
#             '01_Morris_n-visited-trees.png'), height = 7, width = 10)


morris %>%
  dplyr::filter(metric =="R_seeds_mean" |
                  metric == "R_seeds_p_mean") %>%
  # tidyr::pivot_wider(names_from = "metric", values_from = c(value) ) %>%
  # dplyr::filter(R_seeds_p_mean <= 0.05) %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on seed aggreation (R index)")

# # Save plot
# ggsave(paste0(outpath, "/",
#               '01_Morris_R-index.png'), height = 7, width = 10)

morris %>%
  dplyr::filter(metric =="NN_seeds_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on seed aggreation (Nearest neighbor distance)")

# # Save plot
# ggsave(paste0(outpath, "/",
#               '01_Morris_NN-distance.png'), height = 7, width = 10)
