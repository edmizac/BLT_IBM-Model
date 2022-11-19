# Script name: 04_construct-siminputrow_matrix.R
# Script purpose: Construct the siminputrow matrix to use in run_nl_all() script




###############################################################
# THIS BRANCH IS A BACKUP FOR SOME ANALYSIS I DID IN 
# SEED_DISPERSAL/02_params-siminputrow  AND RESULTED
# IN A WRONG PARAMETER TABLE (MAINLY BECAUSE SANTA MARIA)
# DOES NOT HAVE SEED DISPERSAL DATA. I CONTINUED ON
# v1.1_seed-dispersal-submodel BRANCH
###############################################################




# Date created: 2022-11-16d
# Author: Eduardo Zanette

## Notes --------------------------- 
#
#

## Options -------------------------
# (plotting, memory limit, decimal digits)
# 

## Packages -------------------------
library("here")
library("dplyr")
# library("ggplot2")
library("readxl")
library("lubridate")
library("hms")


#### Siminputrow matrix  -------------------------

# 1) Load siminputrow matrix from simulation times base -------------------------
siminputmatrix <- read.csv(here("Data", "Movement", "Curated", #"Param_Simulation-time",
                                "BLT_groups_data_summary_siminputrow.csv"),
                           sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) # to match all other datasets
siminputmatrix %>% str()

# groups <- siminputmatrix$group
# months <- siminputmatrix$id_month



# 2) Load seed dispersal siminputrow matrix  -------------------------
## to gather GTT, timesteps to morning defecation, number of defecations per day, etc
siminputmatrix_sd <- read.csv(here("Data", "Seed_dispersal", "Curated", "Param_siminputrow",  "Siminputrow_seed-dispersal_params.csv"),
                       sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  # mutate(group = recode(group, "Guarei" = "Guareí")) %>% 
  
  # Rename variables of interest:
  dplyr::rename(
    timesteps_to_def_sameday_mean = timesteps_wakeup_to_def_mean_same_day,
    timesteps_to_def_sameday_sd = timesteps_wakeup_to_def_sd_same_day,
    timesteps_to_def_morningdefecation_mean = timesteps_wakeup_to_def_mean_next_day,
    timesteps_to_def_morningdefecation_sd = timesteps_wakeup_to_def_sd_next_day,
  ) %>%
  
  dplyr::select(c(1:2, starts_with("timesteps_")))

# siminputmatrix_sd <- siminputmatrix_sd %>% 
#   dplyr::select(c("group", "id_month",
#                   "timesteps_to_def_sameday_mean", "timesteps_to_def_sameday_sd",
#                   "timesteps_to_def_morningdefecation_mean", "timesteps_to_def_morningdefecation_sd"))


### 2.1) Load seed dispersal siminputrow matrix  -------------------------
# As one might see, there are missing values. Thus, we can read all data to estimate missing values based on the same group valu
siminputmatrix_sd_all <- read.csv(here("Data", "Seed_dispersal", "Curated", "Param_all-data",  "Summary_seed-dispersal_params-estimated.csv"),
                              sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  # mutate(group = recode(group, "Guarei" = "Guareí")) %>% 
  
  # Rename variables of interest:
  dplyr::rename(
    timesteps_to_def_sameday_mean = timesteps_wakeup_to_def_mean_same_day,
    timesteps_to_def_sameday_sd = timesteps_wakeup_to_def_sd_same_day,
    timesteps_to_def_morningdefecation_mean = timesteps_wakeup_to_def_mean_next_day,
    timesteps_to_def_morningdefecation_sd = timesteps_wakeup_to_def_sd_next_day,
  ) %>%
  
  dplyr::select(c(1:2, starts_with("timesteps_")))# %>% 
  # dplyr::filter(!is.na())

## Guareí
siminputmatrix_sd_all[ 5 , "timesteps_to_def_morningdefecation_mean"] <- siminputmatrix_sd_all %>% 
  dplyr::filter(group == "Guareí") %>% 
  dplyr::select("timesteps_to_def_morningdefecation_mean") %>%
  summarise(
    mean = mean(timesteps_to_def_morningdefecation_mean, na.rm = TRUE)
  )

siminputmatrix_sd_all[ 5 , "timesteps_to_def_morningdefecation_sd"] <- siminputmatrix_sd_all %>% 
  dplyr::filter(group == "Guareí") %>% 
  dplyr::select("timesteps_to_def_morningdefecation_sd") %>%
  summarise(
    sd = sd(timesteps_to_def_morningdefecation_sd, na.rm = TRUE)
  )
  
siminputmatrix_sd_all[ 5 , "timesteps_to_def_sameday_mean"] <- siminputmatrix_sd_all %>% 
  dplyr::filter(group == "Guareí") %>% 
  dplyr::select("timesteps_to_def_sameday_mean") %>%
  summarise(
    mean = mean(timesteps_to_def_sameday_mean, na.rm = TRUE)
  )

siminputmatrix_sd_all[ 5 , "timesteps_to_def_sameday_sd"] <- siminputmatrix_sd_all %>% 
  dplyr::filter(group == "Guareí") %>% 
  dplyr::select("timesteps_to_def_sameday_sd") %>%
  summarise(
    sd = sd(timesteps_to_def_sameday_sd, na.rm = TRUE)
  )  

## Santa Maria 
siminputmatrix_sd_all[ 5 , "GTT_mean"] <- siminputmatrix_sd_all %>% 
  dplyr::filter(group == "Santa Maria") %>% 
  dplyr::select("GTT_mean") %>%
  summarise(
    mean = mean(GTT_mean, na.rm = TRUE)
  )

siminputmatrix_sd_all[ 5 , "GTT_sd"] <- siminputmatrix_sd_all %>% 
  dplyr::filter(group == "Santa Maria") %>% 
  dplyr::select("GTT_sd") %>%
  summarise(
    sd = sd(GTT_sd, na.rm = TRUE)
  )

siminputmatrix_sd_all[ 5 , "morning_defecation_GTT_mean"] <- siminputmatrix_sd_all %>% 
  dplyr::filter(group == "Santa Maria") %>% 
  dplyr::select("morning_defecation_GTT_mean") %>%
  summarise(
    mean = mean(morning_defecation_GTT_mean, na.rm = TRUE)
  )

siminputmatrix_sd_all[ 5 , "morning_defecation_GTT_sd"] <- siminputmatrix_sd_all %>% 
  dplyr::filter(group == "Santa Maria") %>% 
  dplyr::select("morning_defecation_GTT_sd") %>%
  summarise(
    sd = sd(morning_defecation_GTT_sd, na.rm = TRUE)
  )   

# Round numeric values and rename
round2 <- function(x) (round(x, digits = 2))
siminputmatrix_sd <- siminputmatrix_sd_all %>% 
  mutate(across(where(is.numeric), round2))

  


# 3) Load movement siminputrow matrix -------------------------

## to gather step length, max turning angles and p_foraging
siminputmatrix_mv <- read.csv(here("Data", "Movement", "Curated", "Param_siminputrow", "Siminputrow_parameters_movement.csv"),
                       sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  dplyr::select(!starts_with("perc_"))



# 4) Gather other siminputrow data -------------------------
## (number of trees, number of sleeping sites, number of tamarins). I'll take this from movement data

dat.mv <- read.csv(here("Data", "Movement", "Curated", "BLT_groups_data_siminputrow.csv")
                    , stringsAsFactors = TRUE
)  %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) #%>%  # to match all other datasets

# dat.mv %>%  dplyr::filter(behavior == "Sleeping site")

target_behav <- c("Sleeping site", "Frugivory") 

siminputmatrix_others <- dat.mv %>% 
  dplyr::filter(behavior %in% target_behav) %>%
  group_by(group, id_month, behavior) %>%
  summarise(
    n_trees = n_distinct(id_tree)
  ) %>% 
  
  # pivot wider
  tidyr::pivot_wider(names_from = behavior, values_from = n_trees,
                     names_glue = "{.value}_{behavior}")
  


# 5) Merge all parameter tables together into one siminputrow table -------------------------
siminputmatrix_complete <- left_join(siminputmatrix_mv, siminputmatrix_sd)
siminputmatrix_complete <- left_join(siminputmatrix_complete, siminputmatrix_others)


# 6) Input number of tamarins by hand -------------------------
siminputmatrix_complete$n_tamarins <- c(5, 5, 5, 5, 2, 2, 6, 4, 4) # values from: Guareí (Dissert. Felipe), Santa Maria (Dissert. Yness), Taquara and Suzano (Dissert. Anne and LaP Demography database https://www.dropbox.com/s/vt81l28hqg5yzaz/Demography_MLP_LaP.xlsx?dl=0) 


# 7)  Write csv -------------------------
siminputmatrix_complete %>% str()
# siminputmatrix_complete %>%
#   write.csv(here("Data", "Parameter_table.csv"),
#             row.names = FALSE)








