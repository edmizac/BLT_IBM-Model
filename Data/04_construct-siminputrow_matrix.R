# Script name: 04_construct-siminputrow_matrix.R
# Script purpose: Construct the siminputrow matrix to use in run_nl_all() script

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

# Load siminputrow matrix from movement data as base -------------------------
siminputmatrix <- read.csv(here("Data", "Movement", "Curated", "BLT_groups_data_summary_siminputrow.csv"),
                           sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) # to match all other datasets
siminputmatrix %>% str()

# groups <- siminputmatrix$group
# months <- siminputmatrix$id_month



# Load movement dataset to gather step length, max turning angles and p_foraging
dat.all.mv <- read.csv(here("Data", "Movement", "Curated", "BLT_groups_data.csv"),
                       sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) %>% 
  rename(datetime = POSIXct)


# Load seed dispersal siminputrow matrix to gather GTT, timesteps to morning defecation, number of defecations per day, etc
dat.all.sd <- read.csv(here("Data", "Seed_dispersal", "Curated", "Param_siminputrow",  "Siminputrow_disp-day_nex-day_params.csv"),
                       sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  # mutate(group = recode(group, "Guarei" = "Guareí")) %>% 
  
  # Rename variables of interest:
  rename(
    GTT_mean = GTT_timesteps_mean._same_day,
    GTT_sd = GTT_timesteps_sd._same_day,
    morning_defecation_GTT_mean = timesteps_wakeup_to_def_mean._same_day,
    morning_defecation_GTT_sd = timesteps_wakeup_to_def_sd._same_day,
  ) %>% 
  dplyr::select(c("group":"mean_timesteps", "GTT_mean":"morning_defecation_GTT_sd"))
  
# Guess missing values based on the same group values
## Guareí
dat.all.sd[ 1 , "GTT_mean"] <- dat.all.sd %>% 
  dplyr::filter(group == "Guareí") %>% 
  select("GTT_mean") %>%
  summarise(
    mean = mean(GTT_mean, na.rm = TRUE)
  )

dat.all.sd[ 1 , "GTT_sd"] <- dat.all.sd %>% 
  dplyr::filter(group == "Guareí") %>% 
  select("GTT_sd") %>%
  summarise(
    sd = sd(GTT_sd, na.rm = TRUE)
  )
  
dat.all.sd[ 1 , "morning_defecation_GTT_mean"] <- dat.all.sd %>% 
  dplyr::filter(group == "Guareí") %>% 
  select("morning_defecation_GTT_mean") %>%
  summarise(
    mean = mean(morning_defecation_GTT_mean, na.rm = TRUE)
  )

dat.all.sd[ 1 , "morning_defecation_GTT_sd"] <- dat.all.sd %>% 
  dplyr::filter(group == "Guareí") %>% 
  select("morning_defecation_GTT_sd") %>%
  summarise(
    sd = sd(morning_defecation_GTT_sd, na.rm = TRUE)
  )  

## Santa Maria  
dat.all.sd[ 5 , "GTT_mean"] <- dat.all.sd %>% 
  dplyr::filter(group == "Guareí") %>% 
  select("GTT_mean") %>%
  summarise(
    mean = mean(GTT_mean, na.rm = TRUE)
  )

dat.all.sd[ 5 , "GTT_sd"] <- dat.all.sd %>% 
  dplyr::filter(group == "Guareí") %>% 
  select("GTT_sd") %>%
  summarise(
    sd = sd(GTT_sd, na.rm = TRUE)
  )

dat.all.sd[ 5 , "morning_defecation_GTT_mean"] <- dat.all.sd %>% 
  dplyr::filter(group == "Guareí") %>% 
  select("morning_defecation_GTT_mean") %>%
  summarise(
    mean = mean(morning_defecation_GTT_mean, na.rm = TRUE)
  )

dat.all.sd[ 5 , "morning_defecation_GTT_sd"] <- dat.all.sd %>% 
  dplyr::filter(group == "Guareí") %>% 
  select("morning_defecation_GTT_sd") %>%
  summarise(
    sd = sd(morning_defecation_GTT_sd, na.rm = TRUE)
  )   

# Round numeric values
round2 <- function(x) (round(x, digits = 2))
dat.all.sd <- dat.all.sd %>% 
  mutate(across(where(is.numeric), round2))
  
  
    