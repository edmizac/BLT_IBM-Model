# Script name: 03_Validation-patterns_movement.R
# Script purpose: summarize, plot and analize Movement empirical data for VALIDATION.






#### ***********************************************************************************
#### ********** CHECK SENSITIVITY ANALYSIS FOLDER **************************************
#### ***********************************************************************************





# Date created: 2022-11-03d
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
library("ggplot2")
library("readxl")
library("lubridate")
library("hms")


#### Siminputrow matrix  -------------------------

# Load siminputrow matrix from movement data -------------------------
siminputmatrix <- read.csv(here("Data", "Movement", "Curated", "BLT_groups_data_summary_siminputrow.csv"),
                           sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) # to match all other datasets
siminputmatrix %>% str()


# # Read siminputrow movement data
# dat.all.mv <- read.csv(here("Data", "Movement", "Curated", "Param_siminputrow", "Siminputrow_movement-data.csv"),
#             row.names = FALSE)


# Summarize activity budget
dat.all.ltraj.df$behavior %>% levels()
target_behav <- c("Frugivory", "Travel", "Foraging", "Resting")

dat.ab.summary <- dat.all.ltraj.df %>% 
  group_by(group, id_month, date, behavior) %>%
  summarise(
    n = n()
  ) %>% 
  mutate(
    perc_behavior = 100 * (n / sum(n))
  ) %>% 
  
  # filter only behaviors of interest of the model
  dplyr::filter(behavior %in% target_behav)


# Summarize DPL


