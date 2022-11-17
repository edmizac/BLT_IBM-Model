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