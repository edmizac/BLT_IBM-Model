# Script name: 01_filter-data-siminputrow.R
# Script purpose: make a stable version from the raw seed dispersal data without having to modify it (rename columns, etc) everytime

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
library("tidyverse")
library("dplyr")
library("ggplot2")
library("readxl")


# Empirical data for parameterisation:
dat.summary <- read_csv(here("Data", "Curated", "BLT_groups_data_summary_aftercleaning.csv")) %>% 
  dplyr::mutate(group = recode(group, "Guarei" = "Guareí")) # only to match those of the NetLogo model