# Script name: 03_params-all-data.R
# Script purpose: derive empirical values for parameterizing the model to run in the nine situations
# assigned in BLT_groups_data_summary_aftercleaning.csv in Data/Movement/Curated

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

