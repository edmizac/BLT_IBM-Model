# Script name: 01_params-all-data.R
# Script purpose: summarize, plot and analize Movement empirical data for PARAMETERIZATION
# Derive empirical values for parameterizing the model to run GENERALLY (CHAPTER 2)
# and NOT in the nine situations assigned in BLT_groups_data_summary_aftercleaning.csv in 
# Data/Movement/Curated. For this, Go to 02_params-siminputrow.R
# Date created: 2022-11-16d
# Author: Eduardo Zanette

## Notes --------------------------- 
#
#

## Options -------------------------
# (plotting, memory limit, decimal digits)
# 
our_crs <- "+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

## Packages -------------------------
library("here")
# library("lubridate")
# library("hms")
library("dplyr")
library("readxl")
# library("sf")


# Read data
dat.all <- read.csv(here("Data", "Movement", "Curated", "BLT_groups_data.csv")
                    , stringsAsFactors = TRUE
)
# str(dat.all)
# dat.all$id %>% levels()
dat.all$id_day_all %>% levels()
dat.all$id_month %>% levels()



