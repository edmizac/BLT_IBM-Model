# Script name: CurateData_summary.R
# Script purpose: summarize number of days per month and other metrics for simulations

# Date created: 2022-10-14d
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


# Read data
dat.all <- read.csv(here("Data", "Curated", "BLT_groups_data.csv")
                    , stringsAsFactors = TRUE
                    )
# str(dat.all)
# dat.all$id %>% levels()
dat.all$id_day_all %>% levels()
dat.all$id_month %>% levels()

# Summarise important variables
dat.summary <- dat.all %>% 
  group_by(group, id_month) %>% 
  dplyr::summarise(
    timesteps = n(),
    ndays = n_distinct(id_day_all),
    mean_timesteps = round(timesteps/ndays)
  ) 

# Save csv:
dat.summary %>% 
  write.csv(here("Data", "Curated", "BLT_groups_data_summary.csv"),
                 row.names = FALSE)

