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
dat.all <- read.csv(here("Data", "Movement", "Curated", "BLT_groups_data.csv")
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
  write.csv(here("Data","Movement",  "Curated", "BLT_groups_data_summary.csv"),
            row.names = FALSE)


# Filter timeframes with ndays < 2
dat.summary.siminputrow <- dat.summary %>%
  dplyr::filter(ndays > 2)

# Save csv:
dat.summary.siminputrow %>% 
  write.csv(here("Data","Movement",  "Curated", "BLT_groups_data_summary_siminputrow.csv"),
                 row.names = FALSE)


# Filter curated data to the months specified on the siminputrow table (from 7096 to 6727 rows (=timesteps))
dat.all.siminputrow <- dat.all %>% 
  dplyr::filter(group %in% dat.summary.siminputrow$group & id_month %in% dat.summary.siminputrow$id_month)

# Save csv
dat.all.siminputrow %>% 
  write.csv(here("Data","Movement",  "Curated", "BLT_groups_data_siminputrow.csv"),
            row.names = FALSE)
  
  
  
  
 