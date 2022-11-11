# Script name: 01_filter-data-siminputrow.R
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


# Load siminputrow matrix from movement data
siminputmatrix <- read.csv(here("Data", "Movement", "Curated", "BLT_groups_data_summary_aftercleaning.csv"),
                           sep = ";", dec = ".", stringsAsFactors = TRUE)
siminputmatrix %>% str()

# groups <- siminputmatrix$group
# months <- siminputmatrix$id_month

# Empirical seed dispersal data for summary:
# Load data
dat.all <- read.csv(here("Data", "Seed_dispersal", "Curated", "All-areas_SeedDispersal.csv"),
                    sep = ",", stringsAsFactors = TRUE) %>% 
  rename(id_month = month_id)
dat.all %>% str()

# Check matching data
dat.all %>% 
  # dplyr::filter(!(group %in% siminputmatrix$group | id_month %in% siminputmatrix$id_month)) # all data matches with the exception of one line!
  dplyr::filter(group %in% siminputmatrix$group | id_month %in% siminputmatrix$id_month)


# Wrangle data (as in 01_Plots.R):
# Load data
dat.all <- read.csv(here("Data", "Seed_dispersal", "Curated", "All-areas_SeedDispersal.csv"),
                    sep = ",", stringsAsFactors = TRUE)
# dat.all %>% str()
# is.na(dat.all$def_datetime)
# nrow(dat.all[is.na(dat.all$def_datetime), ])


# Wrangling data
dat.all <- dat.all %>%
  
  # make datetime POSIXct
  mutate(
    def_datetime = lubridate::as_datetime(def_datetime), #, tz = "America/Sao_Paulo"),
    feed_datetime = lubridate::as_datetime(feed_datetime) #, tz = "America/Sao_Paulo"),
  ) %>% 
  
  # gather yyyy/mm/dd as id
  mutate(
    day = lubridate::as_date(def_datetime),
  ) %>% 
  
  # Defining each dispersal event as in the same day or next day
  mutate(disp_day = ifelse(lubridate::day(def_datetime) == lubridate::day(feed_datetime), # use case_when for a dplyr-er solution: https://stackoverflow.com/questions/22337394/dplyr-mutate-with-conditional-values
                           "same day",
                           "next day"),
         disp_day = as.factor(disp_day)
  ) %>% 
  
  # Order group levels and drop NA
  dplyr::filter(!is.na(group)) %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "Santa Maria", "Taquara"))


# Make summary of interest variables for parameterization:
a <- dat.all %>% 
  group_by(group, id_month, disp_day) %>% 
  summarise(
    GTT_timesteps_mean = mean(gut_transit_time) / 5, # 1 timestep = 5 mins
    GTT_timesteps_sd = sd(gut_transit_time) / 5,
    max_time_seeds_nextday_mean = mean(def_datetime) / 5,
    max_time_seeds_nextday_sd = sd(def_datetime) / 5
  )


