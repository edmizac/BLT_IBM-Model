# Script name: 02_params-siminputrow.R
# Script purpose: derive empirical values of Movement parameters for parameterizing 
# the model to run on the nine situations assigned in BLT_groups_data_summary_aftercleaning.csv in 
# Data/Movement/Curated.

# Date created: 2022-11-16d
# Author: Eduardo Zanette

## Notes --------------------------- 
# Code adapted from "asltraj_script_2022-06-02d.R" and 
# "TurningAnglesScript_EMZ6_2022-07-27d.R" in BLT-Movement-Patterns github repository


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
library("sp")
library("adehabitatLT")
library("circular")


# Read data (filtered by siminputrow on CurateData.R)
dat.all <- read.csv(here("Data", "Movement", "Curated", "BLT_groups_data_siminputrow.csv")
                    #, stringsAsFactors = TRUE
                    )  %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) %>%  # to match all other datasets
  rename(datetime = POSIXct,
         id_tree = id)

str(dat.all)
# dat.all$id %>% levels()
dat.all$id_day_all %>% levels()
dat.all$id_month %>% levels()

# Make POSIXct
dat.all <- dat.all %>% 
  mutate(datetime = ymd_hms(datetime, tz = "America/Sao_Paulo"))

# Transforming to ltraj
dat.all.ltraj <- adehabitatLT::as.ltraj(xy = dat.all[, c("x", "y")], 
                                       date = dat.all$datetime, 
                                       id = dat.all$group,
                                       burst = lubridate::as_date(dat.all$datetime), # bursts as date because all sampling days are complete (from sleepint tree to sleeping tree)
                                       proj4string = sp::CRS(our_crs),
                                       typeII = TRUE,
                                       infolocs = dat.all[ , c(1, 3:(ncol(dat.all)))] #only non-used collumns
)

# Check ltraj objetct
dat.all.ltraj # type II # no NAs, Regular. Bursts = days. Day 2019-08-19 is too short **CHECK WITH FELIPE


# convert to df and save as csv
dat.all.ltraj.df <- adehabitatLT::ld(dat.all.ltraj)

# Remove repeated columns
dat.all.ltraj.df <- dat.all.ltraj.df %>% 
  dplyr::select(!y.1)

# dat.all.ltraj.df %>% colnames()
dat.all.ltraj.df %>% str()
a %>% str()


## Summary  -------------------

# Transform rel.ang to circular
dat.all.ltraj.df <- dat.all.ltraj.df %>% 
  mutate(rel.angle = circular::as.circular(rel.angle))

# Summarize step length and turning angles
dat.mv.summary <- dat.all.ltraj.df %>% 
  group_by(group, id_month) %>%
  summarise(
    step_len_mean = mean(dist, na.rm = TRUE),
    step_len_sd = sd(dist, na.rm = TRUE),
    rel_angle_mean = circular::mean.circular(rel.angle, type = "angles", na.rm = TRUE),
    rel_angle_sd = circular::sd.circular(rel.angle, type = "angles", na.rm = TRUE)
    )

# # Write csv  
# dat.mv.summary %>% 
#   write.csv(here("Data", "Movement", "Curated", "Param_siminputrow", "Siminputrow_sl_ta.csv"),
#             row.names = FALSE)


