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
library("lubridate")
library("hms")
library("dplyr")
library("ggplot2")
library("readxl")
library("ggspatial")
library("sf")
library("purrr")


# path <- here("Model_analysis", "Sensitivity-analysis",
#              "v1.1_November2022", "temp")

# GIS
our_crs <- "+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
gua_xlim_all = c(780000, 782350)
gua_ylim_all = c(7407080, 7408250)
gua.x.min = 781587
gua.x.max = 782361.5
gua_xlim_set = c(781050, 782350)   # c(781200, 782350) # closer
gua_ylim_set = c(7407050, 7408250) # c(7407250, 7408250)  # closer
sma_xlim_set = c(364400, 366000)
sma_ylim_set = c(7540500, 7541700)
taq_xlim_set = c(370500, 373200)
taq_ylim_set = c(7498750, 7500550)
suz_xlim_set = c(705300, 706200)
suz_ylim_set = c(7480800, 7481600) # 7480990,

# Read data
dat.all <- read.csv(here("Data", "Movement", "Curated", "BLT_groups_data.csv")
                    , stringsAsFactors = TRUE) %>% 
  rename(month = id_month) %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "Santa Maria", "Taquara")) %>% 
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec"))

str(dat.all)
dat.all$id_day_all %>% levels()
dat.all$month %>% levels()


# É a mesma coisa que os dados que uso n oCap 2 do modelo:
dat.all %>% 
  group_by(group, month) %>% 
  summarise(n=n())



# Plots -----
theme_update(axis.title.x = element_blank())

## DPL -----



