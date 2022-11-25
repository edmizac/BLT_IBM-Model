# Script name: 05_validation_table.R
# Script purpose: Construct the validation table to gather values for genetic algorithm analysis

# Date created: 2022-11-25d
# Author: Eduardo Zanette

## Notes --------------------------- 
# Code similar to 00_Validatin_patterns_v1_travelspeedval.rmd
#

## Options -------------------------
# (plotting, memory limit, decimal digits)
# 

## Packages -------------------------
library("here")
library("dplyr")
library("readxl")
library("lubridate")
library("hms")
library("readr")
library("ggplot2")
library("ggspatial")
library("sf")

## Paths -------------------------


## Plots -------------------------

theme_set(theme_bw(base_size = 15))



## Movement variables  -------------------------

act.bud <- read.csv(here("Data", "Movement", "Curated", "Param_siminputrow", 
                         "Siminputrow_Activity-budget_By-month.csv")
                    #, stringsAsFactors = TRUE
                    )  %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) #%>%  # to match all other datasets




