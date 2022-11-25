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

# Activity budget
data.ab <- read.csv(here("Data", "Movement", "Curated", "Validation", 
                         "Siminputrow_Activity-budget_By-month.csv")
                    #, stringsAsFactors = TRUE
                    )  %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) %>%  # to match all other datasets
  # dplyr::select(-id) %>% 
  tidyr::pivot_wider(names_from = behavior, values_from = starts_with("perc_"),
                     names_glue = "{behavior}_{.value}")

# DPL
data.dpl <- read.csv(here("Data", "Movement", "Curated", "Validation", 
                         "Siminputrow_DPL_by-month.csv")
                    #, stringsAsFactors = TRUE
)  %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) %>%  # to match all other datasets
  dplyr::select(c(group, id_month, DPL_mean))

# Home range
data.hr <- read.csv(here("Data", "Movement", "Curated", "Validation", 
                         "Siminputrow_Home-range_by-month.csv")
                    #, stringsAsFactors = TRUE
)  %>% 
  dplyr::select(-id) %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) %>%  # to match all other datasets
  dplyr::select(-c(level, area)) %>% 
  tidyr::pivot_wider(names_from = "KDE_value", values_from = "hr_area_ha") #%>% 
  # tidyr::unite(KDE95, KDE50, col = "hr_area_ha", na.rm = TRUE) %>% 


# Combine all and define levels
dat.mv.val <- left_join(data.dpl, data.hr)
dat.mv.val <- left_join(dat.mv.val, data.ab)


round2 <- function(x, na.rm = FALSE) (round(x, 2))

dat.mv.val <- dat.mv.val %>% 
  mutate_if(is.numeric, round2)

# # Write csv
dat.mv.val %>%
  write.csv(here("Data", "Validation-table.csv"),
            row.names = FALSE)

# mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "Santa Maria", "Taquara")) %>% 
#   mutate(id_month = forcats::fct_relevel(id_month, "Jan", "Mar", "Apr", "May", 
#                                          "Jun", "Jul", "Aug", "Sep", "Dec"))

