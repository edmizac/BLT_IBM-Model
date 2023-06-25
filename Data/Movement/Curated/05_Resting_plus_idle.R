# Script name: 04_Feedingbout.R
# Script purpose: summarize resting (siminputmatrix) for model parameterization

# Date created: 2023-06-24d
# Author: Eduardo Zanette

## Notes --------------------------- 
# 
#

## Options -------------------------
# (plotting, memory limit, decimal digits)

# ggplot theme
theme_set(theme_bw(base_size = 15))

## Packages -------------------------
library("here")
library("dplyr")
library("ggplot2")
library("readxl")
library("lubridate")
library("hms")


# # Load siminputrow matrix  -------------------
# siminputmatrix <- read.csv(here("Data", "Movement", "Curated", "Param_Simulation-time",
#                                 "BLT_groups_data_summary_siminputrow.csv"),
#                            sep = ",", dec = ".", stringsAsFactors = TRUE) %>%
#   mutate(group = recode(group, "Guarei" = "Guareí")) # to match all other datasets


# Read siminputrow movement data
mv <- read.csv(here("Data", "Movement", "Curated", "Param_siminputrow", 
                            "Siminputrow_movement-data.csv") #"BLT_groups_data_siminputrow.csv"),
                       #row.names = FALSE
) %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) %>%  # to match all other datasets
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "Santa Maria", "Taquara")) %>% 
  mutate(id_month = forcats::fct_relevel(id_month, "Jan", "Mar", "Apr", "May", 
                                         "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  # mutate(
  #   datetime = ymd_hms(datetime),
  #   date = lubridate::date(datetime)
  # ) %>% 
  rename(
    month = id_month
  ) %>% 
  mutate(
    datetime = lubridate::ymd_hms(datetime)
  )

mv$behavior %>% unique()

# First make idle = resting (the model does not distinguish them)
mv <- mv %>% 
  mutate(
    behavior = case_when(
      behavior == "Inactive" ~ "Resting",
      TRUE ~ behavior
    )
  )
# Check:
a$behavior %>% unique()

  
  
  
  

