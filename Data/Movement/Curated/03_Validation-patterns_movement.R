# Script name: 03_Validation-patterns_movement.R
# Script purpose: summarize, plot and analize Movement empirical data for VALIDATION.
# Also generates variable values for Validation table (necessary for ga analysis)



# **** STIL NEEDS TO PLOT THE FIGURES ***8


#### ***********************************************************************************
#### ********** CHECK SENSITIVITY ANALYSIS FOLDER ( for plots and data filtering ) *****
#### ***********************************************************************************





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
library("amt")


# # Siminputrow matrix  -------------------------
# 
# ## Load siminputrow matrix from simulation time data -------------------
# siminputmatrix <- read.csv(here("Data", "Movement", "Curated", "Param_Simulation-time", 
#                                 "BLT_groups_data_summary_siminputrow.csv"),
#                            sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
#   mutate(group = recode(group, "Guarei" = "Guareí")) # to match all other datasets


# Read siminputrow movement data
dat.all.mv <- read.csv(here("Data", "Movement", "Curated", "BLT_groups_data_siminputrow.csv"),
            #row.names = FALSE
            ) %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) #%>%  # to match all other datasets


# Summarize activity budget -------------------------
dat.all.mv$behavior %>% levels()
target_behav <- c("Frugivory", "Travel", "Foraging", "Resting")

# By day
dat.ab.summary.day <- dat.all.mv %>% 
  group_by(group, id_month, date, behavior) %>%
  summarise(
    n = n()
  ) %>% 
  mutate(
    perc_behavior = 100 * (n / sum(n))
  ) %>% 
  
  # filter only behaviors of interest of the model
  dplyr::filter(behavior %in% target_behav
  )

# # Write csv 
# dat.ab.summary.day %>%
#   write.csv(here("Data", "Movement", "Curated", "Validation", "Siminputrow_Activity-budget_By-day.csv"),
#             row.names = FALSE)

# By month (first summarize by day and get average of percentages)
dat.ab.summary <- dat.ab.summary.day %>% 
  ungroup() %>% 
  group_by(group, id_month, behavior) %>% 
  summarise(
    perc_behavior_mean = mean(perc_behavior),
    perc_behavior_sd = sd(perc_behavior)
  )

# # Write csv 
# dat.ab.summary %>%
#   write.csv(here("Data", "Movement", "Curated", "Validation", "Siminputrow_Activity-budget_By-month.csv"),
#             row.names = FALSE)

  


# Summarize DPL -------------------------
dat.dpl.summary <- dat.all.mv %>% group_by(group, id_month, date) %>% 
  summarise(
    DPL = sum(dist, na.rm = TRUE)
  )

# # Write csv
# dat.dpl.summary %>%
#   write.csv(here("Data", "Movement", "Curated", "Validation", "Siminputrow_DPL_by-day.csv"),
#             row.names = FALSE)

dat.dpl.summary.mo <- dat.dpl.summary %>% group_by(group, id_month) %>% 
  summarise(
    DPL_mean = mean(DPL, na.rm = TRUE)
  )
# # Write csv
# dat.dpl.summary %>%
#   write.csv(here("Data", "Movement", "Curated", "Validation", "Siminputrow_DPL_by-month.csv"),
#             row.names = FALSE)




# Summarize Home range -------------------------
dat.all.mv.tr <- dat.all.mv %>%
  mutate(
    id = paste0(group, " - ", id_month)
  ) %>% 
  make_track(.x=x, .y=y, id = id, crs = our_crs) %>% nest(data = -c(id))

hrvalues <- dat.all.mv.tr %>% 
  mutate( KDE95 = amt::map(data, ~ hr_kde(., level = 0.95)), 
          KDE50 = amt::map(data, ~ hr_kde(., level = 0.50)) 
  )

hrvalues <- hrvalues %>%  
  select(-data) %>% pivot_longer(KDE95:KDE50, names_to = 'KDE_value', values_to = 'hr')

hrvalues <- hrvalues %>% 
  mutate(hr_area = map(hr, hr_area)) %>%
  unnest(cols = hr_area)

hrvalues <- hrvalues %>% dplyr::select(-c('what', 'hr'))

hrvalues <- hrvalues %>% mutate(hr_area_ha = area / 10000)

# # Write csv
# hrvalues %>%
#   write.csv(here("Data", "Movement", "Curated", "Validation", "Siminputrow_Home-range_by-month.csv"),
#             row.names = FALSE)



# Other parameters not initially assessed -------------------------
# (R2n = Square displacement)

