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

# ggplot theme
theme_set(theme_bw(base_size = 15))

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


# Read siminputrow movement data and define levels
dat.all.mv <- read.csv(here("Data", "Movement", "Curated", "BLT_groups_data_siminputrow.csv"),
            #row.names = FALSE
            ) %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) %>%  # to match all other datasets
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "Santa Maria", "Taquara")) %>% 
  mutate(id_month = forcats::fct_relevel(id_month, "Jan", "Mar", "Apr", "May", 
                                         "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  mutate(
    datetime = ymd_hms(datetime),
    date = lubridate::date(datetime)
    )



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
  make_track(.x=x, .y=y, id = id, crs = our_crs, all_cols = TRUE) %>% 
  nest(data = -c(id, group, id_month))

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



# Plot home range -------------------------

# Define factor order

# KDE 95
hrvalues %>% 
  dplyr::filter(KDE_value == "KDE95") %>%
  group_by(group, id_month) %>%
  # facet_wrap(~KDE_value) +
  ggplot(aes(x = group, y = hr_area_ha, color = id_month)) +
  geom_boxplot() +
  guides(fill=FALSE) +
  ylim(0, 350) +
  ylab("Area (ha)") +
  # xlab("Group - month") + 
  theme(axis.title.x=element_blank()) +
  ggtitle("KDE 95% (Home range)") +
  scale_colour_viridis_d()
# geom_jitter(width = 0.15) +
# annotate("text", x=2, y=5000, label= paste0("Mean ± sd = ", round(avg_dist, 2), " ± ", round(sd_dist, 2)))

# ggsave(filename = here("Data", "Movement", "Curated", "Validation",
#                        "Siminputrow_HomeRange_KDE95.png"),
#        dpi = 300, width = 15, height = 10 , units = "cm")

# KDE50
hrvalues %>% 
  dplyr::filter(KDE_value == "KDE50") %>%
  group_by(group, id_month) %>%
  # facet_wrap(~KDE_value) +
  ggplot(aes(x = group, y = hr_area_ha, color = id_month)) +
  geom_boxplot() +
  # geom_point() +
  guides(fill=FALSE) +
  ylim(0, 350) +
  ylab("Area (ha)") +
  # xlab("Group - month") +
  theme(axis.title.x=element_blank()) +
  ggtitle("KDE 50% (Core area)") +
  scale_colour_viridis_d()
# geom_jitter(width = 0.15) +
# annotate("text", x=2, y=5000, label= paste0("Mean ± sd = ", round(avg_dist, 2), " ± ", round(sd_dist, 2)))

ggsave(filename = here("Data", "Movement", "Curated", "Validation",
                       "Siminputrow_HomeRange_KDE50.png"),
       dpi = 300, width = 15, height = 10, units = "cm")




# Other parameters not initially assessed -------------------------
# (R2n = Square displacement)






