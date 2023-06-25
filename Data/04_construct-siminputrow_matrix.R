# Script name: 04_construct-siminputrow_matrix.R
# Script purpose: Construct the siminputrow matrix to use in run_nl_all() script

# Date created: 2022-11-16d
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
# library("ggplot2")
library("readxl")
library("lubridate")
library("hms")


#### Siminputrow matrix  -------------------------

# 1) Load siminputrow matrix from simulation timeas base -------------------------
siminputmatrix <- read.csv(here("Data", "Movement", "Curated", #"Param_Simulation-time",
                                "BLT_groups_data_summary_siminputrow.csv"),
                           sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) # to match all other datasets
siminputmatrix %>% str()

# groups <- siminputmatrix$group
# months <- siminputmatrix$id_month



# 2) Load seed dispersal siminputrow matrix  -------------------------
## to gather GTT, timesteps to morning defecation, number of defecations per day, etc
dat.all.sd <- read.csv(here("Data", "Seed_dispersal", "Curated", "Param_siminputrow",  "Siminputrow_disp-day_nex-day_params.csv"),
                       sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  # mutate(group = recode(group, "Guarei" = "Guareí")) %>% 
  
  # Rename variables of interest:
  rename(
    GTT_mean = GTT_timesteps_mean_same_day,
    GTT_sd = GTT_timesteps_sd_same_day,
    morning_defecation_GTT_mean = timesteps_wakeup_to_def_mean_next_day,
    morning_defecation_GTT_sd = timesteps_wakeup_to_def_sd_next_day,
  ) %>% 
  dplyr::select(c(1:5, "GTT_mean", "GTT_sd",
                  "morning_defecation_GTT_mean", "morning_defecation_GTT_sd"))

# dat.all.sd$GTT_timesteps_mean._same_day
# dat.all.sd$GTT_timesteps_sd._same_day
# dat.all.sd$timesteps_wakeup_to_def_mean._next_day
# dat.all.sd$timesteps_wakeup_to_def_sd._next_day

  
# (NOT NEEDED) Guess missing values based on the same group values 
#(it was for Guareí August, but it only does not have SDD, but has dispersal data (it was being filtered out on the 00_prepare-data.R))
## Guareí
# dat.all.sd[ 1 , "GTT_mean"] <- dat.all.sd %>% 
#   dplyr::filter(group == "Guareí") %>% 
#   dplyr::select("GTT_mean") %>%
#   summarise(
#     mean = mean(GTT_mean, na.rm = TRUE)
#   )
# 
# dat.all.sd[ 1 , "GTT_sd"] <- dat.all.sd %>% 
#   dplyr::filter(group == "Guareí") %>% 
#   dplyr::select("GTT_sd") %>%
#   summarise(
#     sd = sd(GTT_sd, na.rm = TRUE)
#   )
#   
# dat.all.sd[ 1 , "morning_defecation_GTT_mean"] <- dat.all.sd %>% 
#   dplyr::filter(group == "Guareí") %>% 
#   dplyr::select("morning_defecation_GTT_mean") %>%
#   summarise(
#     mean = mean(morning_defecation_GTT_mean, na.rm = TRUE)
#   )
# 
# dat.all.sd[ 1 , "morning_defecation_GTT_sd"] <- dat.all.sd %>% 
#   dplyr::filter(group == "Guareí") %>% 
#   dplyr::select("morning_defecation_GTT_sd") %>%
#   summarise(
#     sd = sd(morning_defecation_GTT_sd, na.rm = TRUE)
#   )  

## Santa Maria APRIL HAS NO SEED DISPERSAL DATA. AS SANTA MARIA HAS ONLY ONE MORE MONTH, I'LL AVERAGE OUT FROM GUAREI AS WELL
target <- c("Santa Maria", "Guareí")

dat.all.sd[ 5 , "GTT_mean"] <- dat.all.sd %>% 
  dplyr::filter(group %in% target) %>% 
  dplyr::select("GTT_mean") %>%
  summarise(
    mean = mean(GTT_mean, na.rm = TRUE)
  )

dat.all.sd[ 5 , "GTT_sd"] <- dat.all.sd %>% 
  dplyr::filter(group %in% target) %>% 
  dplyr::select("GTT_sd") %>%
  summarise(
    sd = sd(GTT_sd, na.rm = TRUE)
  )

target <- "Guareí"

dat.all.sd[ 5 , "morning_defecation_GTT_mean"] <- dat.all.sd %>% 
  dplyr::filter(group %in% target) %>% 
  dplyr::select("morning_defecation_GTT_mean") %>%
  summarise(
    mean = mean(morning_defecation_GTT_mean, na.rm = TRUE)
  )

dat.all.sd[ 5 , "morning_defecation_GTT_sd"] <- dat.all.sd %>% 
  dplyr::filter(group %in% target) %>% 
  dplyr::select("morning_defecation_GTT_sd") %>%
  summarise(
    sd = sd(morning_defecation_GTT_sd, na.rm = TRUE)
  )   


## Santa Maria MARCH HAS NO NEXT DAY SEED DISPERSAL EVENTS; I'LL MAKE IT EQUAL TO SANTA MARIA APRIL (GUAREÍ AVERAGE)
target <- c("Guareí")

dat.all.sd[ 6 , "morning_defecation_GTT_mean"] <- dat.all.sd %>% 
  dplyr::filter(group %in% target) %>% 
  dplyr::select("morning_defecation_GTT_mean") %>%
  summarise(
    mean = mean(morning_defecation_GTT_mean, na.rm = TRUE)
  )

dat.all.sd[ 6 , "morning_defecation_GTT_sd"] <- dat.all.sd %>% 
  dplyr::filter(group %in% target) %>% 
  dplyr::select("morning_defecation_GTT_sd") %>%
  summarise(
    sd = sd(morning_defecation_GTT_sd, na.rm = TRUE)
  )   

# Round numeric values
round2 <- function(x) (round(x, digits = 2))
dat.all.sd <- dat.all.sd %>% 
  mutate(across(where(is.numeric), round2))

# Rename siminputmatrix objetct
siminputmatrix_sd <- dat.all.sd
  


# 3) Load movement siminputrow matrix -------------------------

## to gather step length, max turning angles and p_foraging
siminputmatrix_mv <- read.csv(here("Data", "Movement", "Curated", "Param_siminputrow", "Siminputrow_parameters_movement.csv"),
                       sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  dplyr::select(!starts_with("perc_"))



# 4) Gather other siminputrow data -------------------------
## (number of trees, number of sleeping sites, number of tamarins). I'll take this from movement data

dat.mv <- read.csv(here("Data", "Movement", "Curated", "BLT_groups_data_siminputrow.csv")
                    , stringsAsFactors = TRUE
)  %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) #%>%  # to match all other datasets

# dat.mv %>%  dplyr::filter(behavior == "Sleeping site")

target_behav <- c("Sleeping site", "Frugivory") 

siminputmatrix_others <- dat.mv %>% 
  dplyr::filter(behavior %in% target_behav) %>%
  group_by(group, id_month, behavior) %>%
  summarise(
    n_trees = n_distinct(id_tree)
  ) %>% 
  
  # pivot wider
  tidyr::pivot_wider(names_from = behavior, values_from = n_trees,
                     names_glue = "{.value}_{behavior}")
  

# Derive p_resting (idle+resting/all behaviors)
siminputmatrix <- siminputmatrix %>% 
  mutate(p_resting = count_rest/timesteps)


# 5) Merge all parameter tables together into one siminputrow table -------------------------
siminputmatrix_complete <- left_join(siminputmatrix_mv, siminputmatrix_sd)
siminputmatrix_complete <- left_join(siminputmatrix_complete, siminputmatrix_others)
siminputmatrix_complete <- left_join(siminputmatrix_complete, siminputmatrix)


# 6) Input number of tamarins by hand -------------------------
siminputmatrix_complete$n_tamarins <- c(5, 5, 5, 5, 2, 2, 6, 4, 4) # values from: Guareí (Dissert. Felipe), Santa Maria (Dissert. Yness), Taquara and Suzano (Dissert. Anne and LaP Demography database https://www.dropbox.com/s/vt81l28hqg5yzaz/Demography_MLP_LaP.xlsx?dl=0) 


# 7)  Write csv -------------------------
siminputmatrix_complete %>% str()
# siminputmatrix_complete %>%
#   write.csv(here("Data", "Parameter_table.csv"),
#             row.names = FALSE)








