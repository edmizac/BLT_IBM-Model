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
library("lubridate")
# library("hms")
library("dplyr")
library("tidyr")
library("readxl")
# library("sf")
library("sp")
library("adehabitatLT")
library("circular")


# Read data (filtered by siminputrow on CurateData.R)
dat.all <- read.csv(here("Data", "Movement", "Curated", "BLT_groups_data_siminputrow.csv")
                    #, stringsAsFactors = TRUE
                    )  %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) #%>%  # to match all other datasets
  # rename(datetime = POSIXct,
  #        id_tree = id)

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
  dplyr::select(!c(y.1,date)) %>% 
  rename(date = burst) %>% 
  mutate_if(is.character, as.factor)

# dat.all.ltraj.df %>% colnames()
dat.all.ltraj.df %>% str()
# a %>% str()

# # Write csv
# dat.all.ltraj.df %>%
#   write.csv(here("Data", "Movement", "Curated", "Param_siminputrow", "Siminputrow_movement-data.csv"),
#             row.names = FALSE)


# a <- dat.all.ltraj.df$rel.angle %>%
#   rad2deg(
#   # circular::circular(
#     # units='degrees'
#                      # , rotation='clock', 
# # ,zero=pi/2,
# # modulo='2pi'
# ) %>% 
#   plot()
# 
# a$zero
# a$rotation
# a$next.points





## Summary  -------------------

# Transform rel.ang to circular
# dat.all.ltraj.df <- dat.all.ltraj.df %>% 
  # mutate(rel.angle = circular::as.circular(rel.angle))

# Convert rel.angles to degrees
# dat.all.ltraj.df$rel.angle %>% max(na.rm = TRUE) # 3.1415 = 180°. Why is not relative angles bigger than 180°? Because it is to the left or to the right! # adehabitatLT: "abs.angle,rel.angle are expressed in radians"
# dat.all.ltraj.df$rel.angle %>% circular() %>% plot() # https://www.google.com/search?q=convert+pi+to+degrees&oq=converting+pi+&aqs=chrome.1.69i57j0i13i19i512l9.7254j0j4&sourceid=chrome&ie=UTF-8
# dat.all.ltraj.df <- dat.all.ltraj.df %>% 
#   mutate(
#     rel.angle.degree = rel.angle * 180 / pi,
#     abs.angle.degree = abs.angle * 180 / pi
#   )


# Summarize step length and turning angles for travel behavior only
target_behav <- c("Travel", "Foraging")
dat.mv.summary <- dat.all.ltraj.df %>% 
  dplyr::filter(behavior %in% target_behav) %>% # or (after group_by) dplyr::filter(behavior == "Travel") %>% # Try "Frugivory", "Travel" and "Foraging"
  droplevels() %>% 
  group_by(group, id_month, behavior) %>%
  summarise(
    step_len_median = median(dist, na.rm = TRUE),
    step_len_mean = mean(dist, na.rm = TRUE),
    step_len_sd = sd(dist, na.rm = TRUE),
    rel_angle_mean = mean(circular(rel.angle, type="angles", units="degrees",
                                             modulo="pi", template='geographics'),
                                    na.rm = TRUE),
    rel_angle_sd = sd(circular(rel.angle, type="angles", units="degrees",
                                 modulo="pi", template='geographics'),
                        na.rm = TRUE),
    # Although the mean is output in degreees, sd is not # https://stackoverflow.com/questions/55870751/is-the-standard-deviation-in-the-circular-package-in-r-correct
    rel_angle_sd = rel_angle_sd * 180 / pi,
    
    # Max angle quantiles
    max_random_angle_95q = quantile(
      Mod(rel.angle), 
      0.95, na.rm = TRUE
    ),
    max_random_angle_75q = quantile(
      Mod(rel.angle), 
      0.75, na.rm = TRUE
    )
  )

# pivor wider
dat.mv.summary <- dat.mv.summary %>% 
  tidyr::pivot_wider(id_cols = c(group, id_month), names_from = "behavior", 
                     values_from = c(step_len_mean:max_random_angle_75q)
  )

# Mod(dat.all.ltraj.df$rel.angle)

# # Write csv
# dat.mv.summary %>%
#   write.csv(here("Data", "Movement", "Curated", "Param_siminputrow", "Siminputrow_sl_ta.csv"),
#             row.names = FALSE)



# Summarize activity budget for calculating p_foraging_while_traveling
dat.all.ltraj.df$behavior %>% levels()
target_behav <- c("Frugivory", "Travel", "Foraging", "Resting")

# By day
dat.ab.summary.day <- dat.all.ltraj.df %>% 
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
#   write.csv(here("Data", "Movement", "Curated", "Param_siminputrow", "Siminputrow_Activity-budget_By-day.csv"),
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
#   write.csv(here("Data", "Movement", "Curated", "Param_siminputrow", "Siminputrow_Activity-budget_By-month.csv"),
#             row.names = FALSE)



# Derive p_foraging_while_traveling based on Ronald's suggestion: 
## p_foraging_while_traveling = foraging / (foraging + traveling)
target_behav <- c("Travel", "Foraging")
dat.ab.summary <- dat.ab.summary %>% 
  dplyr::filter(behavior %in% target_behav) %>%  # only behaviors of interest 
  tidyr::pivot_wider(values_from = c(perc_behavior_mean, perc_behavior_sd), names_from = "behavior")

dat.ab.summary.p_travel <- dat.ab.summary %>% 
  group_by(group, id_month) %>%
  # dplyr::filter(behavior %in% target_behav) %>% 
  mutate(
    p_foraging_while_traveling = perc_behavior_mean_Foraging / (perc_behavior_mean_Foraging + perc_behavior_mean_Travel)
  ) 

# # Write csv
# dat.ab.summary.p_travel %>%
#   write.csv(here("Data", "Movement", "Curated", "Param_siminputrow", "Siminputrow_Activity-budget_p-foraging.csv"),
#             row.names = FALSE)


## Load siminputrow matrix from simulation time data -------------------
siminputmatrix <- read.csv(here("Data", "Movement", "Curated", "Param_Simulation-time", 
                                "BLT_groups_data_summary_siminputrow.csv"),
                           sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) # to match all other datasets
siminputmatrix %>% str()


## Merge parameters data derived here to siminputrow matrix -------------------
siminputmatrix_mov <- left_join(siminputmatrix, dat.mv.summary)
siminputmatrix_mov <- left_join(siminputmatrix_mov, dat.ab.summary.p_travel)

# # Write csv
# siminputmatrix_mov %>%
#   write.csv(here("Data", "Movement", "Curated", "Param_siminputrow", "Siminputrow_parameters_movement.csv"),
#             row.names = FALSE)


