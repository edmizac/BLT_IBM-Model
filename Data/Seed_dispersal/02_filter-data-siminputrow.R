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


#### Siminputrow matrix  -------------------------

# Load siminputrow matrix from movement data -------------------------
siminputmatrix <- read.csv(here("Data", "Movement", "Curated", "BLT_groups_data_summary_siminputrow.csv"),
                           sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) # to match all other datasets
siminputmatrix %>% str()

# groups <- siminputmatrix$group
# months <- siminputmatrix$id_month



#### Movement data  -------------------------

# Load movement dataset to gather the time tamarins wake up and mean time tamarins go to sleep 
dat.all.mv <- read.csv(here("Data", "Movement", "Curated", "BLT_groups_data.csv"),
                           sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) %>% 
  rename(datetime = POSIXct)

# dat.all.mv %>% str

# Filter by siminputrow
mv <- dat.all.mv %>% 
  dplyr::filter(group %in% siminputmatrix$group & id_month %in% siminputmatrix$id_month) %>% 
  droplevels()  

mv <- mv %>%
# get HH:mm:ss of feeding and defecation
  mutate(
    datetime = lubridate::as_datetime(datetime),
    date = as_date(datetime),
    time = as_hms(datetime)
  ) 

# Getting mean weaking hours by group and timeframe
mv.summary <- mv %>%
  group_by(group, id_month, date) %>% 
  
  # wake up and sleepint time
  summarise(
    time_wakeup = round(seconds_to_period(min(seconds(lubridate::hms(time))))),
    time_sleep = round(seconds_to_period(max(seconds(lubridate::hms(time)))))
  ) %>%
  
  # mean wake up and sleeping time
  group_by(group, id_month) %>% 
  summarise(
    time_wakeup_mean = round(seconds_to_period(mean(seconds(lubridate::hms(time_wakeup))))),
    time_sleep_mean = round(seconds_to_period(mean(seconds(lubridate::hms(time_sleep)))))
  ) %>% 
  
  # differente between wakeup and sleeping time (=activity time)
  mutate(
    time_wakesleep_diff = round(seconds_to_period(seconds(time_sleep_mean) - seconds(time_wakeup_mean)))
  )
  
# Get all back to hms
mv.summary <- mv.summary %>% 
  mutate_if(is.period, period_to_seconds) %>% 
  mutate_at(vars(matches("time")), as_hms)

# Merge into siminputmatrix
siminputmatrix <- dplyr::left_join(siminputmatrix, mv.summary)

  
# Empirical seed dispersal data for summary:  -------------------------
## Load data
dat.all.sd <- read.csv(here("Data", "Seed_dispersal", "Curated", "All-areas_SeedDispersal.csv"),
                    sep = ",", stringsAsFactors = TRUE)
# dat.all.sd %>% str()


# Make seed dispersal data match siminputrow/movement data -------------------------
dat.all.sd <- dat.all.sd %>%
  # dplyr::filter(!(group %in% siminputmatrix$group & id_month %in% siminputmatrix$id_month)) # only ~50 seed dispersal events don't match!
  dplyr::filter(group %in% siminputmatrix$group & id_month %in% siminputmatrix$id_month) %>% 
  droplevels()


# Wrangle seed dispersal data  -------------------------
a <- dat.all.sd %>%
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
  
  # get HH:mm:ss of feeding and defecation
  mutate(
    def_time = as_hms(def_datetime),
    feed_time = as_hms(feed_datetime)
  ) %>% 
  
  # # get time of feeding and defecation in period
  # mutate(    
  #   def_time_per = lubridate::seconds_to_period(def_time),
  #   feed_time_per = lubridate::seconds_to_period(feed_time)
  # ) %>%                 
  # 
  # # get duration of feeding and defecation in seconds
  # mutate(    
  #   def_time_sec = lubridate::period_to_seconds(def_time_per),
  #   feed_time_sec = lubridate::period_to_seconds(feed_time_per)
  # ) %>%                 
  
  # Order group levels and drop NA
  dplyr::filter(!is.na(group)) %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "Santa Maria", "Taquara")) %>% 
  mutate(id_month = forcats::fct_relevel(id_month, "Jan", "Mar", "Apr", "May", 
                                         "Jun", "Jul", "Aug", "Sep", "Dec"))

# dat.all.sd$id_month %>% levels()
a %>% str()
a$feed_time_per %>% sum()

# Make summary of interest variables for parameterization:  -------------------------
b <- a %>%

  group_by(group, id_month, disp_day) %>% 
  summarise(
    GTT_timesteps_mean = mean(gut_transit_time) / 5, # 1 timestep = 5 mins
    GTT_timesteps_sd = sd(gut_transit_time) / 5,
    
    # Quantos timesteps se passam até os micos terem a última defecaçao do dia?
    # time_def_mean = round(seconds_to_period(mean(seconds(lubridate::hms(def_time))))),
    # time_def_sd = round(seconds_to_period(sd(seconds(lubridate::hms(def_time))))),
    # Terei que fazer depois, ja que o número de timesteps que os micos demoraram para defecar depende do horário que eles acordaram (timestep = 1)
    # timestep_def_mean = mean(def_time_sec / 60 / 5), # 08H 33M = # 510 min (9 * 60) - 30); 510 / 5 = 102 timesteps
    # timestep_def_sd = sd(def_time_sec / 60 / 5)

  ) %>% 
  dplyr::filter(disp_day == "same day")
  #%>% 
  mutate(
    time_def_mean = (time_def_mean)
    # max_time_seeds_mean = seconds_to_period(max_time_seeds_mean), # divided by 60 (= minutes)
      # max_time_seeds_sd = seconds_to_period(max_time_seeds_sd)     # divided by 60 (= minutes) 
      )

b %>% str()
dat.all.sd %>% str()

