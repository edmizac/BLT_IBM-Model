# Script name: 01_filter-data-siminputrow.R
# Script purpose: derive empirical values for parameterizing the model to run in 
# the nine situations assigned in BLT_groups_data_summary_aftercleaning.csv in 
# Data/Movement/Curated.

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


#### Siminputrow matrix  -------------------------

# Load siminputrow matrix from movement data -------------------------
siminputmatrix <- read.csv(here("Data", "Movement", "Curated",  "BLT_groups_data_summary_siminputrow.csv"),
                           sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) # to match all other datasets
siminputmatrix %>% str()

# groups <- siminputmatrix$group
# months <- siminputmatrix$id_month



# Load movement dataset to gather the time tamarins wake up and mean time tamarins go to sleepv -------------------------
dat.all.mv <- read.csv(here("Data", "Movement", "Curated", "BLT_groups_data.csv"),
                           sep = ",", dec = ".", stringsAsFactors = TRUE) %>% 
  mutate(group = recode(group, "Guarei" = "Guareí")) %>% 
  rename(datetime = POSIXct)

# dat.all.mv %>% str
dat.all.mv[ , c("group", "id_month") ] %>% distinct() # This dataset is not filtered by siminputrow


mv <- dat.all.mv %>%
# get HH:mm:ss of wakeup and sleeping times
  mutate(
    datetime = lubridate::as_datetime(datetime),
    date = as_date(datetime),
    time = as_hms(datetime)
  ) 

# Derive activity time
mv.days <- mv %>%
  group_by(group, id_month, date) %>% 
  
  # wake up and sleepint time
  summarise(
    time_wakeup = round(seconds_to_period(min(seconds(lubridate::hms(time))))),
    time_sleep = round(seconds_to_period(max(seconds(lubridate::hms(time)))))
  ) %>% 
  
  # differente between wakeup and sleeping time (=activity time)
  mutate(
    time_wakesleep_diff = round(seconds_to_period(seconds(time_sleep) - seconds(time_wakeup)))
  ) %>% 
  
  # Get all back to hms
  mutate_if(is.period, period_to_seconds) %>%
  mutate_at(vars(matches("time")), as_hms)


# 1) Getting mean waking hours by days ------------------
foo <- function(x) {
  posixct <- paste(Sys.Date(), x)
  return(posixct)
}

mv.summary.days <- mv.days %>%
  group_by(group, id_month, date) %>% 
  summarise(
    time_wakeup_mean = round(seconds_to_period(mean(seconds(lubridate::hms(time_wakeup))))),
    time_sleep_mean = round(seconds_to_period(mean(seconds(lubridate::hms(time_sleep))))),
    time_wakesleep_mean_diff = round(seconds_to_period(seconds(time_sleep_mean) - seconds(time_wakeup_mean)))
  ) %>% 
  
  # Get all back to hms
  mutate_if(is.period, period_to_seconds) %>%
  mutate_at(vars(matches("time")), as_hms) %>% 
  
  # Function to make datetime POSIXct to get difftime (= gut_transit_time in 00_prepare-data.R)
  mutate_at(vars(matches("time")), foo) %>% 
  mutate_at(vars(matches("time")), ymd_hms) %>% 
  
  # Get mean difftime in minutes and timesteps
  mutate(
    time_wakesleep_diff_minutes = round(difftime(time_sleep_mean, time_wakeup_mean, unit = "mins"), digits = 0),
    timesteps_wakesleep_diff = round(as.numeric(time_wakesleep_diff_minutes / 5, digits = 0))
  )

# # Write csv (all data)
# mv.summary.days %>% write.csv(here("Data", "Seed_dispersal", "Curated", "Param_all-data", "Summary_activity-time_days.csv"),
#                       row.names = FALSE)


# Make activity data match siminputrow table
target <- siminputmatrix[ , 1:2]

mv.summary.siminputrow <- mv.summary.days %>%
  inner_join(target) %>%  # FILTER IS WRONG. YOU HAVE TO USE INNER_JOIN()
  # dplyr::filter(group %in% siminputmatrix$group & id_month %in% siminputmatrix$id_month) %>%
  droplevels()

# gm <- mv.summary.siminputrow %>% group_by(group, id_month) %>%
#   summarise(
#     count = n()
#   )

# # Write csv (siminputrow data)
# mv.summary.siminputrow %>% write.csv(here("Data", "Seed_dispersal", "Curated", "Param_siminputrow", 
#                                           "Siminputrow_activity-time_days.csv"),
#                       row.names = FALSE)




# 2) Getting mean waking hours by group and timeframe (=siminputrow)  ------------------
foo <- function(x) {
  posixct <- paste(Sys.Date(), x)
  return(posixct)
}

mv.summary.means <- mv.days %>%
  group_by(group, id_month) %>% # not by date
  summarise(
    time_wakeup_mean = round(seconds_to_period(mean(seconds(lubridate::hms(time_wakeup))))),
    time_sleep_mean = round(seconds_to_period(mean(seconds(lubridate::hms(time_sleep))))),
    time_wakesleep_mean_diff = round(seconds_to_period(seconds(time_sleep_mean) - seconds(time_wakeup_mean)))
  ) %>% 
  
  # Get all back to hms
  mutate_if(is.period, period_to_seconds) %>%
  mutate_at(vars(matches("time")), as_hms) %>% 
  
  # Function to make datetime POSIXct to get difftime (= gut_transit_time in 00_prepare-data.R)
  mutate_at(vars(matches("time")), foo) %>% 
  mutate_at(vars(matches("time")), ymd_hms) %>% 
  
  # Get mean difftime in minutes and timesteps
  mutate(
    time_wakesleep_diff_minutes_mean = round(difftime(time_sleep_mean, time_wakeup_mean, unit = "mins"), digits = 0),
    timesteps_wakesleep_diff_mean = round(as.numeric(time_wakesleep_diff_minutes_mean / 5, digits = 0))
  )
 
# mv.summary.days$time_wakeup_mean %>% str()
# mv.summary.days$time_sleep_mean %>% str()
# mv.summary.days$time_wakesleep_mean_diff %>% str()
# mv.summary.days$time_wakesleep_diff_minutes %>% str()
# mv.summary.days$timesteps_wakesleep_diff %>% str()


# # Write csv (all data)
# mv.summary.means %>% write.csv(here("Data", "Seed_dispersal", "Curated", "Param_all-data",
#                                     "Summary_activity-time_month.csv"),
#                                row.names = FALSE)


# Make activity data match siminputrow table
target <- siminputmatrix[ , 1:2]

mv.summary.siminputrow <- mv.summary.means %>%
  inner_join(target) %>%  # FILTER IS WRONG. YOU HAVE TO USE INNER_JOIN()
  # dplyr::filter(group %in% siminputmatrix$group & id_month %in% siminputmatrix$id_month) %>%
  droplevels()

# Drop unuseful columns
mv.summary.siminputrow <- mv.summary.siminputrow %>%
  select(-c(time_sleep_mean, time_wakeup_mean, time_wakesleep_mean_diff))

# # Write csv (siminputrow data)
# mv.summary.siminputrow %>% write.csv(here("Data", "Seed_dispersal", "Curated", "Param_siminputrow",
#                                           "Siminputrow_activity-time_month.csv"),
#                       row.names = FALSE)



# Now we can check how long is the maximum in timesteps it takes for tamarins to defecate seeds after the start of the day.
# timesteps_wakesleep_mean_diff matches mean_timesteps, this means my calculations are correct

# With the summary by day it is also possible to check the wakeup and sleep times, which are very useful for the next plot


  


# Load empirical seed dispersal data for summary:  -------------------------
## Load data
dat.all.sd <- read.csv(here("Data", "Seed_dispersal", "Curated", "All-areas_SeedDispersal.csv"),
                    sep = ",", stringsAsFactors = TRUE)
# dat.all.sd %>% str()


# ## Make seed dispersal data match siminputrow/movement data -------------------------
# target <- siminputmatrix[ , 1:2]
# 
# dat.all.sd <- dat.all.sd %>%
#   inner_join(target) %>%  # FILTER IS WRONG. YOU HAVE TO USE INNER_JOIN()
#   # dplyr::filter(group %in% siminputmatrix$group & id_month %in% siminputmatrix$id_month) %>%
#   droplevels()
# 
# gm <- dat.all.sd %>% group_by(group, id_month) %>%
#   summarise(
#     count = n()
#   ) ## SANTA MARIA APRIL AND GUAREI AUGUST DOES NOT HAVE SEED DISPERSAL DATA!
# 










### Wrangle seed dispersal data  -------------------------
dat.all.sd <- dat.all.sd %>%
  # make datetime POSIXct
  mutate(
    def_datetime = lubridate::as_datetime(def_datetime), #, tz = "America/Sao_Paulo"),
    feed_datetime = lubridate::as_datetime(feed_datetime) #, tz = "America/Sao_Paulo"),
  ) %>% 
  
  # gather yyyy/mm/dd as id
  mutate(
    date = lubridate::as_date(def_datetime),
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
dat.all.sd %>% str()
# a$feed_time_per %>% sum()

### Merge wake up and sleep times to seed dispersal data
sd.mv <- left_join(dat.all.sd, mv.days) # not mv.summary.day

# colnames(sd.mv)
# colnames(mv.days)
# colnames(mv.days) %in% colnames(sd.mv)


### Define complete days (for which we have wake up and sleeping time according to timediff collum)
sd.mv <- sd.mv %>% 
  mutate(full_day = case_when(is.na(time_wakeup) ~ "incomplete",
                              TRUE ~ "complete"
                              ))

# ### Calculate GTT (original)
# a <- a %>% 
#   group_by(group, id_month) %>% 
#   summarise(
#     GTT_timesteps_mean = round(mean(gut_transit_time, na.rm = TRUE) / 5, digits = 0), # 1 timestep = 5 mins
#     GTT_timesteps_sd = round(sd(gut_transit_time, na.rm = TRUE) / 5, digits = 0), 
#     
#   )

### Calculate mean GTT and save
sd.mv.summary <- sd.mv %>%
  group_by(group, id_month, disp_day) %>%
  summarise(
    GTT_timesteps_mean = round(mean(gut_transit_time, na.rm = TRUE) / 5, digits = 0), # 1 timestep = 5 mins
    GTT_timesteps_sd = round(sd(gut_transit_time, na.rm = TRUE) / 5, digits = 0),
    n_disp_events = n()
  ) %>% 
  dplyr::filter(!is.na(id_month))
# ### Write csv
# sd.mv.summary %>%
#   write.csv(here("Data", "Seed_dispersal", "Curated", "Param_all-data", "Summary_GTT_mean.csv"),
#             row.names = FALSE)


### Diminish the start hour from the defecation hour (complete days only)
str(a)
sd.mv.complete <- sd.mv  %>%
  # filter NAs
  # dplyr::filter(full_day == "complete") %>%  # mesma coisa
  dplyr::filter(!is.na(time_wakeup)) # mesma coisa

### Make it POSIXct to get difftime (= gut_transit_time in 00_prepare-data.R)
# foo <- function(x) {
#   posixct <- paste(Sys.Date(), x)
#   return(posixct)
# }

sd.mv.complete <- sd.mv.complete %>%
  mutate(
    def_time = case_when(
      disp_day == "same day" ~ paste(Sys.Date(), def_time),
      disp_day == "next day" ~ paste(Sys.Date()+1, def_time)
    ),
    
    time_wakeup = case_when(
      disp_day == "same day" ~ paste(Sys.Date(), time_wakeup),
      disp_day == "next day" ~ paste(Sys.Date()+1, time_wakeup)
    )
  ) %>% 
  # mutate_at(c("def_time", "time_wakeup"), foo) %>% 
  mutate_at(c("def_time", "time_wakeup"), ymd_hms)

# sd.mv.complete %>% str()

### Get mean difftime in minutes and timesteps
sd.mv.complete <- sd.mv.complete %>%
  mutate(
    time_wakeup_to_def = round(difftime(def_time, time_wakeup, unit = "mins"), digits = 0),
    timesteps_wakeup_to_def = round(as.numeric(time_wakeup_to_def / 5, digits = 0))
  )


### Make summary of timesteps for defecation by disp_day:  -------------------------  
c <- sd.mv.complete %>%
  group_by(group, id_month, disp_day) %>% 
  summarise(
    # Quantos timesteps se passam em média até os micos terem a última defecaçao do dia?
    timesteps_wakeup_to_def_mean = round(mean(timesteps_wakeup_to_def), digits = 0),
    timesteps_wakeup_to_def_sd = round(sd(timesteps_wakeup_to_def), digits = 0),
    max_timestep_to_def = max(timesteps_wakeup_to_def_mean + timesteps_wakeup_to_def_sd)
  )

### Get the latest morning defecation
e <-  c %>%
  dplyr::filter(disp_day == "next day")
# e <- f %>%
# e <- c %>%
#   summarise(
#     max_timestep = max(timesteps_wakeup_to_def_mean + timesteps_wakeup_to_def_sd)
#   )
hline2 <- max(e$max_timestep)
hline3 <- mean(e$max_timestep)
### From this we can see that the timesteps taken to morning defecation after tamarins wake up is very small (from 1 to 12)  

sd.mv.complete.nextday <- sd.mv.complete %>%
  dplyr::filter(disp_day == "next day")

### Plot it
# e %>% 
c %>%
  ggplot(
    # aes(x = group, y = timesteps_wakeup_to_def_mean,
    # 
    #     #group = interaction(disp_day, id_month),
    #     color = id_month,
    #     #shape = disp_day,
    #     ymin = timesteps_wakeup_to_def_mean - timesteps_wakeup_to_def_sd,
    #     ymax = timesteps_wakeup_to_def_mean + timesteps_wakeup_to_def_sd)
  ) +
  
  # geom_boxplot(
  #   aes(x = group, y = timesteps_wakeup_to_def_mean, color = id_month)
  #   ) +

  geom_pointrange(
    aes(x = group, y = timesteps_wakeup_to_def_mean, color = id_month,

    #group = interaction(disp_day, id_month),
    #shape = disp_day,
    ymin = timesteps_wakeup_to_def_mean - timesteps_wakeup_to_def_sd,
    ymax = timesteps_wakeup_to_def_mean + timesteps_wakeup_to_def_sd),
    position = position_dodge(width = 0.5)
    ) +
  
  # Add points from raw data
  geom_point(
    # data = sd.mv.complete, aes(x = group, y = timesteps_wakeup_to_def, color = id_month),
    data = sd.mv.complete.nextday, aes(x = group, y = timesteps_wakeup_to_def, color = id_month),
    alpha = 0.8, shape = 2, position = position_jitterdodge(jitter.width =  0)
    ) +
  
  # Add horizontal lines:
  geom_hline(yintercept = 0, col = "black") +
  # geom_hline(yintercept = hline2, col = "red") +
  # geom_hline(yintercept = hline3, col = "blue") +
  # coord_cartesian(clip = "off") + # this allows plotting text outside of plot
  # annotate("text", label = paste("max morning defecation time ( = ", hline2, ")"),  x = 2.5, y = hline2 + 2.5, color = "red") +
  # annotate("text", label = paste("mean =", hline3),  x = 2.5, y = hline3 + 2.5, color = "blue") +
  annotate("text", label = "wake up time",  x = 2.5, y = 0 - 2, color = "black") +
  
  ylab("timesteps (5 min)") +
  ggtitle("timesteps from waking up to defecation (complete days only)") +
  theme_bw(base_size = 15) +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(
          hjust = 0.3  ,
          size = 12
                                  )) +
  scale_color_viridis_d()

#### Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", "Param_all-data",
#             'Summary_GTT_morning-defecation_complete-days.png'), height = 7, width = 5)

##### Write csv
# c %>%
#   write.csv(here("Data", "Seed_dispersal", "Curated", "Param_all-data", "Summary_GTT_morning-defecation_complete-days.csv"),
#             row.names = FALSE)


### From this we can see that the timesteps taken to morning defecation after tamarins wake up is very small (from 1 to 12, but mean = 6.3)  


# Filtering for siminputrow
target <- siminputmatrix[ , 1:2]

c_siminputrow <- c %>%
  inner_join(target) %>%  # FILTER IS WRONG. YOU HAVE TO USE INNER_JOIN()
  # dplyr::filter(group %in% siminputmatrix$group & id_month %in% siminputmatrix$id_month) %>%
  droplevels()

sd.mv.complete.nextday_siminputrow <- sd.mv.complete.nextday %>% 
  inner_join(target) %>%
  droplevels()

### Get the latest morning defecation
e_siminputrow <-  c_siminputrow %>%
  dplyr::filter(disp_day == "next day")
# e <- c %>%
#   summarise(
#     max_timestep = max(timesteps_wakeup_to_def_mean + timesteps_wakeup_to_def_sd)
#   )
hline2 <- max(e_siminputrow$max_timestep)
hline3 <- mean(e_siminputrow$max_timestep)

### Plot it
# e %>% 
# e_siminputrow %>%
c_siminputrow %>%
  ggplot(
    # aes(x = group, y = timesteps_wakeup_to_def_mean,
    # 
    #     #group = interaction(disp_day, id_month),
    #     color = id_month,
    #     #shape = disp_day,
    #     ymin = timesteps_wakeup_to_def_mean - timesteps_wakeup_to_def_sd,
    #     ymax = timesteps_wakeup_to_def_mean + timesteps_wakeup_to_def_sd)
  ) +
  
  # geom_boxplot(
  #   aes(x = group, y = timesteps_wakeup_to_def_mean, color = id_month)
  #   ) +
  
  geom_pointrange(
    aes(x = group, y = timesteps_wakeup_to_def_mean, color = id_month,
        
        #group = interaction(disp_day, id_month),
        #shape = disp_day,
        ymin = timesteps_wakeup_to_def_mean - timesteps_wakeup_to_def_sd,
        ymax = timesteps_wakeup_to_def_mean + timesteps_wakeup_to_def_sd),
    position = position_dodge(width = 0.5)
  ) +
  
  # Add points from raw data
  geom_point(
    # data = sd.mv.complete, aes(x = group, y = timesteps_wakeup_to_def, color = id_month),
    data = sd.mv.complete.nextday_siminputrow, aes(x = group, y = timesteps_wakeup_to_def, color = id_month),
    alpha = 0.8, shape = 2, position = position_jitterdodge(jitter.width =  0)
  ) +
  
  # Add horizontal lines:
  geom_hline(yintercept = 0, col = "black") +
  geom_hline(yintercept = hline2, col = "red") +
  geom_hline(yintercept = hline3, col = "blue") +
  # coord_cartesian(clip = "off") + # this allows plotting text outside of plot
  annotate("text", label = paste("max morning defecation time ( = ", hline2, ")"),  x = 2.5, y = hline2 + 2.5, color = "red") +
  annotate("text", label = paste("mean =", hline3),  x = 2.5, y = hline3 + 2.5, color = "blue") +
  annotate("text", label = "wake up time",  x = 2.5, y = 0 - 2, color = "black") +
  
  ylab("timesteps (5 min)") +
  ggtitle("timesteps from waking up to defecation (complete days only)") +
  theme_bw(base_size = 15) +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(
          hjust = 0.3  ,
          size = 12
        )) +
  scale_color_viridis_d()

#### Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", "Param_siminputrow",
#             'Siminputrow_GTT_morning-defecation_complete-days_siminputrow.png'), height = 7, width = 5)

##### Write csv
# c %>%
#   write.csv(here("Data", "Seed_dispersal", "Curated", "Param_siminputrow", "Siminputrow_GTT_morning-defecation_complete-days.csv"),
#             row.names = FALSE)



#### Write csv for next day events (same day values are not entering the siminputmatrix as this is estimated by GTT in 00_prepare-data.R already)
#### Write csv for all data and siminputrow. First join all data from summary
c_join <- dplyr::left_join(sd.mv.summary, c, by = c("group", "id_month", "disp_day"))

d1 <- c_join %>% 
  dplyr::filter(disp_day == "next day") %>% 
  ungroup()

d2 <- c_join %>% 
  dplyr::filter(disp_day == "same day") %>% 
  ungroup()

to_app1 <- "_next_day"
to_app2 <- "_same_day"
cols1 <- d1 %>% dplyr::select("timesteps_wakeup_to_def_mean":"max_timestep_to_def") %>% colnames()
cols2 <- d2 %>% dplyr::select("timesteps_wakeup_to_def_mean":"max_timestep_to_def") %>% colnames()

d1 <- d1 %>% 
  rename_with(., ~paste(cols1, to_app1), all_of(cols1)) %>% 
  # tidyr::pivot_wider()
  select(-disp_day)

d2 <- d2 %>% 
  rename_with(., ~paste(cols2, to_app2), all_of(cols2)) %>% 
  # tidyr::pivot_wider()
  select(-disp_day)

### Join and save

#### All data
d_all <- left_join(d1,d2)
# # Write csv
# d_all %>%
#   write.csv(here("Data", "Seed_dispersal", "Curated", "Param_all-data", "Summary_seed-dispersal_params.csv"),
#           row.names = FALSE)

#### Siminputrow data
target <- siminputmatrix[ , 1:2]

d_siminputrow <- d_all %>%
  inner_join(target) %>%
  droplevels()


# d_siminputrow %>%
#   write.csv(here("Data", "Seed_dispersal", "Curated", "Param_siminputrow", "Siminputrow_seed-dispersal_params.csv"),
#             row.names = FALSE)

### Unfortunately there's no seed dispersal for two of the nine siminputrows (Guarei Aug and Santa Maria Apri). 
### These parameters will be gathered from averages on 04_construct_siminputrow.R

































  

# Continue with the siminputmatrix
b <- b %>% 
  dplyr::filter(disp_day == "same day")


  mutate(
    # time_def_mean = (time_def_mean)
    # max_time_seeds_mean = seconds_to_period(max_time_seeds_mean), # divided by 60 (= minutes)
      # max_time_seeds_sd = seconds_to_period(max_time_seeds_sd)     # divided by 60 (= minutes) 
      )

b %>% str()
dat.all.sd %>% str()

