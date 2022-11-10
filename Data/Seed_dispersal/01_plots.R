# Script name: 01_plots.R
# Script purpose: plot and analize Seed dispersal empirical data

# Date created: 2022-11-03d
# Author: Eduardo Zanette

## Notes --------------------------- 
#
#

## Packages -------------------------
library("here")
# library("tidyverse")
library("dplyr")
library("readxl")
library("lubridate")
# library("hms")
library("ggplot2")
library("ggalt")

## Options -------------------------
# (plotting, memory limit, decimal digits)
theme_set(theme_bw(base_size = 15))


# Load data
dat.all <- read.csv(here("Data", "Seed_dispersal", "Curated", "All-areas_SeedDispersal.csv"),
                     sep = ",", stringsAsFactors = TRUE)
# dat.all %>% str()
# is.na(dat.all$def_datetime)
# nrow(dat.all[is.na(dat.all$def_datetime), ])


# Wrangling data
dat.all <- dat.all %>%
  
  mutate(
    def_datetime = lubridate::as_datetime(def_datetime), #, tz = "America/Sao_Paulo"),
    feed_datetime = lubridate::as_datetime(feed_datetime) #, tz = "America/Sao_Paulo"),
    ) %>% 
  
  # mutate(
  #   def_datetime = ymd_hms(def_datetime),
  #   feed_datetime = ymd_hms(feed_datetime),
  #   ) #%>%
  
  mutate(
    day = lubridate::as_date(def_datetime),
    # date_time = parse_date_time(def_datetime, orders = "ymd "),
    # hours = lubridate::as_datetime(def_datetime),
    # hours = lubridate::hms(hours)
    # day = lubridate::dmy(def_datetime, locale = "English")
  ) %>% 
  
  # Assiging dispersal event as in the same day or next day
  mutate(disp_day = ifelse(lubridate::day(def_datetime) == lubridate::day(feed_datetime), # use case_when for a dplyr-er solution: https://stackoverflow.com/questions/22337394/dplyr-mutate-with-conditional-values
                           "same day",
                           "previous day"),
         disp_dau = as.factor(disp_day)
  )

# str(dat.all)
# str(a)
  

# Check data
dat.all %>% 
  dplyr::filter(day == "2017-12-03") %>% 
  str()


##### Density plots #####

### Gut transit time ###
dat.all %>% ggplot(
  aes(x = gut_transit_time / 60, fill = group, group = group)
  ) +
  geom_density(alpha = 0.4) +
  xlab("gut transit time (in hours)") +
  # Option 1.1:
  facet_wrap(~disp_day, nrow = 2)
  # Option 1.2:
  # facet_wrap(~species, ncol = 5) +
  # Option2:
  # facet_grid(rows = vars(species), cols = vars(disp_day))
  # theme_bw(base_size = 10)

# Save plot  
# ggsave('testplot.png', height = 15, width = 8.5)  


### SDD ###
dat.all %>% ggplot(
  aes(x = SDD, fill = group, group = group)
) +
  geom_density(alpha = 0.4) +
  xlab("Seed dispersal distance (SDD in meters)") +
  # Option 1.1:
  facet_wrap(~disp_day, nrow = 2) +
  # Option 1.2?
  # facet_wrap(~species) +
  theme_bw(base_size = 15) # +
  # theme(axis.text.x = element_text(size=10))

dat.all %>% 
  ggplot() +
  aes(x = group, y = SDD, fill = group) +
  geom_boxplot()


### n defecations per day ###
dat.all.summary <- dat.all %>% 
  group_by(group, day) %>% 
  summarize(
    defecation_events = n(),
    mean_SDD = mean(SDD),
    sd_SDD = sd(SDD)
  )

dat.all.summary %>% 
  ggplot() +
  aes(x = group, y = defecation_events, fill = group) +
  geom_boxplot()


#### defecations by hour ##### 

# prep data (use hms package instead of lubridate)
dat.all.hourwise.sameday <- dat.all %>% 
  dplyr::filter(disp_day == "same day") %>%
  dplyr::filter(group == "Suzano") %>% # for plotting each group
  
  # for hms datetime (scale_x_time)
  mutate(
    month_id = lubridate::month(feed_datetime, label = TRUE, locale="English_United States"),
    def_datetime = as_hms(def_datetime),
    feed_datetime = as_hms(feed_datetime),
    difftime = feed_datetime - def_datetime)

  # filter specific month
  # dplyr::filter(month_id == "May")
# 
# ### create function to get circular hours (bases on https://rvanmazijk.github.io/circular-data/)
# interval(gua.nextday$feed_datetime, gua.nextday$def_datetime)
# get_month_seq <- function(grt) {
#   # Creates seq from start to end, unless end <= start
#   # (e.g. 16 h to 07 h = 11, 12, 1, 2)
#   
#   map2(def_interval, function(def_interval) {
#     hour_seq <- 
#     if (int_start(def_interval) > int_end(def_interval))          lubridate::int_flip(def_interval)
#     else if (int_start(def_interval) < int_end(def_interval))     def_interval
#     hour_seq
#   })
# }
# 
# grttimes <- gua.nextday %>%
#   mutate(def_times = get_month_seq(feed_datetime, def_datetime))
# grttimes
# 
# gua.nextday <- gua.nextday
# 
# # for POSIXct datetime (scale_x_datetime)
# # mutate(feed_datetime = as.POSIXct(strptime(feed_datetime, "%Y-%m-%d %H:%M")),
# # def_datetime = as.POSIXct(strptime(def_datetime, "%Y-%m-%d %H:%M")))
# 
# lims <- as.POSIXct(strptime(c("2011-01-01 13:00","2011-01-02 12:00"), format = "%Y-%m-%d %H:%M"))


### Option 1 - Dunbell plot based on: https://stackoverflow.com/questions/70816256/how-to-plot-time-interval-data-in-r-using-ggplot
dat.all.hourwise.sameday %>%
  ggplot(
    aes(
      x = feed_datetime,
      xend = def_datetime,
      y = species# disp_event
    )
  ) +
  xlab("time (hours)") +
  # scale_x_time() +

  geom_dumbbell(
    colour = "#a3c4dc",
    colour_xend = "#e68a00",
    size = 1
  ) +
  geom_jitter(height = 0.1) #+
  # theme(axis.text.y = element_text(size=5)) #+
#scale_x_continuous(limits = lims)

### Option 2 - geom_segment # https://stackoverflow.com/questions/55185599/plotting-time-intervals-as-segments
###                          or https://stackoverflow.com/questions/70816256/how-to-plot-time-interval-data-in-r-using-ggplot
dat.all.hourwise.sameday %>%
  ggplot() +
  geom_point(aes(x=feed_datetime, 
                 y=species #disp_event
                 ), 
             size=1) +
    geom_segment(aes(x=feed_datetime, xend=def_datetime, y = species, yend = species), # y and yend=disp_event
               arrow=arrow(30,unit(1.25,"mm"),"last","closed")) +
  theme(axis.text.y = element_text(size=15)) #+
   # facet_wrap(~month_id)

### Option 2.2 - to jitter lines (based on https://stackoverflow.com/questions/21904364/how-to-jitter-dodge-geom-segments-so-they-remain-parallel
###                               and https://stackoverflow.com/questions/60981619/apply-jitter-uniformly-across-geoms-in-ggplot2)

dat.all.hourwise.sameday %>%
  ggplot() +
  geom_point(aes(y = feed_datetime, x = species), 
             position = position_jitter(seed = 123, width =0.5)) +
  geom_point(aes(y = def_datetime, x = species), shape = 10,
             position = position_jitter(seed = 123, width =0.5)) +
  # geom_point(aes(y=def_datetime, x = species), shape = 17) +
  # geom_text(aes(y=feed_datetime, x = species), label = "▶", size = 3, family = "HiraKakuPro-W3") +
  geom_linerange(
    aes(y = feed_datetime,
        ymin=feed_datetime, ymax=def_datetime, 
        x = species, 
        # xmin = species, xmax = species,  
        group = disp_event),
    position = position_jitter(seed = 123, .5)) +
  coord_flip()

# Funciona:
# dat.all.hourwise.sameday %>%
#   # group_by(disp_event) %>% 
#   ggplot(aes(y = feed_datetime,
#              ymin=feed_datetime, ymax=def_datetime, 
#              x = disp_event, 
#              xmin = disp_event, xmax = disp_event,  
#              group = species,
#              color = species)) +
#   # geom_point(aes(y = feed_datetime, x = species)) +
#   # geom_point(aes(y = def_datetime, x = species), shape = 10) +
#   # geom_point(aes(y=def_datetime, x = species), shape = 17) +
#   # geom_text(aes(y=feed_datetime, x = species), label = "▶", size = 3, family = "HiraKakuPro-W3") +
#   geom_linerange(
#     position = position_dodge(.75)) +
#   coord_flip()