# Script name: 01_plots.R
# Script purpose: plot and analize Seed dispersal empirical data

# Date created: 2022-11-03d
# Author: Eduardo Zanette

## Notes --------------------------- 
#
#

## Packages -------------------------
library("here")
library("tidyverse")
library("dplyr")
library("ggplot2")
library("readxl")
library("ggalt")
library("lubridate")
# library("hms")

## Options -------------------------
# (plotting, memory limit, decimal digits)
theme_set(theme_bw(base_size = 15))


# Load data
dat.all <- read.csv2(here("Data", "Seed_dispersal", "Curated", "All-areas_SeedDispersal.csv"),
                     sep = ",")
dat.all %>% str()
is.na(dat.all$def_datetime)

# detach("package:hms", unload=TRUE)

# Assiging dispersal event as in the same day or next day
dat.all <- dat.all %>%
  mutate(def_datetime = as.POSIXct(def_datetime),
         feed_datetime = as.POSIXct(feed_datetime),
         SDD = as.numeric(SDD)) %>%
  # mutate(def_datetime = ymd_hms(def_datetime),
         # feed_datetime = ymd_hms(feed_datetime)) %>% 
  

  # mutate(gut_transit_time_h2 = hm(def_datetime)) %>%
  mutate(disp_day = ifelse(lubridate::day(def_datetime) == lubridate::day(feed_datetime), # use case_when for a dplyr-er solution: https://stackoverflow.com/questions/22337394/dplyr-mutate-with-conditional-values
                           "same day",
                           "previous day")
  )

a %>% str()

# Get hour-normalized dispersal events (*! still needs to solve the problem with circular/directional data)
  # for hms datetime (scale_x_time)
# a <- dat.all %>% 
#   mutate(
#     def_hour = as_hms(def_datetime),
#     feed_hour = as_hms(feed_datetime),
#     difftime_hour = difftime(feed_hour, def_hour))



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
  theme_bw(base_size = 10) # +
  # theme(axis.text.x = element_text(size=10))


### n defecations per day ###


##### Checking when seeds are dispersed next day #####

## for seeds defecated on the next day:
dat.gua.5 <- dat.gua %>% 
  dplyr::filter(gut_transit_time > 5)

dat.gua.5$feed_datetime == dat.gua.5$def_datetime # all days are diferent = seeds were not defecated in the same day

## for seeds dispersed on the same day

dat.gua.sameday <- dat.gua %>% 
  dplyr::filter(gut_transit_time < 5)

## mean and sd
sd.data <- data.frame(
  
  data = c("All", "other day", "same day"),
  
  mean = c(
    mean(dat.gua$gut_transit_time),
    mean(dat.gua.5$gut_transit_time),
    mean(dat.gua.sameday$gut_transit_time)
  ), 
  
  sd = c(
    sd(dat.gua$gut_transit_time), 
    sd(dat.gua.5$gut_transit_time),
    sd(dat.gua.sameday$gut_transit_time)
  )
)


## plots (use hms package instead of lubridate)
gua.nextday <- dat.gua %>% 
  # dplyr::filter(gut_transit_time > 5) %>% 
  
  # for hms datetime (scale_x_time)
  mutate(
    month_id = lubridate::month(feed_datetime, label = TRUE, locale="English_United States"),
    def_datetime = as_hms(def_datetime),
    feed_datetime = as_hms(feed_datetime),
    difftime = feed_datetime - def_datetime)

  # filter specific month
  # dplyr::filter(month_id == "May")

### create function to get circular hours (bases on https://rvanmazijk.github.io/circular-data/)
interval(gua.nextday$feed_datetime, gua.nextday$def_datetime)
get_month_seq <- function(grt) {
  # Creates seq from start to end, unless end <= start
  # (e.g. 16 h to 07 h = 11, 12, 1, 2)
  
  map2(def_interval, function(def_interval) {
    hour_seq <- 
    if (int_start(def_interval) > int_end(def_interval))          lubridate::int_flip(def_interval)
    else if (int_start(def_interval) < int_end(def_interval))     def_interval
    hour_seq
  })
}

grttimes <- gua.nextday %>%
  mutate(def_times = get_month_seq(feed_datetime, def_datetime))
grttimes

gua.nextday <- gua.nextday

# for POSIXct datetime (scale_x_datetime)
# mutate(feed_datetime = as.POSIXct(strptime(feed_datetime, "%Y-%m-%d %H:%M")),
# def_datetime = as.POSIXct(strptime(def_datetime, "%Y-%m-%d %H:%M")))

lims <- as.POSIXct(strptime(c("2011-01-01 13:00","2011-01-02 12:00"), format = "%Y-%m-%d %H:%M"))

### Option 1 - Dunbell plot based on: https://stackoverflow.com/questions/70816256/how-to-plot-time-interval-data-in-r-using-ggplot
gua.nextday %>%
  ggplot(
    aes(
      x = feed_datetime,
      xend = def_datetime,
      y = disp_event#species
    )
  ) +
  xlab("time (hours)") +
  scale_x_time() +

  geom_dumbbell(
    colour = "#a3c4dc",
    colour_xend = "#e68a00",
    size = 1
  ) #+
#scale_x_continuous(limits = lims)

### Option 2 - geom_segment # https://stackoverflow.com/questions/55185599/plotting-time-intervals-as-segments
###                          or https://stackoverflow.com/questions/70816256/how-to-plot-time-interval-data-in-r-using-ggplot
gua.nextday %>%
  ggplot() +
  geom_point(aes(x=feed_datetime, y=disp_event), size=1) +
    geom_segment(aes(x=feed_datetime, xend=def_datetime, y = disp_event, yend = disp_event),
               arrow=arrow(30,unit(1.25,"mm"),"last","closed")) #+
   # facet_wrap(~month_id)

