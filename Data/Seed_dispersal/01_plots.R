# Script name: 01_plots.R
# Script purpose: plot and analize Seed dispersal empirical data

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
library("tidyverse")
library("dplyr")
library("ggplot2")
library("readxl")
library("ggalt")


# Checking when seeds are dispersed next day 

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

