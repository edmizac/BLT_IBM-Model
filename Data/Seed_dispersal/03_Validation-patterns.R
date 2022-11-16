# Script name: 03_Validation-patterns.R
# Script purpose: summarize, plot and analize Seed dispersal empirical data for VALIDATION.

# Date created: 2022-11-16d
# Author: Eduardo Zanette

## Notes --------------------------- 
#
#

## Packages -------------------------
library("here")
library("dplyr")
library("forcats")
library("readxl")
library("lubridate")
library("hms")
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
  
  # Order group levels and drop NA
  dplyr::filter(!is.na(id_feces)) %>% 
  droplevels() %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "Santa Maria", "Taquara")) %>% 
  mutate(id_month = forcats::fct_relevel(id_month, "Jan", "Feb", "Mar", "Apr", "May", 
                                         "Jun", "Jul", "Aug", "Sep", "Nov",  "Dec", "Dec2017", "Dec2018"))

# dat.all$id_month %>% levels()
# str(dat.all)
# str(a)


# Check data
dat.all %>% 
  # dplyr::filter(day == "2017-12-03") %>% 
  summarise_all(funs(sum(is.na(.)))) #-> extra_NA

dat.all %>% 
  str()


# General summary by month -> THESE ARE THE VALUES I WILL USE FORVALIDATION ON CHAPTER 1
round2 <- function(x) (round(x, digits = 2))

dat.all.summary.month <- dat.all %>% 
  group_by(group, id_month, disp_day) %>% 
  summarize(
    # nseeds_mean = mean((n_seeds)),
    # nseeds_sd = sd((n_seeds)),
    mean_SDD = mean(SDD),
    sd_SDD = sd(SDD),
    
    # Agreggation patterns
    
  ) %>% 
  mutate(across(where(is.numeric), round2))

## Write csv
# dat.all.summary.month %>%
#   write.csv(here("Data", "Seed_dispersal", "Curated", "Validation", "Summary_by-month.csv"),
#             row.names = FALSE)