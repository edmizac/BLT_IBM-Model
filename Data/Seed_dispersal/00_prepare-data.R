# Script name: 00_prepare-data.R
# Script purpose: make a stable version from the raw seed dispersal data without having to modify it (rename columns, etc) everytime

# Date created: 2022-11-03d
# Author: Eduardo Zanette

## Notes --------------------------- 
#
#

## Options -------------------------
# (plotting, memory limit, decimal digits)
# 
our_crs <- "+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

## Packages -------------------------
library("here")
library("tidyverse")
library("lubridate")
library("dplyr")
library("ggplot2")
library("readxl")
library("sf")

#### Guareí ####
# I'm following Mayara seed dispersal data ("/LaP/Mayara-Dropbox/") file pattern (collum names)
# thus I won't handle Guareí data too much

# Read data

## Defecation events
gua.fec <- read_excel(here("Data", "Seed_dispersal", "Raw_Guarei_Feces-samples-Mayara_edit.xlsx"), sheet = 1)
gua.fec <- gua.fec %>%
  select(-c(1:5)) %>% 
  mutate(datetime = lubridate::dmy_hms(datetime)) %>% 
  mutate_if(is.character, as.factor) %>% 
  select(c(id_feces:def_north))
# gua.fec %>% str()
gua.fec <- gua.fec %>% 
  dplyr::select(-n_seeds, -n_sp_feces) %>% # n_seeds = total number of seeds per feces in Guarei data, we want to focus on the disp_event, thus n_seed_sp 
  rename(n_seeds = n_seeds_sp)  # rename for consistency with the other datasets
gua.fec <- gua.fec %>%
  dplyr::filter(dispersal == "Yes") %>% 
  select(disp_event, id_feces, n_seeds)
### Write csv
# gua.fec %>% write.csv(here("Data", "Seed_dispersal", "Guarei_DefecationEvents.csv"),
#                       row.names = FALSE)


## Parental trees
gua.pt <- read_excel(here("Data", "Seed_dispersal", "Raw_Guarei_SDD-Mayara-final.xlsx"), sheet = 1)
gua.pt <- gua.pt %>% 
  mutate(feed_datetime = lubridate::dmy_hms(feed_datetime),
         def_datetime = lubridate::dmy_hms(def_datetime)) %>% 
  mutate_if(is.character, as.factor) %>% 
  rename(disp_event = id_dispersal) %>% 
  select(c(1:def_y)) %>% 
  select(-n_seeds)
# gua.pt %>% str()


## SDD
gua.sdd <- read_excel(here("Data", "Seed_dispersal", "Raw_Guarei_SDD-Mayara-final.xlsx"), sheet = 2)
gua.sdd <- gua.sdd %>% 
  rename(disp_event = id_dispersal) %>% 
  select(disp_event, SDD) %>% 
  mutate_if(is.character, as.factor)
  

## Gut transit time  
gua.gtt <- read_excel(here("Data", "Seed_dispersal", "Raw_Guarei_SDD-Mayara-final.xlsx"), sheet = 3)
gua.gtt <- gua.gtt %>% 
  rename(disp_event = id_dispersal) %>% 
         
  # check if gut_transit_time is correct:
  mutate(feed_datetime = lubridate::dmy_hms(feed_datetime),
         def_datetime = lubridate::dmy_hms(def_datetime)) %>%
  # mutate(gtt_test = difftime(def_datetime, feed_datetime, unit = "min")) %>% # yes, it is correct!
  mutate(gtt_test = interval(feed_datetime, def_datetime) %/% hours(1)) %>% # yes, it is correct! (with lubridate)
  select(-gut_transit_time) %>% 
  rename(gut_transit_time = gtt_test) %>% 
  
  select(disp_event, gut_transit_time) %>% 
  mutate_if(is.character, as.factor) 

# gua.gtt %>% str()

# Merge data and finish data prep
dat.gua <- dplyr::left_join(gua.pt, gua.fec)
dat.gua <- dplyr::left_join(gua.pt, gua.sdd)
dat.gua <- dplyr::left_join(dat.gua, gua.gtt)
dat.gua <- dat.gua %>%
  dplyr::filter(!is.na(id_feedtree)) 
  
### Write csv
dat.gua %>% write.csv(here("Data", "Seed_dispersal", "Curated", "Guarei_SeedDispersal.csv"),
                      row.names = FALSE)    



#### Suzano ####
# Read data
dat.suz <- read_excel(here("Data", "Seed_dispersal", "Raw_SuzanoAnne_07d-04-2021_Seed-dispersal.xlsx"))

# Convert coordinates
## coordenadas importadas contendo duas colunas, X e Y
dat.suz.coords <- dat.suz[ , c(9, 10)]
str(dat.suz.coords)

## atribuindo sistema de georef e transformando em special feature (sf)
dat.suz.coords.sf <- sf::st_as_sf(dat.suz.coords, coords = c("def_longitude", "def_latitude"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs")
# plot(dat.suz.coords.sf)
st_crs(dat.suz.coords.sf)


## converting from coordinates to UTM
dat.suz.coords.sf <- sf::st_transform(dat.suz.coords.sf, crs = 32722) # funciona com o numero da projeção
# plot(dat.suz.coords.sf)
# class(dat.suz.coords.sf)

dat.suz.coords_UTM <- dat.suz.coords.sf %>% st_coordinates() %>% as.data.frame()

dat.suz <- cbind(dat.suz.coords_UTM, dat.suz) %>% 
  select(-def_longitude, -def_latitude) %>% 
  rename(def_x = X,
         def_y = Y)


## calc gut transit time

dat.suz <- dat.suz %>% 
  dplyr::mutate(`group behav` = recode(`group behav`,"SS" = "Sleeping site"))
dat.suz <- dat.suz %>% 
  filter(full_day == 1)


## SDD
### Write csv
# dat.suz.coords.sf %>% write.csv(here("Data","Suzano_SDD.csv"),
#                       row.names = FALSE)

## Defecation events
### Write csv
# dat.suz.coords.sf %>% write.csv(here("Data","Suzano_SDD.csv"),
#                       row.names = FALSE)


#### Taquara ####


#### Santa Maria ####


