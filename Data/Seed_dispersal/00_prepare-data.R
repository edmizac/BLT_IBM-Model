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
library("dplyr")
library("ggplot2")
library("readxl")
library("sf")

#### Guareí ####
# I'm following Mayara seed dispersal data ("/LaP/Mayara-Dropbox/") file pattern (collum names)
# thus I won't handle Guareí data too much

# Read data

## Defecation events
gua.fec <- read_excel(here("Data", "Seed_dispersal", "Raw_Guarei_Feces-samples-Mayara_edit.xlsx"))
gua.fec <- gua.fec %>%
  mutate(datetime = lubridate::dmy_hms(datetime)) %>% 
  mutate_if(is.character, as.factor) %>% 
  select(c(id_feces:def_north))
# gua.fec %>% str()
  
### Write csv
gua.fec %>% write.csv(here("Data", "Seed_dispersal", "Guarei_DefecationEvents.csv"),
                      row.names = FALSE)

## SDD
gua.sdd <- read_excel(here("Data", "Seed_dispersal", "Raw_Guarei_SDD-Mayara-final.xlsx"))
gua.sdd <- gua.sdd %>% 
  mutate(feed_datetime = lubridate::dmy_hms(feed_datetime),
         def_datetime = lubridate::dmy_hms(def_datetime)) %>% 
  mutate_if(is.character, as.factor) %>% 
  rename(disp_event = id_dispersal) %>% 
  select(c(1:def_datetime))
# gua.sdd %>% str()

### Write csv
gua.sdd %>% write.csv(here("Data", "Seed_dispersal", "Guarei_SDD.csv"),
                      row.names = FALSE)


#### Suzano ####
# Read data
dat.suz <- read.csv2(here("Data", "Seed_dispersal", "Suzano_AS_FB_2022_07_d29_Seed-dispersal.csv"),
                     dec=".")

# Convert coordinates
## coordenadas importadas contendo duas colunas, X e Y
dat.suz.coords <- dat.suz[ , c(1, 2)]
str(dat.suz.coords)

## atribuindo sistema de georef e transformando em special feature (sf)
dat.suz.coords.sf <- sf::st_as_sf(dat.suz.coords, coords = c("longitude", "latitude"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs")
# plot(dat.suz.coords.sf)
st_crs(dat.suz.coords.sf)


## converting from coordinates to UTM
dat.suz.coords.sf <- sf::st_transform(dat.suz.coords.sf, crs = 32722) # funciona com o numero da projeção
# plot(dat.suz.coords.sf)
# class(dat.suz.coords.sf)

dat.suz.coords_UTM <- dat.suz.coords.sf %>% st_coordinates() %>% as.data.frame()

dat.suz <- cbind(dat.suz.coords_UTM, dat.suz) %>% 
  select(-longitude, -latitude) %>% 
  rename(x = X,
         y = Y)



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


