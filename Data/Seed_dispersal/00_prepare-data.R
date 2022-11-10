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
library("lubridate")
library("hms")
library("dplyr")
library("readxl")
library("sf")


#### Guareí ####
# I'm following Mayara seed dispersal data ("/LaP/Mayara-Dropbox/") file pattern (collum names)
# thus I won't handle Guareí data too much

# Read data

## Defecation events
gua.fec <- read_excel(here("Data", "Seed_dispersal", "Raw_Guarei_Feces-samples-Mayara_edit.xlsx"), sheet = 1)
gua.fec <- gua.fec %>%
  dplyr::select(-c(1:5)) %>% 
  mutate(datetime = lubridate::dmy_hms(datetime)) %>% 
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(c(id_feces:def_north))
# gua.fec %>% str()
gua.fec <- gua.fec %>% 
  dplyr::select(-n_seeds, -n_sp_feces) %>% # n_seeds = total number of seeds per feces in Guarei data, we want to focus on the disp_event, thus n_seed_sp 
  rename(n_seeds = n_seeds_sp)  # rename for consistency with the other datasets
gua.fec <- gua.fec %>%
  dplyr::filter(dispersal == "Yes") %>% 
  dplyr::select(disp_event, id_feces, n_seeds)
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
  dplyr::select(c(1:def_y)) %>% 
  dplyr::select(-n_seeds)
# gua.pt %>% str()


## SDD
gua.sdd <- read_excel(here("Data", "Seed_dispersal", "Raw_Guarei_SDD-Mayara-final.xlsx"), sheet = 2)
gua.sdd <- gua.sdd %>% 
  rename(disp_event = id_dispersal) %>% 
  dplyr::select(disp_event, SDD) %>% 
  mutate_if(is.character, as.factor)
  

## Gut transit time  
gua.gtt <- read_excel(here("Data", "Seed_dispersal", "Raw_Guarei_SDD-Mayara-final.xlsx"), sheet = 3)
gua.gtt <- gua.gtt %>% 
  # rename(disp_event = id_dispersal) %>% 
         
  # check if gut_transit_time is correct:
  mutate(feed_datetime = lubridate::dmy_hms(feed_datetime),
         def_datetime = lubridate::dmy_hms(def_datetime),
         month_id = lubridate::month(feed_datetime, label = TRUE, locale="English_United States")) %>%
  # mutate(gtt_test = difftime(def_datetime, feed_datetime, unit = "min")) %>% # yes, it is correct!
  mutate(gut_transit_time = hms::as_hms(gut_transit_time), 
         gtt_test = interval(feed_datetime, def_datetime) %/% minutes(1),
         gut_transit_time_h = interval(feed_datetime, def_datetime) %/% hours(1)) %>% # yes, it is correct! (with lubridate)
  dplyr::select(-gut_transit_time) %>% 
  rename(gut_transit_time = gtt_test) %>% 
  
  dplyr::select(disp_event, gut_transit_time, gut_transit_time_h, month_id) %>% 
  mutate_if(is.character, as.factor)  %>% 
  mutate(group = "Guareí")

# gua.gtt %>% str()

# Merge data and finish data prep
dat.gua <- dplyr::left_join(gua.pt, gua.fec)
dat.gua <- dplyr::left_join(dat.gua, gua.sdd)
dat.gua <- dplyr::left_join(dat.gua, gua.gtt)
dat.gua <- dat.gua %>%
  dplyr::filter(!is.na(id_feedtree)) 
  
# ### Write csv
# dat.gua %>% write.csv(here("Data", "Seed_dispersal", "Curated", "Guarei_SeedDispersal.csv"),
#                       row.names = FALSE)



#### Suzano ####
# Read data
dat.suz <- read_excel(here("Data", "Seed_dispersal", "Raw_SuzanoAnne_07d-04-2021_Seed-dispersal.xlsx"))

# Convert coordinates
## coordenadas importadas contendo duas colunas, X e Y
dat.suz.coords.feed <- dat.suz[ , c(4, 5)]
dat.suz.coords.def <- dat.suz[ , c(9, 10)]
str(dat.suz.coords.feed)
str(dat.suz.coords.def)

## atribuindo sistema de georef e transformando em special feature (sf)
dat.suz.coords.feed.sf <- sf::st_as_sf(dat.suz.coords.feed, coords = c("feed_longitude", "feed_latitude"),
                                    crs = "+proj=longlat +datum=WGS84 +no_defs")

dat.suz.coords.def.sf <- sf::st_as_sf(dat.suz.coords.def, coords = c("def_longitude", "def_latitude"),
                            crs = "+proj=longlat +datum=WGS84 +no_defs")

# plot(dat.suz.coords.feed.sf, col = "green")
# plot(dat.suz.coords.def.sf, col = "red", add = TRUE)
# st_crs(dat.suz.coords.sf)


## converting from coordinates to UTM
dat.suz.coords.feed.sf <- sf::st_transform(dat.suz.coords.feed.sf, crs = 32722) # funciona com o numero da projeção
dat.suz.coords.def.sf <- sf::st_transform(dat.suz.coords.def.sf, crs = 32722) # funciona com o numero da projeção

# plot(dat.suz.coords.sf)
# class(dat.suz.coords.sf)

dat.suz.coords.feed_UTM <- dat.suz.coords.feed.sf %>% st_coordinates() %>% as.data.frame() %>% 
  rename(feed_x = X,
         feed_y = Y)
dat.suz.coords.def_UTM <- dat.suz.coords.def.sf %>% st_coordinates() %>% as.data.frame() %>% 
  rename(def_x = X,
         def_y = Y)

dat.suz <- cbind(dat.suz.coords.feed_UTM, dat.suz.coords.def_UTM, dat.suz) %>% 
  dplyr::select(-def_longitude, -def_latitude) %>% 
  dplyr::select(-feed_longitude, -feed_latitude)

# dat.suz %>% str()

## calc gut transit time

dat.suz <- dat.suz %>% 
  # for hms datetime (scale_x_time)
  dplyr::mutate(
    month_id = lubridate::month(feed_datetime, label = TRUE, locale="English_United States"),
    # check if gut_transit_time is correct:
    # feed_datetime = lubridate::dmy_hms(feed_datetime), # data is already POSIXct
    # def_datetime = lubridate::dmy_hms(def_datetime),   # data is already POSIXct
    # gut_transit_time = difftime(def_datetime, feed_datetime, unit = "mins") # %>% # yes, it is correct!
    gut_transit_time = interval(feed_datetime, def_datetime) %/% minutes(1), # yes, it is correct! (with lubridate)
    gut_transit_time_h = interval(feed_datetime, def_datetime) %/% hours(1)
    ) %>%
  rename(n_seeds = n_seeds_sp) %>%   # rename for consistency with the other datasets
  rename(id_feedtree = id_tree_gps) %>% 
  dplyr::mutate(id_feedtree = as.character(id_feedtree)) %>% 
  # dplyr::filter(n_seeds == ">100") %>% 
  dplyr::mutate(n_seeds = as.numeric(dplyr::recode(n_seeds, ">100" = "100"))) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(group = "Suzano")

# dat.suz %>% str()

# filter specific month
# dplyr::filter(month_id == "May")


# ### Write csv
# dat.suz %>% write.csv(here("Data", "Seed_dispersal", "Curated", "Suzano_SeedDispersal.csv"),
#                       row.names = FALSE)




#### Taquara ####
# Read data
dat.taq <- read_excel(here("Data", "Seed_dispersal", "Raw_TaquaraAnne_EMZ4_Seed-dispersal.xlsx"))

# Convert coordinates
## coordenadas importadas contendo duas colunas, X e Y
dat.taq.coords.feed <- dat.taq[ , c(4, 5)]
dat.taq.coords.def <- dat.taq[ , c(8, 9)]
str(dat.taq.coords.feed)
str(dat.taq.coords.def)

## atribuindo sistema de georef e transformando em special feature (sf)
dat.taq.coords.feed.sf <- sf::st_as_sf(dat.taq.coords.feed, coords = c("feed_longitude", "feed_latitude"),
                                       crs = "+proj=longlat +datum=WGS84 +no_defs")

dat.taq.coords.def.sf <- sf::st_as_sf(dat.taq.coords.def, coords = c("def_longitude", "def_latitude"),
                                      crs = "+proj=longlat +datum=WGS84 +no_defs")

# plot(dat.taq.coords.feed.sf, col = "green")
# plot(dat.taq.coords.def.sf, col = "red", add = TRUE)
# st_crs(dat.taq.coords.sf)


## converting from coordinates to UTM
dat.taq.coords.feed.sf <- sf::st_transform(dat.taq.coords.feed.sf, crs = 32722) # funciona com o numero da projeção
dat.taq.coords.def.sf <- sf::st_transform(dat.taq.coords.def.sf, crs = 32722) # funciona com o numero da projeção

# plot(dat.taq.coords.sf)
# class(dat.taq.coords.sf)

dat.taq.coords.feed_UTM <- dat.taq.coords.feed.sf %>% st_coordinates() %>% as.data.frame() %>% 
  rename(feed_x = X,
         feed_y = Y)
dat.taq.coords.def_UTM <- dat.taq.coords.def.sf %>% st_coordinates() %>% as.data.frame() %>% 
  rename(def_x = X,
         def_y = Y)

dat.taq <- cbind(dat.taq.coords.feed_UTM, dat.taq.coords.def_UTM, dat.taq) %>% 
  dplyr::select(-def_longitude, -def_latitude) %>% 
  dplyr::select(-feed_longitude, -feed_latitude)

# dat.taq %>% str()

## calc gut transit time

dat.taq <- dat.taq %>% 
  # for hms datetime (scale_x_time)
  dplyr::mutate(
    month_id = lubridate::month(feed_datetime, label = TRUE, locale="English_United States"),
    # check if gut_transit_time is correct:
    # feed_datetime = lubridate::dmy_hms(feed_datetime), # data is already POSIXct
    # def_datetime = lubridate::dmy_hms(def_datetime),   # data is already POSIXct
    # gut_transit_time = difftime(def_datetime, feed_datetime, unit = "mins") # %>% # yes, it is correct!
    gut_transit_time = interval(feed_datetime, def_datetime) %/% minutes(1), # yes, it is correct! (with lubridate)
    gut_transit_time_h = interval(feed_datetime, def_datetime) %/% hours(1)
  ) %>%
  # rename(n_seeds = n_seeds_sp) %>%   # rename for consistency with the other datasets
  rename(id_feedtree = id_tree_gps) %>%
  dplyr::mutate(id_feedtree = as.character(id_feedtree)) %>% 
  # dplyr::filter(n_seeds == ">100") %>% 
  dplyr::mutate(n_seeds = as.numeric(dplyr::recode(n_seeds, ">100" = "100"))) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(group = "Taquara")

dat.taq %>% str()


# ### Write csv
# dat.taq %>% write.csv(here("Data", "Seed_dispersal", "Curated", "Taquara_SeedDispersal.csv"),
#                       row.names = FALSE)



#### Santa Maria ####



#### Bind all data together ####

# Check colnames
dat.gua %>% colnames()
dat.suz %>% colnames()
dat.taq %>% colnames()

# Check rownumber
sum(
  nrow(dat.gua)
  ,
  nrow(dat.suz)
  ,
  nrow(dat.taq)
)

# merge dataframes
# library(purrr)
# dat.all <- purrr::reduce(list(dat.gua, dat.suz, dat.taq), dplyr::inner_join#, by = 'disp_event' # nope
                         # )
dat.all <- dplyr::bind_rows(dat.gua, dat.suz, dat.taq)
nrow(dat.all) # stimmt!
dat.all %>% str()

# ### Write csv
# dat.all %>% write.csv(here("Data", "Seed_dispersal", "Curated", "All-areas_SeedDispersal.csv"),
#                       row.names = FALSE)

