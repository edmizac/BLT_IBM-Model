# Script name: CurateData.R
# Script purpose: make a stable version from the raw data without having to modify it (rename columns, etc) everytime

# Date created: 2022-10-14d
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

#### Guareí ####
# Read data
dat.gua <- read_excel(here("Data", "DataSet_Guarei_FB_2022_07_d08.xlsx"))

# Rename and select cols
dat.gua <- dat.gua %>% 
  dplyr::rename(x = longitude_x_GAL,
                y = latitude_y_GAL,
                behavior = behav,
                id = tree,
                species = sp) %>%
  mutate(id_month = lubridate::month(POSIX.ct, label = TRUE, abbr = TRUE, 
                                     locale = Sys.setlocale("LC_TIME", "English"))) %>% 
  dplyr::select(c("id_nday", "x", "y", "id_month", "POSIX.ct", "behavior", "species", "id")) %>% 
  dplyr::rename(id_day = id_nday)

# Write csv
dat.gua %>% write.csv(here("Data", "Curated","Guarei.csv"),
          row.names = FALSE)


#### Santa Maria ####

# Read data
dat.sma <- read_excel(here("Data", "DataSet_SMa_FB_2022_08_d17.xlsx"))
dat.sma <- dat.sma %>% 
  dplyr::mutate(group_behav = recode(group_behav,"SS" = "Sleeping site"))

aa <- dat.sma %>% filter(!is.na(original_longitude_x)) # tem três observações sem coordenadas -> ***rever com o Felipe.

# Rename and select cols
dat.sma <- dat.sma %>% 
  dplyr::rename(x = original_longitude_x,
                y = original_latitude_y,
                behavior = group_behav,
                month_number = id_month,
                species = sp #,
                #id_month = id_day_mon
  ) %>%
  mutate(POSIXct = lubridate::ymd_hms(POSIXct)) %>% 
  mutate(id_month = lubridate::month(POSIXct, label = TRUE, abbr = TRUE, 
                                     locale = Sys.setlocale("LC_TIME", "English")))

dat.sma <- dat.sma %>% 
  dplyr::select(c("id_day_all", "x", "y", "id_month", "POSIXct", "behavior", "species", "id"))

# Write csv
dat.sma %>% write.csv(here("Data", "Curated","SantaMaria.csv"),
                      row.names = FALSE)


#### Taquara ####

# Read data
dat.taq <- read_excel(here("Data", "DataSet_PEMDTaquara_AS_FB_2022_07_d29.xlsx"))
dat.taq <- dat.taq %>% 
  dplyr::mutate(`group behav` = recode(`group behav`, "SS" = "Sleeping site"))
dat.taq <- dat.taq %>% 
  filter(full_day == 1) # %>%  # dados da Anne só possuem buracos na entrada e saída do sleeping site. Como estarei
  # olhando pro DPL, acredito não haver problema usar os dias não completos. O problema
  # é que corrigimos x e y somente pra full_day = 1

# Rename and select cols
dat.taq <- dat.taq %>% 
  dplyr::rename(x = longitude_x_GAL,
                y = latitude_y_GAL,
                behavior = `group behav`,
                species = sp) %>%
  mutate(POSIXct = lubridate::ymd_hms(POSIX.ct)) %>% 
  mutate(id_month = lubridate::month(POSIXct, label = TRUE, abbr = TRUE, 
                                     locale = Sys.setlocale("LC_TIME", "English"))) %>% 
  mutate(id = as.factor(id))

dat.taq <- dat.taq %>% dplyr::select(c("id_day_all", "x", "y", "id_month", "POSIXct", "behavior", "species", "id"))


# Write csv
dat.taq %>% write.csv(here("Data", "Curated", "Taquara.csv"),
                      row.names = FALSE)

#### Suzano ####

# Read data
dat.suz <- read_excel(here("Data", "DataSet_Suzano_AS_FB_2022_07_d29.xlsx"))
dat.suz <- dat.suz %>% 
  dplyr::mutate(`group behav` = recode(`group behav`,"SS" = "Sleeping site"))
dat.suz <- dat.suz %>% 
  filter(full_day == 1) #%>%  # dados da Anne só possuem buracos na entrada e saída do sleeping site. Como estarei
  # olhando pro DPL, acredito não haver problema usar os dias não completos. O problema
  # é que corrigimos x e y somente pra full_day = 1

# Rename and select cols
dat.suz <- dat.suz %>% 
  dplyr::rename(x = longitude_x_GAL,
                y = latitude_y_GAL,
                behavior = "group behav",
                species = sp) %>%
  mutate(POSIXct = lubridate::ymd_hms(POSIX.ct)) %>% 
  mutate(id_month = lubridate::month(POSIXct, label = TRUE, abbr = TRUE, 
                                     locale = Sys.setlocale("LC_TIME", "English"))) %>% 
  mutate(id = as.factor(id))

dat.suz <- dat.suz %>% dplyr::select(c("id_day_all", "x", "y", "id_month", "POSIXct", "behavior", "species", "id"))

# Write csv
dat.suz %>% write.csv(here("Data", "Curated","Suzano.csv"),
                      row.names = FALSE)
