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
                species = sp,
                POSIXct = POSIX.ct) %>%
  dplyr::mutate(#behavior = recode(behavior,"" = "Sleeping site"),
                id_month = lubridate::month(POSIXct, label = TRUE, abbr = TRUE, 
                                     locale = Sys.setlocale("LC_TIME", "English"))) %>%  # Guareí has good timeframes (we don't need to assign days of different months to the same timefrae)
  dplyr::select(c("id_nday", "x", "y", "id_month", "POSIXct", "behavior", "species", "id")) %>% 
  dplyr::rename(id_day = id_nday)

dat.gua$id_day %>% as.factor() %>% levels()

# Check behaviors
dat.gua$behavior %>% as.factor() %>% levels()
dat.gua <- dat.gua %>%
  dplyr::mutate(behavior = recode(behavior, 
                                  "out of sight" = "Out of sight"))  


# Write csv
# dat.gua %>% write.csv(here("Data", "Movement",  "Curated", "Guarei.csv"),
#           row.names = FALSE)


#### Santa Maria ####

# Read data
dat.sma <- read_excel(here("Data", "DataSet_SMa_FB_2022_08_d18.xlsx"))
dat.sma <- dat.sma %>% 
  dplyr::mutate(group_behav = recode(group_behav,"SS" = "Sleeping site"))

# aa <- dat.sma %>% filter(!is.na(original_longitude_x)) # tem três observações sem coordenadas -> ***rever com o Felipe.

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

# * Recode timeframes (Mar 15, 18, 20, 21 should be one timeframe and 29 and 30 another one [it would be dropped as n < 2 days is not enough], but I used them as one anyways (ver "BLT_groups_data_summary.csv" e Obsidian 'Parameterization' note) *
# dat.sma$id_month <- dat.sma$id_month %>% as.factor
# dat.sma <- dat.sma %>%
#   dplyr::filter(id_day_mon != "15/03/2015") %>%
#   dplyr::filter(id_day_mon != "18/03/2015") %>%
# dat.sma$id_month <- dat.sma$id_month %>% droplevels()

dat.sma <- dat.sma %>% 
  dplyr::select(c("id_day_all", "x", "y", "id_month", "POSIXct", "behavior", "species", "id")) #%>% 
  # dplyr::filter(id_month != "May") # less than two days # timeframes (check BLT-Movement-Patterns repository) can be ignored because the field trip timeframes are not taking two months (e.g. 31/03 and 01/04)

dat.sma %>% dplyr::filter(is.na(x)) 

# Check behaviors
dat.sma$behavior %>% as.factor() %>% levels()
dat.sma <- dat.sma %>%
  dplyr::mutate(
    behavior = na_if(behavior, 'NA'),
    species = na_if(species, 'NA'),
  )

# Check species
dat.sma$species %>% as.factor() %>% levels()
dat.sma <- dat.sma %>%
  dplyr::mutate(species = recode(species, 
                                  "Myrcia splendens (Sw.) DC." = "Myrcia splendens"))  


# Write csv
# dat.sma %>% write.csv(here("Data", "Movement", "Curated","SantaMaria.csv"),
# row.names = FALSE)


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
                id_month = id_nmonth,
                behavior = `group behav`,
                species = sp) %>%
  mutate(POSIXct = lubridate::ymd_hms(POSIX.ct)) %>% 
  mutate(id_month = lubridate::month(POSIXct, label = TRUE, abbr = TRUE, 
                                     locale = Sys.setlocale("LC_TIME", "English"))) %>% 
  mutate(id = as.factor(id)) %>% 
  dplyr::select(-id_day) %>% 
  dplyr::rename(id_day = id_nday)

# dat.taq$id_day %>% as.factor() %>% levels()

# * Recode timeframes (Jan 27-31 + Feb 03rd in the same timeframe "January" and 28/02/2017 should be = "March", but it is not a full day so it is discarded) (ver "BLT_groups_data_summary.csv") *
dat.taq$id_month <- dat.taq$id_month %>% as.factor
dat.taq <- dat.taq %>%
  dplyr::mutate(id_month = recode(id_month, "Feb" = "Jan"),
                # id_month = case_when(ymd(POSIXct) ...
                id_month = case_when(id_day_all == "002" ~ "Dec_2017", # because there are two Dec months in raw data (maybe Dec 2018 has not any complete days)
                                     TRUE ~ as.character(id_month)
                )) #
dat.taq$id_month <- dat.taq$id_month %>% as.factor() %>%  droplevels()

dat.taq$id_month %>% levels()

dat.taq <- dat.taq %>% dplyr::select(c("id_day_all", "x", "y", "id_month", "POSIXct", "behavior", "species", "id")) # %>% 
  # dplyr::filter(id_month == "Jan") # only month with more than 2 days of data 

# Check behaviors
dat.taq$behavior %>% as.factor() %>% levels()
dat.taq <- dat.taq %>%
  dplyr::mutate(behavior = recode(behavior, 
                                  "Descanso" = "Resting",
                                  "Interação social" =  "Social interaction"))

# # Write csv
# dat.taq %>% write.csv(here("Data", "Movement", "Curated", "Taquara.csv"),
#                       row.names = FALSE)


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

# choosen_mo <- c("Dec", "Sep")
dat.suz <- dat.suz %>% dplyr::select(c("id_day_all", "x", "y", "id_month", "POSIXct", "behavior", "species", "id")) #%>% 
  # dplyr::filter(id_month %in% choosen_mo)
dat.suz$id_month <- dat.suz$id_month %>% droplevels()

dat.suz$id_month %>% levels()

# Check behaviors
dat.suz$behavior %>% as.factor() %>% levels()
dat.suz <- dat.suz %>%
  dplyr::mutate(behavior = recode(behavior, 
                                  "Idle" = "Inactive",
                                  "RES" =  "Resting"))

# Write csv
# dat.suz %>% write.csv(here("Data","Movement",  "Curated","Suzano.csv"),
#                       row.names = FALSE)


#### Concatenate everything: ####
dat.gua <- read.csv(here("Data", "Movement",  "Curated", "Guarei.csv"))
dat.sma <- read.csv(here("Data", "Movement",  "Curated", "SantaMaria.csv"))
dat.suz <- read.csv(here("Data", "Movement",  "Curated", "Suzano.csv"))
dat.taq <- read.csv(here("Data", "Movement",  "Curated", "Taquara.csv"))

# add id
dat.gua <- dat.gua %>% 
  mutate(group = "Guarei") %>% 
  # select(-id_day)
  rename(id_day_all = id_day)
dat.sma <- dat.sma %>% 
  mutate(group = "Santa Maria") %>% 
  # select(-id_day_all)
  mutate(id_day_all = as.character(id_day_all),
         id_day_all = paste0(id_month, "_", id_day_all)
         )

dat.taq <- dat.taq %>% 
  mutate(group = "Taquara",
  ) %>% 
  # select(-id_day_all)
  mutate(id_day_all = as.character(id_day_all),
         id_day_all = paste0(id_month, "_", id_day_all)
  )

dat.suz <- dat.suz %>% 
  mutate(group = "Suzano") %>% 
  # select(-id_day_all)
  mutate(id_day_all = as.character(id_day_all),
         id_day_all = paste0(id_month, "_", id_day_all)
  )


df_list <- list(dat.gua, dat.sma, dat.taq, dat.suz)
dat.all <- df_list %>% purrr::reduce(full_join)

dat.all <- dat.all %>% 
  mutate(behavior = recode(behavior, "Locomotion" = "Travel"))

# dat.all %>% write.csv(here("Data", "Movement", "Curated","BLT_groups_data.csv"),
#                       row.names = FALSE)

