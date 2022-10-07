library("here") 
library("tidyverse")
library("readxl")
library("stringr")


###### All trees #####

#### Guareí ####
# Read data
dat.gua <- read_excel(here("Data", "Resource-trees", "DataSet_Guarei_FB_2022_07_d08.xlsx"))
dat.gua <- dat.gua %>% filter(behav %in% c("Frugivory", "Sleeping site"))

# Rename and select cols
dat.gua <- dat.gua %>% 
  dplyr::rename(x = longitude_x_GAL,
                y = latitude_y_GAL,
                behavior = behav,
                id = tree) %>%
  mutate(id_month = lubridate::month(POSIX.ct, label = TRUE, abbr = TRUE, 
                                     locale = Sys.setlocale("LC_TIME", "English"))) %>% 
  dplyr::select(c("id_nday", "x", "y", "id_month", "POSIX.ct", "behavior", "sp", "id")) %>% 
  dplyr::rename(id_day = id_nday)


# Filter by month, get unique and save to distinct .csv files:
# all months
dat.gua[!duplicated(dat.gua[ , 2:3]), ] %>% 
  write.csv(here("Data", "Resource-trees","guarei_trees_unique_all.csv"),
             row.names = FALSE)

# every month:
dat.gua$id_month <- as.factor(dat.gua$id_month) %>% droplevels()
for (i in levels(dat.gua$id_month)) {
  #i <- "august"
  df <- dat.gua %>% 
    dplyr::filter(id_month == i)
  
  filename <- paste0("guarei_trees_unique_", i, ".csv")
  
  df[!duplicated(df[, 2:3]), ] %>% 
    write.csv(paste0("D:/Data/Documentos/github/BLT_IBM-Model/Data/Resource-trees/", filename),
             row.names = FALSE)
}



#### Santa Maria ####

# Read data
dat.sma <- read_excel(here("Data", "Resource-trees", "DataSet_SMa_FB_2022_08_d17.xlsx"))
dat.sma <- dat.sma %>% 
  dplyr::mutate(group_behav = recode(group_behav,"SS" = "Sleeping site"))
dat.sma <- dat.sma %>% filter(group_behav %in% c("Frugivory", "Sleeping site"))

dat.sma <- dat.sma %>% filter(!is.na(original_longitude_x)) # tem três observações sem coordenadas -> ***rever com o Felipe.

# Rename and select cols
dat.sma <- dat.sma %>% 
  dplyr::rename(x = original_longitude_x,
                y = original_latitude_y,
                behavior = group_behav,
                month_number = id_month #,
                #id_month = id_day_mon
                ) %>%
  mutate(POSIXct = lubridate::ymd_hms(POSIXct)) %>% 
  mutate(id_month = lubridate::month(POSIXct, label = TRUE, abbr = TRUE, 
                                     locale = Sys.setlocale("LC_TIME", "English")))

# dat.sma$id_month <- stringr::str_sub(dat.sma$id_month, start = 1, end = 3)
dat.sma <- dat.sma %>% dplyr::select(c("id_day_all", "x", "y", "id_month", "POSIXct", "behavior", "sp", "id"))


# Filter by month, get unique and save to distinct .csv files:
# all months 
dat.sma[!duplicated(dat.sma[ , 2:3]), ] %>% 
  write.csv(here("Data", "Resource-trees","sma_trees_unique_all.csv"),
            row.names = FALSE)

# every month:
dat.sma$id_month <- as.factor(dat.sma$id_month) %>% droplevels()
for (i in levels(dat.sma$id_month)) {
  #i <- "august"
  df <- dat.sma %>% 
    dplyr::filter(id_month == i)
  
  filename <- paste0("sma_trees_unique_", i, ".csv")
  
  df[!duplicated(df[, 2:3]), ] %>% 
    write.csv(paste0("D:/Data/Documentos/github/BLT_IBM-Model/Data/Resource-trees/", filename),
               row.names = FALSE)
}



#### Taquara ####

# Read data
dat.taq <- read_excel(here("Data", "Resource-trees", "DataSet_PEMDTaquara_AS_FB_2022_07_d29.xlsx"))
dat.taq <- dat.taq %>% 
  dplyr::mutate(`group behav` = recode(`group behav`, "SS" = "Sleeping site"))
dat.taq <- dat.taq %>% 
  filter(full_day == 1) %>%  # dados da Anne só possuem buracos na entrada e saída do sleeping site. Como estarei
                               # olhando pro DPL, acredito não haver problema usar os dias não completos. O problema
                               # é que corrigimos x e y somente pra full_day = 1
  filter(`group behav` %in% c("Frugivory", "Sleeping site"))

# Rename and select cols
dat.taq <- dat.taq %>% 
  dplyr::rename(x = longitude_x_GAL,
                y = latitude_y_GAL,
                behavior = `group behav`) %>%
  mutate(POSIXct = lubridate::ymd_hms(POSIX.ct)) %>% 
  mutate(id_month = lubridate::month(POSIXct, label = TRUE, abbr = TRUE, 
                                     locale = Sys.setlocale("LC_TIME", "English"))) %>% 
  mutate(id = as.factor(id))

dat.taq <- dat.taq %>% dplyr::select(c("id_day_all", "x", "y", "id_month", "POSIXct", "behavior", "sp", "id"))


# Filter by month, get unique and save to distinct .csv files:
# all months 
dat.taq[!duplicated(dat.taq[ , 2:3]), ] %>% 
  write.csv(here("Data", "Resource-trees","taq_trees_unique_all.csv"),
            row.names = FALSE)

# every month:
dat.taq$id_month <- as.factor(dat.taq$id_month) %>% droplevels()
for (i in levels(dat.taq$id_month)) {
  #i <- "august"
  df <- dat.taq %>% 
    dplyr::filter(id_month == i)
  
  filename <- paste0("taq_trees_unique_", i, ".csv")
  
  df[!duplicated(df[, 2:3]), ] %>% 
    write.csv(paste0("D:/Data/Documentos/github/BLT_IBM-Model/Data/Resource-trees/", filename),
               row.names = FALSE)
}



#### Suzano ####

# Read data
dat.suz <- read_excel(here("Data", "Resource-trees", "DataSet_Suzano_AS_FB_2022_07_d29.xlsx"))
dat.suz <- dat.suz %>% 
  dplyr::mutate(`group behav` = recode(`group behav`,"SS" = "Sleeping site"))
dat.suz <- dat.suz %>% 
  filter(full_day == 1) %>%  # dados da Anne só possuem buracos na entrada e saída do sleeping site. Como estarei
  # olhando pro DPL, acredito não haver problema usar os dias não completos. O problema
  # é que corrigimos x e y somente pra full_day = 1
  filter(`group behav` %in% c("Frugivory", "Sleeping site"))

# Rename and select cols
dat.suz <- dat.suz %>% 
  dplyr::rename(x = longitude_x_GAL,
                y = latitude_y_GAL,
                behavior = "group behav") %>%
  mutate(POSIXct = lubridate::ymd_hms(POSIX.ct)) %>% 
  mutate(id_month = lubridate::month(POSIXct, label = TRUE, abbr = TRUE, 
                                     locale = Sys.setlocale("LC_TIME", "English"))) %>% 
  mutate(id = as.factor(id))

dat.suz <- dat.suz %>% dplyr::select(c("id_day_all", "x", "y", "id_month", "POSIXct", "behavior", "sp", "id"))


# Filter by month, get unique and save to distinct .csv files:
# all months 
dat.suz[!duplicated(dat.suz[ , 2:3]), ] %>% 
  write.csv(here("Data", "Resource-trees","suz_trees_unique_all.csv"),
            row.names = FALSE)

# every month:
dat.suz$id_month <- as.factor(dat.suz$id_month) %>% droplevels()
for (i in levels(dat.suz$id_month)) {
  #i <- "august"
  df <- dat.suz %>% 
    dplyr::filter(id_month == i)
  
  filename <- paste0("suz_trees_unique_", i, ".csv")
  
  df[!duplicated(df[, 2:3]), ] %>% 
    write.csv(paste0("D:/Data/Documentos/github/BLT_IBM-Model/Data/Resource-trees/", filename),
              row.names = FALSE)
}







##### Sleeping trees data #####


#### Guareí ####

# Read data
dat.gua <- read_excel(here("Data", "Resource-trees", "DataSet_Guarei_FB_2022_07_d08.xlsx"))
dat.gua <- dat.gua %>% filter(behav %in% c("Sleeping site"))

# Rename and select cols
dat.gua <- dat.gua %>% 
  dplyr::rename(x = longitude_x_GAL,
                y = latitude_y_GAL,
                behavior = behav,
                id = tree) %>%
  mutate(id_month = lubridate::month(POSIX.ct, label = TRUE, abbr = TRUE, 
                                     locale = Sys.setlocale("LC_TIME", "English"))) %>% 
  dplyr::select(c("id_nday", "x", "y", "id_month", "POSIX.ct", "behavior", "sp", "id")) %>% 
  dplyr::rename(id_day = id_nday)


# Filter by month, get unique and save to distinct .csv files:
# all months
dat.gua[!duplicated(dat.gua[ , 2:3]), ] %>% 
  write.csv(here("Data", "Resource-trees","guarei_trees_unique_slp.csv"),
            row.names = FALSE)

# Santa Maria
# Read data
dat.sma <- read_excel(here("Data", "Resource-trees", "DataSet_SMa_FB_2022_08_d17.xlsx"))
dat.sma <- dat.sma %>% 
  dplyr::mutate(group_behav = recode(group_behav,"SS" = "Sleeping site"))
dat.sma <- dat.sma %>% filter(group_behav %in% c("Sleeping site"))

dat.sma <- dat.sma %>% filter(!is.na(original_longitude_x)) # tem três observações sem coordenadas -> ***rever com o Felipe.

# Rename and select cols
dat.sma <- dat.sma %>% 
  dplyr::rename(x = original_longitude_x,
                y = original_latitude_y,
                behavior = group_behav,
                month_number = id_month #,
                #id_month = id_day_mon
  ) %>%
  mutate(POSIXct = lubridate::ymd_hms(POSIXct)) %>% 
  mutate(id_month = lubridate::month(POSIXct, label = TRUE, abbr = TRUE, 
                                     locale = Sys.setlocale("LC_TIME", "English")))

# dat.sma$id_month <- stringr::str_sub(dat.sma$id_month, start = 1, end = 3)
dat.sma <- dat.sma %>% dplyr::select(c("id_day_all", "x", "y", "id_month", "POSIXct", "behavior", "sp", "id"))


# Filter by month, get unique and save to distinct .csv files:
# all months 
dat.sma[!duplicated(dat.sma[ , 2:3]), ] %>% 
  write.csv(here("Data", "Resource-trees","sma_trees_unique_slp.csv"),
            row.names = FALSE)



#### Taquara ####

# Read data
dat.taq <- read_excel(here("Data", "Resource-trees", "DataSet_PEMDTaquara_AS_FB_2022_07_d29.xlsx"))
dat.taq <- dat.taq %>% 
  dplyr::mutate(`group behav` = recode(`group behav`, "SS" = "Sleeping site"))
dat.taq <- dat.taq %>% 
  filter(full_day == 1) %>%  # dados da Anne só possuem buracos na entrada e saída do sleeping site. Como estarei
  # olhando pro DPL, acredito não haver problema usar os dias não completos. O problema
  # é que corrigimos x e y somente pra full_day = 1
  filter(`group behav` %in% c("Sleeping site"))

# Rename and select cols
dat.taq <- dat.taq %>% 
  dplyr::rename(x = longitude_x_GAL,
                y = latitude_y_GAL,
                behavior = `group behav`) %>%
  mutate(POSIXct = lubridate::ymd_hms(POSIX.ct)) %>% 
  mutate(id_month = lubridate::month(POSIXct, label = TRUE, abbr = TRUE, 
                                     locale = Sys.setlocale("LC_TIME", "English"))) %>% 
  mutate(id = as.factor(id))

dat.taq <- dat.taq %>% dplyr::select(c("id_day_all", "x", "y", "id_month", "POSIXct", "behavior", "sp", "id"))


# Filter by month, get unique and save to distinct .csv files:
# all months 
dat.taq[!duplicated(dat.taq[ , 2:3]), ] %>% 
  write.csv(here("Data", "Resource-trees","taq_trees_unique_slp.csv"),
            row.names = FALSE)


#### Suzano ####

# Read data
dat.suz <- read_excel(here("Data", "Resource-trees", "DataSet_Suzano_AS_FB_2022_07_d29.xlsx"))
dat.suz <- dat.suz %>% 
  dplyr::mutate(`group behav` = recode(`group behav`,"SS" = "Sleeping site"))
dat.suz <- dat.suz %>% 
  filter(full_day == 1) %>%  # dados da Anne só possuem buracos na entrada e saída do sleeping site. Como estarei
  # olhando pro DPL, acredito não haver problema usar os dias não completos. O problema
  # é que corrigimos x e y somente pra full_day = 1
  filter(`group behav` %in% c("Sleeping site"))

# Rename and select cols
dat.suz <- dat.suz %>% 
  dplyr::rename(x = longitude_x_GAL,
                y = latitude_y_GAL,
                behavior = "group behav") %>%
  mutate(POSIXct = lubridate::ymd_hms(POSIX.ct)) %>% 
  mutate(id_month = lubridate::month(POSIXct, label = TRUE, abbr = TRUE, 
                                     locale = Sys.setlocale("LC_TIME", "English"))) %>% 
  mutate(id = as.factor(id))

dat.suz <- dat.suz %>% dplyr::select(c("id_day_all", "x", "y", "id_month", "POSIXct", "behavior", "sp", "id"))


# Filter by month, get unique and save to distinct .csv files:
# all months 
dat.suz[!duplicated(dat.suz[ , 2:3]), ] %>% 
  write.csv(here("Data", "Resource-trees","suz_trees_unique_slp.csv"),
            row.names = FALSE)

