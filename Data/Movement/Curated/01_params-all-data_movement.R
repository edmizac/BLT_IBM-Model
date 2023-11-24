# Script name: 01_params-all-data.R
# Script purpose: summarize, plot and analize Movement empirical data for PARAMETERIZATION
# Derive empirical values for parameterizing the model to run GENERALLY (CHAPTER 2)
# and NOT in the nine situations assigned in BLT_groups_data_summary_aftercleaning.csv in 
# Data/Movement/Curated. For this, Go to 02_params-siminputrow.R
# Date created: 2022-11-16d
# Author: Eduardo Zanette

## Notes --------------------------- 
#
#

## Packages -------------------------
library("here")
library("dplyr")
library("ggplot2")
library("readxl")
library("lubridate")
library("hms")
library("amt")
library("sf")
library("stringr")
library("purrr")

## Options -------------------------
# (plotting, memory limit, decimal digits)
# 
theme_update(axis.title.x = element_blank())


# GIS
our_crs <- "+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
gua_xlim_all = c(780000, 782350)
gua_ylim_all = c(7407080, 7408250)
gua.x.min = 781587
gua.x.max = 782361.5
gua_xlim_set = c(781050, 782350)   # c(781200, 782350) # closer
gua_ylim_set = c(7407050, 7408250) # c(7407250, 7408250)  # closer
sma_xlim_set = c(364400, 366000)
sma_ylim_set = c(7540500, 7541700)
taq_xlim_set = c(370500, 373200)
taq_ylim_set = c(7498750, 7500550)
suz_xlim_set = c(705300, 706200)
suz_ylim_set = c(7480800, 7481600) # 7480990,

# Read data
dat.gua <- read.csv(here("Data", "Movement", "Curated", "Guarei.csv")
                    , stringsAsFactors = TRUE) %>% 
  mutate(group = "Guareí") %>% 
  rename(id_day_all = id_day)

dat.sma <- read.csv(here("Data", "Movement", "Curated", "SantaMaria.csv")
                    , stringsAsFactors = TRUE) %>% 
  mutate(group = "SantaMaria") %>% 
  mutate(id_day_all = as.factor(id_day_all))

dat.suz <- read.csv(here("Data", "Movement", "Curated", "Suzano.csv")
                    , stringsAsFactors = TRUE) %>% 
  mutate(group = "Suzano") %>% 
  mutate(id_day_all = as.factor(id_day_all))

dat.taq <- read.csv(here("Data", "Movement", "Curated", "Taquara.csv")
                    , stringsAsFactors = TRUE)  %>% 
  mutate(group = "Taquara") %>% 
  mutate(id_day_all = as.factor(id_day_all))
  
dat.all.mv <- bind_rows(dat.gua, dat.sma, dat.suz, dat.taq) %>% 
  rename(
    month = id_month,
    datetime = POSIXct
    ) %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "SantaMaria", "Taquara")) %>% 
  mutate(month = forcats::fct_relevel(month, "Dec_2017", "Jan", "Feb", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  mutate(
    datetime = ymd_hms(datetime),
    date = lubridate::date(datetime)
  )

str(dat.all.mv)
dat.all.mv$id_day_all %>% levels()
dat.all.mv$month %>% levels()






# DPL -----



# Home range -----
## Same as in Validation ("03_Validation-patterns_movement.R") #

## Per month -----
dat.all.mv.tr <- dat.all.mv %>%
  mutate(
    id = paste0(group, " - ", month)
  ) %>% 
  make_track(.x=x, .y=y, id = id, crs = 32722, all_cols = TRUE) %>% 
  nest(data = -c(id, group, month))

KDE <- dat.all.mv.tr %>%
  # hrvalues <- dat.all.mv.tr %>%
  mutate( KDE95 = amt::map(data, ~ hr_kde(., level = 0.95)), 
          KDE50 = amt::map(data, ~ hr_kde(., level = 0.50)) 
  )

# Transform KDE to sf
# KDE_sf <- amt::hr_isopleths(hrvalues) %>% sf::st_as_sf(data, crs = 32722)

KDE_sf <- KDE %>% 
  mutate(
    isop_95 = amt::map(KDE95, ~hr_isopleths(.)) %>% 
      purrr::map(., ~sf::st_as_sf(., 
                                  coords = c("x_", "y_"),
                                  crs = 32722)), #%>% 
    # purrr::map(., ~sf::st_transform(., crs = 32722)),
    isop_50 = amt::map(KDE50, ~hr_isopleths(.)) %>% 
      purrr::map(., ~sf::st_as_sf(., 
                                  coords = c("x_", "y_"),
                                  crs = 32722)), # %>% 
    # purrr::map(., ~sf::st_transform(., crs = 32722))
  ) %>% 
  dplyr::select(-data) #%>% 
# amt::unnest()

KDE_sf$isop_95[[1]] %>% st_crs()

# Add shapefiles to crop
KDE_sf <- KDE_sf %>% 
  mutate(
    shp = case_when(
      group == "Suzano" ~ "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Suzano_polygon_unishp.shp",
      group == "Guareí" ~ "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Guarei_polyg_sept2022.shp",
      group == "SantaMaria" ~ "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/SantaMaria_only_rec.shp",
      group == "Taquara" ~ "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Taquara_only2.shp",
      TRUE ~ NA
    )
  ) %>% 
  # read shp as sf
  mutate(
    shp = purrr::map(shp, ~ sf::read_sf(.)) #%>% 
    # purrr::map(., ~ sf::st_set_crs(., 32722))
  ) #%>% 
# KDE_sf$shp[[1]] %>% st_crs() #%>%   #checking crs
# set crs
# mutate(
#   shp = purrr::map(shp, ~ sf::st_set_crs(., 32722))
# )
# (no need to set crs because it is 32722 already)


# Intersecting only one line of the df:
library("tmap")
ex_shp <- KDE_sf$shp[[3]]
ex_shp %>% st_crs()
tm_shape(ex_shp) +
  tm_polygons()

ex_isop <- KDE_sf$isop_95[[3]]
ex_isop %>% st_crs()
tm_shape(ex_isop) +
  tm_polygons()

cropped_tm <- tm_shape(ex_shp) +
  tm_polygons() +
  tm_shape(ex_isop) +
  tm_polygons(alpha = 0.2)
cropped_tm

# # Save tmap as example
# tmap_save(
#   tm = cropped_tm,
#   filename = here("Data", "Movement", "Curated", "Param_all-data", "Cropped_HR_example6.png"), dpi = 300
# )

cropped <- st_intersection(ex_isop, ex_shp)
tm_shape(cropped) +
  tm_polygons()
# It works. Why it does not work inside map()?

for (i in 1:nrow(KDE_sf)) {
  ex_shp <- KDE_sf$shp[i]
  ex_isop <- KDE_sf$isop_50[i]
  ex_isop95 <- KDE_sf$isop_95[i]
  
  print(st_crs(ex_shp) == st_crs(ex_isop))
  print(st_crs(ex_shp) == st_crs(ex_isop95))
  # sf::st_intersection(ex_isop, ex_shp) # the problem is within the st_intersection() function
  # sf::st_intersection(ex_isop95, ex_shp) # the problem is within the st_intersection() function
  
  print(i)
}

# For all lines/groups:
KDE_sf <- KDE_sf %>% 
  mutate(
    cropped_95 = purrr::map(isop_95, ~sf::st_intersection(., shp)),
    cropped_50 = purrr::map(isop_50, ~sf::st_intersection(., shp))
  )
# OH MY GOSH WHY THIS DOES NOT WORK

# I will just do it manually:
KDE_sf$cropped95[[1]] <- sf::st_intersection(KDE_sf$isop_95[[1]], KDE_sf$shp[[1]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[2]] <- sf::st_intersection(KDE_sf$isop_95[[2]], KDE_sf$shp[[2]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[3]] <- sf::st_intersection(KDE_sf$isop_95[[3]], KDE_sf$shp[[3]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[4]] <- sf::st_intersection(KDE_sf$isop_95[[4]], KDE_sf$shp[[4]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[5]] <- sf::st_intersection(KDE_sf$isop_95[[5]], KDE_sf$shp[[5]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[6]] <- sf::st_intersection(KDE_sf$isop_95[[6]], KDE_sf$shp[[6]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[7]] <- sf::st_intersection(KDE_sf$isop_95[[7]], KDE_sf$shp[[7]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[8]] <- sf::st_intersection(KDE_sf$isop_95[[8]], KDE_sf$shp[[8]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[9]] <- sf::st_intersection(KDE_sf$isop_95[[9]], KDE_sf$shp[[9]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[10]] <- sf::st_intersection(KDE_sf$isop_95[[10]], KDE_sf$shp[[10]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[11]] <- sf::st_intersection(KDE_sf$isop_95[[11]], KDE_sf$shp[[11]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[12]] <- sf::st_intersection(KDE_sf$isop_95[[12]], KDE_sf$shp[[12]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[13]] <- sf::st_intersection(KDE_sf$isop_95[[13]], KDE_sf$shp[[13]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[14]] <- sf::st_intersection(KDE_sf$isop_95[[14]], KDE_sf$shp[[14]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[15]] <- sf::st_intersection(KDE_sf$isop_95[[15]], KDE_sf$shp[[15]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[16]] <- sf::st_intersection(KDE_sf$isop_95[[16]], KDE_sf$shp[[16]]) #%>% sf::st_area(.)

KDE_sf$cropped50[[1]] <- sf::st_intersection(KDE_sf$isop_50[[1]], KDE_sf$shp[[1]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[2]] <- sf::st_intersection(KDE_sf$isop_50[[2]], KDE_sf$shp[[2]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[3]] <- sf::st_intersection(KDE_sf$isop_50[[3]], KDE_sf$shp[[3]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[4]] <- sf::st_intersection(KDE_sf$isop_50[[4]], KDE_sf$shp[[4]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[5]] <- sf::st_intersection(KDE_sf$isop_50[[5]], KDE_sf$shp[[5]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[6]] <- sf::st_intersection(KDE_sf$isop_50[[6]], KDE_sf$shp[[6]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[7]] <- sf::st_intersection(KDE_sf$isop_50[[7]], KDE_sf$shp[[7]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[8]] <- sf::st_intersection(KDE_sf$isop_50[[8]], KDE_sf$shp[[8]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[9]] <- sf::st_intersection(KDE_sf$isop_50[[9]], KDE_sf$shp[[9]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[10]] <- sf::st_intersection(KDE_sf$isop_50[[10]], KDE_sf$shp[[10]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[11]] <- sf::st_intersection(KDE_sf$isop_50[[11]], KDE_sf$shp[[11]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[12]] <- sf::st_intersection(KDE_sf$isop_50[[12]], KDE_sf$shp[[12]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[13]] <- sf::st_intersection(KDE_sf$isop_50[[13]], KDE_sf$shp[[13]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[14]] <- sf::st_intersection(KDE_sf$isop_50[[14]], KDE_sf$shp[[14]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[15]] <- sf::st_intersection(KDE_sf$isop_50[[15]], KDE_sf$shp[[15]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[16]] <- sf::st_intersection(KDE_sf$isop_50[[16]], KDE_sf$shp[[16]]) #%>% sf::st_area(.)

tm_shape(KDE_sf$cropped50[[9]]) +
  tm_polygons()

# Visualize cropped plots
i <- 1
for (feature in 1:nrow(KDE_sf)) {
  tm_obj <- KDE_sf$cropped95[[i]] %>% 
    tm_shape() +
    tm_polygons()
  print(tm_obj)
  
  i <- i+1 
}

KDE_sf <- KDE_sf %>%
  # group_by(id) %>% 
  mutate(
    id.x = id %>% str_remove_all(., " ") %>% str_replace(., "-", "_"),
    pathsave95 = paste0(here("Data", "Movement", "Curated", "Param_all-data"), "/", id.x, "_cropped_KDE95.shp"),
    pathsave50 = paste0(here("Data", "Movement", "Curated", "Param_all-data"), "/", id.x, "_cropped_KDE50.shp")
  )

# Save all shapes separetly
i <- 1
for (feature in 1:nrow(KDE_sf)) {
  
  shp_95 <- KDE_sf$cropped95[[i]] %>% st_as_sf()
  shp_50 <- KDE_sf$cropped50[[i]] %>% st_as_sf()
  
  sf::st_write(shp_95, dsn = KDE_sf$pathsave95[i])
  sf::st_write(shp_50, dsn = KDE_sf$pathsave50[i])
  
  i <- i + 1
}


# Wrangle and select valuable columns
KDE_sf %>% colnames()

KDE_sf <- KDE_sf %>%
  tidyr::pivot_longer(cropped95:cropped50, names_to = 'KDE_value', values_to = 'hr')
KDE_sf <- KDE_sf %>%
  dplyr::select(-c("KDE95", "KDE50", "isop_95", "isop_50", "shp"))


# Calculate home range area from clipped sf
KDE_sf <- KDE_sf %>%
  mutate(
    hr_area = purrr::map(hr, ~ sf::st_area(.))
  ) %>% 
  dplyr::select(-hr) #%>%
# unnest(cols = hr_area)

# Transform to ha
KDE_sf <- KDE_sf %>%
  mutate(
    hr_area_ha = hr_area %>% as.numeric() / 10000
  ) %>% 
  dplyr::select(-hr_area)

# Match cropped with KDE names
KDE_sf <- KDE_sf %>%
  mutate(
    KDE_value = case_when(
      KDE_value == "cropped95" ~ "KDE95",
      KDE_value == "cropped50" ~ "KDE50",
      TRUE ~ KDE_value
    )
  ) %>% 
  dplyr::select(-c(pathsave95, pathsave50, id.x))

KDE_sf %>% str()

# # Write csv
# KDE_sf %>%
#   write.csv(here("Data", "Movement", "Curated", "Param_all-data", "Home-range_by-month.csv"),
#             row.names = FALSE)




## Per area -----
dat.all.mv.tr <- dat.all.mv %>%
  mutate(
    id = paste0(group, " - ", month)
  ) %>% 
  make_track(.x=x, .y=y, id = group, crs = 32722, all_cols = TRUE) %>% 
  nest(data = -c(group))

KDE <- dat.all.mv.tr %>%
  # hrvalues <- dat.all.mv.tr %>%
  mutate( KDE95 = amt::map(data, ~ hr_kde(., level = 0.95)), 
          KDE50 = amt::map(data, ~ hr_kde(., level = 0.50)) 
  )

# Transform KDE to sf
# KDE_sf <- amt::hr_isopleths(hrvalues) %>% sf::st_as_sf(data, crs = 32722)

KDE_sf <- KDE %>% 
  mutate(
    isop_95 = amt::map(KDE95, ~hr_isopleths(.)) %>% 
      purrr::map(., ~sf::st_as_sf(., 
                                  coords = c("x_", "y_"),
                                  crs = 32722)), #%>% 
    # purrr::map(., ~sf::st_transform(., crs = 32722)),
    isop_50 = amt::map(KDE50, ~hr_isopleths(.)) %>% 
      purrr::map(., ~sf::st_as_sf(., 
                                  coords = c("x_", "y_"),
                                  crs = 32722)), # %>% 
    # purrr::map(., ~sf::st_transform(., crs = 32722))
  ) %>% 
  dplyr::select(-data) #%>% 
# amt::unnest()

KDE_sf$isop_95[[1]] %>% st_crs()

# Add shapefiles to crop
KDE_sf <- KDE_sf %>% 
  mutate(
    shp = case_when(
      group == "Suzano" ~ "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Suzano_polygon_unishp.shp",
      group == "Guareí" ~ "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Guarei_polyg_sept2022.shp",
      group == "SantaMaria" ~ "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/SantaMaria_only_rec.shp",
      group == "Taquara" ~ "D:/Data/Documentos/Study/Mestrado/Model_Documentation/shapefiles-to-rasterize/Taquara_only2.shp",
      TRUE ~ NA
    )
  ) %>% 
  # read shp as sf
  mutate(
    shp = purrr::map(shp, ~ sf::read_sf(.)) #%>% 
    # purrr::map(., ~ sf::st_set_crs(., 32722))
  ) #%>% 
# KDE_sf$shp[[1]] %>% st_crs() #%>%   #checking crs
# set crs
# mutate(
#   shp = purrr::map(shp, ~ sf::st_set_crs(., 32722))
# )
# (no need to set crs because it is 32722 already)


# Intersecting only one line of the df:
library("tmap")
ex_shp <- KDE_sf$shp[[3]]
ex_shp %>% st_crs()
tm_shape(ex_shp) +
  tm_polygons()

ex_isop <- KDE_sf$isop_95[[3]]
ex_isop %>% st_crs()
tm_shape(ex_isop) +
  tm_polygons()

cropped_tm <- tm_shape(ex_shp) +
  tm_polygons() +
  tm_shape(ex_isop) +
  tm_polygons(alpha = 0.2)
cropped_tm

# # Save tmap as example
# tmap_save(
#   tm = cropped_tm,
#   filename = here("Data", "Movement", "Curated", "Param_all-data", "Cropped_HR_example6.png"), dpi = 300
# )

cropped <- st_intersection(ex_isop, ex_shp)
tm_shape(cropped) +
  tm_polygons()
# It works. Why it does not work inside map()?

for (i in 1:nrow(KDE_sf)) {
  ex_shp <- KDE_sf$shp[i]
  ex_isop <- KDE_sf$isop_50[i]
  ex_isop95 <- KDE_sf$isop_95[i]
  
  print(st_crs(ex_shp) == st_crs(ex_isop))
  print(st_crs(ex_shp) == st_crs(ex_isop95))
  # sf::st_intersection(ex_isop, ex_shp) # the problem is within the st_intersection() function
  # sf::st_intersection(ex_isop95, ex_shp) # the problem is within the st_intersection() function
  
  print(i)
}

# For all lines/groups:
KDE_sf <- KDE_sf %>% 
  mutate(
    cropped_95 = purrr::map(isop_95, ~sf::st_intersection(., shp)),
    cropped_50 = purrr::map(isop_50, ~sf::st_intersection(., shp))
  )
# OH MY GOSH WHY THIS DOES NOT WORK

# I will just do it manually:
KDE_sf$cropped95[[1]] <- sf::st_intersection(KDE_sf$isop_95[[1]], KDE_sf$shp[[1]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[2]] <- sf::st_intersection(KDE_sf$isop_95[[2]], KDE_sf$shp[[2]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[3]] <- sf::st_intersection(KDE_sf$isop_95[[3]], KDE_sf$shp[[3]]) #%>% sf::st_area(.)
KDE_sf$cropped95[[4]] <- sf::st_intersection(KDE_sf$isop_95[[4]], KDE_sf$shp[[4]]) #%>% sf::st_area(.)

KDE_sf$cropped50[[1]] <- sf::st_intersection(KDE_sf$isop_50[[1]], KDE_sf$shp[[1]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[2]] <- sf::st_intersection(KDE_sf$isop_50[[2]], KDE_sf$shp[[2]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[3]] <- sf::st_intersection(KDE_sf$isop_50[[3]], KDE_sf$shp[[3]]) #%>% sf::st_area(.)
KDE_sf$cropped50[[4]] <- sf::st_intersection(KDE_sf$isop_50[[4]], KDE_sf$shp[[4]]) #%>% sf::st_area(.)

tm_shape(KDE_sf$cropped50[[4]]) +
  tm_polygons()

# Visualize cropped plots
i <- 1
for (feature in 1:nrow(KDE_sf)) {
  tm_obj <- KDE_sf$cropped95[[i]] %>% 
    tm_shape() +
    tm_polygons()
  print(tm_obj)
  
  i <- i+1 
}

KDE_sf <- KDE_sf %>%
  # group_by(id) %>% 
  mutate(
    id.x = group %>% str_remove_all(., " ") %>% str_replace(., "-", "_"),
    pathsave95 = paste0(here("Data", "Movement", "Curated", "Param_all-data"), "/", id.x, "_cropped_KDE95.shp"),
    pathsave50 = paste0(here("Data", "Movement", "Curated", "Param_all-data"), "/", id.x, "_cropped_KDE50.shp")
  )

# Save all shapes separetly
i <- 1
for (feature in 1:nrow(KDE_sf)) {
  
  shp_95 <- KDE_sf$cropped95[[i]] %>% st_as_sf()
  shp_50 <- KDE_sf$cropped50[[i]] %>% st_as_sf()
  
  sf::st_write(shp_95, dsn = KDE_sf$pathsave95[i])
  sf::st_write(shp_50, dsn = KDE_sf$pathsave50[i])
  
  i <- i + 1
}


# Wrangle and select valuable columns
KDE_sf %>% colnames()

KDE_sf <- KDE_sf %>%
  tidyr::pivot_longer(cropped95:cropped50, names_to = 'KDE_value', values_to = 'hr')
KDE_sf <- KDE_sf %>%
  dplyr::select(-c("KDE95", "KDE50", "isop_95", "isop_50", "shp"))


# Calculate home range area from clipped sf
KDE_sf <- KDE_sf %>%
  mutate(
    hr_area = purrr::map(hr, ~ sf::st_area(.))
  ) %>% 
  dplyr::select(-hr) #%>%
# unnest(cols = hr_area)

# Transform to ha
KDE_sf <- KDE_sf %>%
  mutate(
    hr_area_ha = hr_area %>% as.numeric() / 10000
  ) %>% 
  dplyr::select(-hr_area)

# Match cropped with KDE names
KDE_sf <- KDE_sf %>%
  mutate(
    KDE_value = case_when(
      KDE_value == "cropped95" ~ "KDE95",
      KDE_value == "cropped50" ~ "KDE50",
      TRUE ~ KDE_value
    )
  ) %>% 
  dplyr::select(-c(pathsave95, pathsave50, id.x))

KDE_sf %>% str()

# # Write csv
# KDE_sf %>%
#   write.csv(here("Data", "Movement", "Curated", "Param_all-data", "Home-range_by-area.csv"),
#             row.names = FALSE)






