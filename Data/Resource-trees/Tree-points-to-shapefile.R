library("here") 
library("tidyverse")
library("readxl")
library("sf")
library("stringr")

# Define CRS
our_crs <- "+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
our_crs <- st_crs(our_crs)
# class(our_crs)


# Grep files
csvs_to_sf <- list.files(here("Data", "Resource-trees"), pattern = ".csv")


# Convert csv files to .shp
i <- 1
for (file in csvs_to_sf) {
  
  j <- csvs_to_sf[i]
  k <- paste0(here("Data", "Resource-trees"), "/", j)
  
  csv <- readr::read_csv(k)
  
  shp <- st_as_sf(csv, coords = c("x", "y"), 
           crs = our_crs)
  
  j <- stringr::str_remove(k, ".csv")
  
  st_write(shp,
           paste0(j, ".shp"),
           driver = "ESRI Shapefile",
           delete_layer = TRUE)
  i <-  i + 1
  
}





