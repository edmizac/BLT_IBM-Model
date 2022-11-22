# Script name: 01_filter-data-siminputrow.R
# Script purpose: derive empirical values for parameterizing the model to run in 
# the nine situations assigned in BLT_groups_data_summary_aftercleaning.csv in 
# Data/Movement/Curated.

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
library("nlrx")
library("dplyr")
library("readr")
library("ggplot2")
library("ggspatial")
library("sf")

path <- here("Model_analysis", "Sensitivity-analysis",
             "v1.1_November2022", "temp")

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


# Plots
theme_set(theme_bw(base_size = 35))

simulated_shapes <- c("foraging" = 1,
                      "frugivory" = 19, 
                      "resting" = 1, 
                      "sleeping" = 17, 
                      "travel" = 1)

simulated_colors <- c("foraging" = "magenta",
                      "frugivory" = "#1E8449", 
                      "resting" = "grey", 
                      "sleeping" = "#E74C3C", 
                      "travel" = "grey")

# sf objects
load(here("Data", "06_sf-plots.RData"))





## Plot example runs -------------------------

# 1) Guareí
rseed <- db$random_seed[1]
group <- "Guareí"
breed <- "seeds"


db_filt <- db %>%
  dplyr::filter(
    study_area == group &
      breed == breed &
      random_seed == rseed
  )

# gua.p1  
gua.p1 <- gua.sf +
  geom_point(data = db_filt, aes(x = x_UTM, y = y_UTM, color = species,
                                 size = SDD
                                 # shape = disp_day
                                 ),
                                 
                                  alpha = 0.2) +
  scale_shape_manual(17) +
  ggtitle("Simulated seed dispersal coordinates")

gua.p1

# gua.p2
trees.gua <- db %>% 
  dplyr::filter(
    study_area == group &
      breed == "feeding-trees" &
      random_seed == rseed
  )

gua.sf +
  # geom_point(data = db_filt, aes(x = x_UTM, y = y_UTM, color = species,
  #                                size = SDD
  #                                # shape = disp_day
  # ),
  # 
  # alpha = 0.2) +
  # # scale_shape_manual(17) +
  # ggtitle("Simulated seed dispersal coordinates") +
  # 
  geom_point(data = trees.gua,
             aes(x = x_UTM, y = y_UTM, color = species)) +
  scale_shape_manual(values = c(17))

gua.p2

geom_point(data = sleeping_trees_gua,
           aes(x = x_UTM, y = y_UTM, color = species))



