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


# ggplot theme
theme_set(theme_bw(base_size = 15))

db <- db1


## Plot example runs -------------------------

# 1) Guareí
group <- "Guareí"
rseed <- db %>% 
  dplyr::filter(study_area == group) %>% 
  dplyr::select(random_seed) %>%
  pull(1) %>% 
  sample(size = 1)
breed_ <- "seeds"


db_filt <- db %>%
  dplyr::filter(
    study_area == group &
      breed == breed_ &
      random_seed == rseed
  )

# gua.p1  
gua.p1 <- gua.sf +
  geom_point(data = subset(db_filt, species != "Syagrus romanzoffiana"),  # they don't disperse Syagrus (this has to be corrected in the model)
             aes(x = x, y = y, color = species,
                                 size = SDD
                                 # shape = disp_day
                                 ),
                                 
                                  alpha = 0.2) +
  # scale_shape_manual(17) +
  ggtitle(paste0("Simulated seed dispersal coordinates", 
                 " (", unique(db_filt$feeding_trees_scenario), ")")
  )

gua.p1

# gua.p2
# target <- c("feeding-trees", "sleeping-trees")
trees.gua <- db %>% 
  dplyr::filter(
    study_area == group &
      breed == "feeding-trees" | breed == "sleeping-trees" &
      # breed %in% target &
      random_seed == rseed
  )
# trees.gua$breed %>% as.factor %>%  levels()

gua.sf + 
  geom_point(data = subset(db_filt, species != "Syagrus romanzoffiana"),  # they don't disperse Syagrus (this has to be corrected in the model)
             aes(x = x, y = y, color = species,
                 size = SDD
                 # shape = disp_day
             ),
             
             alpha = 0.2) +
  # scale_shape_manual(17) +
  ggtitle(paste0("Simulated seed dispersal coordinates", 
                 " (", unique(db_filt$feeding_trees_scenario), ")")
  ) +
  
  geom_point(data = trees.gua, # they don't disperse Syagrus (this has to be corrected in the model)
             aes(x = x, y = y, #group = breed,
                 # size = breed,
                 shape = breed
             ),
             
             alpha = 0.2) +
  scale_shape_manual(values = c(1, 12))
  # scale_size_manual(values = c(1, 3))
  

  # gua.p2
# 
# geom_point(data = sleeping_trees_gua,
#            aes(x = x_UTM, y = y_UTM, color = species))

# 2) Santa Maria
group <- "Santa Maria"
rseed <- db %>% 
  dplyr::filter(study_area == group) %>% 
  dplyr::select(random_seed) %>%
  pull(1) %>% 
  sample(size = 1)
breed_ <- "seeds"


db_filt <- db %>%
  dplyr::filter(
    study_area == group &
      breed == breed_ &
      random_seed == rseed
  )

# gua.p1  
sma.p1 <- sma.sf +
  geom_point(data = subset(db_filt, species != "Syagrus romanzoffiana"),  # they don't disperse Syagrus (this has to be corrected in the model)
             aes(x = x, y = y, color = species,
                 size = SDD
                 # shape = disp_day
             ),
             
             alpha = 0.2) +
  # scale_shape_manual(17) +
  ggtitle(paste0("Simulated seed dispersal coordinates", 
                 " (", unique(db_filt$feeding_trees_scenario), ")")
  )

sma.p1

# sma.p2
# target <- c("feeding-trees", "sleeping-trees")
trees.sma <- db %>% 
  dplyr::filter(
    study_area == group &
      breed == "feeding-trees" | breed == "sleeping-trees" &
      # breed %in% target &
      random_seed == rseed
  )
# trees.sma$breed %>% as.factor %>%  levels()

sma.sf + 
  geom_point(data = subset(db_filt, species != "Syagrus romanzoffiana"),  # they don't disperse Syagrus (this has to be corrected in the model)
             aes(x = x, y = y, color = species,
                 size = SDD
                 # shape = disp_day
             ),
             
             alpha = 0.2) +
  # scale_shape_manual(17) +
  ggtitle(paste0("Simulated seed dispersal coordinates", 
                 " (", unique(db_filt$feeding_trees_scenario), ")")
  ) +
  
  geom_point(data = trees.sma, # they don't disperse Syagrus (this has to be corrected in the model)
             aes(x = x, y = y, #group = breed,
                 # size = breed,
                 shape = breed
             ),
             
             alpha = 0.2) +
  scale_shape_manual(values = c(1, 12))
# scale_size_manual(values = c(1, 3))



## SDD -------------------------
rm(group)
db <- db %>% 
  rename(group= study_area,
         month = feeding_trees_scenario)
db_sdd <- db %>% 
  dplyr::filter(breed == "seeds") %>% 
  group_by(group, random_seed)
# db_sdd <- db_sdd %>% 
#   filter(SDD > 0)
db_sdd <- db_sdd %>%
  dplyr::filter(disp_day == "sameday") # next_day SDD is all 0 (check model)


## Density Option 1: by day of dispersal
# By group

db_sdd %>% ggplot(
  aes(x = SDD, fill = group, group = group)
) +
  geom_density(alpha = 0.4) +
  xlab("SDD (in meters)") +
  facet_wrap(~disp_day, nrow = 2)

# Save plot
# ggsave(paste0(path, 'SDD_disp_day_By-group_density_sim.png'), height = 3.5, width = 6)

## Boxplot
# By group
db_sdd %>%
  ggplot() +
  aes(x = group, y = SDD, fill = group) +
  geom_violin() +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~disp_day, nrow = 2) +
  ylab("SDD (m)") +
  ylim(0, 1000)

# Save plot
# ggsave(paste0(path, 'SDD_disp_day_By-group_violin.png'), height = 2.5, width = 7)


db_sdd %>% ggplot(
  aes(x = group, y = SDD, color = month)
) +
  geom_boxplot() +
  geom_point(
    position = position_jitterdodge(jitter.width = .05) # only way of dodging the points and jitter it
  ) +
  ylab("SDD (in meters)") +
  # facet_wrap(~disp_day, nrow = 2) +
  scale_color_viridis_d()

# Save plot
ggsave(paste0(path, 'SDD_disp_day_By-month_boxplot.png'), height = 2.5, width = 7)



