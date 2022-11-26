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
library("purrr")

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
group <- "Taquara" # "Guareí"
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
group <- "Taquara"#; "Santa Maria"
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




  ylim(0, 1000) +
  facet_wrap(~disp_day, nrow = 2) +
  scale_color_viridis_d()

# Save plot
# ggsave(paste0(path, 'SDD_disp_day_By-month_boxplot.png'), height = 2.5, width = 7)




## DPL -------------------------
  db <- db1
db_mv <- db %>% 
dplyr::filter(breed == "monkeys") %>% 
  # group_by(group, random_seed)
  group_by(study_area, `random-seed`)

a <- db_mv$DPL_d %>% str_replace_all(., c("\\[" = "", "\\]" = ""))#(pattern = "\\[", simplify = TRUE)
a <- a %>% str_split(pattern = "_") #, simplify = TRUE)
# a <- a %>% str_split(pattern = "_", simplify = TRUE)

a %>% str()
a <- a %>% map(as.numeric)
a <- a %>% map(round, 2)
a <- a %>% map_dbl(mean, na.rm=TRUE)

# a %>% str()

db_mv$DPL_mean <- a #%>% round(., 2)


# db_mv %>% str()



# db_mv %>% group_nest() %>% 
#   unnest_longer(data)
# 
# db_mv_DPL  <- db_mv %>% unnest_legacy()

# db_mv %>% saveRDS(paste0(path, "/", "Camila.RDS"))
# readRDS("Camila.RDS")


# aa %>% map_int(as.numeric)
# a %>% map_dbl(as.numeric)
# 
# a %>% str()
# 
# c <- a
# a <- a %>% as.numeric()
# length(a)
# db_mv$DPL_d %>% length()  
# 
# b <- a %>% str_remove(., "[[:digit:]].,")
# a <- a %>% str_split(pattern = "[[:digit:]].,", simplify = TRUE)
# 
# 
# c <- lapply( c, function(x) trimws( unlist ( strsplit( x, ",") ) ) )
# 
# db_mv$c <- c
# 
# purrr::map_int()
# 
# 
# db_mv$DPL_d <- db_mv$DPL_d %>% str_replace_all(., c("\\[" = "", "\\]" = ""))#(pattern = "\\[", simplify = TRUE)
# db_mv$DPL_d <- db_mv$DPL_d %>% str_split(pattern = "_") #, simplify = TRUE)
# # db_mv$DPL_d <- db_mv$DPL_d%>% map(as.numeric)
# 
# db_mv$DPL_d
# 
# teste <- db_mv
# # teste$DPL_d <- lapply(db_mv$DPL_d, function(x) trimws( unlist ( strsplit( x, ",") ) ) )
# teste$DPL_d <- as.numeric(str_extract_all(teste$DPL_d, "[0-9.]+")[[1]])
#   #map(as.numeric)
# teste$DPL_d
# 
# 
# teste$DPL_d %>% map(~ str_extract_all(., "[0-9.]+")[[1]]) %>% 
#   as.numeric()
# 
# 
# 
# library(purrr)
# library(repurrrsive)
# 
# got_chars[1:3]
# 
# tibble::tibble(
#   # name = map_chr(db_mv$DPL_d, "name"),
#   id = map_dbl(db_mv, "DPL_d")
# )
# got_chars[[5]]["id"]

db_mv_DPL <- db_mv_DPL %>% 
  rename(DPL = DPL_d) %>%
  mutate(DPL = 10 * DPL)

db_mv_DPL %>% 
  ggplot(aes(x = group, y = DPL, color = month)) +
  geom_boxplot() +
  guides(fill=FALSE) +
  ylab("DPL (m)") +
  ylim(c(0,5000)) +
  scale_colour_viridis_d()

# Save plot
# ggsave(paste0(path, 'DPL_By-month_boxplot.png'), height = 5, width = 7)




## Home range -------------------------
db_mv <- db %>% 
  dplyr::filter(breed == "monkeys") %>% 
  group_by(group, random_seed) %>% 
  dplyr::select(-"DPL_d")
  


a <- db_mv$KDE_values %>% str_replace_all(., c("\\[" = "", "\\]" = ""))#(pattern = "\\[", simplify = TRUE)
a <- a %>% str_split(pattern = "_") #, simplify = TRUE)
# a <- a %>% str_split(pattern = "_", simplify = TRUE)

a <- a %>% map(as.numeric)


db_mv$KDE_values <- a
# teste <- db_mv %>% tidyr::nest(data="KDE_values") %>% unnest("data")



db_mv %>% group_nest() %>% 
  unnest_longer(data)

db_mv_HR  <- db_mv %>% unnest_legacy()

KDE_levels <- rep(c("KDE95", "KDE50", "drop"), nrow(db_mv))
db_mv_HR$KDE_levels  <- KDE_levels 

db_mv_HR %>% str()

db_mv_HR$KDE_levels  <-  as.factor(db_mv_HR$KDE_levels)


db_mv_HR <- db_mv_HR %>% 
  filter(KDE_levels != 'drop') %>% 
  pivot_wider(names_from = "KDE_levels", values_from = "KDE_values",
              values_fn = "list")

# line 1 is still nested
# db_mv_HR <- db_mv_HR %>% 
#   dplyr::filter(row_number()==1) %>% 
#   as_tibble()
db_mv_HR <- db_mv_HR[-1, ]


db_mv_HR %>% str()

# Divide HR by 10000 (ha)
db_mv_HR <- db_mv_HR %>% 
  # unlist as numeric
  mutate(
    KDE95 = unlist(KDE95),
    KDE50 = unlist(KDE50)
  ) %>% 
  
  mutate(
    KDE95 = KDE95 / 100000,
    KDE50 = KDE50 / 100000
    )
  
  db_mv_HR$KDE50 %>% str()

# KDE 95
db_mv_HR %>% 
  ggplot(aes(x = group, y = KDE95, color = month)) +
  geom_boxplot() +
  guides(fill=FALSE) +
  ylim(0, 350) +
  ylab("Area (ha)") +
  ggtitle("KDE 95% (Home range)") +
  scale_colour_viridis_d()
# geom_jitter(width = 0.15) +
# annotate("text", x=2, y=5000, label= paste0("Mean ± sd = ", round(avg_dist, 2), " ± ", round(sd_dist, 2)))

# ggsave(filename = here("Model_analysis", "Workflow", "Run_travelspeedvar",
#                        "HomeRangeKDE95-speedval.png"),
#        dpi = 300, width = 30, height = 20, units = "cm")

# KDE 50
db_mv_HR %>% 
  ggplot(aes(x = group, y = KDE50, color = month)) +
  geom_boxplot() +
  guides(fill=FALSE) +
  ylim(0, 350) +
  ylab("Area (ha)") +
  ggtitle("KDE 50% (Core area)") +
  scale_colour_viridis_d()
# geom_jitter(width = 0.15) +
# annotate("text", x=2, y=5000, label= paste0("Mean ± sd = ", round(avg_dist, 2), " ± ", round(sd_dist, 2)))

# ggsave(filename = here("Model_analysis", "Workflow", "Run_travelspeedvar",
#                        "HomeRangeKDE95-speedval.png"),
#        dpi = 300, width = 30, height = 20, units = "cm")
