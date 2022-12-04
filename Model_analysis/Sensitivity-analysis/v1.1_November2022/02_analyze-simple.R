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
# behav_simulated_shapes <- c("foraging" = 1,
#                       "frugivory" = 19, 
#                       "resting" = 1, 
#                       "sleeping" = 17, 
#                       "travel" = 1)
# 
# behav_simulated_colors <- c("foraging" = "magenta",
#                       "frugivory" = "#1E8449", 
#                       "resting" = "grey", 
#                       "sleeping" = "#E74C3C", 
#                       "travel" = "grey")

# sf objects
load(here("Data", "06_sf-plots.RData"))


# ggplot theme
theme_set(theme_bw(base_size = 15))

# db1 <- db1

db1 %>% str()

db1$month %>% unique()
db1$group %>% unique()
db1$group %>% unique()

db1 <- db1 %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "Santa Maria", "Taquara")) %>% 
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                         "Jun", "Jul", "Aug", "Sep", "Dec"))

db1_monkeys <- db1 %>% 
  dplyr::filter(breed == "monkeys") #%>% 
  # dplyr::select(-c("x":disp_day)) %>% 
  # mutate_all(~stringr::str_replace_all(., c("\\[" = "", "\\]" = "")))


# Plants (feeding, sleeping-trees and seeds)
db1 <- db1 %>%
  dplyr::filter(breed != "monkeys") %>%
  dplyr::select(-c(energy:PT)) %>%
  dplyr::select(-species.y) #%>%
  # dplyr::filter(if (breed != "seeds") species != "Syagrus romanzoffiana" else TRUE)
  
db1$species %>% as.factor() %>% levels()


## Plot example runs -------------------------

### 1) Guareí -------------------------
rseed <- db1 %>% 
  dplyr::filter(group == "Guareí") %>% 
  dplyr::select(random_seed) %>%
  pull(1) %>% 
  sample(size = 1)
breed_ <- "seeds"


db1_filt <- db1 %>%
  dplyr::filter(
    group == "Guareí" &
      breed == breed_ &
      random_seed == rseed
  )

# gua.p1  
gua.p1 <- gua.sf +
  geom_point(data = subset(db1_filt, species != "Syagrus romanzoffiana"),  # they don't disperse Syagrus (this has to be corrected in the model)
             aes(x = x, y = y, color = species,
                                 size = SDD
                                 # shape = disp_day
                                 ),
                                 
                                  alpha = 0.4) +
  # scale_shape_manual(17) +
  ggtitle(paste0("Simulated seed dispersal coordinates", 
                 " (", unique(db1_filt$month), ") - one run")
  )

gua.p1

# gua.p2
# target <- c("feeding-trees", "sleeping-trees")
trees.gua <- db1 %>% 
  dplyr::filter(
    group == group &
      breed != "seeds" &
      # breed == "feeding-trees" | breed == "sleeping-trees" &
      # breed %in% target &
      random_seed == rseed
  )
# trees.gua$breed %>% as.factor %>%  levels()
# trees.gua$species %>% as.factor %>%  levels()


gua.sf + 
  geom_point(data = subset(db1_filt, species != "Syagrus romanzoffiana"),  # they don't disperse Syagrus (this has to be corrected in the model)
             aes(x = x, y = y, color = species,
                 size = SDD
                 # shape = disp_day
             ),
             
             alpha = 0.4) +
  # scale_shape_manual(17) +
  
  ggtitle(paste0("Simulated seed dispersal coordinates", 
                 " (", unique(db1_filt$month), ") - one run")
  ) +
  
  geom_point(data = trees.gua, # they don't disperse Syagrus (this has to be corrected in the model)
             aes(x = x, y = y, #group = breed,
                 # size = breed,
                 # color = species,
                 shape = breed
             ),
             size = 2
             # alpha = 0.4
             ) +
  scale_shape_manual(values = c(16, 17)) +
  # scale_color_viridis_d(option = "inferno") +
  
  # change SDD legend point size:
  scale_size_continuous(
    limits=c(1,1000), 
    breaks=c(100,200, 300, 500, 750, 1000),
    range=c(1,10) 
  ) +
  # change legend symbol sizes:
  guides(
    color = guide_legend(override.aes = list(size = 3, alpha = 1, shape = 15) ),  # , alpha = 0.6
    shape = guide_legend(override.aes = list(size = 3, alpha = 1) )
  )
  
#### Save plot
# ggsave(paste0(path, "/",
#             'Spatial_SDD_Guareí-example.png'), height = 7, width = 10)


### 2) Santa Maria -------------------------
rseed <- db1 %>% 
  dplyr::filter(group == "Santa Maria") %>% 
  dplyr::select(random_seed) %>%
  pull(1) %>% 
  sample(size = 1)
breed_ <- "seeds"


db1_filt <- db1 %>%
  dplyr::filter(
    group == "Santa Maria" &
      breed == breed_ &
      random_seed == rseed
  )


sma.p1 <- sma.sf +
  geom_point(data = subset(db1_filt, species != "Syagrus romanzoffiana"),  # they don't disperse Syagrus (this has to be corrected in the model)
             aes(x = x, y = y, color = species,
                 size = SDD
                 # shape = disp_day
             ),
             
             alpha = 0.4) +
  # scale_shape_manual(17) +
  ggtitle(paste0("Simulated seed dispersal coordinates", 
                 " (", unique(db1_filt$month), ") - one run")
  )

sma.p1

# sma.p2
# target <- c("feeding-trees", "sleeping-trees")
trees.sma <- db1 %>% 
  dplyr::filter(
    group == "Santa Maria" &
      breed != "seeds" &
      # breed == "feeding-trees" | breed == "sleeping-trees" &
      # breed %in% target &
      random_seed == rseed
  )
# trees.gua$breed %>% as.factor %>%  levels()
# trees.gua$species %>% as.factor %>%  levels()

sma.sf + 
  geom_point(data = subset(db1_filt, species != "Syagrus romanzoffiana"),  # they don't disperse Syagrus (this has to be corrected in the model)
             aes(x = x, y = y, color = species,
                 size = SDD
                 # shape = disp_day
             ),
             
             alpha = 0.4) +
  # scale_shape_manual(17) +
  ggtitle(paste0("Simulated seed dispersal coordinates", 
                 " (", unique(db1_filt$month), ") - one run")
  ) +
  
  geom_point(data = trees.sma, # they don't disperse Syagrus (this has to be corrected in the model)
             aes(x = x, y = y, #group = breed,
                 # size = breed,
                 # color = species,
                 shape = breed
             ),
             size = 2
             # alpha = 0.4
             ) +
  scale_shape_manual(values = c(16, 17)) +
  # scale_color_viridis_d(option = "inferno") +
  
  # change SDD legend point size:
  scale_size_continuous(
    limits=c(1,1000), 
    breaks=c(100,200, 300, 500, 750, 1000),
    range=c(1,10) 
  ) +
  # change legend symbol sizes:
  guides(
    color = guide_legend(override.aes = list(size = 2, alpha = 1) ),  # , alpha = 0.6
    shape = guide_legend(override.aes = list(size = 3, alpha = 1) )
  )

#### Save plot
# ggsave(paste0(path, "/",
#             'Spatial_SDD_SantaMaria-example.png'), height = 7, width = 10)



### 3) Taquara -------------------------
rseed <- db1 %>% 
  dplyr::filter(group == "Taquara") %>%
  droplevels() %>% 
  dplyr::select(random_seed) %>%
  pull(1) %>% 
  sample(size = 1)
breed_ <- "seeds"


db1_filt <- db1 %>%
  dplyr::filter(
    group == group &
      breed == breed_ &
      random_seed == rseed
  )


taq.p1 <- taq.sf +
  geom_point(data = subset(db1_filt, species != "Syagrus romanzoffiana"),  # they don't disperse Syagrus (this has to be corrected in the model)
             aes(x = x, y = y, color = species,
                 size = SDD
                 # shape = disp_day
             ),
             
             alpha = 0.4) +
  # scale_shape_manual(17) +
  ggtitle(paste0("Simulated seed dispersal coordinates", 
                 " (", unique(db1_filt$month), ") - one run")
  )

taq.p1

# taq.p2
# target <- c("feeding-trees", "sleeping-trees")
trees.taq <- db1 %>% 
  dplyr::filter(
    group == "Taquara" &
      breed != "seeds" &
      # breed == "feeding-trees" | breed == "sleeping-trees" &
      # breed %in% target &
      random_seed == rseed
  )
# trees.gua$breed %>% as.factor %>%  levels()
# trees.gua$species %>% as.factor %>%  levels()

taq.sf + 
  geom_point(data = subset(db1_filt, species != "Syagrus romanzoffiana"),  # they don't disperse Syagrus (this has to be corrected in the model)
             aes(x = x, y = y, color = species,
                 size = SDD
                 # shape = disp_day
             ),
             
             alpha = 0.4) +
  # scale_shape_manual(17) +
  ggtitle(paste0("Simulated seed dispersal coordinates", 
                 " (", unique(db1_filt$month), ") - one run")
  ) +
  
  geom_point(data = trees.taq, # they don't disperse Syagrus (this has to be corrected in the model)
             aes(x = x, y = y, #group = breed,
                 # size = breed,
                 # color = species,
                 shape = breed
             ),
             size = 2
             # alpha = 0.4
  ) +
  scale_shape_manual(values = c(16, 17)) +
  # scale_color_viridis_d(option = "inferno") +
  
  # change SDD legend point size:
  scale_size_continuous(
    limits=c(1,1000), 
    breaks=c(100,200, 300, 500, 750, 1000),
    range=c(1,10) 
  ) +
  # change legend symbol sizes:
  guides(
    color = guide_legend(override.aes = list(size = 3, alpha = 1) ),  # , alpha = 0.6
    shape = guide_legend(override.aes = list(size = 3, alpha = 1) )
  )

#### Save plot
# ggsave(paste0(path, "/",
#             'Spatial_SDD_Taquara-example.png'), height = 7, width = 10)


### 4) Suzano -------------------------
rseed <- db1 %>% 
  dplyr::filter(group == "Suzano") %>%
  droplevels() %>% 
  dplyr::select(random_seed) %>%
  pull(1) %>% 
  sample(size = 1)
breed_ <- "seeds"


db1_filt <- db1 %>%
  dplyr::filter(
    group == group &
      breed == breed_ &
      random_seed == rseed
  )


suz.p1 <- suz.sf +
  geom_point(data = subset(db1_filt, species != "Syagrus romanzoffiana"),  # they don't disperse Syagrus (this has to be corrected in the model)
             aes(x = x, y = y, color = species,
                 size = SDD
                 # shape = disp_day
             ),
             
             alpha = 0.4) +
  # scale_shape_manual(17) +
  ggtitle(paste0("Simulated seed dispersal coordinates", 
                 " (", unique(db1_filt$month), ") - one run")
  )

suz.p1

# suz.p2
# target <- c("feeding-trees", "sleeping-trees")
trees.suz <- db1 %>% 
  dplyr::filter(
    group == "Suzano" &
      breed != "seeds" &
      # breed == "feeding-trees" | breed == "sleeping-trees" &
      # breed %in% target &
      random_seed == rseed
  )
# trees.gua$breed %>% as.factor %>%  levels()
# trees.gua$species %>% as.factor %>%  levels()

suz.sf + 
  geom_point(data = subset(db1_filt, species != "Syagrus romanzoffiana"),  # they don't disperse Syagrus (this has to be corrected in the model)
             aes(x = x, y = y, color = species,
                 size = SDD
                 # shape = disp_day
             ),
             
             alpha = 0.4) +
  # scale_shape_manual(17) +
  ggtitle(paste0("Simulated seed dispersal coordinates", 
                 " (", unique(db1_filt$month), ") - one run")
  ) +
  
  geom_point(data = trees.suz, # they don't disperse Syagrus (this has to be corrected in the model)
             aes(x = x, y = y, #group = breed,
                 # size = breed,
                 # color = species,
                 shape = breed
             ),
             size = 2
             # alpha = 0.4
  ) +
  scale_shape_manual(values = c(16, 17)) +
  # scale_color_viridis_d(option = "inferno") +
  
  # change SDD legend point size:
  scale_size_continuous(
    limits=c(1,1000), 
    breaks=c(100,200, 300, 500, 750, 1000),
    range=c(1,10) 
  ) +
  # change legend symbol sizes:
  guides(
    color = guide_legend(override.aes = list(size = 3, alpha = 1) ),  # , alpha = 0.6
    shape = guide_legend(override.aes = list(size = 3, alpha = 1) )
  )

#### Save plot
# ggsave(paste0(path, "/",
#             'Spatial_SDD_Suzano-example.png'), height = 7, width = 10)


### 5) Facet -------------------------

# gridExtra::grid.arrange()



## SDD -------------------------
rm(group)
# db1 <- db1 %>%
#   rename(group= group,
#          month = month)
db1_sdd <- db1 %>% 
  dplyr::filter(breed == "seeds") %>% 
  group_by(group, random_seed)
# db1_sdd <- db1_sdd %>% 
#   filter(SDD > 0)
# db1_sdd <- db1_sdd %>%
#   dplyr::filter(disp_day == "sameday") # next_day SDD is all 0 (check model)


## Density Option 1: by day of dispersal
# By group

db1_sdd %>% ggplot(
  aes(x = SDD, fill = group, group = group)
) +
  geom_density(alpha = 0.4) +
  xlab("SDD (in meters)") +
  facet_wrap(~disp_day, nrow = 2)

# Save plot
# ggsave(paste0(path, "/", 'SDD_disp_day_density.png'), height = 3.5, width = 6)

## Boxplot
# By group
db1_sdd %>%
  ggplot() +
  aes(x = group, y = SDD, fill = group) +
  geom_violin() +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~disp_day, nrow = 2) +
  ylab("SDD (m)") +
  ylim(0, 1500)

# Save plot
# ggsave(paste0(path, "/", 'SDD_disp_day_violin.png'), height = 3.5, width = 7)


db1_sdd %>% ggplot(
  aes(x = group, y = SDD, color = month)
) +
  geom_boxplot() +
  geom_point(
    position = position_jitterdodge(jitter.width = .05) # only way of dodging the points and jitter it
  ) +
  ylab("SDD (in meters)") +
  ylim(0, 1500) +
  facet_wrap(~disp_day, nrow = 2) +
  scale_color_viridis_d()

# Save plot
# ggsave(paste0(path, "/", 'SDD_disp_day_boxplot.png'), height = 3.5, width = 7)


  facet_wrap(~disp_day, nrow = 2) +
  scale_color_viridis_d()

# Save plot
# ggsave(paste0(path, "/", 'SDD_disp_day_grid-boxplot.png'), height = 3.5, width = 7)




## DPL -------------------------
db1_mv <- db1_monkeys %>% 
dplyr::filter(breed == "monkeys") %>% 
  group_by(group, random_seed)


db1_mv_DPL <- db1_mv_DPL %>% 
  rename(DPL = DPL_d) %>%
  mutate(DPL = 10 * DPL)

db1_mv_DPL %>% 
  ggplot(aes(x = group, y = DPL, color = month)) +
  geom_boxplot() +
  guides(fill=FALSE) +
  ylab("DPL (m)") +
  ylim(c(0,5000)) +
  scale_colour_viridis_d()

# Save plot
# ggsave(paste0(path, 'DPL_By-month_boxplot.png'), height = 5, width = 7)




## Home range -------------------------
db1_mv <- readRDS(paste0(path, "/", "02_Simoutput-simple_monkeys.rds"))
db1_mv %>% str()


db1_mv <- db1 %>% 
  dplyr::filter(breed == "monkeys") %>% 
  group_by(group, random_seed) #%>% 
  # dplyr::select(-"DPL_d")
  


a <- db1_mv$KDE_values %>% str_replace_all(., c("\\[" = "", "\\]" = ""))#(pattern = "\\[", simplify = TRUE)
a <- a %>% str_split(pattern = "_") #, simplify = TRUE)
# a <- a %>% str_split(pattern = "_", simplify = TRUE)

a <- a %>% map(as.numeric)


db1_mv$KDE_values <- a
# teste <- db1_mv %>% tidyr::nest(data="KDE_values") %>% unnest("data")



db1_mv %>% group_nest() %>% 
  unnest_longer(data)

db1_mv_HR  <- db1_mv %>% unnest_legacy()

KDE_levels <- rep(c("KDE95", "KDE50", "drop"), nrow(db1_mv))
db1_mv_HR$KDE_levels  <- KDE_levels 

db1_mv_HR %>% str()

db1_mv_HR$KDE_levels  <-  as.factor(db1_mv_HR$KDE_levels)


db1_mv_HR <- db1_mv_HR %>% 
  filter(KDE_levels != 'drop') %>% 
  pivot_wider(names_from = "KDE_levels", values_from = "KDE_values",
              values_fn = "list")

# line 1 is still nested
# db1_mv_HR <- db1_mv_HR %>% 
#   dplyr::filter(row_number()==1) %>% 
#   as_tibble()
db1_mv_HR <- db1_mv_HR[-1, ]


db1_mv_HR %>% str()

# Divide HR by 10000 (ha)
db1_mv_HR <- db1_mv_HR %>% 
  # unlist as numeric
  mutate(
    KDE95 = unlist(KDE95),
    KDE50 = unlist(KDE50)
  ) %>% 
  
  mutate(
    KDE95 = KDE95 / 100000,
    KDE50 = KDE50 / 100000
    )
  
  db1_mv_HR$KDE50 %>% str()

# KDE 95
db1_mv_HR %>% 
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
db1_mv_HR %>% 
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
