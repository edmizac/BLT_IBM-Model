# Script name: 02_analyze-simple.R
# Script purpose: create validation plots with calibrated parameters 
# to compare with v1.1 results (dissertation plots)

# Date created: 2023-04-23d
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
library("ggridges")
library("ggspatial")
library("sf")
library("purrr")

path <- here("Model_analysis", "Sensitivity-analysis",
             "v1.2_2023Jan", "Param_calibrated", "temp")


# ggplot theme
theme_set(theme_bw(base_size = 15))
theme_update(
  axis.text.x = element_text(size = 11)
)

# 1) Only if spatial data was outputted (for spatial plots):  -----

# 
# # GIS
# our_crs <- "+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# gua_xlim_all = c(780000, 782350)
# gua_ylim_all = c(7407080, 7408250)
# gua.x.min = 781587
# gua.x.max = 782361.5
# gua_xlim_set = c(781050, 782350)   # c(781200, 782350) # closer
# gua_ylim_set = c(7407050, 7408250) # c(7407250, 7408250)  # closer
# sma_xlim_set = c(364400, 366000)
# sma_ylim_set = c(7540500, 7541700)
# taq_xlim_set = c(370500, 373200)
# taq_ylim_set = c(7498750, 7500550)
# suz_xlim_set = c(705300, 706200)
# suz_ylim_set = c(7480800, 7481600) # 7480990,
# 
# 
# # Plots
# # behav_simulated_shapes <- c("foraging" = 1,
# #                       "frugivory" = 19, 
# #                       "resting" = 1, 
# #                       "sleeping" = 17, 
# #                       "travel" = 1)
# # 
# # behav_simulated_colors <- c("foraging" = "magenta",
# #                       "frugivory" = "#1E8449", 
# #                       "resting" = "grey", 
# #                       "sleeping" = "#E74C3C", 
# #                       "travel" = "grey")
# 
# # sf objects
# load(here("Data", "06_sf-plots.RData"))
# 
# 
# # ggplot theme
# theme_set(theme_bw(base_size = 15,
#                    theme(axis.text.x = element_text(size = 11))))
# 
# # db1 <- db1
# 
# 
# db1 <- readRDS(paste0(path, "/", "02_Simoutput-simple.rds"))
# # db1 <- read.csv(paste0(path, "/", "02_Simoutput-simple.csv"))
# # db_monkeys <- readRDS(paste0(path, "/", "02_Simoutput-simple_monkeys_long.rds"))
# # db_plants <- readRDS(paste0(path, "/", "02_Simoutput-simple_plants.rds"))
# 
# # db1 %>% str()
# 
# db1$month %>% unique()
# db1$group %>% unique()
# # db_monkeys$month %>% unique()
# # db_monkeys$group %>% unique()
# 
# db1 <- db1 %>% 
#   mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "Santa Maria", "Taquara")) %>% 
#   mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
#                                          "Jun", "Jul", "Aug", "Sep", "Dec"))
# # # 
# # db_monkeys <- db_monkeys %>%
# #   mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "Santa Maria", "Taquara")) %>%
# #   mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May",
# #                                       "Jun", "Jul", "Aug", "Sep", "Dec"))
# 
# 
# # db1_monkeys <- db1 %>%
# #   dplyr::filter(breed == "monkeys") #%>%
# #   dplyr::select(-c("x":disp_day)) %>%
# #   mutate_all(~stringr::str_replace_all(., c("\\[" = "", "\\]" = "")))
# 
# 
# # Plants (feeding, sleeping-trees and seeds)
# # db1 <- db1 %>%
# #   dplyr::filter(breed != "monkeys") %>%
# #   dplyr::select(-c(energy:PT)) %>%
# #   dplyr::select(-species.y) #%>%
# #   # dplyr::filter(if (breed != "seeds") species != "Syagrus romanzoffiana" else TRUE)
#   
# db1$species %>% as.factor() %>% levels()
# 
# 
# # define boxplot width
# bpw <- 4/length(unique(paste(db1$group, db1$month)))
# bpw # 0.44444
# 
# ## Plot example runs -------------------------
# 
# ### 1) Guareí -------------------------
# rseed <- db1 %>% 
#   dplyr::filter(group == "Guareí") %>% 
#   dplyr::select(random_seed) %>%
#   pull(1) %>% 
#   sample(size = 1)
# breed_ <- "seeds"
# 
# 
# db1_filt <- db1 %>%
#   dplyr::filter(
#     group == "Guareí" &
#       breed == breed_ &
#       random_seed == rseed
#   )
# 
# # gua.p1  
# gua.p1 <- gua.sf +
#   geom_point(data = subset(db1_filt, species != "Syagrus romanzoffiana"),  # they don't disperse Syagrus (this has to be corrected in the model)
#              aes(x = x, y = y, color = species,
#                                  size = SDD
#                                  # shape = disp_day
#                                  ),
#                                  
#                                   alpha = 0.4) +
#   # scale_shape_manual(17) +
#   ggtitle(paste0("Simulated seed dispersal coordinates", 
#                  " (", unique(db1_filt$month), ") - one run")
#   )
# 
# gua.p1
# 
# # gua.p2
# # target <- c("feeding-trees", "sleeping-trees")
# trees.gua <- db1 %>% 
#   dplyr::filter(
#     group == group &
#       breed != "seeds" &
#       # breed == "feeding-trees" | breed == "sleeping-trees" &
#       # breed %in% target &
#       random_seed == rseed
#   )
# # trees.gua$breed %>% as.factor %>%  levels()
# # trees.gua$species %>% as.factor %>%  levels()
# 
# 
# gua.sf + 
#   geom_point(data = subset(db1_filt, species != "Syagrus romanzoffiana"),  # they don't disperse Syagrus (this has to be corrected in the model)
#              aes(x = x, y = y, color = species,
#                  size = SDD
#                  # shape = disp_day
#              ),
#              
#              alpha = 0.4) +
#   # scale_shape_manual(17) +
#   
#   ggtitle(paste0("Simulated seed dispersal coordinates", 
#                  " (", unique(db1_filt$month), ") - one run")
#   ) +
#   
#   geom_point(data = trees.gua, # they don't disperse Syagrus (this has to be corrected in the model)
#              aes(x = x, y = y, #group = breed,
#                  # size = breed,
#                  # color = species,
#                  shape = breed
#              ),
#              size = 2
#              # alpha = 0.4
#              ) +
#   scale_shape_manual(values = c(16, 17)) +
#   # scale_color_viridis_d(option = "inferno") +
#   
#   # change SDD legend point size:
#   scale_size_continuous(
#     limits=c(1,1000), 
#     breaks=c(100,200, 300, 500, 750, 1000),
#     range=c(1,10) 
#   ) +
#   # change legend symbol sizes:
#   guides(
#     color = guide_legend(override.aes = list(size = 3, alpha = 1, shape = 15) ),  # , alpha = 0.6
#     shape = guide_legend(override.aes = list(size = 3, alpha = 1) )
#   )
#   
# #### Save plot
# # ggsave(paste0(path, "/",
# #             '02_simple_Spatial_SDD_Guareí-example.png'), height = 7, width = 10)
# 
# 
# ### 2) Santa Maria -------------------------
# rseed <- db1 %>% 
#   dplyr::filter(group == "Santa Maria") %>% 
#   dplyr::select(random_seed) %>%
#   pull(1) %>% 
#   sample(size = 1)
# breed_ <- "seeds"
# 
# 
# db1_filt <- db1 %>%
#   dplyr::filter(
#     group == "Santa Maria" &
#       breed == breed_ &
#       random_seed == rseed
#   )
# 
# 
# sma.p1 <- sma.sf +
#   geom_point(data = subset(db1_filt, species != "Syagrus romanzoffiana"),  # they don't disperse Syagrus (this has to be corrected in the model)
#              aes(x = x, y = y, color = species,
#                  size = SDD
#                  # shape = disp_day
#              ),
#              
#              alpha = 0.4) +
#   # scale_shape_manual(17) +
#   ggtitle(paste0("Simulated seed dispersal coordinates", 
#                  " (", unique(db1_filt$month), ") - one run")
#   )
# 
# sma.p1
# 
# # sma.p2
# # target <- c("feeding-trees", "sleeping-trees")
# trees.sma <- db1 %>% 
#   dplyr::filter(
#     group == "Santa Maria" &
#       breed != "seeds" &
#       # breed == "feeding-trees" | breed == "sleeping-trees" &
#       # breed %in% target &
#       random_seed == rseed
#   )
# # trees.gua$breed %>% as.factor %>%  levels()
# # trees.gua$species %>% as.factor %>%  levels()
# 
# sma.sf + 
#   geom_point(data = subset(db1_filt, species != "Syagrus romanzoffiana"),  # they don't disperse Syagrus (this has to be corrected in the model)
#              aes(x = x, y = y, color = species,
#                  size = SDD
#                  # shape = disp_day
#              ),
#              
#              alpha = 0.4) +
#   # scale_shape_manual(17) +
#   ggtitle(paste0("Simulated seed dispersal coordinates", 
#                  " (", unique(db1_filt$month), ") - one run")
#   ) +
#   
#   geom_point(data = trees.sma, # they don't disperse Syagrus (this has to be corrected in the model)
#              aes(x = x, y = y, #group = breed,
#                  # size = breed,
#                  # color = species,
#                  shape = breed
#              ),
#              size = 2
#              # alpha = 0.4
#              ) +
#   scale_shape_manual(values = c(16, 17)) +
#   # scale_color_viridis_d(option = "inferno") +
#   
#   # change SDD legend point size:
#   scale_size_continuous(
#     limits=c(1,1000), 
#     breaks=c(100,200, 300, 500, 750, 1000),
#     range=c(1,10) 
#   ) +
#   # change legend symbol sizes:
#   guides(
#     color = guide_legend(override.aes = list(size = 2, alpha = 1) ),  # , alpha = 0.6
#     shape = guide_legend(override.aes = list(size = 3, alpha = 1) )
#   )
# 
# #### Save plot
# # ggsave(paste0(path, "/",
# #             '02_simple_Spatial_SDD_SantaMaria-example.png'), height = 7, width = 10)
# 
# 
# 
# ### 3) Taquara -------------------------
# rseed <- db1 %>% 
#   dplyr::filter(group == "Taquara") %>%
#   droplevels() %>% 
#   dplyr::select(random_seed) %>%
#   pull(1) %>% 
#   sample(size = 1)
# breed_ <- "seeds"
# 
# 
# db1_filt <- db1 %>%
#   dplyr::filter(
#     group == group &
#       breed == breed_ &
#       random_seed == rseed
#   )
# 
# 
# taq.p1 <- taq.sf +
#   geom_point(data = subset(db1_filt, species != "Syagrus romanzoffiana"),  # they don't disperse Syagrus (this has to be corrected in the model)
#              aes(x = x, y = y, color = species,
#                  size = SDD
#                  # shape = disp_day
#              ),
#              
#              alpha = 0.4) +
#   # scale_shape_manual(17) +
#   ggtitle(paste0("Simulated seed dispersal coordinates", 
#                  " (", unique(db1_filt$month), ") - one run")
#   )
# 
# taq.p1
# 
# # taq.p2
# # target <- c("feeding-trees", "sleeping-trees")
# trees.taq <- db1 %>% 
#   dplyr::filter(
#     group == "Taquara" &
#       breed != "seeds" &
#       # breed == "feeding-trees" | breed == "sleeping-trees" &
#       # breed %in% target &
#       random_seed == rseed
#   )
# # trees.gua$breed %>% as.factor %>%  levels()
# # trees.gua$species %>% as.factor %>%  levels()
# 
# taq.sf + 
#   geom_point(data = subset(db1_filt, species != "Syagrus romanzoffiana"),  # they don't disperse Syagrus (this has to be corrected in the model)
#              aes(x = x, y = y, color = species,
#                  size = SDD
#                  # shape = disp_day
#              ),
#              
#              alpha = 0.4) +
#   # scale_shape_manual(17) +
#   ggtitle(paste0("Simulated seed dispersal coordinates", 
#                  " (", unique(db1_filt$month), ") - one run")
#   ) +
#   
#   geom_point(data = trees.taq, # they don't disperse Syagrus (this has to be corrected in the model)
#              aes(x = x, y = y, #group = breed,
#                  # size = breed,
#                  # color = species,
#                  shape = breed
#              ),
#              size = 2
#              # alpha = 0.4
#   ) +
#   scale_shape_manual(values = c(16, 17)) +
#   # scale_color_viridis_d(option = "inferno") +
#   
#   # change SDD legend point size:
#   scale_size_continuous(
#     limits=c(1,1000), 
#     breaks=c(100,200, 300, 500, 750, 1000),
#     range=c(1,10) 
#   ) +
#   # change legend symbol sizes:
#   guides(
#     color = guide_legend(override.aes = list(size = 3, alpha = 1) ),  # , alpha = 0.6
#     shape = guide_legend(override.aes = list(size = 3, alpha = 1) )
#   )
# 
# #### Save plot
# # ggsave(paste0(path, "/",
# #             '02_simple_Spatial_SDD_Taquara-example.png'), height = 7, width = 10)
# 
# 
# ### 4) Suzano -------------------------
# rseed <- db1 %>% 
#   dplyr::filter(group == "Suzano") %>%
#   droplevels() %>% 
#   dplyr::select(random_seed) %>%
#   pull(1) %>% 
#   sample(size = 1)
# breed_ <- "seeds"
# 
# 
# db1_filt <- db1 %>%
#   dplyr::filter(
#     group == group &
#       breed == breed_ &
#       random_seed == rseed
#   )
# 
# 
# suz.p1 <- suz.sf +
#   geom_point(data = subset(db1_filt, species != "Syagrus romanzoffiana"),  # they don't disperse Syagrus (this has to be corrected in the model)
#              aes(x = x, y = y, color = species,
#                  size = SDD
#                  # shape = disp_day
#              ),
#              
#              alpha = 0.4) +
#   # scale_shape_manual(17) +
#   ggtitle(paste0("Simulated seed dispersal coordinates", 
#                  " (", unique(db1_filt$month), ") - one run")
#   )
# 
# suz.p1
# 
# # suz.p2
# # target <- c("feeding-trees", "sleeping-trees")
# trees.suz <- db1 %>% 
#   dplyr::filter(
#     group == "Suzano" &
#       breed != "seeds" &
#       # breed == "feeding-trees" | breed == "sleeping-trees" &
#       # breed %in% target &
#       random_seed == rseed
#   )
# # trees.gua$breed %>% as.factor %>%  levels()
# # trees.gua$species %>% as.factor %>%  levels()
# 
# suz.sf + 
#   geom_point(data = subset(db1_filt, species != "Syagrus romanzoffiana"),  # they don't disperse Syagrus (this has to be corrected in the model)
#              aes(x = x, y = y, color = species,
#                  size = SDD
#                  # shape = disp_day
#              ),
#              
#              alpha = 0.4) +
#   # scale_shape_manual(17) +
#   ggtitle(paste0("Simulated seed dispersal coordinates", 
#                  " (", unique(db1_filt$month), ") - one run")
#   ) +
#   
#   geom_point(data = trees.suz, # they don't disperse Syagrus (this has to be corrected in the model)
#              aes(x = x, y = y, #group = breed,
#                  # size = breed,
#                  # color = species,
#                  shape = breed
#              ),
#              size = 2
#              # alpha = 0.4
#   ) +
#   scale_shape_manual(values = c(16, 17)) +
#   # scale_color_viridis_d(option = "inferno") +
#   
#   # change SDD legend point size:
#   scale_size_continuous(
#     limits=c(1,1000), 
#     breaks=c(100,200, 300, 500, 750, 1000),
#     range=c(1,10) 
#   ) +
#   # change legend symbol sizes:
#   guides(
#     color = guide_legend(override.aes = list(size = 3, alpha = 1) ),  # , alpha = 0.6
#     shape = guide_legend(override.aes = list(size = 3, alpha = 1) )
#   )
# 
# #### Save plot
# # ggsave(paste0(path, "/",
# #             '02_simple_Spatial_SDD_Suzano-example.png'), height = 7, width = 10)
# 
# 
# ### 5) Facet -------------------------
# 
# # gridExtra::grid.arrange()
# 













# 2) Validation plots only:  -----

## SDD -------------------------
rm(group)

# Load empirical data
obs <- read.csv(here("Data", "Seed_dispersal", "Curated", "Validation", "Siminputrow_SDD.csv"),
                    sep = ",", stringsAsFactors = TRUE)  %>%  
  # mutate(group = recode(group, "Guarei" = "Guareí")) #%>%  # to match all other datasets
  mutate(source = as.factor("observed")) %>% 
  rename(month = id_month) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  mutate_if(is.character, as.factor)

# Simulated data
db_sd <- read.csv(paste0(path, "/", "02_Simoutput-simple-all.csv"))
db_sd %>% str()

db_sd <- db_sd %>% 
  # rename(group = study_area) %>% 
  dplyr::filter(breed == "seeds") %>%
  mutate_if(is.character, as.factor) %>% 
  group_by(group, random_seed) %>% 
  mutate(source = "simulated") %>% 
  mutate(day = as.factor(day),
         disp_day = recode(disp_day,
                        "sameday" = "same day",
                        "nextday" = "next day"
                        ) %>% as.factor()
  ) %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "SantaMaria", "Taquara")) %>% 
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec")) 

db_sd$disp_day %>% str()
db_sd$source %>% str()
db_sd$group %>% levels()
db_sd$month %>% levels()
# db1_sdd <- db1_sdd %>% 
#   filter(SDD > 0)
# db1_sdd <- db1_sdd %>%
#   dplyr::filter(disp_day == "sameday") # next_day SDD is all 0 (check model)

# Merge obserded data into db1_sd
db_sd <- db_sd %>% dplyr::bind_rows(obs)

db_sd <- db_sd %>% 
  mutate(source = as.factor(source)) %>% 
  mutate(group = recode(group,
    "Santa Maria" = "SantaMaria"
  )) %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "SantaMaria", "Taquara")) %>% 
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
mutate_if(is.character, as.factor)

db_sd$disp_day %>% str()
db_sd$disp_day %>% levels()
db_sd$group %>% levels()
length(db_sd$disp_day %>% is.na(.)) # no NAs

db_sd$source %>% str()
db_sd$source %>% levels()
length(db_sd$source %>% is.na(.)) # no NAs

db_sd$group %>% str()
db_sd$group %>% levels()
length(db_sd$group %>% is.na(.)) # no NAs

# db_sd$group %>% str()
## Density Option 1: by day of dispersal
# By group

db_sd <- db_sd %>% 
  dplyr::filter(!is.na(SDD)) %>% 
  dplyr::filter(!is.na(disp_day))

db_sd %>% str()


### Plots ------

#### By group ####
# density
db_sd %>% 
  # Santa Maria April is missing from observed data (not enough observations), so we drop the simulations
  dplyr::filter(
    group != "SantaMaria" | month != "Apr"
  ) %>%
  droplevels() %>% 
  ggplot(
  aes(x = SDD, fill = group, group = group)
) +
  geom_density(
    alpha = 0.4
    ) +
  xlab("SDD (in meters)") +
  # facet_grid(. ~ disp_day, rows = 2)
  # facet_wrap(vars(source, disp_day), nrow = 2) +
  facet_grid(disp_day ~ source) +
  
  # others
  theme(axis.text = element_text(size = 9))

# Save plot
# ggsave(paste0(path, "/", '02_simple_SDD_disp_day_density.png'), height = 5, width = 7)


# boxplot
db_sd %>%
  # Santa Maria April is missing from observed data (not enough observations), so we drop the simulations
  dplyr::filter(
    group != "SantaMaria" | month != "Apr"
  ) %>%
  # nrow() 
  # droplevels() %>% 
  ggplot() +
  aes(x = group, y = SDD, fill = group) +
  geom_violin() +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~disp_day, nrow = 2) +
  ylab("SDD (m)") +
  ylim(0, 1500) +
  # facet_wrap(vars(disp_day, source), nrow = 2) +
  facet_grid(disp_day ~ source) +
  
  # others
  theme(axis.text = element_text(size = 9))
# geom_point(position = position_jitterdodge(jitter.width = 0.7)) 


# Save plot
# ggsave(paste0(path, "/", '02_simple_SDD_disp_day_violin.png'), height = 5, width = 7)



#### By group and month ####

# density
db_sd %>% 
  # Santa Maria April is missing from observed data (not enough observations), so we drop the simulations
  dplyr::filter(
    group != "SantaMaria" | month != "Apr"
  ) %>%
  dplyr::filter(disp_day == "same day") %>% 
  ggplot(
    aes(x = SDD, y = group, fill = month) #, height = ..density..)
  ) +
  geom_density_ridges( #alpha = 0.4,
    # adjust = 5,
    # position = "stack"
    # stat = "identity",
    # trim = TRUE,
    scale = 1.2, # heigth
    jittered_points = TRUE,
    point_shape = "|", point_size = 2,
    position = position_points_jitter(width = 0.1, height = 0)
  ) +
  xlab("SDD (in meters)") +
  # facet_grid(source ~ disp_day) +
  facet_grid(rows = vars(source)) +
  scale_fill_viridis_d() +
  ggtitle("Distance of seeds dispersed on the same day")

# # Save plot
# ggsave(paste0(path, "/", '02_simple_SDD_same-day_density_ridges.png'), height = 5, width = 7)


db_sd %>% 
  # Santa Maria April is missing from observed data (not enough observations), so we drop the simulations
  dplyr::filter(
    group != "SantaMaria" | month != "Apr"
  ) %>%
  dplyr::filter(disp_day == "next day") %>% 
  ggplot(
    aes(x = SDD, y = group, fill = month) #, height = ..density..)
  ) +
  geom_density_ridges( #alpha = 0.4,
    # adjust = 5,
    # position = "stack"
    # stat = "identity",
    # trim = TRUE,
    scale = 1.2, # heigth
    jittered_points = TRUE,
    point_shape = "|", point_size = 2,
    position = position_points_jitter(width = 0.1, height = 0)
  ) +
  xlab("SDD (in meters)") +
  # facet_grid(source ~ disp_day) +
  facet_grid(rows = vars(source)) +
  scale_fill_viridis_d() +
  ggtitle("Distance of seeds dispersed on the next day")

# Save plot
# ggsave(paste0(path, "/", '02_simple_SDD_next-day_density_ridges.png'), height = 5, width = 7)


db_sd %>% 
  # Santa Maria April is missing from observed data (not enough observations), so we drop the simulations
  dplyr::filter(
    group != "SantaMaria" | month != "Apr"
  ) %>%
  ggplot(
    aes(x = SDD, y = month, fill = month) #, height = ..density..)
  ) +
  geom_density_ridges( #alpha = 0.4,
    # adjust = 5,
    # position = "stack"
    # stat = "identity",
    # trim = TRUE,
    scale = 1.5, # heigth
    jittered_points = TRUE,
    point_shape = "|", point_size = 2,
    position = position_points_jitter(width = 0.5, height = 0)
  ) +
  xlab("SDD (in meters)") +
  # facet_grid(source ~ disp_day) +
  facet_grid(source ~ disp_day) +
  scale_fill_viridis_d()

# # Save plot
# ggsave(paste0(path, "/", '02_simple_SDD_disp_day_density_ridges1.png'), height = 5, width = 14)


db_sd %>% 
  # Santa Maria April is missing from observed data (not enough observations), so we drop the simulations
  dplyr::filter(
    group != "SantaMaria" | month != "Apr"
  ) %>%
  ggplot(
    aes(x = SDD, y = month, fill = month) #, height = ..density..)
  ) +
  geom_density_ridges( #alpha = 0.4,
    # adjust = 5,
    # position = "stack"
    # stat = "identity",
    # trim = TRUE,
    scale = 1.5, # heigth
    jittered_points = TRUE,
    point_shape = "|", point_size = 2,
    position = position_points_jitter(width = 0.5, height = 0)
  ) +
  xlab("SDD (in meters)") +
  # facet_grid(source ~ disp_day) +
  facet_grid(~source) +
  scale_fill_viridis_d() +
  ggtitle("Seed dispersal distance of all events (same and next day)")

# # Save plot
# ggsave(paste0(path, "/", '02_simple_SDD_disp_day_density_ridges2.png'), height = 5, width = 7)


# boxplot
db_sd %>% 
  # Santa Maria April is missing from observed data (not enough observations), so we drop the simulations
  dplyr::filter(
    group != "SantaMaria" | month != "Apr"
  ) %>%
  ggplot(
  aes(x = group, y = SDD, color = month)
) +
  geom_boxplot() +
  geom_point(
    size = 0.5,
    position = position_jitterdodge(jitter.width = .65) # only way of dodging the points and jitter it
  ) +
  
  # geom_boxplot(position=position_dodge2(preserve = "single"),
  #              aes(group = month)) + # to preserve SantaMaria April missing
  # # geom_point(
  # #   position = position_jitterdodge(jitter.width = .05) # only way of dodging the points and jitter it
  # #   # position = position_dodge2(0.1, preserve = "total")
  # # ) +
  # geom_point(position = position_jitterdodge(jitter.width = .6),
  #            # size = 3, 
  #            aes(group = month)
  #            ) +
  # ylab("SDD (in meters)") +
  ylim(0, 1000) +
  facet_wrap(~disp_day, nrow = 2) +
  # scale_color_viridis_d() +
  # facet_wrap(vars(disp_day, source), nrow = 2) +
  
  # others
  theme(axis.text = element_text(size = 9)) +
# geom_point(position = position_jitterdodge(jitter.width = 0.7)) 


# Save plot
# ggsave(paste0(path, "/", '02_simple_SDD_disp_day_boxplot.png'), height = 5, width = 7)


  # facet_grid(cols = vars(disp_day), rows = vars(source)) +
  facet_grid(cols = vars(source), rows = vars(disp_day)) +
  scale_color_viridis_d() +
  ylab("SDD (m)")

# # Save plot
# ggsave(paste0(path, "/", '02_simple_SDD_disp_day_grid-boxplot.png'), height = 5, width = 7)


  
  
  
## Movement patterns -------------------------
theme_update(
      axis.title.x = element_blank()
  )

# define boxplot width
bpw <- 6/length(unique(paste(db1$group, db1$month)))
bpw # 0.44444
  
  
# db1_mv <- readRDS(paste0(path, "/", "02_Simoutput-simple_monkeys_long.rds")) %>% 
db1_mv <- read.csv(paste0(path, "/", "02_Simoutput-simple_monkeys.csv")) %>% 
    # mutate(group = recode(group, "Guarei" = "Guareí")) #%>%  # to match all other datasets
    ungroup() %>% 
    as.data.frame()
  
db1_mv %>% str()

db1_mv  <-  db1_mv %>%
  dplyr::select(-c(
    # "DPL", 
    "SDD"))

  
# Wrangle data:
  
db1_mv <- db1_mv %>% 
  
# dplyr::filter(breed == "monkeys") %>% 
  rename_with(~ str_remove(.x, "g_"), starts_with("g_")
  ) %>%
  rename(
         KDE95 = KDE_95,
         KDE50 = KDE_50,
         # DPL = g_DPL,
         # p_feeding = g_p_feeding,
         # p_feeding = g_p_feeding,
         # 
         # 
         ) #%>%
  mutate(date = as.factor(date))


# load DPL empirical data
obs.dpl <- read.csv(here("Data", "Movement", "Curated", "Validation", 
                         "Siminputrow_DPL_by-day.csv")
                    , stringsAsFactors = TRUE
)  %>% 
  # mutate(group = recode(group, "Guarei" = "Guareí")) %>% # to match all other datasets
  mutate(source = as.factor("observed")) %>% 
  rename(month = id_month) #%>% 
  # as_tibble()

# load HR empirical data
obs.hr <- read.csv(here("Data", "Movement", "Curated", "Validation", 
                         "Siminputrow_Home-range_by-month.csv")
                    , stringsAsFactors = TRUE
)  %>% 
  # mutate(group = recode(group, "Guarei" = "Guareí")) %>% # to match all other datasets
  mutate(source = as.factor("observed")) %>% 
  rename(month = id_month)

obs.hr <- obs.hr %>% 
  dplyr::select(group, month, source, KDE_value, area) %>% 
  tidyr::pivot_wider(
    names_from = KDE_value,
    values_from = area
  ) %>% 
  as.data.frame()

obs <- obs.dpl %>% dplyr::left_join(obs.hr, by = c("group"="group", "month"= "month",
                                                     "source"="source"
                                                     # "KDE95" = "KDE95",
                                                     # "KDE50" = "KDE50"
))

# Merge obserded data into db1_mv
db1_mv <- dplyr::bind_rows(db1_mv, obs)

db1_mv$group %>% levels()
db1_mv %>% str()

db1_mv <- db1_mv %>% 
  mutate(group = recode(group,
                        "Santa Maria" = "SantaMaria"
  ))

db1_mv$group %>% as.factor() %>% levels()

db1_mv <- db1_mv %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "SantaMaria", "Taquara")) %>% 
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  mutate(
    KDE95 = KDE95 / 10000,
    KDE50 = KDE50 / 10000
  )
                  


# obs.hr$group
# db1_mv$group
# 
# obs.hr %>% str()
# db1_mv %>% str()
# obs.dpl %>% str()
# 
# db1_mv %>% str()

# db1_mv %>% str()
# obs.dpl %>% str()


### DPL -------------------------

# Check data:
# d <- db1_mv %>% dplyr::filter(source == "observed")
# d <- a %>%  dplyr::filter(random_seed == "-1996108983") # por algum motivo essa simullação tem 16 dias em vez de 8 

# # Option 1 (can't identify simulated from observed ones)
# db1_mv %>% 
#   ggplot(aes(x = group, y = DPL, group = paste(source, month), color = month)) +
#   # ggplot(aes(x = group, y = DPL, fill = source, color = month)) +
#   geom_boxplot() +
#   guides(scale="none") +
#   ylab("DPL (m)") +
#   ylim(c(0,5000)) +
#   scale_colour_viridis_d() +
#   scale_fill_discrete(guide = "none")


# # Option2  (oly way of identifyin simulated from observed ones)
db1_mv %>% 
  ggplot(aes(x = group, y = DPL, 
             color = month,
             # fill = month
             ), shape = source) +
  # geom_boxplot(position = position_dodge(preserve = "single")) +
  geom_boxplot() +
  guides(scale="none") +
  ylab("DPL (m)") +
  ylim(c(0,5000)) +
  scale_colour_viridis_d() +
  # scale_fill_viridis_d() +
  facet_grid(~source) +
  
  # others
  theme(axis.text = element_text(size = 9))
# geom_point(position = position_jitterdodge(jitter.width = 0.7)) 


# db1_mv %>%
#   ggplot(aes(x = group, y = DPL, color = month)) +
#   geom_boxplot() +
#   guides(scale="none") +
#   ylab("DPL (m)") +
#   ylim(c(0,5000)) +
#   scale_colour_viridis_d()

# # Save plot
# ggsave(paste0(path,  "/", '02_simple_DPL_By-month_boxplot.png'), height = 3.5, width = 7)





  



### Home range -------------------------

# KDE 95 log
db1_mv %>% 
  ggplot(aes(x = group, y = log10(KDE95), color = month)) +
  # geom_boxplot(position = position_dodge(preserve = "single")) +
  geom_boxplot() +
  guides(fill=FALSE) +
  ylim(0, 2.5) +
  ylab("log 10(Area in ha)") +
  ggtitle("KDE 95% (monthly area used)") +
  scale_colour_viridis_d() +
  facet_grid(~source) +
  
  # others
  theme(axis.text = element_text(size = 9))
# geom_point(position = position_jitterdodge(jitter.width = 0.7)) 

# geom_jitter(width = 0.15) +
# annotate("text", x=2, y=5000, label= paste0("Mean ± sd = ", round(avg_dist, 2), " ± ", round(sd_dist, 2)))

# Save plot
ggsave(paste0(path,  "/", '02_simple_HR_KDE95_boxplot_log10.png'), height = 3.5, width = 7)

db1_mv %>% 
  ggplot(aes(x = group, y = KDE95, color = month)) +
  # geom_boxplot(position = position_dodge(preserve = "single")) +
  geom_boxplot() +
  guides(fill=FALSE) +
  # ylim(0, 350) +
  ylab("Area (in ha)") +
  ggtitle("KDE 95% (monthly area used") +
  scale_colour_viridis_d() +
  facet_grid(~source) +
  
  # others
  theme(axis.text = element_text(size = 9))
# geom_point(position = position_jitterdodge(jitter.width = 0.7)) 

# geom_jitter(width = 0.15) +
# annotate("text", x=2, y=5000, label= paste0("Mean ± sd = ", round(avg_dist, 2), " ± ", round(sd_dist, 2)))

# Save plot
ggsave(paste0(path,  "/", '02_simple_HR_KDE95_boxplot.png'), height = 3.5, width = 7)

# ggsave(filename = here("Model_analysis", "Workflow", "Run_travelspeedvar",
#                        "HomeRangeKDE95-speedval.png"),
#        dpi = 300, width = 30, height = 20, units = "cm")

# KDE 50 log
db1_mv %>% 
  ggplot(aes(x = group, y = log10(KDE50), color = month)) +
  # geom_boxplot(position = position_dodge(preserve = "single")) +
  geom_boxplot() +
  ylim(0, 2.5) +
  guides(fill=FALSE) +
  ylab("Area (ha)") +
  ggtitle("log10(Area in ha)") +
  scale_colour_viridis_d() +
  facet_grid(~source) +
  
  # others
  theme(axis.text = element_text(size = 9))# +
  # geom_point(position = position_jitterdodge(jitter.width = 0.7)) 

# geom_jitter(width = 0.15)
# annotate("text", x=2, y=5000, label= paste0("Mean ± sd = ", round(avg_dist, 2), " ± ", round(sd_dist, 2)))

# Save plot
ggsave(paste0(path,  "/", '02_simple_HR_KDE50_boxplot_log.png'), height = 3.5, width = 7)


# KDE 50
db1_mv %>% 
  ggplot(aes(x = group, y = KDE50, color = month)) +
  # geom_boxplot(position = position_dodge(preserve = "single")) +
  geom_boxplot() +
  guides(fill=FALSE) +
  # ylim(0, 350) +
  ylim(0, 100) +
  ylab("Area (ha)") +
  ggtitle("KDE 50% (Core area)") +
  scale_colour_viridis_d() +
  facet_grid(~source) +
  
  # others
  theme(axis.text = element_text(size = 9))# +
# geom_point(position = position_jitterdodge(jitter.width = 0.7)) 

# geom_jitter(width = 0.15)
# annotate("text", x=2, y=5000, label= paste0("Mean ± sd = ", round(avg_dist, 2), " ± ", round(sd_dist, 2)))

# Save plot
ggsave(paste0(path,  "/", '02_simple_HR_KDE50_boxplot.png'), height = 3.5, width = 7)

# ggsave(filename = here("Model_analysis", "Workflow", "Run_travelspeedvar",
#                        "HomeRangeKDE95-speedval.png"),
#        dpi = 300, width = 30, height = 20, units = "cm")



### Activity budget -------------------------

# load activity budget empirical data
obs.ab <- read.csv(here("Data", "Movement", "Curated", "Validation", 
                         "Siminputrow_Activity-budget_By-month.csv")
                    , stringsAsFactors = TRUE
)  %>% 
  # mutate(group = recode(group, "Guarei" = "Guareí")) %>% # to match all other datasets
  mutate(source = as.factor("observed")) %>% 
  rename(month = id_month) %>%
  as.data.frame() %>%
  mutate(group = recode(group,
                        "Santa Maria" = "SantaMaria"
  ))
# as_tibble()

obs.ab$group %>% levels()

obs.ab$perc_behavior_mean %>% str()
obs.ab %>% str()

foo <- function(x) ( x = x / 100 )
obs.ab <- obs.ab %>% 
  dplyr::select(-perc_behavior_sd) %>%  # we are interested in mean values (perc_behavior_mean)
  tidyr::pivot_wider(names_from = behavior,
                     values_from = perc_behavior_mean,
                     values_fill = 0 # only for Guareí in the month they didn't rest (0% resting)
                    ) %>% 
  rename(
    p_feeding = Frugivory,
    p_foraging = Foraging,
    p_traveling = Travel,
    p_resting = Resting
  ) %>% 
  mutate_if(is.numeric, foo) %>% 
  as.data.frame()

db1_mv %>% str()
# a %>% str()
# a$p_traveling %>% hist()
# a$p_traveling %>% tail()
obs.ab %>% str()

# Merge obserded data into db1_mv
db1_mv <- db1_mv %>%  dplyr::left_join(obs.ab, by = c("group", "month", "source")) %>% #,  by = c("group"="group", "month"= "month",
                                             # "source"="source")) #, # PORQUE DIABOS N FUNCIOMA
                                             # "p_feeding" = "p_feeding",
                                             # "p_foraging" = "p_foraging",
                                             # "p_traveling" = "p_traveling",
                                             # "p_resting" = "p_resting"
                                             # ))
  tidyr::unite(p_feeding.x, p_feeding.y, col = "p_feeding", remove = TRUE, na.rm = TRUE) %>% 
  tidyr::unite(p_foraging.x, p_foraging.y, col = "p_foraging", remove = TRUE, na.rm = TRUE) %>% 
  tidyr::unite(p_traveling.x, p_traveling.y, col = "p_traveling", remove = TRUE, na.rm = TRUE) %>% 
  tidyr::unite(p_resting.x, p_resting.y, col = "p_resting", remove = TRUE, na.rm = TRUE) #%>%

# a$p_feeding %>% tail()
# a$p_foraging %>% tail()
# a$p_feeding %>% as.numeric() %>%  tail()


db1_mv <- db1_mv %>%  
  mutate(
    p_feeding = as.numeric(p_feeding),
    p_foraging = as.numeric(p_foraging),
    p_traveling = as.numeric(p_traveling),
    p_resting = as.numeric(p_resting),
  )
  # mutate(across(starts_with("p_"), as.numeric(na.rm = TRUE)))

db1_mv %>% select(starts_with("p_")) %>% tail()


db1_mv <- db1_mv %>% 
  # mutate(group = recode(group,
  #                       "Santa Maria" = "SantaMaria"
  # )) %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "SantaMaria", "Taquara")) %>% 
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec"))

db1_mv$group %>% levels()


db1_mv_longer <- db1_mv %>% 
  tidyr::pivot_longer(cols = starts_with("p_"), names_to = "behavior", values_to = "percentage_mean") %>% 
  dplyr::filter(behavior != "p_visited_trees" & behavior!= "p_forage_param."
  )
  # group_by(group, month, source)

db1_mv_longer <- db1_mv_longer %>% 
  mutate(percentage_mean = stringr::str_replace_all(percentage_mean, c("\\[" = "", "\\]" = "")),
         percentage_mean = as.numeric(percentage_mean))

db1_mv_longer %>% str()

db1_mv_longer$percentage_mean <- db1_mv_longer$percentage_mean %>% as.numeric()



# Option 1

db1_mv_longer %>% 
  ggplot(aes(x = interaction(group, source), y = percentage_mean, fill = behavior)) +
  # geom_histogram() +
  # geom_col(position = position_dodge()) +
  # geom_bar(position = position_dodge()) +
  # geom_bar(position = "dodge", stat = "identity") +
  # geom_bar(position = "fill", stat = "identity") +
  geom_bar(method="mean", stat = "identity", position=position_dodge()) +

  ggtitle("Activity budget of 4 behaviors") +
  # ylab("Proportion (%)") +
  xlab("")  +
  theme(
    # legend.position = "none",
    axis.text = element_text(size = 9),
    axis.title.x = element_blank()
    # axis.text.y = element_blank()
    ) +


  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_viridis_d() #+
  # scale_y_continuous(labels = scales::percent)



# Option 2 (stat summary)
# ggplot(data=db1_mv_longer,aes(x=interaction(group, month),y=percentage_mean,fill=behavior)) +
ggplot(data=db1_mv_longer,aes(x=group ,y=percentage_mean
                              ,fill=month
                              )) +
  stat_summary(geom='bar', fun='mean', position='dodge',
               # fill = "white", color = "red"
               ) +
  stat_summary(geom='errorbar', fun.data='mean_cl_boot', position='dodge') +
  # ylim(0, 1) +
  ylab("Percentage (%)") +
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text = element_text(size = 9),
    axis.title.x = element_blank()#,
    # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1) 
  ) +
  scale_fill_viridis_d() +
  # facet_wrap(~source)
  facet_grid(behavior ~ source)

# Save plot
# ggsave(paste0(path,  "/", '02_simple_ActivityBudget_barplot_statsummary.png'), height = 5, width = 7)


# Option 3
db1_mv_sum <- db1_mv_longer %>% 
  group_by(group, month, source, behavior) %>% 
  summarise(
    mean = mean(percentage_mean),
    sd = sd(percentage_mean)
  )
# db1_mv_sum %>% str()
# db1_mv_longer %>%
# db1_mv_sum %>%
ggplot(data = db1_mv_sum, aes(x = factor(group), y = mean, fill = month)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  geom_errorbar(aes(ymin=mean, ymax=mean+sd), position = position_dodge(.9)) +
  # stat_summary(geom='errorbar', fun.data='mean_cl_boot', position='dodge') +
  ylab("Percentage (%)") +
  facet_grid(behavior ~ source) +
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text = element_text(size = 9),
    axis.title.x = element_blank()
  ) +
  scale_fill_viridis_d()

# Save plot
# ggsave(paste0(path,  "/", '02_simple_ActivityBudget_barplot_errorbar.png'), height = 5, width = 7)



# Option 3
ggplot(data = db1_mv_longer, aes(x = group, y = percentage_mean, fill = month))  +
  # geom_bar(stat="identity", position="dodge", colour="black") +  # to check the errors
  geom_bar(stat="summary", fun = "median", position="dodge", colour="black") + 
  ylab("Percentage (%)") +
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.title.x = element_blank()
  ) +
  scale_fill_viridis_d() +
  facet_grid(behavior ~ source)

# a <- db1_mv_longer %>% 
#   # dplyr::filter(p_resting > 0.1)
#   dplyr::filter(group == "Guareí") %>% 
#   dplyr::filter(behavior == "p_resting") #%>% 
#   dplyr::filter(group == "Guareí") 
  
# Save plot
# ggsave(paste0(path,  "/", '02_simple_ActivityBudget_barplot_option2.png'), height = 5, width = 7)






### Movement rate (MR) and Path twisting (PT) -------------------------
# Load empirical data and calculate MR and PT 
obs.mv.metrics <- read.csv(here("Data", "Movement", "Curated", "Validation", 
                                "Siminputrow_MR-PT_by-day.csv")) %>% 
  dplyr::select(c(1:MR, PT)) %>% 
  rename(month = id_month) %>% 
  mutate(group = recode(group,
                        "Santa Maria" = "SantaMaria"
  ))

obs.mv.metrics %>% str()
obs.mv.metrics$group %>% as.factor() %>% levels()

obs.mv.metrics.summary <- obs.mv.metrics %>% 
  group_by(group, month) %>% 
  summarise(
    MR_mean = mean(MR, na.rm = TRUE),
    MR_sd = sd(MR, na.rm = TRUE),
    PT_mean = mean(PT, na.rm = TRUE),
    PT_sd = sd(PT, na.rm = TRUE)
  ) %>% 
  mutate(source = "observed")

db1_mv_summarry <- db1_mv %>% 
  group_by(source, group, month) %>% 
  dplyr::summarise(
    MR_mean = mean(MR, na.rm = TRUE),
    MR_sd = sd(MR, na.rm = TRUE),
    PT_mean = mean(PT, na.rm = TRUE),
    PT_sd = sd(PT, na.rm = TRUE)
  ) %>% 
  dplyr::filter(source == "simulated")

db1_mv_summarry <- db1_mv_summarry %>% 
  bind_rows(obs.mv.metrics.summary)

db1_mv_summarry <- db1_mv_summarry %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "SantaMaria", "Taquara")) %>% 
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec")) %>% 
  mutate(source = factor(source, levels = c("simulated", "observed")))


# Movement rate
db1_mv_summarry %>% ggplot(
  aes(x = group, y = MR_mean, 
      ymin = MR_mean - MR_sd,
      ymax = MR_mean + MR_sd,
      color = month,
      shape = source
      )
  ) +
  geom_pointrange(aes(shape = source), position = position_dodge2(width = .5)) +
  scale_color_viridis_d() +
  ylab("DPL / hours active") +
  facet_grid(~source) +
  ggtitle("Movement rate")  +
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text = element_text(size = 9),
    axis.title.x = element_blank()
  ) +
  guides(shape = FALSE) # drop legend of shape only
# Save plot
# ggsave(paste0(path,  "/", '02_simple_MR.png'), height = 3.5, width = 7)



# Path twisting
db1_mv_summarry %>% ggplot(
  aes(x = group, y = PT_mean, 
      ymin = PT_mean - PT_sd,
      ymax = PT_mean + PT_sd,
      color = month,
      shape = source
  )
) +
  geom_pointrange(aes(shape = source), position = position_dodge2(width = .5)) +
  scale_color_viridis_d() +
  # ylab(expression(DPL^{'2-'}/home range)) +
  ylab(bquote(~DPL^2~'/home range size')) +
  ggtitle("Path twisting")  +
  # facet_grid(~factor(source, levels = c("simulated", "observed"))) +
  facet_grid(~source) +
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text = element_text(size = 9),
    axis.title.x = element_blank()
  )+
  guides(shape = FALSE) # drop legend of shape only
# Save plot
# ggsave(paste0(path,  "/", '02_simple_PT.png'), height = 3.5, width = 7)





### R index (defecations) -------------------------
R.obs <- read.csv(here("Data", "Seed_dispersal", "Curated", "Validation", 
                       "Summary_by-month_R-aggregation.csv")) %>% 
  mutate(
    source = "observed"
  )

R.sim <- db_sd %>% 
  dplyr::select(group, month, random_seed, R_seeds, R_seeds_p) %>% 
  rename(R = R_seeds,
         p = R_seeds_p) %>% 
  mutate(
    source = "simulated"
  ) #%>% 
  # group_by(group, month) %>% 
  # summarise(
  #   R = mean(R, na.rm = TRUE),
  #   p = mean(p, na.rm = TRUE),
  #   R_sd = mean(R, na.rm = TRUE),
  #   p_sd = mean(p, na.rm = TRUE)
  # )

R.all <- bind_rows(R.obs, R.sim) %>% 
  distinct() %>% 
  mutate(
    point_pattern = case_when(
      R > 1 & p <= 0.05 ~ "ordered",
      R < 1 & p <= 0.05 ~ "clustered",
      TRUE ~ "random"
    )
  ) %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "SantaMaria", "Taquara")) %>% 
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec"))



# Check why some R values are 0 (filtering them out for now)
R.all <- R.all %>% 
  dplyr::filter(R > 0.1)


R.all %>% ggplot(
  aes(x = group, y = R, 
      color = month,
      shape = point_pattern
  )
) +
  # geom_boxplot() +
  geom_point(
    aes(size = 1.5),
    # position = position_jitterdodge(jitter.width = 0.25)
    position = position_dodge2(width = .5)
             ) +
  scale_color_viridis_d() +
  # ylab(expression(DPL^{'2-'}/home range)) +
  ylim(0, 1.5) +
  ylab("R index") +
  ggtitle("Seed aggregation")  +
  # facet_grid(~factor(source, levels = c("simulated", "observed"))) +
  facet_grid(~source) +
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text = element_text(size = 9),
    axis.title.x = element_blank()
  ) +
  guides(size = FALSE) # drop legend of size only

# Save plot
# ggsave(paste0(path,  "/", '02_simple_R-aggregation.png'), height = 5, width = 8)

