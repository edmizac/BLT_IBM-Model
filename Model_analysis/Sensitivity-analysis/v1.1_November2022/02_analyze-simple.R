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
theme_set(theme_bw(base_size = 15,
                   theme(axis.text.x = element_text(size = 11))))

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


# define boxplot width
bpw <- 4/length(unique(paste(db1$group, db1$month)))
bpw # 0.44444

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
#             '02_simple_Spatial_SDD_Guareí-example.png'), height = 7, width = 10)


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
#             '02_simple_Spatial_SDD_SantaMaria-example.png'), height = 7, width = 10)



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
#             '02_simple_Spatial_SDD_Taquara-example.png'), height = 7, width = 10)


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
#             '02_simple_Spatial_SDD_Suzano-example.png'), height = 7, width = 10)


### 5) Facet -------------------------

# gridExtra::grid.arrange()
















## SDD -------------------------
rm(group)

# Load empirical data
obs <- read.csv(here("Data", "Seed_dispersal", "Curated", "Validation", "Siminputrow_SDD.csv"),
                    sep = ",", stringsAsFactors = TRUE)  %>%  
  # mutate(group = recode(group, "Guarei" = "Guareí")) #%>%  # to match all other datasets
  mutate(source = as.factor("observed")) %>% 
  rename(month = id_month) %>% 
  ungroup() %>% 
  as.data.frame()

# Simulated data
db_sd <- readRDS(paste0(path, "/", "02_Simoutput-simple_plants.rds"))
db_sd <- db_sd %>% 
  dplyr::filter(breed == "seeds") %>% 
  group_by(group, random_seed) %>% 
  mutate(source = "simulated") %>% 
  mutate(day = as.factor(day),
         disp_day = recode(disp_day, 
                        "sameday" = "same day",
                        "nextday" = "next day"
                        ) %>% as.factor()
  )
db_sd$disp_day %>% str()
db_sd$source %>% str()
# db1_sdd <- db1_sdd %>% 
#   filter(SDD > 0)
# db1_sdd <- db1_sdd %>%
#   dplyr::filter(disp_day == "sameday") # next_day SDD is all 0 (check model)

# Merge obserded data into db1_sd
db_sd <- db_sd %>% dplyr::bind_rows(obs)

db_sd <- db_sd %>% 
  mutate(source = as.factor(source)) %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "Santa Maria", "Taquara")) %>% 
  mutate(month = forcats::fct_relevel(month, "Jan", "Mar", "Apr", "May", 
                                      "Jun", "Jul", "Aug", "Sep", "Dec"))

db_sd$disp_day %>% str()
db_sd$disp_day %>% levels()
length(db_sd$disp_day %>% is.na(.)) # no NAs

db_sd$source %>% str()
db_sd$disp_day %>% levels()
length(db_sd$disp_day %>% is.na(.)) # no NAs

# db_sd$group %>% str()
## Density Option 1: by day of dispersal
# By group

db_sd <- db_sd %>% 
  dplyr::filter(!is.na(SDD)) %>% 
  dplyr::filter(!is.na(disp_day))

db_sd %>% ggplot(
  aes(x = SDD, fill = group, group = group)
) +
  geom_density(alpha = 0.4) +
  xlab("SDD (in meters)") +
  # facet_grid(. ~ disp_day, rows = 2)
  # facet_wrap(vars(source, disp_day), nrow = 2) +
  facet_grid(disp_day ~ source) +
  
  # others
  theme(axis.text = element_text(size = 9))


# Save plot
# ggsave(paste0(path, "/", '02_simple_SDD_disp_day_density.png'), height = 5, width = 7)

## Boxplot
# By group
db_sd %>%
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


db_sd %>% ggplot(
  aes(x = group, y = SDD, color = month)
) +
  geom_boxplot() +
  geom_point(
    position = position_jitterdodge(jitter.width = .45) # only way of dodging the points and jitter it
  ) +
  ylab("SDD (in meters)") +
  ylim(0, 1500) +
  facet_wrap(~disp_day, nrow = 2) +
  # scale_color_viridis_d() +
  # facet_wrap(vars(disp_day, source), nrow = 2) +
  
  # others
  theme(axis.text = element_text(size = 9)) +
# geom_point(position = position_jitterdodge(jitter.width = 0.7)) 


# Save plot
# ggsave(paste0(path, "/", '02_simple_SDD_disp_day_boxplot.png'), height = 5, width = 7)


  # facet_grid(cols = vars(disp_day), rows = vars(source)) +
  facet_grid(cols = vars(disp_day), rows = vars(source)) +
  scale_color_viridis_d()

# Save plot
# ggsave(paste0(path, "/", '02_simple_SDD_disp_day_grid-boxplot.png'), height = 5, width = 7)




  
  
  
## Movement patterns -------------------------
theme_update(
      axis.title.x = element_blank()
  )

# define boxplot width
bpw <- 6/length(unique(paste(db1$group, db1$month)))
bpw # 0.44444
  
  
db1_mv <- readRDS(paste0(path, "/", "02_Simoutput-simple_monkeys_long.rds")) %>% 
    # mutate(group = recode(group, "Guarei" = "Guareí")) #%>%  # to match all other datasets
    ungroup() %>% 
    as.data.frame()
  
# db1_mv %>% str()
  
# Wrangle data:
  
db1_mv <- db1_mv %>% 
# dplyr::filter(breed == "monkeys") %>% 
  rename(DPL = DPL_d,
         KDE95 = KDE_95,
         KDE50 = KDE_50) %>% 
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

db1_mv <- db1_mv %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "Santa Maria", "Taquara")) %>% 
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

# Save plot
ggsave(paste0(path,  "/", '02_simple_DPL_By-month_boxplot.png'), height = 3.5, width = 7)





  



### Home range -------------------------

# KDE 95
db1_mv %>% 
  ggplot(aes(x = group, y = KDE95, color = month)) +
  # geom_boxplot(position = position_dodge(preserve = "single")) +
  geom_boxplot() +
  guides(fill=FALSE) +
  ylim(0, 350) +
  ylab("Area (ha)") +
  ggtitle("KDE 95% (Activity range)") +
  scale_colour_viridis_d() +
  facet_grid(~source) +
  
  # others
  theme(axis.text = element_text(size = 9))
# geom_point(position = position_jitterdodge(jitter.width = 0.7)) 

# geom_jitter(width = 0.15) +
# annotate("text", x=2, y=5000, label= paste0("Mean ± sd = ", round(avg_dist, 2), " ± ", round(sd_dist, 2)))

# Save plot
# ggsave(paste0(path,  "/", '02_simple_HR_KDE95_boxplot.png'), height = 3.5, width = 7)

# ggsave(filename = here("Model_analysis", "Workflow", "Run_travelspeedvar",
#                        "HomeRangeKDE95-speedval.png"),
#        dpi = 300, width = 30, height = 20, units = "cm")

# KDE 50
db1_mv %>% 
  ggplot(aes(x = group, y = KDE50, color = month)) +
  # geom_boxplot(position = position_dodge(preserve = "single")) +
  geom_boxplot() +
  guides(fill=FALSE) +
  ylim(0, 350) +
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
# ggsave(paste0(path,  "/", '02_simple_HR_KDE50_boxplot.png'), height = 3.5, width = 7)

# ggsave(filename = here("Model_analysis", "Workflow", "Run_travelspeedvar",
#                        "HomeRangeKDE95-speedval.png"),
#        dpi = 300, width = 30, height = 20, units = "cm")
