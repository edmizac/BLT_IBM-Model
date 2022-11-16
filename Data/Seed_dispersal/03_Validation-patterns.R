# Script name: 03_Validation-patterns.R
# Script purpose: summarize, plot and analize Seed dispersal empirical data for VALIDATION.

# Date created: 2022-11-16d
# Author: Eduardo Zanette

## Notes --------------------------- 
#
#

## Packages -------------------------
library("here")
library("dplyr")
library("forcats")
library("readxl")
library("lubridate")
library("hms")
library("ggplot2")
library("ggalt")

## Options -------------------------
# (plotting, memory limit, decimal digits)
theme_set(theme_bw(base_size = 15))


# Load data
dat.all <- read.csv(here("Data", "Seed_dispersal", "Curated", "All-areas_SeedDispersal.csv"),
                    sep = ",", stringsAsFactors = TRUE)
# dat.all %>% str()
# is.na(dat.all$def_datetime)
# nrow(dat.all[is.na(dat.all$def_datetime), ])


# Wrangling data
dat.all <- dat.all %>%
  
  # make datetime POSIXct
  mutate(
    def_datetime = lubridate::as_datetime(def_datetime), #, tz = "America/Sao_Paulo"),
    feed_datetime = lubridate::as_datetime(feed_datetime) #, tz = "America/Sao_Paulo"),
  ) %>% 
  
  # gather yyyy/mm/dd as id
  mutate(
    day = lubridate::as_date(def_datetime),
  ) %>% 
  
  # Defining each dispersal event as in the same day or next day
  mutate(disp_day = ifelse(lubridate::day(def_datetime) == lubridate::day(feed_datetime), # use case_when for a dplyr-er solution: https://stackoverflow.com/questions/22337394/dplyr-mutate-with-conditional-values
                           "same day",
                           "next day"),
         disp_day = as.factor(disp_day)
  ) %>% 
  
  # Order group levels and drop NA
  dplyr::filter(!is.na(id_feces)) %>% 
  droplevels() %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "Santa Maria", "Taquara")) %>% 
  mutate(id_month = forcats::fct_relevel(id_month, "Jan", "Feb", "Mar", "Apr", "May", 
                                         "Jun", "Jul", "Aug", "Sep", "Nov",  "Dec", "Dec2017", "Dec2018"))

# dat.all$id_month %>% levels()
# str(dat.all)
# str(a)


# Check data
dat.all %>% 
  # dplyr::filter(day == "2017-12-03") %>% 
  summarise_all(funs(sum(is.na(.)))) #-> extra_NA

dat.all %>% 
  str()


# General summary by month -> THESE ARE THE VALUES I WILL USE FORVALIDATION ON CHAPTER 1
round2 <- function(x) (round(x, digits = 2))

dat.all.summary.month <- dat.all %>% 
  group_by(group, id_month, disp_day) %>% 
  summarize(
    # nseeds_mean = mean((n_seeds)),
    # nseeds_sd = sd((n_seeds)),
    mean_SDD = mean(SDD),
    sd_SDD = sd(SDD),
    
    # Agreggation patterns
    
  ) %>% 
  mutate(across(where(is.numeric), round2))

## Write csv
# dat.all.summary.month %>%
#   write.csv(here("Data", "Seed_dispersal", "Curated", "Validation", "Summary_by-month.csv"),
#             row.names = FALSE)





#### SDD #####

## Density Option 1: by day of dispersal
# By group
dat.all %>% ggplot(
  aes(x = SDD, fill = group, group = group)
) +
  geom_density(alpha = 0.4) +
  xlab("SDD (in meters)") +
  facet_wrap(~disp_day, nrow = 2)

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", "Validation", 'SDD_disp_day_By-group_density.png'), height = 7, width = 6)


# By group and month
dat.all %>% ggplot(
  aes(x = SDD, color = id_month)
) +
  geom_density(alpha = 0.4,
               adjust = 5,
               position = "stack"
  ) +
  xlab("SDD (in meters)") +
  facet_wrap(~disp_day, nrow = 2) +
  scale_color_viridis_d()

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", "Validation", 'SDD_disp_day_By-month_density-stack.png'), height = 7, width = 6)


library("ggridges")
dat.all %>% ggplot(
  aes(x = SDD, y = group, fill = id_month)
) +
  geom_density_ridges( #alpha = 0.4,
               # adjust = 5,
               # position = "stack"
  ) +
  xlab("SDD (in meters)") +
  facet_wrap(~disp_day, nrow = 2) +
  scale_fill_viridis_d()

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", "Validation", 'SDD_disp_day_By-month_density-ridges.png'), height = 7, width = 6)


## Density Option 2: by plant species
# dat.all %>% ggplot(
#   aes(x = SDD, group = species)
# ) +
#   geom_density(fill = "black") +
#   facet_wrap(~species, ncol = 6) +
#   theme_minimal(base_size = 10) +
#   theme(legend.position="none") #+
#   # facet_wrap(~disp_day, nrow = 2)
#   # theme(axis.text.x = element_text(size=10))


## Boxplot
# By group
dat.all %>%
  ggplot() +
  aes(x = group, y = SDD, fill = group) +
  geom_violin() +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~disp_day, nrow = 2) +
  ylab("SDD (m)") +
  ylim(0, 1000)

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", "Validation", 'SDD_disp_day_By-group_violin.png'), height = 5, width = 7)


# By group and month
dat.all %>%
  ggplot() +
  aes(x = group, y = SDD, color = id_month) +
  geom_violin() +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~disp_day, nrow = 2) +
  ylab("SDD (m)") +
  ylim(0, 1000) +
  scale_color_viridis_d()

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", "Validation", 'SDD_disp_day_By-month_violin.png'), height = 5, width = 7)


dat.all %>% ggplot(
  aes(x = group, y = SDD, color = id_month)
) +
  geom_violin() +
  # geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
  geom_point(
    position = position_jitterdodge(jitter.width = .01) # only way of dodging the points and jitter it
  ) +
  ylab("SDD (in meters)") +
  facet_wrap(~disp_day, nrow = 2) +
  scale_color_viridis_d()

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", "Validation", 'SDD_disp_day_By-month_violin.png'), height = 5, width = 7)


dat.all %>% ggplot(
  aes(x = group, y = SDD, color = id_month)
) +
  geom_boxplot() +
  geom_point(
    position = position_jitterdodge(jitter.width = .05) # only way of dodging the points and jitter it
  ) +
  ylab("SDD (in meters)") +
  facet_wrap(~disp_day, nrow = 2) +
  scale_color_viridis_d()

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", "Validation", 'SDD_disp_day_By-month_boxplot.png'), height = 5, width = 7)



# By plant species
# dat.all %>%
#   ggplot() +
#   aes(x = group, y = SDD, fill = group) +
#   geom_boxplot() +
#   theme(axis.title.x = element_blank()) +
#   # theme(aspect.ratio = 0.5) +
#   theme(legend.text=element_text(size=10)) +
#   theme(strip.text = element_text(size = 10),
#         axis.text.x = element_text(size = 8,
#                                    angle = 45),
#         axis.text.y = element_text(size = 8)) +
#   # theme(legend.position="none") +
#   # facet_grid(~species, rows = 6)#, scales = "free", space = "free")
#   facet_wrap(~species, nrow = 6)

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", "Validation", 'Plot_species_SDD_byarea.png'), height = 15, width = 15)




