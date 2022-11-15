# Script name: 01_plots.R
# Script purpose: plot and analize Seed dispersal empirical data

# Date created: 2022-11-03d
# Author: Eduardo Zanette

## Notes --------------------------- 
#
#

## Packages -------------------------
library("here")
# library("tidyverse")
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
  dplyr::filter(!is.na(group)) %>% 
  mutate(group = forcats::fct_relevel(group, "Suzano", "Guareí", "Santa Maria", "Taquara"))

# str(dat.all)
# str(a)
  

# Check data
dat.all %>% 
  dplyr::filter(day == "2017-12-03") %>% 
  str()


#### Gut transit time #####

## Density Option 1: by day of dispersal
dat.all %>% ggplot(
  aes(x = gut_transit_time / 60, fill = group, group = group)
) +
  geom_density(alpha = 0.4) +
  facet_wrap(~disp_day, nrow = 2) +
  xlab("gut transit time (in hours)") # +
  # Option 1.2:
  # xlab("hours after each defecation event") +
  # ggtitle("Gut transit time (in hours)") +
  # theme(plot.title = element_text(hjust = 0.5))

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", 'Plot_GTT_disp_day.png'), height = 7, width = 6)


## Density Option 2: by plant species (total mess)
dat.all %>% ggplot(
  aes(x = gut_transit_time / 60, fill = group, group = group)
) +
  geom_density(alpha = 0.8) +
  facet_wrap(~species, ncol = 8) +
  theme_bw(base_size = 10) +
  xlab("gut transit time (in hours)")

# Save plot  
# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", 'Plot_GTT_species.png'), height = 15, width = 12.5)




#### SDD #####

## Density Option 1: by day of dispersal
dat.all %>% ggplot(
  aes(x = SDD, fill = group, group = group)
) +
  geom_density(alpha = 0.4) +
  xlab("seed dispersal distance (SDD in meters)") +
  facet_wrap(~disp_day, nrow = 2)

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", 'Plot_SDD_disp_day.png'), height = 7, width = 6)


## Density Option 2: by plant species
dat.all %>% ggplot(
  aes(x = SDD, group = species)
) +
  geom_density(fill = "black") +
  facet_wrap(~species, ncol = 6) +
  theme_minimal(base_size = 10) +
  theme(legend.position="none") #+
  # facet_wrap(~disp_day, nrow = 2)
  # theme(axis.text.x = element_text(size=10))


## Boxplot
# By area
dat.all %>%
  ggplot() +
  aes(x = group, y = SDD, fill = group) +
  geom_boxplot() +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~disp_day, nrow = 2)

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", 'Plot_SDD_disp_day.png'), height = 7, width = 6)


# By plant species
dat.all %>%
  ggplot() +
  aes(x = group, y = SDD, fill = group) +
  geom_boxplot() +
  theme(axis.title.x = element_blank()) +
  # theme(aspect.ratio = 0.5) +
  theme(legend.text=element_text(size=10)) +
  theme(strip.text = element_text(size = 10),
        axis.text.x = element_text(size = 8,
                                   angle = 45),
        axis.text.y = element_text(size = 8)) +
  # theme(legend.position="none") +
  # facet_grid(~species, rows = 6)#, scales = "free", space = "free")
  facet_wrap(~species, nrow = 6)

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", 'Plot_species_SDD_byarea.png'), height = 15, width = 15)



#### n defecations per day ####
dat.all.summary <- dat.all %>% 
  group_by(group, day) %>% 
  summarize(
    defecation_events = n()
    # mean_SDD = mean(SDD),
    # sd_SDD = sd(SDD)
  )

dat.all.summary %>% 
  ggplot() +
  aes(x = group, y = defecation_events, fill = group) +
  geom_boxplot() +
  theme(
    axis.title.x = element_blank()
  ) +
  ylab("number of defecations per day") #+
  # facet_wrap(~disp_event, nrow = 2)

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", 'Plot_n_defecations.png'), height = 7, width = 5)



#### n seeds by defecation ####
# unique(dat.all$species) # get small seeded species names
dat.all %>% 
  dplyr::filter(n_seeds > 20) %>% 
  select(species, n_seeds)

small_seeds <- c("Epiphyllum phyllanthus", 
                 "Aechmea bromeliifolia", "Aechmea distichantha", 
                 "Ficus enormis", "Ficus luschnathiana", "Ficus guaranitica",
                 "Rhipsalis teres", "Trichilia catigua",
                 "Phoradendron quadrangulare", "Cereus hildmannianus",
                 "Miconia latecrenata", "Miconia pusilliflora",
                 "Philodendron spp."
                 )

gp1 <- dat.all %>% 
  dplyr::filter(!species %in% small_seeds) %>% 
  ggplot() +
  aes(x = group, y = n_seeds, fill = group) +
  geom_violin() +
  geom_boxplot(width=0.1, fill = "white") +
  theme(
    axis.title.x = element_blank()
  ) +
  ylab("number of seeds per feces") +
  theme(axis.title.y = element_text(size = 11)) +
  ggtitle("Number of seeds per feces (small seeds excluded)") +
  theme(plot.title = element_text(hjust = 0.5, size = 15))

gp2 <- dat.all %>% 
  ggplot() +
  aes(x = group, y = log10(n_seeds), fill = group) +
  geom_violin() +
  geom_boxplot(width=0.1, fill = "white") +
  theme(
    axis.title.x = element_blank()
  ) +
  ylab("log10(number of seeds per feces)") +
  theme(axis.title.y = element_text(size = 11)) +
  ggtitle("Number of seeds per feces (all seeds)") +
  theme(plot.title = element_text(hjust = 0.5, size = 15))

g <- gridExtra::grid.arrange(gp1, gp2)
# g
# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", 'Plot_n_seeds.png'),
       # g, height = 5, width = 7)




#### defecations hourwise ####

# prep data (use hms package instead of lubridate)
dat.all.hourwise.sameday <- dat.all %>% 
  dplyr::filter(disp_day == "same day") %>%
  # dplyr::filter(group == "Suzano") %>% # for plotting each group
  
  # for hms datetime (scale_x_time)
  mutate(
    month_id = lubridate::month(feed_datetime, label = TRUE, locale="English_United States"),
    def_datetime = as_hms(def_datetime),
    feed_datetime = as_hms(feed_datetime),
    difftime = feed_datetime - def_datetime) %>% 
  
  mutate(species = forcats::fct_rev(species)) # otherwise species will be ordered from Z to A

  # filter specific month
  # dplyr::filter(month_id == "May")
# 


### Option 1 - Dunbell plot based on: https://stackoverflow.com/questions/70816256/how-to-plot-time-interval-data-in-r-using-ggplot
for (BLT_group in unique(dat.all.hourwise.sameday$group)) {
  
  # BLT_group <- "Guareí"
  
  gp1 <- dat.all.hourwise.sameday %>%
    dplyr::filter(group == BLT_group) %>%
    ggplot(
      aes(
        x = feed_datetime,
        xend = def_datetime,
        y = species# disp_event
      )
    ) +
    xlab("time (hours)") +
    # scale_x_time() +
    
    geom_dumbbell(
      colour = "#a3c4dc",
      colour_xend = "#e68a00",
      size = 1 #,
      # dot_guide = TRUE, dot_guide_size = 0.5
    ) +
    # hrbrthemes::theme_ipsum_pub(grid = "Y") +
    geom_jitter(height = 0.05) +
    theme_bw(base_size = 15) #+
  # theme(axis.text.y = element_text(size=5)) #+
  #scale_x_continuous(limits = lims)
  
  print(gp1)
  
  # Save plot
  # ggsave(here("Data", "Seed_dispersal", "Curated", paste0('Plot_Dunbell_', BLT_group, '_sameday', '.png')), height = 12, width = 10)
  
}



### Option 2 - geom_segment # https://stackoverflow.com/questions/55185599/plotting-time-intervals-as-segments
###                          or https://stackoverflow.com/questions/70816256/how-to-plot-time-interval-data-in-r-using-ggplot
dat.all.hourwise.sameday %>%
  ggplot() +
  geom_point(aes(x=feed_datetime, 
                 y=species #disp_event
                 ), 
             size=1) +
    geom_segment(aes(x=feed_datetime, xend=def_datetime, y = species, yend = species), # y and yend=disp_event
               arrow=arrow(30,unit(1.25,"mm"),"last","closed")) +
  theme(axis.text.y = element_text(size=15)) #+
   # facet_wrap(~month_id)

# Save plot
# ggsave(here("Data", "Seed_dispersal", "Curated", "Plot_Dunbell_geom-segment_sameday.png"), height = 15, width = 12)


### Option 2.2 - to jitter lines (based on https://stackoverflow.com/questions/21904364/how-to-jitter-dodge-geom-segments-so-they-remain-parallel
###                               and https://stackoverflow.com/questions/60981619/apply-jitter-uniformly-across-geoms-in-ggplot2)
## Get colors:
library(pals)
# pals::pal.bands(alphabet)
unique(dat.all$species)
cols <- unname(alphabet2(n=19))

class(dat.all.hourwise.sameday$feed_datetime)
dat.all.hourwise.sameday %>% str()


dat.all.hourwise.sameday %>%
  mutate(feed_datetime = as.POSIXct(feed_datetime),
         def_datetime = as.POSIXct(def_datetime)) %>% 
  ggplot() +
  geom_point(aes(y = feed_datetime, x = species), 
             position = position_jitter(seed = 123, width = .2)) +
  geom_point(aes(y = def_datetime, x = species), shape = 10,
             position = position_jitter(seed = 123, width = .2)) +
  # geom_point(aes(y=def_datetime, x = species), shape = 17) +
  # geom_text(aes(y=feed_datetime, x = species), label = "▶", size = 3, family = "HiraKakuPro-W3") +
  geom_linerange(
    aes(y = feed_datetime,
        ymin=feed_datetime, ymax=def_datetime, 
        x = species, 
        # xmin = species, xmax = species,  
        group = disp_event,
        color = species),
    size = .6,
    position = position_jitter(seed = 123, .2)) +
  theme(legend.position="none") +
  # Make space between lines bigger:
  # theme(aspect.ratio = 2) +
  # Species name size:
  theme(
    axis.text.y = element_text(
      # lineheight = 1.5
      # ,
      size = 15
    ),
    axis.text.x = element_text(size = 12)
    
  ) +
  # Adjust hours in y axis:
  # scale_y_date(breaks = scales::date_breaks("4 hours"), date_labels = "%H:%M") +
  scale_y_time(limits = c(lubridate::hm("06:00"), lubridate::hm("20:00"))) +
  theme(
    strip.text = element_text(size = 17)
    # angle = 90
  ) +
  # Define colors:
  # scale_color_manual(values = cols) +
  coord_flip() +
  facet_grid(~group, scales = "fixed") +
  
  ylab("hour of day") +
  ggtitle("Seed dispersal events occuring on the same day") +
  theme(plot.title = element_text(hjust = 0.5, size = 30))

# Save plot  
# ggsave(here("Data", "Seed_dispersal", "Curated", "Plot_Dunbell_geom-linerange_sameday.png"), height = 15, width = 15)


library(pals)
library(Polychrome)
# By defecation event (color by species)
for (BLT_group in unique(dat.all.hourwise.sameday$group)) {
  BLT_group <- "Guareí" # 31 colors needed
  
  aux <- dat.all.hourwise.sameday %>% 
    dplyr::filter(group == BLT_group) %>%
    droplevels() %>%
    # add_tally() 
    dplyr::count(species)
  n_cols <- nrow(aux)
  # Option 1 with pals
  cols <- unname(glasbey(n=n_cols)) # try polychrome or glasbey
  # pal.bands(cols)
  # Option 2 with Polychrome
  # set.seed(111)
  # cols = createPalette(n_cols, c("#ff0000", "#00ff00", "#0000ff"))
  # swatch(cols)
  
  df.l <- dat.all.hourwise.sameday %>%
    # group_by(species) %>%
    # alter the order of events to match those of species (same species are close on the graph)
    add_count(species) %>% # instead of summarizing and then merging: https://stackoverflow.com/questions/68121047/reorder-factors-by-a-group-statistic
    mutate(disp_event = fct_reorder(disp_event,
                                    n, # )) %>%
                                    .desc = TRUE)) %>%
    # alter the order of species on the legent to match the order given for the disp_events
    mutate(species = fct_reorder(species,
                                 n,
                                 .desc = FALSE)) %>% 
    # filter groups to match the loop
    dplyr::filter(group == BLT_group) %>% 
    droplevels()
  
  # df.l %>% str()
  # df.l %>% dplyr::filter(species == "Species 5")
  
  df.l %>% 
    ggplot(aes(y = feed_datetime,
               ymin=feed_datetime, ymax=def_datetime,
               x = disp_event,
               xmin = disp_event, xmax = disp_event,
               group = species,
               color = species)) +
    geom_point(aes(y = feed_datetime, x = disp_event)) +
    geom_point(aes(y = def_datetime, x = disp_event), shape = 10) +
    # geom_point(aes(y=def_datetime, x = species), shape = 17) +
    # geom_text(aes(y=feed_datetime, x = species), label = "▶", size = 3, family = "HiraKakuPro-W3") +
    geom_linerange(
      size = 1.25,
      position = position_dodge(.75)) +
    scale_y_time(limits = c(lubridate::hm("06:00"), lubridate::hm("20:00"))) +
    coord_flip() +
    
    ylab("time (hours)") +
    xlab("Dispersal event") +
    
    theme(axis.text.y = element_text(size = 7)) +
    # Define colors:
    scale_color_manual(values = cols)
  # scale_color_brewer(palette = "PuOr")
  # scale_color_brewer(palette = "YlOrRd")
  # theme(legend.position="none")
  
  # # Save plot  
  # ggsave(here("Data", "Seed_dispersal", "Curated", paste0("Plot_Dunbell_geom-linerange_sameday",
  #             BLT_group,  "_disp-event2", ".png")), height = 15, width = 12)
}




###### next day defecations #######

# prep data (use hms package instead of lubridate)
dat.all.hourwise.nextday <- dat.all %>% 
  dplyr::filter(disp_day == "next day") %>%
  # dplyr::filter(group == "Suzano") %>% # for plotting each group
  
  # for hms datetime (scale_x_time)
  mutate(
    month_id = lubridate::month(feed_datetime, label = TRUE, locale="English_United States"),
    def_datetime = as_hms(def_datetime),
    feed_datetime = as_hms(feed_datetime),
    difftime = feed_datetime - def_datetime) %>% 
  
  mutate(species = forcats::fct_rev(species)) # otherwise species will be ordered from Z to A

# filter specific month
# dplyr::filter(month_id == "May")
# 
# ### create function to get circular hours (bases on https://rvanmazijk.github.io/circular-data/)
# interval(gua.nextday$feed_datetime, gua.nextday$def_datetime)
# get_month_seq <- function(grt) {
#   # Creates seq from start to end, unless end <= start
#   # (e.g. 16 h to 07 h = 11, 12, 1, 2)
#   
#   map2(def_interval, function(def_interval) {
#     hour_seq <- 
#     if (int_start(def_interval) > int_end(def_interval))          lubridate::int_flip(def_interval)
#     else if (int_start(def_interval) < int_end(def_interval))     def_interval
#     hour_seq
#   })
# }
# 
# grttimes <- gua.nextday %>%
#   mutate(def_times = get_month_seq(feed_datetime, def_datetime))
# grttimes
# 
# gua.nextday <- gua.nextday
# 
# # for POSIXct datetime (scale_x_datetime)
# # mutate(feed_datetime = as.POSIXct(strptime(feed_datetime, "%Y-%m-%d %H:%M")),
# # def_datetime = as.POSIXct(strptime(def_datetime, "%Y-%m-%d %H:%M")))
# 
# lims <- as.POSIXct(strptime(c("2011-01-01 13:00","2011-01-02 12:00"), format = "%Y-%m-%d %H:%M"))

### Option 2.2 
## Get colors:
library(pals)
# pals::pal.bands(alphabet)
unique(dat.all$species)
cols <- unname(alphabet2(n=19))

class(dat.all.hourwise.sameday$feed_datetime)
dat.all.hourwise.nextday %>% str()


a <- dat.all %>%
  # dplyr::filter(group == "Suzano" | month_id == "Apr") %>% 
  dplyr::filter(disp_day == "next day") %>%
  droplevels() %>%
  # mutate(feed_datetime = as.POSIXct(feed_datetime),
         # def_datetime = as.POSIXct(def_datetime)) %>%
  mutate(
    def_datetime = as_hms(def_datetime),
    feed_datetime = as_hms(feed_datetime),
    
    daydef = as_date(ifelse(disp_day == "next day", Sys.Date()+1, Sys.Date())),
    dayfed = Sys.Date(),
  
    def_datetime = ymd_hms(paste(daydef, def_datetime)),
    feed_datetime = ymd_hms(paste(dayfed, feed_datetime))
  )  %>% 

  # alter the order of events to match those of species (same species are close on the graph)
  add_count(species) %>% # instead of summarizing and then merging: https://stackoverflow.com/questions/68121047/reorder-factors-by-a-group-statistic
  mutate(disp_event = fct_reorder(disp_event,
                                  n, # )) %>%
                                  .desc = TRUE)) %>%
  # alter the order of species on the legent to match the order given for the disp_events
  mutate(species = fct_reorder(species,
                               n,
                               .desc = FALSE))


a %>%  
  ggplot() +
  geom_point(aes(y = feed_datetime, x = species), shape = 1,  
             position = position_jitter(seed = 123, width = .2)) +
  geom_point(aes(y = def_datetime, x = species), shape = 4, #10
             position = position_jitter(seed = 123, width = .2)) +
  # geom_point(aes(y=def_datetime, x = species), shape = 17) +
  # geom_text(aes(y=feed_datetime, x = species), label = "▶", size = 3, family = "HiraKakuPro-W3") +
  geom_linerange(
    aes(y = feed_datetime,
        ymin=feed_datetime, ymax=def_datetime, 
        x = species, 
        # xmin = species, xmax = species,  
        group = disp_event,
        color = species),
    size = .6,
    position = position_jitter(seed = 123, .2)) +
  theme(legend.position="none") +
  # Make space between lines bigger:
  # theme(aspect.ratio = 2) +
  # Species name size:
  theme(
    axis.text.y = element_text(
      # lineheight = 1.5
      # ,
      size = 15
    ),
    axis.text.x = element_text(size = 15)
    
  ) +
  # Adjust hours in y axis:
  
  # scale_y_time(limits=c(as_hms(min(a$feed_datetime)), as_hms(max(a$def_datetime)))) +
  scale_y_datetime(date_labels = "%H") + # solution: https://bookdown.dongzhuoer.com/hadley/ggplot2-book/date-time.html

  # scale_y_time(limits = c(lubridate::hm("10:00"), NA)) +
  theme(
    strip.text = element_text(size = 17)
    # angle = 90
  ) +
  # Define colors:
  # scale_color_manual(values = cols) +
  coord_flip() +
  facet_grid(~group, scales = "fixed") +
  
  ylab("hour of day") +
  ggtitle("Seed dispersal events occuring on the following day") +
  theme(plot.title = element_text(hjust = 0.5, size = 30)) +
  
  # Add background colors
  annotate("rect", fill = "#546bab", alpha = 0.3, 
           xmin = -Inf, xmax = Inf,
           # ymin = max(a$feed_datetime), ymax = min(a$def_datetime)
           ymin = max(a$feed_datetime), ymax = min(a$def_datetime)
  ) +
  annotate("text", 
           y = ymd_hm(paste(Sys.Date()+1, "00:00")),#min(a$def_datetime), 
           x = length(unique(a$species)) / 2,
           size = 8,
           label = "Night time",
           color = '#2e4482', 
           alpha = 0.6,
           )
  
# Save plot  
# ggsave(here("Data", "Seed_dispersal", "Curated", "Plot_Dunbell_geom-linerange_nextday.png"), height = 15, width = 15)

