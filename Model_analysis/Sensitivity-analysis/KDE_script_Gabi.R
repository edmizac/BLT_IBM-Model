### 

# simulated data
trk <- db_subset %>% 
  amt::make_track(.x=x, .y=y, .t = timestep, id = id
                  , all_cols = TRUE
                  , crs = our_crs
  )

# guarei original data:
trk_gua <- dat.gua.orig %>% 
  amt::make_track(.x=x, .y=y, .t=date
                  , all_cols = TRUE
                  , crs = our_crs)

# save(trk_gua, db_subset, file = "KDE_script_Gabi.Rdata")

# Estimate KDE for each group  by using map and saving the results to a new column using mutate
#but first, make raster (trast)
trast <- make_trast(trk, res = 5)
trast <- make_trast(trk_gua, res = 5)

# Next, we group the track by id and nest the track.
trk <- trk %>% nest(data = -"id")
trk

# sem especificar h (erro)
trk <- trk %>% 
  mutate(KDE95 = amt::map(data, function(x) 
    x %>% hr_kde(trast=trast, levels = c(0.5, 0.95))))

# especificando h = 30 (gera valores baixos de home range e gera vários wrnings)
trk <- trk %>% 
  mutate(homerange = amt::map(data, function(x) 
    x %>% hr_kde(trast=trast, levels = c(0.5, 0.95), h = 30)))

trk <- trk %>% 
  mutate(hr_area = map(homerange, hr_area))

hrs_val <- trk %>% 
  unnest(cols = hr_area)

# Extract home range values
hrs_val_merge <- hrs_val %>%
  dplyr::select(id, level, area) %>%
  pivot_wider(names_from = level, values_from = area)

colnames(hrs_val_merge) <- c("id", "KDE95"
                             , "KDE50"
                             )

# merge
db <- left_join(db_subset, hrs_val_merge, by = "id")

db <- db %>% 
  # rename("phenology_on?" = "phenology_on.") %>%
  mutate(KDE95_ha = KDE95 / 10000) %>% 
  mutate(KDE50_ha = KDE50 / 10000)

# plot
db %>% 
  ggplot(aes(x=KDE95_ha, color = `phenology_on?`, fill = `phenology_on?`)) +
  geom_density(alpha = 0.4) +
  labs(x = "Home range (ha, KDE 95%))")

db %>% 
  ggplot(aes(x=KDE50_ha, color = `phenology_on?`, fill = `phenology_on?`)) +
  geom_density(alpha = 0.4) +
  labs(x = "Home range (ha, KDE 50%))")

# summary
hr_values_summary <- db %>% 
  dplyr::group_by(step_forget, visual, duration, 
                  prop_trees_to_reset_memory,
                  p_foraging_while_traveling, 
                  random_seed) %>% 
  summarise(mean_95 = mean(KDE95_ha), sd_95 = sd(KDE95_ha), 
            mean_50 = mean(KDE50_ha), sd_50 = sd(KDE50_ha), 
            n = n())

flextable(hr_values_summary)
