trk %>% dplyr::select(id, homerange) %>%
  unnest(cols = homerange)

# For a specific group:
a1 <- db %>% filter(id == "GuaSim_19") %>%
  make_track(.x=x, .y=y, id = id, crs = our_crs)
library("amt", quietly = TRUE, warning=FALSE)
db <- db %>% dplyr::rename_all(~str_replace_all(., c("-" = "_",
                                                     "\\." = "_"))) %>%  # Remove - and space characters.
  rename("travel_mode" = "travel_mode_",
         "x" = "x_UTM",
         "y" = "y_UTM",
         "run_number" = "X_run_number_",
         "step" = "X_step_",
         "phenology_on?" = "phenology_on_")
# Group and define unique id
db <- db %>% group_by(random_seed, siminputrow, `phenology_on?`,
                      step_forget, visual, prop_trees_to_reset_memory,
                      p_foraging_while_traveling, duration, day) %>%
  mutate(id = paste0("GuaSim_", cur_group_id())) # create id groups = day for each combination of variables. Now it is not needed to group every time and we can calculate variables by id (DPL, home range, etc)
db <- db %>% mutate(dist_traveled = case_when(dist_traveled == 0.7 ~ 19.80162,
                                              dist_traveled == 0.0 ~ 0))
db <- db %>%
  group_by(id) %>%
  mutate(DPL = sum(dist_traveled))
db %>%
  ggplot(aes(x=DPL, color = `phenology_on?`, fill = `phenology_on?`)) +
  geom_density(alpha = 0.4) +
  labs(x = "Daily Path Length (m)")
# amt vignette: https://cran.r-project.org/web/packages/amt/vignettes/p1_getting_started.html
db <- db %>% ungroup() %>% as_tibble() # the tbl has to be ungrouped for the amt pckage nest() function to work

kde1 <- a1 %>%
  hr_kde(levels = c(0.3, 0.5, 0.95))

# save(trk, file = "trk.Rdata")
load("trk.Rdata")

hrs_val <- trk %>%
  unnest(cols = hr_area)

hrs_val_merge <- hrs_val %>%
  dplyr::select(id, level, area)

db$run_number %>% as.factor() %>% levels() # There's something wrong, it should not have only one run number
db$random_seed %>% as.factor() %>% levels() # only 2 seeds per run (2 runs = one with phenology on, one with phenology off)
db %>% slice_sample(n=10) %>%
  flextable() %>%
  set_caption("10 random rows of the output file")

# Group and define unique id
db <- db %>% group_by(random_seed, siminputrow, `phenology_on?`,
                      step_forget, visual, prop_trees_to_reset_memory,
                      p_foraging_while_traveling, duration, day) %>%
  mutate(id = paste0("GuaSim_", cur_group_id())) # create id groups = day for each combination of variables. Now it is not needed to group every time and we can calculate variables by id (DPL, home range, etc)
# db$dist_traveled %>% unique()
# Calculate how much is a step with vel = 0.7
steps <- db %>%
  slice(10, 11) %>%
  dplyr::select(c("x", "y"))
x1 <- steps[1,1]
x2 <- steps[2,1]
y1 <- steps[1,2]
y2 <- steps[2,2]
steplength <- sqrt((x1-x2)^2 + (y1-y2)^2)
# for vel = 0.7
# 19.80162
db <- db %>% mutate(dist_traveled = case_when(dist_traveled == 0.7 ~ 19.80162,
                                              dist_traveled == 0.0 ~ 0))
db <- db %>%
  group_by(id) %>%
  mutate(DPL = sum(dist_traveled))
db %>%
  ggplot(aes(x=DPL, color = `phenology_on?`, fill = `phenology_on?`)) +
  geom_density(alpha = 0.4) +
  labs(x = "Daily Path Length (m)")
# amt vignette: https://cran.r-project.org/web/packages/amt/vignettes/p1_getting_started.html
db <- db %>% ungroup() %>% as_tibble() # the tbl has to be ungrouped for the amt pckage nest() function to work
# Merge to original df
hrs_val_merge %>% names()

db <- left_join(db, hrs_val_merge, by = "id")

# Extract home range values
hrs_val_merge <- hrs_val %>%
  dplyr::select(id, level, area) %>%
  pivot_wider(names_from = level, values_from = area) %>%
  
  
  colnames(hrs_val_merge) <- c("id", "KDE95", "KDE50")

db <- left_join(db, hrs_val_merge, by = "id")
View(db)

# Save csv with DPL and Home range values
db %>% write.csv(here("Model_analysis",
                      "Sensitivity-analysis",
                      "Model_v1.1_Sensitivity_2022-07-14d_Output.csv"))


# Save csv with DPL and Home range values
# db %>% write.csv(here("Model_analysis",
#                       "Sensitivity-analysis",
#                       "Model_v1.1_Sensitivity_2022-07-14d_Output.csv"))
# rm(db)

db <- read.csv("Model_v1.1_Sensitivity_2022-07-14d_Output.csv", row.names = 1)

hr_values_summary <- db %>%
  dplyr::group_by(step_forget, visual, duration,
                  prop_trees_to_reset_memory,
                  p_foraging_while_traveling,
                  random_seed) %>%
  summarise(mean_95 = mean(KDE95_ha), sd_95 = sd(KDE95_ha),
            mean_50 = mean(KDE50_ha), sd_50 = sd(KDE50_ha),
            n = n()) # %>%
View(hr_values_summary)
# path_img <- "D:/Data/Documentos/github/BLT_IBM-Model/Model_analysis/Workflow/Run_travelspeedvar/HomeRange_comparisons.html"
path_output <- here("Model_analysis", "Sensitivity-analysis",
                    "HomeRange_comparisons.html")
path_output
# path_img <- tempfile(fileext = ".html")
save_as_html(
  a,
  path = path_output)