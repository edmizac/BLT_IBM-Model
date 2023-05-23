# PLOTS THAT HAVE WORKED 


library("here")
library("dplyr")
library("nlrx")
# library("magrittr")
library("stringr")
library("ggplot2")
library("Hmisc")
library("ggh4x")

# ggplot theme
theme_set(theme_bw(base_size = 15))
theme_update(
  axis.text.x = element_text(size = 11)
)


## Step 1: Create nl object ------------------------ 

if(Sys.info()[["nodename"]] == "DESKTOP-R12V3D6") {
  netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
  # modelpath <- here("Model_development", "BLT_model_v1.2.nlogo")
  modelpath <- here("Model_simulations", "BLT_model_v1.2.nlogo") # Last version with stored-energy
  outpath <- here("Model_analysis", "Sensitivity-analysis", "v1.2_2023Jan", "Param_bestguess", "temp")
  user_scp = "\"Eduardo\""
}
if(Sys.info()[["nodename"]] == "PC9") { # LEEC
  netlogopath <- file.path("C:/Program Files/NetLogo 6.2.2")
  # modelpath <- here("Model_development", "BLT_model_v1.2.nlogo")
  modelpath <- here("Model_simulations", "BLT_model_v1.2.nlogo") # Last version with stored-energy
  # outpath <- here("Model_analysis", "Sensitivity-analysis", "v1.2_December2022", "temp")
  Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8.0_351")
  user_scp = "\"LEEC\""
}


# Morris analysis for all areas -----

path <- paste0(outpath, "/")



### Grep files -----
# nls_to_df <- list.files(here("Model_analysis", "Sensitivity-analysis",
#                              "v1.2_2023Jan", "temp"), pattern = "[0-9].rds") #%>% # .RData does not work
nls_to_df <- list.files(path, pattern = "[0-9].rds") # .RData does not work

n <- 1 #counter
for (f in nls_to_df) {
  
  nl_file <- readRDS(paste0(path, "/", f))
  
  # test loop:
  # print(f)
  # }
  # nl_file <- readRDS(paste0(path, "/", "v1.2_Taquara_Jan_simple1671135962_tempRDS.Rdata"))
  # nl_file <- readRDS(paste0(path, "/", "v1.2_Suzano_Sep_simple453130432_tempRDS.Rdata"))
  # f <- nls_to_df[9]
  
  
  ### Remove runs where tamarins died (DPL or KDE = 0)
  out <- nl_file@simdesign@simoutput %>% 
    dplyr::filter(g_DPL == 0) %>% as_tibble()
  
  nl_file@simdesign@simoutput <- nl_file@simdesign@simoutput %>% 
    dplyr::filter(g_DPL != 0) %>% as_tibble()
  
  
  ### For initializing the first dbl
  if (n == 1) {
    morris_db <- analyze_nl(nl_file, "simoutput") #; rm(nl_file)
    
    # add identifiers
    morris_db$area_run <- nl_file@experiment@constants$study_area %>% gsub(., pattern = ('\"'), replacement = '', fixed = T)
    morris_db$month_run <- nl_file@experiment@constants$`feeding-trees-scenario` %>% gsub(., pattern = ('\"'), replacement = '', fixed = T)
    
    if (nl_file@experiment@constants$`feedingbout-on?` == "true") {
      morris_db$feedingbout <- "feedingbout on"
    } else {
      morris_db$feedingbout <- "feedingbout off"
    }
    
    # morris_db$area_run <- gsub(morris_db$area_run, pattern = ('\"'), replacement = '', fixed = T)
    # morris_db$month_run <- gsub(morris_db$month_run, pattern = ('\"'), replacement = '', fixed = T)
    
  } else {
    
    # f <- nls_to_df[2]
    # nl_file <- readRDS(paste0(path, "/", f))
    
    ### Attatch data from following  dbl
    morris_n <- analyze_nl(nl_file, "simoutput")
    
    # add identifiers
    morris_n$area_run <- nl_file@experiment@constants$study_area %>% gsub(., pattern = ('\"'), replacement = '', fixed = T)
    morris_n$month_run <- nl_file@experiment@constants$`feeding-trees-scenario` %>% gsub(., pattern = ('\"'), replacement = '', fixed = T)
    
    if (nl_file@experiment@constants$`feedingbout-on?` == "true") {
      morris_n$feedingbout <- "feedingbout on"
    } else {
      morris_n$feedingbout <- "feedingbout off"
    }
    
    
    
    morris_db <- dplyr::bind_rows(morris_db, morris_n)
    
    
  }
  
  # morris_db %>% str()
  
  n <- n + 1
}


### Recode parameters and Rename columns ----
morris_db <- morris_db %>% 
  dplyr::mutate(parameter =
                  str_replace_all(parameter, "-", "_"))

morris_db <- morris_db %>% 
  mutate(parameter = recode(parameter, "prop_trees_to_reset_memory" = "prop_reset_memory"))

morris_db <- morris_db %>% 
  mutate(
    metric = str_replace(metric, "g_", "")
  )


### Add categories ----
morris_db <- morris_db %>% 
  mutate(
    var_category = case_when(
      metric == "day_mean" ~ "simulation time",
      metric == "timestep_mean" ~ "simulation time",
      
      metric == "NN_seeds_mean" ~ "resource aggregation",
      metric == "NN_feeding_trees_mean" ~ "resource aggregation",
      metric == "NN_sleeping_trees_mean" ~ "resource aggregation",
      
      metric == "energy_stored_mean" ~ "energy",
      
      metric == "KDE95" ~ "home range",
      metric == "KDE50" ~ "home range",
      
      metric == "DPL_mean" ~ "movement",
      metric == "DPL_sd_mean" ~ "movement",
      metric == "MR_mean" ~ "movement",
      metric == "MR_sd_mean" ~ "movement",
      metric == "PT_mean" ~ "movement",
      metric == "PT_sd_mean" ~ "movement",
      
      
      metric == "p_feeding_mean" ~ "activity budget",
      metric == "p_foraging_mean" ~ "activity budget",
      metric == "p_traveling_mean" ~ "activity budget",
      metric == "p_resting_mean" ~ "activity budget",
      
      
      metric == "p-visited-trees_mean" ~ "revisits",
      
    )
  )

morris_db <- morris_db %>% 
  mutate(
    param_category = case_when(
      
      parameter == "start_energy" ~ "energy levels",
      parameter == "energy_level_1" ~ "energy levels",
      parameter == "energy_level_2" ~ "energy levels",
      
      parameter == "duration" ~ "others",
      parameter == "species_time_val" ~ "others",
      parameter == "prop_reset_memory" ~ "others",
      
      parameter == "step_forget" ~ "revisits",
      
      parameter == "energy_from_fruits" ~ "energy gain/loss",
      parameter == "energy_from_prey" ~ "energy gain/loss",
      parameter == "energy_loss_foraging" ~ "energy gain/loss",
      parameter == "energy_loss_traveling" ~ "energy gain/loss",
      parameter == "energy_loss_resting" ~ "energy gain/loss"
      
    )
  )

### Define area/month order ----
morris_db <- morris_db %>% 
  mutate(area_run = forcats::fct_relevel(area_run, "Suzano", "Guareí", "SantaMaria", "Taquara")) %>% 
  mutate(month_run = forcats::fct_relevel(month_run, "Jan", "Mar", "Apr", "May", 
                                          "Jun", "Jul", "Aug", "Sep", "Dec"))




## Plots -----
morris_db %>%
  dplyr::filter(metric=="energy_stored_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  # facet_grid(~area_run) +
  facet_grid(feedingbout ~ area_run) +
  ggtitle("Morris effects on stored energy")

### Home range ----

morris_db %>%
  dplyr::filter(metric=="KDE95") %>%
  ggplot(aes(x=value/10000, y=parameter, fill = index)) +
  # ggplot(aes(x=log(value/10000), y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  # facet_grid(~area_run) +
  facet_grid(feedingbout ~ area_run) +
  ggtitle("Morris effects on KDE95") +
  xlab("Home range area (ha)")



# Option 1
set.seed(42)
morris_db %>%
  dplyr::filter(area_run =="Guareí") %>%
  # dplyr::filter(var_category =="home range") %>%
  dplyr::filter(metric =="KDE95") %>%
  ggplot(aes(x=value/10000, y=parameter, shape = index, fill = month_run, color = month_run)) + # position_jitterdodge() requires fill and color
  # geom_point(alpha = 0.7, position = position_jitter(w=0.1, seed = 42)) +
  # geom_boxplot() +
  # geom_vline(xintercept = 0) +
  # facet_grid(~area_run) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar"
               , position = position_jitterdodge(dodge.width = 0.5)
               # , position = position_dodge2(w=0.1, #seed = 42,
               # preserve = "single")
  ) +
  geom_point(alpha = 0.8, size = 2, #geom = "point",
             position = position_jitterdodge(dodge.width = 0.5, seed = 42
                                             #preserve = "single"
             )) +
  # facet_grid(metric ~ feedingbout) +
  facet_grid(~feedingbout) +
  scale_color_viridis_d() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 10
    )
    # , axis.title.y = element_blank()
    # , axis.title.x = element_blank()
    # , legend.position = "bottom"
  )
# ggtitle("Morris effects on KDE95") +
# xlab("Home range area (ha)")


# Option 2 (as in Hess et al 2020 Fig 9)
set.seed(42)
morris_db %>%
  dplyr::filter(area_run =="Guareí") %>%
  # dplyr::filter(var_category =="home range") %>%
  dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(metric =="KDE_50_mean") %>%
  dplyr::filter(index =="mustar" | index =="mu") %>%
  tidyr::pivot_wider(names_from = index, values_from = value) %>% 
  ggplot(aes(x=mustar, y=mu, shape = parameter, fill = month_run, color = month_run)) + # position_jitterdodge() requires fill and color
  geom_point(alpha = 0.8, size = 4
             #, geom = "point",
             #, position = position_jitterdodge(dodge.width = 0.5, seed = 42
             #preserve = "single")
  ) +
  # facet_grid(metric ~ feedingbout) +
  facet_grid(~feedingbout) +
  scale_color_viridis_d()



# Option 3 (as in Hess et al 2020 Fig 9, but with all areas)
morris_db %>%
  # dplyr::filter(var_category =="home range") %>%
  dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(metric =="KDE_50_mean") %>%
  dplyr::filter(index =="mustar" | index =="mu") %>%
  tidyr::pivot_wider(names_from = index, values_from = value) %>% 
  ggplot(aes(x=mustar/10000, y=mu/10000, shape = parameter, fill = month_run, color = month_run)) +
  geom_point(alpha = 0.8, size = 4) +
  facet_grid(area_run~feedingbout) +
  scale_color_viridis_d()

# Trying to drop feedingbout from the grid (with the collors inside the points)
fill.col <- c('black', 'white')
morris_db %>%
  mutate(
    fill.col = case_when( #https://stackoverflow.com/questions/67047077/how-to-change-the-inside-color-fill-of-a-subset-of-points-with-the-same-shape
      feedingbout == "feedingbout on" ~ "black",
      TRUE ~ "white")
  ) %>% 
  dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(metric =="KDE_50_mean") %>%
  dplyr::filter(index =="mustar" | index =="mu") %>%
  tidyr::pivot_wider(names_from = index, values_from = value) %>% 
  ggplot(aes(x=mustar/10000, y=mu/10000, shape = parameter, group = feedingbout, 
             fill = feedingbout, color = month_run, stroke = 2)) +
  geom_point(alpha = 0.8, size = 4) +
  scale_fill_manual(values = fill.col) +
  scale_shape_manual(values = c(21, 22, 24)) +
  facet_grid(~area_run) +
  scale_color_viridis_d()

# Trying to add multiple collumns comparing variables + feedingbout
morris_db %>%
  dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(var_category =="home range") %>%
  # dplyr::filter(metric =="KDE_50_mean") %>%
  dplyr::filter(index =="mustar" | index =="mu") %>%
  tidyr::pivot_wider(names_from = index, values_from = value) %>% 
  ggplot(aes(x=log10(mustar/10000), y=log10(mu/10000), shape = parameter, fill = month_run, color = month_run)) +
  geom_point(alpha = 0.5, size = 2.5) +
  facet_nested(area_run ~ metric+feedingbout) +
  scale_color_viridis_d() +
  ggtitle("Effect of energy parameters on home range size")


### Movement patterns ----
morris_db %>%
  dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(var_category =="movement") %>%
  dplyr::filter(metric == "DPL_mean") %>%
  droplevels() %>% 
  dplyr::filter(index =="mustar" | index =="mu") %>%
  tidyr::pivot_wider(names_from = index, values_from = value) %>% 
  ggplot(aes(x=mustar, y=mu, shape = parameter, fill = month_run, color = month_run)) +
  geom_point(alpha = 0.5, size = 2.5) +
  facet_nested(area_run ~ metric+feedingbout) +
  scale_color_viridis_d() +
  ggtitle("Effect of energy parameters on DPL")

morris_db %>%
  dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(var_category =="movement") %>%
  dplyr::filter(metric == "MR_mean") %>%
  droplevels() %>% 
  dplyr::filter(index =="mustar" | index =="mu") %>%
  tidyr::pivot_wider(names_from = index, values_from = value) %>% 
  ggplot(aes(x=mustar, y=mu, shape = parameter, fill = month_run, color = month_run)) +
  geom_point(alpha = 0.5, size = 2.5) +
  facet_nested(area_run ~ metric+feedingbout) +
  scale_color_viridis_d() +
  ggtitle("Effect of energy parameters on Movement rate")

morris_db %>%
  dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(var_category =="movement") %>%
  dplyr::filter(metric == "PT_mean") %>%
  dplyr::filter(index =="mustar" | index =="mu") %>%
  tidyr::pivot_wider(names_from = index, values_from = value) %>% 
  ggplot(aes(x=mustar, y=mu, shape = parameter, fill = month_run, color = month_run)) +
  geom_point(alpha = 0.5, size = 2.5) +
  facet_nested(area_run ~ metric+feedingbout) +
  scale_color_viridis_d() +
  ggtitle("Effect of energy parameters on Path twisting")


### Activity budget -----
morris_db %>%
  dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(var_category =="activity budget") %>%
  dplyr::filter(metric == "p_feeding_mean" | metric == "p_foraging_mean" | metric == "p_resting_mean" | metric == "p_traveling_mean") %>%
  dplyr::filter(index =="mustar" | index =="mu") %>%
  tidyr::pivot_wider(names_from = index, values_from = value) %>% 
  ggplot(aes(x=mustar, y=mu, shape = parameter, fill = month_run, color = month_run)) +
  geom_point(alpha = 0.5, size = 2.5) +
  facet_nested(area_run ~ metric+feedingbout) +
  scale_color_viridis_d() +
  ggtitle("Effect of energy parameters on Activity budget")


### Energy -----
morris_db %>%
  dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(var_category =="energy") %>%
  # dplyr::filter(metric == "p_feeding_mean" | metric == "p_foraging_mean" | metric == "p_resting_mean" | metric == "p_traveling_mean") %>%
  dplyr::filter(index =="mustar" | index =="mu") %>%
  tidyr::pivot_wider(names_from = index, values_from = value) %>% 
  ggplot(aes(x=mustar, y=mu, shape = parameter, fill = month_run, color = month_run)) +
  geom_point(alpha = 0.5, size = 2.5) +
  facet_nested(area_run ~ metric+feedingbout) +
  scale_color_viridis_d() +
  ggtitle("Effect of energy parameters on Stored energy")

### Revisits -----
morris_db %>%
  dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(var_category =="revisits") %>%
  # dplyr::filter(metric == "p_feeding_mean" | metric == "p_foraging_mean" | metric == "p_resting_mean" | metric == "p_traveling_mean") %>%
  dplyr::filter(index =="mustar" | index =="mu") %>%
  tidyr::pivot_wider(names_from = index, values_from = value) %>% 
  ggplot(aes(x=mustar, y=mu, shape = parameter, fill = month_run, color = month_run)) +
  geom_point(alpha = 0.5, size = 2.5) +
  facet_nested(area_run ~ metric+feedingbout) +
  scale_color_viridis_d() +
  ggtitle("Effect of energy parameters on Proportion of visited trees")


# Option 3 (as in Hess et al 2020 Fig 9, but with all areas)
morris_db %>%
  # dplyr::filter(var_category =="home range") %>%
  dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(metric =="KDE_50_mean") %>%
  dplyr::filter(index =="mustar" | index =="mu") %>%
  tidyr::pivot_wider(names_from = index, values_from = value) %>% 
  ggplot(aes(x=mustar/10000, y=mu/10000, shape = parameter, fill = month_run, color = month_run)) +
  geom_point(alpha = 0.8, size = 4) +
  facet_grid(area_run~feedingbout) +
  scale_color_viridis_d()


scale_color_viridis_d()

# Trying to add multiple collumns comparing variables + feedingbout
morris_db %>%
  dplyr::filter(param_category =="energy levels") %>%
  # dplyr::filter(var_category =="home range") %>%
  dplyr::filter(metric =="KDE_50_mean") %>%
  # dplyr::filter(index =="mustar" | index =="mu") %>%
  tidyr::pivot_wider(names_from = index, values_from = value) %>% 
  ggplot(aes(x=log10(mustar/10000), y=log10(sigma/10000), shape = parameter, fill = month_run, color = month_run)) +
  geom_point(alpha = 0.5, size = 2.5) +
  facet_nested(area_run ~ metric+feedingbout) +
  scale_color_viridis_d() +
  ggtitle("Effect of energy parameters on home range size")
# Trying to drop feedingbout from the grid (with the collors inside the points)
fill.col <- c('black', 'white')
morris_db %>%
  mutate(
    fill.col = case_when( #https://stackoverflow.com/questions/67047077/how-to-change-the-inside-color-fill-of-a-subset-of-points-with-the-same-shape
      feedingbout == "feedingbout on" ~ "black",
      TRUE ~ "white")
  ) %>% 
  dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(metric =="KDE_50_mean") %>%
  dplyr::filter(index =="mustar" | index =="mu") %>%
  tidyr::pivot_wider(names_from = index, values_from = value) %>% 
  ggplot(aes(x=mustar/10000, y=mu/10000, shape = parameter, group = feedingbout, 
             fill = feedingbout, color = month_run, stroke = 2)) +
  geom_point(alpha = 0.8, size = 4) +
  scale_fill_manual(values = fill.col) +
  scale_shape_manual(values = c(21, 22, 24)) +
  facet_grid(~area_run)

### Movement patterns ----
morris_db %>%
  # dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(var_category =="movement") %>%
  dplyr::filter(metric == "DPL_mean") %>%
  dplyr::filter(index =="mustar" | index =="mu") %>%
  tidyr::pivot_wider(names_from = index, values_from = value) %>% 
  ggplot(aes(x=mustar, y=mu, shape = parameter, fill = month_run, color = month_run)) +
  geom_point(alpha = 0.5, size = 2.5) +
  facet_nested(area_run ~ metric+feedingbout) +
  scale_color_viridis_d() +
  ggtitle("Effect of energy parameters on DPL")

morris_db %>%
  dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(var_category =="movement") %>%
  dplyr::filter(metric == "MR_mean") %>%
  droplevels() %>% 
  dplyr::filter(index =="mustar" | index =="mu") %>%
  tidyr::pivot_wider(names_from = index, values_from = value) %>% 
  ggplot(aes(x=mustar, y=mu, shape = parameter, fill = month_run, color = month_run)) +
  geom_point(alpha = 0.5, size = 2.5) +
  facet_nested(area_run ~ metric+feedingbout) +
  scale_color_viridis_d() +
  ggtitle("Effect of energy parameters on Movement rate")

morris_db %>%
  dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(var_category =="movement") %>%
  dplyr::filter(metric == "PT_mean") %>%
  dplyr::filter(index =="mustar" | index =="mu") %>%
  tidyr::pivot_wider(names_from = index, values_from = value) %>% 
  ggplot(aes(x=mustar, y=mu, shape = parameter, fill = month_run, color = month_run)) +
  geom_point(alpha = 0.5, size = 2.5) +
  facet_nested(area_run ~ metric+feedingbout) +
  scale_color_viridis_d() +
  ggtitle("Effect of energy parameters on Path twisting")


### Activity budget -----
morris_db %>%
  dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(var_category =="activity budget") %>%
  dplyr::filter(metric == "p_feeding_mean" | metric == "p_foraging_mean" | metric == "p_resting_mean" | metric == "p_traveling_mean") %>%
  dplyr::filter(index =="mustar" | index =="mu") %>%
  tidyr::pivot_wider(names_from = index, values_from = value) %>% 
  ggplot(aes(x=mustar, y=mu, shape = parameter, fill = month_run, color = month_run)) +
  geom_point(alpha = 0.5, size = 2.5) +
  facet_nested(area_run ~ metric+feedingbout) +
  scale_color_viridis_d() +
  ggtitle("Effect of energy parameters on Activity budget")


### Energy -----
morris_db %>%
  dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(var_category =="energy") %>%
  # dplyr::filter(metric == "p_feeding_mean" | metric == "p_foraging_mean" | metric == "p_resting_mean" | metric == "p_traveling_mean") %>%
  dplyr::filter(index =="mustar" | index =="mu") %>%
  tidyr::pivot_wider(names_from = index, values_from = value) %>% 
  ggplot(aes(x=mustar, y=mu, shape = parameter, fill = month_run, color = month_run)) +
  geom_point(alpha = 0.5, size = 2.5) +
  facet_nested(area_run ~ metric+feedingbout) +
  scale_color_viridis_d() +
  ggtitle("Effect of energy parameters on Stored energy")

### Revisits -----
morris_db %>%
  dplyr::filter(param_category =="energy levels") %>%
  dplyr::filter(var_category =="revisits") %>%
  # dplyr::filter(metric == "p_feeding_mean" | metric == "p_foraging_mean" | metric == "p_resting_mean" | metric == "p_traveling_mean") %>%
  dplyr::filter(index =="mustar" | index =="mu") %>%
  tidyr::pivot_wider(names_from = index, values_from = value) %>% 
  ggplot(aes(x=mustar, y=mu, shape = parameter, fill = month_run, color = month_run)) +
  geom_point(alpha = 0.5, size = 2.5) +
  facet_nested(area_run ~ metric+feedingbout) +
  scale_color_viridis_d() +
  ggtitle("Effect of energy parameters on Proportion of visited trees")

s


  
  
  
# #### DPL ------
# 
# morris_db %>%
#   dplyr::filter(metric=="DPL_mean") %>%
#   ggplot(aes(x=value, y=parameter, fill = index)) +
#   geom_boxplot(lwd= 0.1) +
#   geom_vline(xintercept = 0) +
#   facet_grid(~area_run) +
#   ggtitle("Morris effects on DPL")
# 
# 
# #### Activity budget ------
# 
# morris_db %>%
#   dplyr::filter(metric=="p_feeding_mean") %>%
#   ggplot(aes(x=value, y=parameter, fill = index)) +
#   geom_boxplot(lwd= 0.1) +
#   geom_vline(xintercept = 0) +
#   facet_grid(~area_run) +
#   ggtitle("Morris effects on % frugivory")
# 
# morris_db %>%
#   dplyr::filter(metric=="p_traveling_mean") %>%
#   ggplot(aes(x=value, y=parameter, fill = index)) +
#   geom_boxplot(lwd= 0.1) +
#   geom_vline(xintercept = 0) +
#   facet_grid(~area_run) +
#   ggtitle("Morris effects on % traveling")
# 
# morris_db %>%
#   dplyr::filter(metric=="p_resting_mean") %>%
#   ggplot(aes(x=value, y=parameter, fill = index)) +
#   geom_boxplot(lwd= 0.1) +
#   geom_vline(xintercept = 0) +
#   facet_grid(~area_run) +
#   ggtitle("Morris effects on % resting")
# 
# morris_db %>%
#   dplyr::filter(metric=="p_foraging_mean") %>%
#   ggplot(aes(x=value, y=parameter, fill = index)) +
#   geom_boxplot(lwd= 0.1) +
#   geom_vline(xintercept = 0) +
#   facet_grid(~area_run) +
#   ggtitle("Morris effects on % foraging")
# 
# morris_db %>%
#   dplyr::filter(metric=="MR_mean") %>%
#   ggplot(aes(x=value, y=parameter, fill = index)) +
#   geom_boxplot(lwd= 0.1) +
#   geom_vline(xintercept = 0) +
#   facet_grid(~area_run) +
#   ggtitle("Morris effects on Movement Rate")
# 
# morris_db %>%
#   dplyr::filter(metric=="PT_mean") %>%
#   ggplot(aes(x=value, y=parameter, fill = index)) +
#   geom_boxplot(lwd= 0.1) +
#   geom_vline(xintercept = 0) +
#   facet_grid(~area_run) +
#   ggtitle("Morris effects on Path Twisting") # wrong?
# 
# morris_db %>%
#   dplyr::filter(metric=="NN_seeds_mean") %>%
#   ggplot(aes(x=value, y=parameter, fill = index)) +
#   geom_boxplot(lwd= 0.1) +
#   geom_vline(xintercept = 0) +
#   facet_grid(~area_run) +
#   ggtitle("Morris effects on seed aggregation")
# 
# morris %>%
#   dplyr::filter(metric=="p-visited-trees_mean") %>%
#   ggplot(aes(x=value, y=parameter, fill = index)) +
#   geom_boxplot(lwd= 0.1) +
#   geom_vline(area_run = 0) +
#   facet_grid(~feedingbout) +
#   ggtitle("Morris effects on proportion of visited trees")
# 
# # # Save plot
# # ggsave(paste0(outpath, "/",
# #             '01_Morris_n-visited-trees.png'), height = 7, width = 10)
# 
# 
# morris %>%
#   dplyr::filter(metric =="R_seeds_mean" |
#                   metric == "R_seeds_p_mean") %>%
#   # tidyr::pivot_wider(names_from = "metric", values_from = c(value) ) %>%
#   # dplyr::filter(R_seeds_p_mean <= 0.05) %>%
#   ggplot(aes(x=value, y=parameter, fill = index)) +
#   geom_boxplot(lwd= 0.1) +
#   geom_vline(xintercept = 0) +
#   facet_grid(~feedingbout) +
#   ggtitle("Morris effects on seed aggreation (R index)")
# 
# # # Save plot
# # ggsave(paste0(outpath, "/",
# #               '01_Morris_R-index.png'), height = 7, width = 10)
# 
# morris %>%
#   dplyr::filter(metric =="NN_seeds_mean") %>%
#   ggplot(aes(x=value, y=parameter, fill = index)) +
#   geom_boxplot(lwd= 0.1) +
#   geom_vline(xintercept = 0) +
#   facet_grid(~feedingbout) +
#   ggtitle("Morris effects on seed aggreation (Nearest neighbor distance)")
# 
# # # Save plot
# # ggsave(paste0(outpath, "/",
# #               '01_Morris_NN-distance.png'), height = 7, width = 10)






































# Morris analysis of one area -----
## Load -----
nl_on <- readRDS(file.path(paste0(outpath, "/"
                                  , "v1.2_Morris_Guareí_Jun_Feedingbout_on_2023-03-31.rds")))
nl_off <- readRDS(file.path(paste0(outpath, "/"
                                   , "morris_2022-12-23d_feedingbout-off.rds")))


nl_on@simdesign@simoutput
nl_on@simdesign@simoutput %>% colnames



# nl <- readRDS(paste0(path, 
#                      "v1.2_Morris_Guareí_Aug_Feedingbout_on_2023-04-01.rds"))
# "v1.2_Taquara_Jan_simple-609482361_tempRDS.Rdata"))

# db1  <-  nl@simdesign@simoutput ; class(db1)
# db2 <- nl@simdesign@siminput

## Merge -----
morris_on <- analyze_nl(nl_on, "simoutput") ; rm(nl_on)
morris_on$feedingbout <- if (feedingbout) {"feedingbout on"} else {"feedinbout off"}
morris_off <- analyze_nl(nl_off) ; rm(nl_off)
morris_off$feedingbout <- "feedingbout off"
morris <- dplyr::bind_rows(morris_on, morris_off)
rm(morris_on); rm(morris_off)

## Plot -----
morris_on %>%
  dplyr::filter(metric=="energy_stored_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on stored energy")

morris_on %>%
  dplyr::filter(metric=="KDE_95_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on KDE95")

morris_on %>%
  dplyr::filter(metric=="DPL_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on DPL")

morris_on %>%
  dplyr::filter(metric=="p_feeding_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on % frugivory")

morris_on %>%
  dplyr::filter(metric=="p_traveling_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on % traveling")

morris_on %>%
  dplyr::filter(metric=="p_resting_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on % resting")

morris_on %>%
  dplyr::filter(metric=="p_foraging_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on % foraging")

morris_on %>%
  dplyr::filter(metric=="MR_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on Movement Rate")

morris_on %>%
  dplyr::filter(metric=="PT_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on Path Twisting") # deu errado

morris_on %>%
  dplyr::filter(metric=="NN_seeds_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on seed aggregation")

morris %>%
  dplyr::filter(metric=="p-visited-trees_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on proportion of visited trees")

# # Save plot
# ggsave(paste0(outpath, "/",
#             '01_Morris_n-visited-trees.png'), height = 7, width = 10)


morris %>%
  dplyr::filter(metric =="R_seeds_mean" |
                  metric == "R_seeds_p_mean") %>%
  # tidyr::pivot_wider(names_from = "metric", values_from = c(value) ) %>%
  # dplyr::filter(R_seeds_p_mean <= 0.05) %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on seed aggreation (R index)")

# # Save plot
# ggsave(paste0(outpath, "/",
#               '01_Morris_R-index.png'), height = 7, width = 10)

morris %>%
  dplyr::filter(metric =="NN_seeds_mean") %>%
  ggplot(aes(x=value, y=parameter, fill = index)) +
  geom_boxplot(lwd= 0.1) +
  geom_vline(xintercept = 0) +
  facet_grid(~feedingbout) +
  ggtitle("Morris effects on seed aggreation (Nearest neighbor distance)")

# # Save plot
# ggsave(paste0(outpath, "/",
#               '01_Morris_NN-distance.png'), height = 7, width = 10)

