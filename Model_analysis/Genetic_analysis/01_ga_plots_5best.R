# Script name: 01_ga_plots.R
# Script purpose: summarize genetic algorithm results

# Date created: 2022-12-26d
# Author: Eduardo Zanette

## Notes --------------------------- 
#
#

## Options -------------------------
# (plotting, memory limit, decimal digits)
# 

## Packages -------------------------
library("here")
library("dplyr")
library("nlrx")
library("tictoc")
library("magrittr")
library("ggplot2")
library("stringr")

# ggplot theme
theme_set(theme_bw(base_size = 18))

# path <- here("Model_analysis", "Genetic_analysis", "temp", "without_stored_energy")
path <- here("Model_analysis", "Genetic_analysis", "temp")

# suz <- filesga <- list.files(path, pattern = "Dec_nl.rds")
# suz <- paste0(path, "/", filesga)
# suz <- readRDS(suz)



filesga <- list.files(path, pattern = "results_feedingbouton.rds")  # use the rgba file
filesga <- paste0(path, "/", filesga)

# dfga <- data.frame("expname" = character(), "parameter" = character(), "value" = double())
# dfga <- data.frame()
n <- 1
for (i in filesga) {
  # i <- filesga[2]
  # resultsrbga <- read.csv(i, encoding = "latin1")
  resultsrbga <- readRDS(i)
  
  population <- resultsrbga[7] %>% purrr::map_df(., ~as.data.frame(.))
  best_results <- resultsrbga[11] %>% unlist()
  
  best5 <- resultsrbga$best %>% #sort(., decreasing = TRUE)
    unique() %>% 
    head(5) # select the 5 best scores (unique)
  
  
  # ### Option 1: filtering despite the number of chromosomes: ###
  # best5_idx <- resultsrbga$best[resultsrbga$best %in% best5] ; length(best5_idx)
  # # idx <- match(best5, resultsrbga$best)
  # optimized_param <- population[1:length(best5_idx), ]
  
  
  ### Option 2:  filtering only one chromossome per fitness value: ###
  best5_idx <- match(best5, best_results)
  best5 <- resultsrbga$best[resultsrbga$best %in% best5] %>% unique() ; length(best5_idx)

  optimized_param <- population[best5_idx, ]
  
  # get parameter names:
  params <- resultsrbga[2] %>% unlist()
  # names(params)
  
  colnames(optimized_param) <- names(params) %>% stringr::str_sub(., end=-5)
  colnames(optimized_param) <- colnames(optimized_param) %>% stringr::str_sub(., start=11)
  colnames(optimized_param)
  # a <- colnames(optimized_param)
  
  optimized_param <- cbind(optimized_param, best5)
  colnames(optimized_param)[12] <- "fitness"
  
  optimized_param$expname <- basename(i) %>% str_match(., '(?:_[^_]+){3}') %>% as.character() %>% 
    str_remove(., '^_{1}') %>% str_remove(., "_results")
  # optimized_param <- optimized_param %>% t() %>% as.data.frame(row.names = TRUE)
  # optimized_param <- cbind(optimized_param, a)
  # optimized_param$simulation_scenario <- paste0(area_run, "_", month_run)
  # optimized_param <- optimized_param[, c(3, 2, 1)]
  
  if (n == 1) {
    dfga <- optimized_param
  }
    
  dfga <- dplyr::bind_rows(dfga, optimized_param)
  n <- n + 1
  
}

# extract min/max parameter info from experiments from last i
min_param <- resultsrbga[2] # parameters min
max_param <- resultsrbga[3] # parameters max

ga_input <- data.frame(
  min = min_param,
  max = max_param
)

rownames(ga_input) <- rownames(ga_input) %>% stringr::str_sub(., end=-5)
ga_input$parameter <- rownames(ga_input)
rownames(ga_input) <- NULL

# create custom min/max for fitness and bind to ga_input
fitinfo <- data.frame(stringMin = 0, 
                      stringMax = 300,
                      parameter = "fitness")

ga_input <- ga_input %>% rbind(fitinfo)


# pivot into longer for plotting
dfga <- dfga %>% 
  tidyr::pivot_longer(names_to = "parameter", values_to = "value",
                      cols = `energy-from-fruits`:fitness)

# join min and max values from ga_input
dfga <- dfga %>% dplyr::left_join(ga_input, by="parameter")


# dfga <- dfga %>% 
#   dplyr::mutate(parameter = 
#                 str_replace(parameter, "-", "_"))

dfga <- dfga %>% 
  dplyr::mutate(parameter = 
                  str_replace_all(parameter, c("-" = "_", " " = "")))


dfga <- dfga %>% 
  mutate(parameter = recode(parameter, "prop_trees_to_reset_memory" = "prop_reset_memory")) #%>% 
dfga <- dfga %>% 
  mutate_if(is.character, as.factor)
dfga <- dfga %>% 
  rename("min" = "stringMin",
         "max" = "stringMax",
         )

dfga %>% str()
dfga$parameter %>% levels()

# take all punctuation out
dfga$expname %>% stringr::str_replace_all(.,"[^[:graph:]]", "") 

# dfga_en <- dfga %>% 
#   dplyr::filter(str_detect(parameter, "energy"))
# 
# dfga_otr <- dfga %>% 
#   dplyr::filter(!str_detect(parameter, "energy"))


target <- c("start_energy", "energy_level_1", "energy_level_2")
dfga_en <- dfga %>% 
    dplyr::filter(
      parameter %in% target
    )

target <- c("energy_from_fruits", "energy_from_prey", "energy_loss_foraging", "energy_loss_traveling", "energy_loss_resting")
dfga_en2 <- dfga %>% 
  dplyr::filter(parameter %in% target)

target <- c("duration", "species_time", "prop_reset_memory"
            # , "energy-loss-foraging", "energy-loss-traveling"
)
dfga_otr <- dfga %>% 
  dplyr::filter(parameter %in% target)


# Plot showing how the optimization differed for each energy parameter
dfga_en %>% 
  ggplot() +
  # geom_pointrange(aes(ymin = min, ymax = max, x = parameter)) +
  geom_errorbar(aes(ymin = min, ymax = max, x = parameter)
                , width = 0.2
                , size = 0.2) +
  geom_point(aes(x = parameter, y = value, color = expname),
             size = 3
             , alpha = 0.4
             , position =  position_jitter(w = 0.2)
             ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 10
    )
    , axis.title.y = element_blank()
  ) +
  # ggtitle("Optimization of energy variables")
  ggtitle("Optimization of energy levels")

# Save plot
# ggsave(paste0(path,  "/with_stored_energy/", '01_GA_optimized-params_feedingbout-on1_best5.png'), height = 5, width = 7)


# Plot showing how the optimization differed for each energy parameter
dfga_en2 %>% 
  ggplot() +
  # geom_pointrange(aes(ymin = min, ymax = max, x = parameter)) +
  geom_errorbar(aes(ymin = min, ymax = max, x = parameter)
                , width = 0.2
                , size = 0.2) +
  geom_point(aes(x = parameter, y = value, color = expname),
             size = 3
             , alpha = 0.4
             , position =  position_jitter(w = 0.2)
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 10
    )
    , axis.title.y = element_blank()
  ) +
  # ggtitle("Optimization of energy variables")
  ggtitle("Optimization of energy gain and loss")

# Save plot
# ggsave(paste0(path,  "/with_stored_energy/", '01_GA_optimized-params_feedingbout-on2.png'), height = 5, width = 7)


# Plot showing how the optimization differed for each parameter (others)
dfga_otr %>% 
  ggplot() +
  # geom_pointrange(aes(ymin = min, ymax = max, x = parameter)) +
  geom_errorbar(aes(ymin = min, ymax = max, x = parameter)
                , width = 0.2
                , size = 0.2) +
  geom_point(aes(x = parameter, y = value, color = expname),
             size = 3
             , alpha = 0.4
             , position =  position_jitter(w = 0.2)
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 10
    )
    , axis.title.y = element_blank()
  ) +
  # ggtitle("Optimization of other parameters")
  ggtitle("Optimization of non-energy parameters")

# Save plot
# ggsave(paste0(path,  "/with_stored_energy/", '01_GA_optimized-params_feedingbout-on3.png'), height = 5, width = 7)


dfga <- dfga %>% 
  mutate(
    param_category = case_when(
      parameter == "fitness" ~ "fitness",
      
      parameter == "start_energy" ~ "energy levels",
      parameter == "energy_level_1" ~ "energy levels",
      parameter == "energy_level_2" ~ "energy levels",
      
      parameter == "duration" ~ "others",
      parameter == "species_time" ~ "others",
      parameter == "prop_reset_memory" ~ "others",

      parameter == "step_forget" ~ "revisits",
      
      parameter == "energy_from_fruits" ~ "energy gain/loss",
      parameter == "energy_from_prey" ~ "energy gain/loss",
      parameter == "energy_loss_foraging" ~ "energy gain/loss",
      parameter == "energy_loss_traveling" ~ "energy gain/loss",
      parameter == "energy_loss_resting" ~ "energy gain/loss"
      
      )
  )

# Plot grid
dfga %>% 
  ggplot() +
  # geom_pointrange(aes(ymin = min, ymax = max, x = parameter)) +
  geom_errorbar(aes(ymin = min, ymax = max, x = parameter)
                , width = 0.2
                , size = 0.2) +
  geom_point(aes(x = parameter, y = value, color = expname),
             size = 3
             , alpha = 0.4
             , position =  position_jitter(w = 0.2)
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 10
    )
    , axis.title.y = element_blank()
    , axis.title.x = element_blank()
    , legend.position = "bottom"
  ) +
  # scale_color_manual(values = c("#C833EC", "#33A4EC")) +
  ggtitle("Optimization of model parameters - best 5 fitness values") +
  facet_wrap(~param_category, scales = "free", nrow = 1)

# Save plot
# ggsave(paste0(path,  "/with_stored_energy/", '01_GA_optimized-params-feedingbout-on_wrap2.png'), height = 5, width = 10)

