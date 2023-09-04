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

# ggplot theme
theme_set(theme_bw(base_size = 18))

# path <- here("Model_analysis", "Genetic_analysis", "temp", "without_stored_energy")
path <- here("Model_analysis", "Genetic_analysis", "temp")

# suz <- filesga <- list.files(path, pattern = "Dec_nl.rds")
# suz <- paste0(path, "/", filesga)
# suz <- readRDS(suz)


filesga <- list.files(path, pattern = "feedingbouton.csv")
filesga <- paste0(path, "/", filesga)

dfga <- data.frame("expname" = character(), "parameter" = character(), "value" = double())
for (i in filesga) {
  # i <- filesga[2]
  filei <- read.csv(i, encoding = "latin1")
  dfga <- dplyr::bind_rows(dfga, filei)
}

dfga <- dfga %>% 
  dplyr::mutate(parameter, str_replace_all(., c("-" = "_", 
                                          # "\\." = "_",
                                          " " = "")))# %>% 
dfga <- dfga %>% 
  mutate(parameter = recode(parameter, "prop_trees_to_reset_memory" = "prop_reset_memory")) #%>% 
dfga <- dfga %>% 
  mutate_if(is.character, as.factor)

dfga %>% str()
dfga$parameter %>% levels()

dfga$expname %>% stringr::str_replace_all(.,"[^[:graph:]]", "") 
guastr <- guastr[1]
dfga <- dfga %>% 
  dplyr::mutate(expname = dplyr::recode(expname, guastr = "Guarei_Jul"
                                        # ,"Santa Maria" = "SantaMaria"
                                        )
                )

# dfga_en <- dfga %>% 
#   dplyr::filter(str_detect(parameter, "energy"))
# 
# dfga_otr <- dfga %>% 
#   dplyr::filter(!str_detect(parameter, "energy"))


target <- c("start-energy", "energy_level_1", "energy_level_2")
dfga_en <- dfga %>% 
    dplyr::filter(
      parameter %in% target
    )

target <- c("energy-from-fruits", "energy-from-prey", "energy-loss-foraging", "energy-loss-traveling", "energy-loss-resting")
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
             # , alpha = 0.5
             # , position = "jitter"
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
# ggsave(paste0(outpath,  "/", '01_GA_optimized-params_feedingbout-on1.png'), height = 5, width = 7)


# Plot showing how the optimization differed for each energy parameter
dfga_en2 %>% 
  ggplot() +
  # geom_pointrange(aes(ymin = min, ymax = max, x = parameter)) +
  geom_errorbar(aes(ymin = min, ymax = max, x = parameter)
                , width = 0.2
                , size = 0.2) +
  geom_point(aes(x = parameter, y = value, color = expname),
             size = 3
             # , alpha = 0.5
             # , position = "jitter"
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
# ggsave(paste0(outpath,  "/", '01_GA_optimized-params_feedingbout-on2.png'), height = 5, width = 7)


# Plot showing how the optimization differed for each parameter (others)
dfga_otr %>% 
  ggplot() +
  # geom_pointrange(aes(ymin = min, ymax = max, x = parameter)) +
  geom_errorbar(aes(ymin = min, ymax = max, x = parameter)
                , width = 0.2
                , size = 0.2) +
  geom_point(aes(x = parameter, y = value, color = expname),
             size = 3
             # , alpha = 0.5
             # , position = "jitter"
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
# ggsave(paste0(outpath,  "/", '01_GA_optimized-params_feedingbout-on3.png'), height = 5, width = 7)


dfga <- dfga %>% 
  mutate(
    param_category = case_when(
      parameter == "start-energy" ~ "energy levels",
      parameter == "energy_level_1" ~ "energy levels",
      parameter == "energy_level_2" ~ "energy levels",
      
      parameter == "duration" ~ "others",
      parameter == "species_time" ~ "others",
      parameter == "prop_reset_memory" ~ "others",

      parameter == "step_forget" ~ "revisits",
      
      parameter == "energy-from-fruits" ~ "energy gain/loss",
      parameter == "energy-from-prey" ~ "energy gain/loss",
      parameter == "energy-loss-foraging" ~ "energy gain/loss",
      parameter == "energy-loss-traveling" ~ "energy gain/loss",
      parameter == "energy-loss-resting" ~ "energy gain/loss"
      
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
             # , alpha = 0.5
             # , position = "jitter"
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
  ggtitle("Optimization of model parameters") +
  facet_wrap(~param_category, scales = "free", nrow = 1)

# Save plot
# ggsave(paste0(outpath,  "/", '01_GA_optimized-params-feedingbout-on_wrap.png'), height = 5, width = 10)

