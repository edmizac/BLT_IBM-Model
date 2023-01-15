library("nlrx")
library("here")
library("dplyr")
library("stringr")
library("ggplot2")

theme_set(theme_bw(base_size = 15))


pathexp1 <- "D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest/Experiment1/batch_all/without_SDD/"

filesexp1 <- list.files(pathexp1, pattern = ".rds")

rm(i)
# i <- 1
# for (i in filesexp1) {
#   # i <- filesexp1[2]
#   
#   if (i == 1) { 
#       dfexp1 <- readRDS(paste0(pathexp1, i)) %>% getsim(., "simoutput") %>% as.data.frame() 
#     dfexp1 <- dfexp1 %>%
#       mutate_if(
#         is.logical, as.character
#         ) %>% 
#       mutate(
#         `survived?` = as.character(`survived?`),
#         `p-visited-trees` = as.numeric(`p-visited-trees`),
#         `feedingbout-on?` = as.character(`feedingbout-on?`),
#         `[run number]` = as.character(`[run number]`)
#       )
#   } 
#   else {
#     filei <- readRDS(paste0(pathexp1, i)) %>% getsim(., "simoutput") %>% as.data.frame()
#     
#     # if (is.numeric(filei$`[step]`) | !is.logical(filei$`feedingbout-on?`)) { # if the simulation blew off, it has generated numeric values in `survived?`
#     if (is.numeric(filei$`[step]`) | is.numeric(filei$`[run number]`) | !is.logical(filei$`feedingbout-on?`)
#         | is.double(filei$`p-visited-trees`)) { # if the simulation blew off, it has generated numeric values in `survived?`
#     # if (!is.na(filei$`[step]`)) {
#       filei <- filei %>%
#         mutate_if(
#           is.logical, as.character
#           ) %>% 
#         mutate(
#           `survived?` = as.character(`survived?`),
#           `p-visited-trees` = as.numeric(`p-visited-trees`),
#           `feedingbout-on?` = as.character(`feedingbout-on?`),
#           `[run number]` = as.character(`[run number]`)
#           )
#       dfexp1 <- dplyr::bind_rows(dfexp1, filei)
#       
#     }
#   }
# }
# dfexp1 <- readRDS("D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest/exampleRDS.rds")
# dfexp1 <- nlrx::getsim(dfexp1, "simoutput")

i <- 999
i <- 1


for (i in filesexp1) {
  # i <- filesexp1[2]
  
  if (i == 1) { 
    i <- filesexp1[500]
    dfexp1 <- readRDS(paste0(pathexp1, i)) %>% getsim(., "simoutput") %>% as.data.frame() 
    dfexp1$`patch-size-ha`
    
    dfexp1 <- dfexp1 %>%
      dplyr::select(-c(`[run number]`, `feedingbout-on?`, `step-model-param?`, `gtt-param?`,
                       `p-forage-param?`)) %>% 
      mutate_if(
        is.logical, as.character
      ) %>%
      mutate(
        `survived?` = as.character(`survived?`),
        `p-visited-trees` = as.numeric(`p-visited-trees`),
        # `feedingbout-on?` = as.character(`feedingbout-on?`)
        # `[run number]` = as.character(`[run number]`)
      )
    
  } else {
    filei <- readRDS(paste0(pathexp1, i)) %>% getsim(., "simoutput") %>% as.data.frame()
    
      if (!anyNA(filei)) {
      filei <- filei %>%
        dplyr::select(-c(`[run number]`, `feedingbout-on?`, `step-model-param?`, `gtt-param?`,
                         `p-forage-param?`)) %>% 
        mutate_if(
          is.logical, as.character
        ) %>%
        mutate(
          `survived?` = as.character(`survived?`),
          `p-visited-trees` = as.numeric(`p-visited-trees`)
          # `feedingbout-on?` = as.character(`feedingbout-on?`)
          # `[run number]` = as.character(`[run number]`)
        )
      
      dfexp1 <- dplyr::bind_rows(dfexp1, filei) %>% 
        mutate(
          `[run number]` = as.character(`[run number]`)
        )
      
    }
  }
}

anyNA(filei)


dfexp1 %>% glimpse()
dfexp1 %>% str()
dfexp1$`feedingbout-on?` %>% str()
filei$`feedingbout-on?` %>% str()

dfexp1$`[run number]` %>% str()
filei$`[run number]` %>% str()

# Stimmt!


# Lets check how many of those runs had dead monkeys
dfexp1 %>% dplyr::filter(`survived?` == "no") %>% count() # none! wow. Why? Because I made the simulations to drop in the NetLogo code if the monkeys were dying

# Wrangle data for our purposes
dfexp1 <- dfexp1 %>% 
  rename(fragment_size = `patch-size-ha`) %>% 
  mutate(density = as.factor(density),
         fragment_size = as.factor(fragment_size))

dfexp1$n %>% as.factor() %>% unique() # 5 levels
dfexp1$fragment_size %>% as.factor() %>% unique() # 5 levels
dfexp1 %>% str()

# General plots -----



## Seed dispersal ----

# Our first interest is seed dispersal. Let's see

# # If distance between feeding trees generate longer dispersal distances
# dfexp1 %>% ggplot() +
#   aes(x = NN_feeding_trees, y = SDD
#       , color = n
#       , shape = density
#   ) +
#   geom_point() +
#   # scale_colour_viridis_d("magma") +
#   # scale_color_manual(values = c("#922B21", "#D98880")) + # red densitty
#   # scale_color_manual(values = c("#85C1E9", "#154360")) + # blue densitty
#   facet_grid(cols = vars(fragment_size), rows = vars(field.shape.factor)) +
#   # facet_wrap(vars(fragment_size, field.shape.factor)) +
#   # rename axis:
#   xlab("NN of fruiting trees") +
#   ylab("NN of seed dispersal events") +
#   # create secondary axis:
#   scale_y_continuous(sec.axis = sec_axis(~ . , name = "Fragment shape\n", breaks = NULL, labels = NULL)) +
#   scale_x_continuous(sec.axis = sec_axis(~ . , name = "Fragment size\n", breaks = NULL, labels = NULL))
# 


# If clumpiness of seeds is higher with increasing distance between feeding trees
dfexp1 %>% ggplot() +
  aes(x = NN_feeding_trees, y = NN_seeds
      , color = n
      , shape = density
  ) +
  geom_point() +
  geom_smooth() +
  # scale_colour_viridis_d("magma") +
  # scale_color_manual(values = c("#922B21", "#D98880")) + # red density
  # scale_color_manual(values = c("#85C1E9", "#154360")) + # blue density
  # scale_color_manual(values = c("#922B21", "#D98880")) + # red n trees
  # scale_color_manual(values = c("#D4E6F1", "#85C1E9", "#2980B9 ", "#1F618D", "#154360")) + # blue n trees
  facet_grid(cols = vars(fragment_size), rows = vars(field.shape.factor)) +
  # facet_wrap(vars(fragment_size, field.shape.factor)) +
  # rename axis:
  xlab("NN distance between fruiting trees") +
  ylab("NN distance between seed dispersal events") +
  # ggtitle("Effect of distance between fruiting trees") +
  # create secondary axis:
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Fragment shape\n", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Fragment size (ha)", breaks = NULL, labels = NULL))

ggsave(filename = paste0(pathexp1, "NN_feeding_trees.png"),
       dpi = 300, width = 25, height = 18, units = "cm")


# If clumpiness of seeds is higher with increasing distance between sleeping trees
dfexp1 %>% ggplot() +
  aes(x = NN_sleeping_trees, y = NN_seeds
      , color = n
      , shape = density
  ) +
  geom_point() +
  geom_smooth() +
  # scale_colour_viridis_d("magma") +
  # scale_color_manual(values = c("#922B21", "#D98880")) + # red density
  # scale_color_manual(values = c("#85C1E9", "#154360")) + # blue density
  # scale_color_manual(values = c("#922B21", "#D98880")) + # red n trees
  # scale_color_manual(values = c("#D4E6F1", "#85C1E9", "#2980B9 ", "#1F618D", "#154360")) + # blue n trees
  scale_color_gradient(low="#922B21", high="#D98880") +
  facet_grid(cols = vars(fragment_size), rows = vars(field.shape.factor)) +
  # facet_wrap(vars(fragment_size, field.shape.factor)) +
  # rename axis:
  xlab("NN distance between sleeping trees") +
  ylab("NN distance between seed dispersal events") +
  # ggtitle("Effect of distance between sleeping trees") +
  # create secondary axis:
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Fragment shape\n", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Fragment size (ha)", breaks = NULL, labels = NULL))

ggsave(filename = paste0(pathexp1, "NN_sleeping_trees.png"),
       dpi = 300, width = 25, height = 18, units = "cm")









