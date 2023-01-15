library("nlrx")
library("here")
library("dplyr")
library("stringr")
library("ggplot2")
library("corrplot")

theme_set(theme_bw(base_size = 15))


# pathexp2 <- "D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest/Experiment2/withoutSDD/"
pathexp2 <- "D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest/Experiment2/"

filesexp2 <- list.files(pathexp2, pattern = ".rds")

#initialize dfexp2
dfexp2 <- readRDS(paste0(pathexp2, i)) %>% getsim(., "simoutput") %>% as.data.frame()
dfexp2 <- dfexp2[ -1, ]

rm(i)
i <- 1
for (i in filesexp2) {
  # i <- filesexp2[2]
  
  if (i == 1) { 
    dfexp2 <- readRDS(paste0(pathexp2, i)) %>% getsim(., "simoutput") %>% as.data.frame() 
    } 
  else {
    filei <- readRDS(paste0(pathexp2, i)) %>% getsim(., "simoutput") %>% as.data.frame()
    dfexp2 <- dplyr::bind_rows(dfexp2, filei)
  }
  
} 

dfexp2 %>% glimpse()

# Stimmt!


# Lets check how many of those runs had dead monkeys
dfexp2 %>% dplyr::filter(`survived?` == "no") %>% count() # none! wow. Why? Because I made the simulations to drop in the NetLogo code if the monkeys were dying

# Wrangle data for our purposes
dfexp2 <- dfexp2 %>% 
  rename(fragment_size = `patch-size-ha`) %>%
  # mutate(density = as.factor(density),
         # fragment_size = as.factor(fragment_size)) %>% 
  rename_with(
    # across(vars(starts_with("g_")), .fns = paste0)
    ~ sub("g_", "", .x), starts_with("g_")
  )

dfexp2$n %>% as.factor() %>% unique() # 5 levels
dfexp2$fragment_size %>% as.factor() %>% unique() # 5 levels
dfexp2$field.shape.factor %>% as.factor() %>% unique() # 5 levels
dfexp2$density %>% as.factor() %>% unique() # why are there 3 levels?
dfexp2 %>% dplyr::filter(density == 0.197) #only one observation lol
dfexp2 <- dfexp2 %>% 
  dplyr::filter(density != 0.197)


# dfexp2 %>% str()
  


# General plots -----



## Seed dispersal ----

# Our first interest is seed dispersal. Let's see

# If distance between feeding trees generate longer dispersal distances
dfexp2 %>% 
  mutate(n = as.factor(n)) %>% 
  ggplot() +
  aes(x = NN_feeding_trees, y = SDD
      , color = n
      , shape = density
  ) +
  geom_point() +
  # geom_smooth() +
  # scale_colour_viridis_d("magma") +
  # scale_color_manual(values = c("#922B21", "#D98880")) + # red densitty
  # scale_color_manual(values = c("#85C1E9", "#154360")) + # blue densitty
  scale_discrete_manual(aesthetics = "color", 
                        values = c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600")) +
  # scale_color_gradient(low = "#ffc197", high = "#d23600") +
  facet_grid(cols = vars(fragment_size), rows = vars(field.shape.factor)) +
  # facet_wrap(vars(fragment_size, field.shape.factor)) +
  # rename axis:
  xlab("NN of fruiting trees") +
  ylab("SSD (m)") +
  # create secondary axis:
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Fragment shape\n", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Fragment size (ha)\n", breaks = NULL, labels = NULL))

ggsave(filename = paste0(pathexp2, "SDDs.png"),
       dpi = 300, width = 25, height = 18, units = "cm")


# If clumpiness of seeds is higher with increasing distance between feeding trees
dfexp2 %>% 
  mutate(n = as.numeric(paste(n))) %>% 
  ggplot() +
  aes(x = NN_feeding_trees, y = NN_seeds
      , color = n
      , shape = density
  ) +
  geom_point() +
  # geom_smooth() +
  # scale_colour_viridis_d("magma") +
  # scale_color_manual(values = c("#922B21", "#D98880")) + # red density
  # scale_color_manual(values = c("#85C1E9", "#154360")) + # blue density
  # scale_color_manual(values = c("#922B21", "#D98880")) + # red n trees
  # scale_color_manual(values = c("#D4E6F1", "#85C1E9", "#2980B9 ", "#1F618D", "#154360")) + # blue n trees
  facet_grid(cols = vars(fragment_size), rows = vars(field.shape.factor)) +
  # facet_wrap(vars(fragment_size, field.shape.factor)) +
  # rename axis:
  xlab("NN distance between fruiting trees (m)") +
  ylab("NN distance between seed dispersal events (m)") +
  # ggtitle("Effect of distance between fruiting trees") +
  # create secondary axis:
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Fragment shape\n", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Fragment size (ha)", breaks = NULL, labels = NULL))

ggsave(filename = paste0(pathexp2, "NN_feeding_trees.png"),
       dpi = 300, width = 25, height = 18, units = "cm")


# If clumpiness of seeds is higher with increasing distance between sleeping trees
dfexp2 %>% 
  mutate(n = as.numeric(paste(n))) %>% 
  ggplot() +
  aes(x = NN_sleeping_trees, y = NN_seeds
      , color = n
      , shape = density
  ) +
  geom_point() +
  # geom_smooth() +
  # scale_colour_viridis_d("magma") +
  # scale_color_manual(values = c("#922B21", "#D98880")) + # red density
  # scale_color_manual(values = c("#85C1E9", "#154360")) + # blue density
  # scale_color_manual(values = c("#922B21", "#D98880")) + # red n trees
  # scale_color_manual(values = c("#D4E6F1", "#85C1E9", "#2980B9 ", "#1F618D", "#154360")) + # blue n trees
  scale_color_gradient(low="#922B21", high="#de8f87") +
  facet_grid(cols = vars(fragment_size), rows = vars(field.shape.factor)) +
  # facet_wrap(vars(fragment_size, field.shape.factor)) +
  # rename axis:
  xlab("NN distance between sleeping trees (m)") +
  ylab("NN distance between seed dispersal events (m)") +
  # ggtitle("Effect of distance between sleeping trees") +
  # create secondary axis:
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Fragment shape\n", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Fragment size (ha)", breaks = NULL, labels = NULL))

ggsave(filename = paste0(pathexp2, "NN_sleeping_trees.png"),
       dpi = 300, width = 25, height = 18, units = "cm")







# Liner regressions -----

## Corrplot -------------------------
dfexp2_num <- dfexp2 %>% 
  dplyr::select_if(is.numeric) %>% 
  dplyr::select(c(`[step]`:PT_sd, -density, -fragment_size, -field.shape.factor,
                  -n_unvisited_trees, -n_visited_trees, -R_seeds_p))

M <- dfexp2_num %>% 
  cor(. 
      #, use="pairwise.complete.obs"
      )
corrplot::corrplot(M
                   , method = "color"
                   , insig='blank'
                   , addCoef.col ='black'
                   , number.cex = .58
                   , number.digits = 1
                   # , order = 'AOE'
                   , order = 'hclust'
                   # , order = 'alphabet'
                   , diag=FALSE
                   , type = "lower"
                   , mar = c(0,0,1,0)
                   )
# Save corrplot
# pdf(file = paste0(pathexp2, "Corrplot.pdf"))
# corrplot::corrplot(M
#                    , method = "color"
#                    , insig='blank'
#                    , addCoef.col ='black'
#                    , number.cex = .58
#                    , number.digits = 1
#                    # , order = 'AOE'
#                    , order = 'hclust'
#                    # , order = 'alphabet'
#                    , diag=FALSE
#                    , type = "lower"
#                    , mar = c(0,0,1,0)
# )
# dev.off()


# Remove correlated variables
library('caret')
hc = findCorrelation(M, cutoff=0.5) # putt any value as a "cutoff" 
hc = sort(hc)
dfexp2_r = dfexp2[,-c(hc)]


# we want to understand the effect of multiple variables on SDD and NN_seeds, so we will produce one model for each
# while filtering out correlated variables

# But less first transform the area variables (forest size and home range size)

dfexp2_r <- dfexp2_r %>% 
  mutate(
    fragment_size_log10 = log10(fragment_size),
    hr_size_predicted_log10 = log10(`hr-size-final`),
    KDE_95_log10 = log10(KDE_95),
    KDE_50_log10 = log10(KDE_50)
  )


# Remove NAs for guaranteed Lms
dfexp2_r %>% anyNA()
dfexp2_r %>% dim()
dfexp2_r %>% na.omit() %>% dim() # 4 NAs
dfexp2_r <- dfexp2_r %>% na.omit()
a <- dfexp2_r %>% na.omit()

dfexp2_r %>% glimpse() 
# dfexp2_r %>% is.na() %>% table()
# dfexp2 %>% is.na() %>% table()

# dfexp2_r$DPL %>% is.na() %>% table()

## LM -----
# We are not using LMM because we don't have run repetitions
# HR sizes are highly correlated with SDD, so we are not using it. As density is a predictor of home range size, we will use it instead


### SDD -----
# Find distribution
descdist(dfexp2_r$SDD,  discrete = FALSE)

fitdist(dfexp2_r$SDD, "norm")
ks.test(dfexp2_r$SDD, "pnorm", mean = mean(dfexp2_r$SDD), sd = sd(dfexp2_r$SDD)) # nope

# *** PS: não consegui usar o DPL, diz que tem valores NA mesmo eu filtrando todo NA fora. Roda com DPL_sd

lm1 <- lm(SDD ~ density, data=dfexp2_r) 
lm2 <- lm(SDD ~ density, fragment_size_log10, data=dfexp2_r)
lm3 <- lm(SDD ~ density, fragment_size_log10 + field.shape.factor, data=dfexp2_r)
lm4 <- lm(SDD ~ density, fragment_size_log10 + field.shape.factor + NN_feeding_trees, data=dfexp2_r)
lm5 <- lm(SDD ~ density, fragment_size_log10 + field.shape.factor + n + NN_feeding_trees, data=dfexp2_r)
lm6 <- lm(SDD ~ density, fragment_size_log10 + field.shape.factor + n + NN_feeding_trees + NN_sleeping_trees, data=dfexp2_r)
lm7 <- lm(SDD ~ density, fragment_size_log10 + field.shape.factor + n + NN_feeding_trees + NN_sleeping_trees
          + DPL, data=dfexp2_r)
lm8 <- lm(SDD ~ density, fragment_size_log10 + field.shape.factor + n + NN_feeding_trees + NN_sleeping_trees
          + DPL + DPL_sd, data=dfexp2_r)
lm9 <- lm(SDD ~ density, fragment_size_log10 + field.shape.factor + n + NN_feeding_trees + NN_sleeping_trees
          + MR, data=dfexp2_r)
lm10 <- lm(SDD ~ density, fragment_size_log10 + field.shape.factor + n + NN_feeding_trees + NN_sleeping_trees
          + MR + MR_sd, data=dfexp2_r)
lm11 <- lm(SDD ~ density, fragment_size_log10 + field.shape.factor + n + NN_feeding_trees + NN_sleeping_trees
          + MR + MR_sd + PT, data=dfexp2_r)
lm12 <- lm(SDD ~ density, fragment_size_log10 + field.shape.factor + n + NN_feeding_trees + NN_sleeping_trees
          + MR + MR_sd + PT + PT_sd, data=dfexp2_r)
lm13 <- lm(SDD ~ density, fragment_size_log10 + field.shape.factor + n + NN_feeding_trees + NN_sleeping_trees
           + MR + MR_sd + PT + PT_sd + n_visited_trees, data=dfexp2_r)


### NN Seeds -----







