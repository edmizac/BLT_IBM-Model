library("nlrx")
library("here")
library("dplyr")
library("stringr")
library("ggplot2")
library("corrplot")
library('caret')
library("fitdistrplus")
library("xlsx")


theme_set(theme_bw(base_size = 15))


# pathexp1 <- "D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest/Experiment1/batch_all/without_SDD/"
pathexp1 <- "D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest/Experiment1/batch_all/"

filesexp1 <- list.files(pathexp1, pattern = ".rds") ; length(filesexp1)

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
dfexp1 <- readRDS("D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest/exampleRDS.rds")
dfexp1 <- nlrx::getsim(dfexp1, "simoutput")

# i <- 999
i <- 1


for (i in filesexp1) {
  # i <- filesexp1[2]
  
  if (i == 1) { 
    # i <- filesexp1[500]
    dfexp1 <- readRDS(paste0(pathexp1, i)) %>% getsim(., "simoutput") %>% as.data.frame() 
    # dfexp1$`patch-size-ha`
    # dfexp1$g_SDD
    
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
    
    # grab resource distribution information (random, clumped or ordered)
    # i <- filesexp1[576] # random
    # i <- filesexp1[990] # ordered
    # i <- filesexp1[967] # clumped
    i_info <- i %>% str_remove(., ".rds")
    i_pattern <- i_info %>% str_extract(., "[:lower:]{4,8}$")
    i_pattern
    
    Rpos <- i_info %>% str_locate(., "_R")
    NNpos <- i_info %>% str_locate(., "_NN")
    Ridx <- i %>% str_sub(., start = Rpos[2], end = NNpos[1]-1)
    Ridx <- Ridx %>% str_split("_")
    resource_pattern_R <- Ridx[[1]][1] %>% str_remove(., "R") %>% as.numeric()
    resource_pattern_R_p <- Ridx[[1]][2] %>% str_remove(., "p") %>% as.numeric()
    
    filei <- filei %>% 
      mutate(
        resource_pattern = i_pattern,
        resource_pattern_R = resource_pattern_R,
        resource_pattern_R_p = resource_pattern_R_p
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
      
      # grab resource distribution information (random, clumped or ordered)
      # i <- filesexp1[576] # random
      # i <- filesexp1[990] # ordered
      # i <- filesexp1[967] # clumped
      i_info <- i %>% str_remove(., ".rds")
      i_pattern <- i_info %>% str_extract(., "[:lower:]{4,8}$")
      i_pattern
      
      Rpos <- i_info %>% str_locate(., "_R")
      NNpos <- i_info %>% str_locate(., "_NN")
      Ridx <- i %>% str_sub(., start = Rpos[2], end = NNpos[1]-1)
      Ridx <- Ridx %>% str_split("_")
      resource_pattern_R <- Ridx[[1]][1] %>% str_remove(., "R") %>% as.numeric()
      resource_pattern_R_p <- Ridx[[1]][2] %>% str_remove(., "p") %>% as.numeric()
      
      filei <- filei %>% 
        mutate(
          resource_pattern = i_pattern,
          resource_pattern_R = resource_pattern_R,
          resource_pattern_R_p = resource_pattern_R_p
        )
      
      # glue them together
      dfexp1 <- dplyr::bind_rows(dfexp1, filei) %>% 
        mutate(
          `[run number]` = as.character(`[run number]`)
        )
      
      
    }
  }
}

anyNA(filei)
anyNA(dfexp1)

dfexp1 %>% glimpse()

# Take first row out as it is from the nlrx example file
dfexp1 <- dfexp1[-1, ]

# check for NAs
anyNA(dfexp1)
# dfexp1NA <- dfexp1 %>% dplyr::filter(across(no_days:g_n_unvisited_trees, ~is.na(.)))
dfexp1NA <- dfexp1 %>% dplyr::filter(!complete.cases(across(no_days:g_n_unvisited_trees))) # one run has dead monkeys
# dfexp1$g_n_unvisited_trees

# filter NA cases
dfexp1 <- anti_join(dfexp1, dfexp1NA)

# These are all true:
dfexp1 %>% str()
dfexp1$`feedingbout-on?` %>% str()
# filei$`feedingbout-on?` %>% str()

# This is = 1 (no repetitions or seeds in the nlrx run file)
dfexp1$`[run number]` %>% str()
# filei$`[run number]` %>% str()

# Stimmt!


# Lets check how many of those runs had dead monkeys. As there are NAs filtered above (with anti_join), maybe this procedure is not working completely
dfexp1 %>% dplyr::filter(`survived?` == "no") %>% count() # none! wow. Why? Because I made the simulations to drop in the NetLogo code if the monkeys were dying



# Wrangle data for our purposes
dfexp1 <- dfexp1 %>% 
  rename(fragment_size = `patch-size-ha`) %>%
  # mutate(field.shape.factor = as.factor(field.shape.factor),
  # mutate(density = as.factor(density),
  # fragment_size = as.factor(fragment_size)) %>% 
  rename_with(
    # across(vars(starts_with("g_")), .fns = paste0)
    ~ sub("g_", "", .x), starts_with("g_")
  )

dfexp1 <- dfexp1 %>% 
  mutate(
    resource_pattern = case_when(resource_pattern == "ordered" ~ "uniform",
                                 TRUE ~ resource_pattern)
  )

dfexp1$resource_pattern %>% unique()

dfexp1$fragment_size %>% as.factor() %>% unique() # 8 levels. fragment size = 0 does not exist
dfexp1$field.shape.factor %>% as.factor() %>% unique() # There should be 3 levels. field.shape.factor = 0 does not exist
dfexp1$n %>% as.factor() %>% unique() # 5 levels

# All the same file:
a <- dfexp1 %>% dplyr::filter(field.shape.factor == 0) ; dim(a) # 1 run with field.shape.factor = 0 (should be 3)
b <- dfexp1 %>% dplyr::filter(fragment_size == 0) ; dim(a) # 1 run with fragment size = 0 (should be 100)
c <- dfexp1 %>% dplyr::filter(density == 0) ; dim(a) # 1 run with fragment size = 0 (should be 100)


# Filter it out
dfexp1 <- dfexp1 %>% 
  dplyr::filter(density != 0) %>% 
  dplyr::filter(SDD != 0) %>% 
  droplevels()

dfexp1 %>% str()



# General plots -----


## Fragment plots -----
# (for suppl. material)


# For ggplot to work with factor levels as shape:
dfexp1gg <- dfexp1 %>% 
  mutate(
    density = as.factor(density),
    fragment_size = as.factor(fragment_size),
    field.shape.factor = as.factor(field.shape.factor)#,
    # n = as.factor(n)
  )


# Clumped patterns sometimes are lacking
dfexp1gg %>% 
  ggplot(
    aes(x = fragment_size, y = field.shape.factor
        , color = NN_feeding_trees
        , shape = density
        )
  ) +
  geom_point(size = 2, alpha = 0.8, 
             # position = position_dodge2(width = .8)
             position = position_jitter(width = .3, height = .08)
  ) +
  scale_shape_manual(values = c(1,4)) +
  scale_color_viridis_c(option = "inferno") + 
  xlab("Fragment size (ha)") +
  ylab("Fragment shape") +
  facet_wrap(vars(resource_pattern))

# ggsave(filename = paste0(pathexp1, "FragmentExamples.png"),
#        dpi = 300, width = 30, height = 17, units = "cm")





## Seed dispersal ----

# Our first interest is seed dispersal. Let's see

# For ggplot to work with factor levels as shape:
dfexp1gg <- dfexp1 %>% 
  mutate(
    density = as.factor(density),
    fragment_size = as.factor(fragment_size),
    field.shape.factor = as.factor(field.shape.factor)#,
    # n = as.factor(n)
  )
dfexp1gg %>% str()

# Check levels
dfexp1gg$fragment_size %>% levels()
dfexp1gg$field.shape.factor %>% levels()
dfexp1gg$density %>% levels()


dfexp1gg %>% glimpse()


# If distance between feeding trees generate longer dispersal distances
dfexp1gg %>% 
  mutate(
    fragment_size = as.numeric(as.character(fragment_size)),
    field.shape.factor = as.factor(as.character(field.shape.factor))
    ) %>% 
  ggplot() +
  aes(x = fragment_size, y = SDD
      # , color = density
      , shape = density
      , group = fragment_size
  ) +
  # geom_boxplot(width = 50) +
  geom_point(size = 2, alpha = 0.3
             , position = position_jitter(width = 30)
             ) +
  # geom_smooth() +
  # scale_colour_viridis_d("magma") +
  # scale_color_manual(values = c("#922B21", "#D98880")) + # red densitty
  # scale_color_manual(values = c("#85C1E9", "#154360")) + # blue densitty
  # scale_discrete_manual(aesthetics = "color", 
  #                       values = c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600", "#012333")) +
  # scale_color_gradient(low = "#ffc197", high = "#d23600") +
  scale_shape_manual(values = c(16,4)) +
  # facet_grid(cols = vars(fragment_size), rows = vars(field.shape.factor)) +
  # facet_grid(cols = vars(fragment_size), rows = vars(density)) +
  facet_wrap(vars(field.shape.factor)) +
  # facet_wrap(vars(fragment_size)) +
  # rename axis:
  xlab("Fragment size (ha)") +
  ylab("mean SSD (m)") +
  # create secondary axis:
  scale_x_continuous(
    sec.axis = sec_axis(~ . , name = "Fragment shape\n", breaks = NULL, labels = NULL)
    , limits = c(0, 1600)
    ) #+

# ggsave(filename = paste0(pathexp1, "SDD_fieldshapegrid.png"),
#        dpi = 300, width = 25, height = 18, units = "cm")


dfexp1gg %>% 
  mutate(
    fragment_size = as.numeric(as.character(fragment_size)),
    field.shape.factor = as.numeric(as.character(field.shape.factor))
  ) %>% 
  ggplot() +
  aes(x = fragment_size, y = SDD
      , color = field.shape.factor
      # , shape = density
  ) +
  geom_point(size = 3, alpha = 0.3, position = position_jitter(width = 20)) +
  # geom_smooth() +
  # scale_colour_viridis_d("magma") +
  # scale_color_manual(values = c("#fc1c03", "#922B21", "#D98880")) + # red densitty
  # scale_color_manual(values = c("#85C1E9", "#154360")) + # blue densitty
  # scale_discrete_manual(aesthetics = "color", 
  #                       values = c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600", "#012333")) +
  scale_color_gradient(low = "#ffc197", high = "#d23600") +
  # facet_wrap(vars(field.shape.factor)) +
  facet_wrap(vars(density)) +
  # rename axis:
  xlab("Fragment size (ha)") +
  ylab("mean SSD (m)") +
  # correct y scale:
  ylim(0, max(dfexp1gg$SDD)) +
  # create secondary axis:
  # scale_y_continuous(sec.axis = sec_axis(~ . , name = "Fragment shape\n", breaks = NULL, labels = NULL)) +
  scale_x_continuous(
    sec.axis = sec_axis(~ . , name = "Density\n", breaks = NULL, labels = NULL)
    , limits = c(0, 1600)
    )

# ggsave(filename = paste0(pathexp1, "SDD_densitygrid.png"),
#        dpi = 300, width = 25, height = 18, units = "cm")






# If clumpiness of seeds is higher with increasing distance between feeding trees
dfexp1gg %>% 
  mutate(
    fragment_size = as.numeric(as.character(fragment_size)),
    field.shape.factor = as.numeric(as.character(field.shape.factor))
  ) %>% 
  ggplot() +
  aes(x = NN_feeding_trees, y = NN_seeds
      # , color = field.shape.factor
      # , color = `hr-size-final`
      # , color = density
      , shape = density
  ) +
  geom_point(size = 2, alpha = 0.3) +
  # geom_smooth() +
  # scale_colour_viridis_d("magma") +
  # scale_color_manual(values = c("#922B21", "#D98880")) + # red density
  # scale_color_manual(values = c("#85C1E9", "#154360")) + # blue density
  scale_shape_manual(values = c(16,4)) +
  facet_grid(cols = vars(fragment_size), rows = vars(field.shape.factor)) +
  # facet_wrap(vars(fragment_size, field.shape.factor)) +
  # rename axis:
  xlab("NN distance between fruiting trees (m)") +
  ylab("NN distance between seed dispersal events (m)") +
  # ggtitle("Effect of distance between fruiting trees") +
  # create secondary axis:
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Fragment shape\n", breaks = NULL, labels = NULL)) +
  scale_x_continuous(
    sec.axis = sec_axis(~ . , name = "Fragment size (ha)", breaks = NULL, labels = NULL)
    , limits = c(30, 90)
    )

# ggsave(filename = paste0(pathexp1, "NN_feeding_trees.png"),
#        dpi = 300, width = 30, height = 18, units = "cm")


# If clumpiness of seeds is higher with increasing distance between sleeping trees
dfexp1gg %>% 
  mutate(
    n = as.factor(n),
    fragment_size = as.numeric(as.character(fragment_size)),
    field.shape.factor = as.numeric(as.character(field.shape.factor))
  ) %>% 
  ggplot() +
  aes(x = NN_sleeping_trees, y = NN_seeds
      # , color = density
      , shape = density
  ) +
  geom_point(size = 2, alpha = 0.3) +
  # geom_smooth() +
  # scale_colour_viridis_d("magma") +
  # scale_color_manual(values = c("#922B21", "#D98880")) + # red density
  # scale_color_manual(values = c("#85C1E9", "#154360")) + # blue density
  # scale_color_manual(values = c("#922B21", "#D98880")) + # red n trees
  scale_shape_manual(values = c(16,4)) +
  facet_grid(cols = vars(fragment_size), rows = vars(field.shape.factor)) +
  # facet_wrap(vars(fragment_size, field.shape.factor)) +
  # rename axis:
  xlab("NN distance between sleeping trees (m)") +
  ylab("NN distance between seed dispersal events (m)") +
  # ggtitle("Effect of distance between sleeping trees") +
  # create secondary axis:
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Fragment shape\n", breaks = NULL, labels = NULL)) +
  scale_x_continuous(
    sec.axis = sec_axis(~ . , name = "Fragment size (ha)", breaks = NULL, labels = NULL)
    , limits = c(50, 550)
    ) +
  theme(axis.text.x = element_text(size=9))

ggsave(filename = paste0(pathexp1, "NN_sleeping_trees.png"),
       dpi = 300, width = 30, height = 18, units = "cm")




# dfexp1_bkp <- dfexp1
dfexp1 <- dfexp1_bkp
dfexp1 %>% glimpse()

# Liner regressions -----

# First lest transform all predictores with Z transf

dfexp1 <- dfexp1 %>%
  mutate(
    fragment_size_num = as.numeric(as.character(fragment_size)),
    # field.shape.factor_num = field.shape.factor,
    # density_num = density,
    fragment_size = as.factor(fragment_size),
    field.shape.factor = as.factor(field.shape.factor),
    density = as.factor(density)
  )

dfexp1_num <- dfexp1 %>%
  # mutate(
  #   fragment_size = as.numeric(as.character(fragment_size)),
  #   field.shape.factor = as.numeric(as.character(field.shape.factor)),
  #   density = as.numeric(as.character(density))
  # ) %>% 
  dplyr::select_if(is.numeric) %>%
  dplyr::select(c(`[step]`:ncol(.))) #, 
# output variables:
# -NN_seeds, -SDD, -SDD_sd, NN_seeds,
# variables of interest (are not predictors):
# - fragment_size, - field.shape.factor, -density,
# These does not matter:
# -R_seeds_p))

## Corrplot -------------------------
dfexp1_num <- dfexp1_num %>% 
  na.omit() %>% # There must be no NA
  dplyr::select(
    # output variables:
    -NN_seeds, -SDD, -SDD_sd,
    # variables of interest (are factors):
    # -fragment_size_num, 
    # -field.shape.factor_num, -density_num,
    # These does not matter:
    -R_seeds, -R_seeds_p, -siminputrow, -n
  )


# And z transform all predictors
scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

z_transf <- dfexp1_num %>%
  # dplyr::select(
  #   fragment_size:n_unvisited_trees
  # ) %>%
  mutate_all(
    ~scale(.)[, 1]
  ) %>%
  rename_with(
    .fn = ~paste0(., "_z")
  )

# anyNA(dfexp1_num)
# dfexp1_num %>% purrr::map(., ~sum(is.nan(.)==TRUE))
# dfexp1_num %>% purrr::map(., ~sum(is.na(.)==TRUE))



M <- z_transf %>% 
  cor(. 
      #, use="pairwise.complete.obs"
      , method = "spearman"
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
                   # , mar = c(0,0,0,0)
)
# # Save corrplot
# pdf(file = paste0(pathexp1, "Corrplot.pdf"))
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
# , mar = c(0,0,0,0)
# )
# dev.off()


## Remove correlated variables -----
# dim(dfexp1_num)
# colnames(dfexp1_num)

hc = findCorrelation(M, cutoff=0.75) # putt any value as a "cutoff" 
hc = sort(hc)
hc

# Predictors to be dropped (high correlation)
colnames(z_transf)[hc]

dfexp1_r <-  z_transf[,-c(hc)]
dim(dfexp1_r)
colnames(dfexp1_r)

anyNA(dfexp1_r)
dfexp1_r %>% purrr::map(., ~sum(is.nan(.)==TRUE))
dfexp1_r %>% purrr::map(., ~sum(is.na(.)==TRUE))


# dfexp1_r <- dfexp1_r %>% bind_cols(z_transf)


# we want to understand the effect of multiple variables on SDD and NN_seeds, so we will produce one model for each
# while filtering out correlated variables

# But less first transform the area variables (forest size and home range size) and distance (SDD, NN) for when we use it as response variables

dfexp1_var <- dfexp1 %>% 
  mutate(
    fragment_size_log10 = log10(as.numeric(as.character(fragment_size_num))),
    hr_size_predicted_log10 = log10(`hr-size-final`),
    KDE_95_log10 = log10(KDE_95),
    KDE_50_log10 = log10(KDE_50),
    SDD_sqrt = sqrt(SDD),
    SDD_sdd_sqrt = sqrt(SDD_sd),
    NN_seeds_sqrt = sqrt(NN_seeds),
    NN_seeds_log10 = log10(NN_seeds),
    NN_sleeping_trees_sqrt = sqrt(NN_sleeping_trees),
    NN_sleeping_trees_sqrt = sqrt(NN_feeding_trees),
    DPL_sqrt = sqrt(DPL),
    DPL_sd_sqrt = sqrt(DPL_sd)
    
  ) %>% 
  dplyr::select(fragment_size_log10:DPL_sd_sqrt)


dfexp1_r <- dfexp1_r %>% bind_cols(dfexp1_var)
dfexp1_r %>% colnames()

dfexp1_r <- dfexp1 %>% 
  dplyr::select(c(
    fragment_size, field.shape.factor, density, 
    NN_seeds, SDD, SDD_sd
    )) %>% # grab the predictors (as factors) 
  bind_cols(dfexp1_r)

# Remove NAs for guaranteed Lms
# dfexp1_r %>% anyNA()
# dfexp1_r %>% dim()
# dfexp1_r %>% na.omit() %>% dim() # 0 NAs
# dfexp1_r <- dfexp1_r %>% na.omit()
# a <- dfexp1_r %>% na.omit()

dfexp1_r %>% glimpse() 
# dfexp1_r %>% is.na() %>% table()
# dfexp1 %>% is.na() %>% table()

# dfexp1_r$DPL %>% is.na() %>% table()

# dfex1_r <- dfexp1_r %>% 
#   mutate(
#     fragment_size = as.factor(fragment_size),
#     density = as.factor(density),
#     field.shape.factor = as.factor(field.shape.factor)
#     
#   )



## REGRESSIONS -----
# We are not using LMM because we don't have run repetitions
# HR sizes are highly correlated with SDD, so we are not using it. As density is a predictor of home range size, we will use it instead


### SDD -----

# Check distributions #http://www.di.fc.ul.pt/~jpn/r/distributions/fitting.html
plotdist(dfexp1_r$SDD, histo = TRUE, demp = TRUE)
plotdist(dfexp1_r$SDD_sqrt, histo = TRUE, demp = TRUE)

# Find distribution
descdist(dfexp1_r$SDD,  discrete = FALSE)
descdist(dfexp1_r$SDD,  discrete = FALSE, boot = 500)
descdist(dfexp1_r$SDD_sqrt,  discrete = FALSE)
descdist(dfexp1_r$SDD_sqrt,  discrete = FALSE, boot = 500)

# Looks like we can approach a normal, lognormal or gamma. Lets test them.

# Let's try the normal first
fit_n <- fitdist(dfexp1_r$SDD, "norm")
ks.test(dfexp1_r$SDD, "pnorm", mean = fit_n$estimate[1], sd = fit_n$estimate[2]) # nope
fit_n_sqrt <- fitdist(dfexp1_r$SDD_sqrt, "norm")
ks.test(dfexp1_r$SDD_sqrt, "pnorm", mean = fit_n$estimate[1], sd = fit_n$estimate[2]) # nope

# Weibull
fit_w  <- fitdist(dfexp1_r$SDD, "weibull")
ks.test(dfexp1_r$SDD, "pweibull", shape = fit_w$estimate[1], scale = fit_w$estimate[2]) # Nope
fit_w_sqrt  <- fitdist(dfexp1_r$SDD_sqrt, "weibull")
ks.test(dfexp1_r$SDD_sqrt, "pweibull", shape = fit_w$estimate[1], scale = fit_w$estimate[2]) # Nope

# Log normal
fit_ln  <- fitdist(dfexp1_r$SDD, "lnorm")
ks.test(dfexp1_r$SDD, "plnorm", meanlog = fit_ln$estimate[1], sdlog = fit_ln$estimate[2]) # Yes!
summary(fit_ln)
fit_ln_sqrt  <- fitdist(dfexp1_r$SDD_sqrt, "lnorm")
ks.test(dfexp1_r$SDD_sqrt, "plnorm", meanlog = fit_ln$estimate[1], sdlog = fit_ln$estimate[2]) # Nope!
summary(fit_ln_sqrt)

# Gamma
fit_g <- fitdist(dfexp1_r$SDD, "gamma")
ks.test(dfexp1_r$SDD, "pgamma", shape = fit_g$estimate[1], rate = fit_g$estimate[2]) # Yes also!
fit_g_sqrt  <- fitdist(dfexp1_r$SDD_sqrt, "gamma")
ks.test(dfexp1_r$SDD_sqrt, "pgamma", shape = fit_g$estimate[1], rate = fit_g$estimate[2]) # Nope!


# Plot distrubtions
par(mfrow=c(2,2))
plot.legend <- c("Normal", "Weibull", "lognormal", "gamma")
denscomp(list(fit_n, fit_w, fit_g, fit_ln), fitlwd = 2, legendtext = plot.legend)
cdfcomp (list(fit_n, fit_w, fit_g, fit_ln), fitlwd = 2, legendtext = plot.legend)
qqcomp  (list(fit_n, fit_w, fit_g, fit_ln), fitlwd = 2, legendtext = plot.legend)
ppcomp  (list(fit_n, fit_w, fit_g, fit_ln), fitlwd = 2, legendtext = plot.legend)

# With sqrt SDD
plot.legend <- c("Normal", "Weibull", "lognormal", "gamma")
denscomp(list(fit_n_sqrt, fit_w_sqrt, fit_g_sqrt, fit_ln_sqrt), fitlwd = 2, legendtext = plot.legend)
cdfcomp (list(fit_n_sqrt, fit_w_sqrt, fit_g_sqrt, fit_ln_sqrt), fitlwd = 2, legendtext = plot.legend)
qqcomp  (list(fit_n_sqrt, fit_w_sqrt, fit_g_sqrt, fit_ln_sqrt), fitlwd = 2, legendtext = plot.legend)
ppcomp  (list(fit_n_sqrt, fit_w_sqrt, fit_g_sqrt, fit_ln_sqrt), fitlwd = 2, legendtext = plot.legend)


# Let's see if we can find another test
library("actuar")
gofstat(list(fit_n, fit_w, fit_g, fit_ln), fitnames = c("Normal", "Weibull", "Gamma", "Lognormal"))
gofstat(list(fit_n_sqrt, fit_w_sqrt, fit_g_sqrt, fit_ln_sqrt), fitnames = c("Normal", "Weibull", "Gamma", "Lognormal"))

# According to the Kolgomorov-Smirnov and the AIC, we should use the lognormal
# And comparing AICs and Kolgomorov-Smirnov statistics (and also inspecting graphics visually) we can see that the non-transformed SDD has better fit

# Fitting the GLM with log normal
# PS: we fit a log normal by transforming Y to log: https://stats.stackexchange.com/questions/21447/how-to-specify-a-lognormal-distribution-in-the-glm-family-argument-in-r
library("lme4")


dfexp1_r <- dfexp1_r %>% 
  rename(
    step_z = `[step]_z`
  )

dfexp1_r %>% glimpse()

glm1 <- glm(log(SDD) ~ as.factor(density), family= gaussian(link='identity'), data=dfexp1_r)
glm2 <- glm(log(SDD) ~ as.factor(density)+ fragment_size_num_z, family= gaussian(link='identity'), data=dfexp1_r)
glm3 <- glm(log(SDD) ~ as.factor(density)+ fragment_size_num_z + as.factor(field.shape.factor), family= gaussian(link='identity'), data=dfexp1_r)

glm4 <- glm(log(SDD) ~ as.factor(density)+ fragment_size_num_z + as.factor(field.shape.factor), family= gaussian(link='identity'), data=dfexp1_r)
glm5 <- glm(log(SDD) ~ as.factor(density)+ fragment_size_num_z + as.factor(field.shape.factor) + step_z, family= gaussian(link='identity'), data=dfexp1_r)
glm6 <- glm(log(SDD) ~ as.factor(density)+ fragment_size_num_z + as.factor(field.shape.factor) + step_z + NN_feeding_trees_z, family= gaussian(link='identity'), data=dfexp1_r)
# glm7 <- glm(log(SDD) ~ as.factor(density)+ fragment_size_num_z + as.factor(field.shape.factor) + step_z + NN_feeding_trees_z, NN_sleeping_trees_z, family= gaussian(link='identity'), data=dfexp1_r)

glm8 <- glm(log(SDD) ~ as.factor(density)+ fragment_size_num_z + as.factor(field.shape.factor) + step_z + NN_feeding_trees_z + #NN_sleeping_trees_z
              energy_stored_z, family= gaussian(link='identity'), data=dfexp1_r)
glm9 <- glm(log(SDD) ~ as.factor(density)+ fragment_size_num_z + as.factor(field.shape.factor) + step_z + NN_feeding_trees_z + #NN_sleeping_trees_z
              energy_stored_z + DPL_z, family= gaussian(link='identity'), data=dfexp1_r)
glm10 <- glm(log(SDD) ~ as.factor(density)+ fragment_size_num_z + as.factor(field.shape.factor) + step_z + NN_feeding_trees_z + #NN_sleeping_trees_z
              energy_stored_z + DPL_z + MR_sd_z, family= gaussian(link='identity'), data=dfexp1_r)

glm11 <- glm(log(SDD) ~ as.factor(density) + fragment_size_num_z + as.factor(field.shape.factor) + step_z + NN_feeding_trees_z + #NN_sleeping_trees_z
            energy_stored_z + DPL_z + MR_sd_z + PT_z, family= gaussian(link='identity'), data=dfexp1_r)

glm12 <- glm(log(SDD) ~ as.factor(density) + fragment_size_num_z + as.factor(field.shape.factor) + step_z + NN_feeding_trees_z + #NN_sleeping_trees_z
            energy_stored_z + DPL_z + MR_sd_z + PT_z + n_unvisited_trees_z, family= gaussian(link='identity'), data=dfexp1_r)
glm13 <- glm(log(SDD) ~ as.factor(density) + fragment_size_num_z + as.factor(field.shape.factor) + step_z + NN_feeding_trees_z + #NN_sleeping_trees_z
               energy_stored_z + DPL_z + MR_sd_z + PT_z + n_unvisited_trees_z + p_feeding_z, family= gaussian(link='identity'), data=dfexp1_r)


glm14 <- glm(log(SDD) ~ as.factor(density) + fragment_size_num_z + as.factor(field.shape.factor) + step_z + NN_feeding_trees_z + #NN_sleeping_trees_z
               energy_stored_z + DPL_z + MR_sd_z + PT_z + n_unvisited_trees_z + p_feeding_z + p_foraging_z, family= gaussian(link='identity'), data=dfexp1_r)
glm15 <- glm(log(SDD) ~ as.factor(density) + fragment_size_num_z + as.factor(field.shape.factor) + step_z + NN_feeding_trees_z + #NN_sleeping_trees_z
               energy_stored_z + DPL_z + MR_sd_z + PT_z + n_unvisited_trees_z + p_feeding_z + p_resting_z, family= gaussian(link='identity'), data=dfexp1_r)
glm16 <- glm(log(SDD) ~ as.factor(density) + fragment_size_num_z + as.factor(field.shape.factor) + step_z + NN_feeding_trees_z + #NN_sleeping_trees_z
               energy_stored_z + DPL_z + MR_sd_z + PT_z + n_unvisited_trees_z + p_feeding_z + p_resting_z + p_traveling_z, family= gaussian(link='identity'), data=dfexp1_r)


# Model selection
library("MuMIn")
AIC.table  <- MuMIn::model.sel(glm1, glm2, glm3, glm4, glm5, glm6, #glm7, 
                               glm8, glm9, glm10, glm11, glm12, glm13,
                               glm14, glm15, glm16
                               )
AIC.table

AIC.table <- AIC.table[ , c("df", "logLik", "AICc", "delta")]
AIC.table

# Save table
# AIC.table %>% write.csv(here("Model_simulations", "Exp1_run", "AIC_table_SDD.csv"))
# AIC.table %>% write.xlsx(here("Model_simulations", "Exp1_run", "AIC_table_SDD.xlsx"))



# Summary
summary(glm16) # best

# Save summary as table
glm16_res <-summary.glm(glm16)$coefficients
# glm16_res %>% write.xlsx(here("Model_simulations", "Exp1_run", "Best_model_SDD.xlsx"))
# summary(glm12) 
# summary(glm11)
# summary(glm10)

# Check VIF
car::vif(glm16) # test for multicolinearity (https://analises-ecologicas.com/cap7.html#regress%C3%A3o-linear-simples)
car::vif(glm16, type = "predictor") # Huge correlation!

# Plot model
library("sjPlot")
plot_model(glm16)
plot_model(glm16, transform = NULL)
plot_model(glm16, show.values = TRUE, value.offset = .4)
# par(mfrow=c(2,2))
plot_model(glm16, type = "est")
plot_model(glm16, type = "std")
plot_model(glm16, type = "diag")
p <- plot_model(glm16, type = "diag")
p <- gridExtra::grid.arrange(grobs=p)

# Save model plot
# ggsave(p, filename = here("Model_simulations", "Exp1_run", "Best_model_SDD_plot.png"),
#               dpi = 300, width = 30, height = 25, units = "cm")


plot_model(glm16, type = "res") # I don't know how to interpret this
plot_model(glm16, type = "pred") 
p1 <- plot_model(glm16, type = "pred") 

# Various models in a grid
plot_models(glm16, glm1, grid = TRUE)



### NN seeds -----

# Check distributions #http://www.di.fc.ul.pt/~jpn/r/distributions/fitting.html
plotdist(dfexp1_r$NN_seeds, histo = TRUE, demp = TRUE)
plotdist(dfexp1_r$NN_seeds_sqrt, histo = TRUE, demp = TRUE)
plotdist(dfexp1_r$NN_seeds_log10, histo = TRUE, demp = TRUE)

# Find distribution
descdist(dfexp1_r$NN_seeds,  discrete = FALSE)
descdist(dfexp1_r$NN_seeds,  discrete = FALSE, boot = 500)
descdist(dfexp1_r$NN_seeds_sqrt,  discrete = FALSE)
descdist(dfexp1_r$NN_seeds_sqrt,  discrete = FALSE, boot = 500)
descdist(dfexp1_r$NN_seeds_log10,  discrete = FALSE)
descdist(dfexp1_r$NN_seeds_log10,  discrete = FALSE, boot = 500)

# Looks like we can approach a normal, lognormal or gamma. Lets test them.

# Let's try the normal first
fit_n <- fitdist(dfexp1_r$NN_seeds, "norm")
ks.test(dfexp1_r$NN_seeds, "pnorm", mean = fit_n$estimate[1], sd = fit_n$estimate[2]) # nope
fit_n_sqrt <- fitdist(dfexp1_r$NN_seeds_sqrt, "norm")
ks.test(dfexp1_r$NN_seeds_sqrt, "pnorm", mean = fit_n$estimate[1], sd = fit_n$estimate[2]) # nope

# Weibull
fit_w  <- fitdist(dfexp1_r$NN_seeds, "weibull")
ks.test(dfexp1_r$NN_seeds, "pweibull", shape = fit_w$estimate[1], scale = fit_w$estimate[2]) # Nope
fit_w_sqrt  <- fitdist(dfexp1_r$NN_seeds_sqrt, "weibull")
ks.test(dfexp1_r$NN_seeds_sqrt, "pweibull", shape = fit_w$estimate[1], scale = fit_w$estimate[2]) # Nope

# Log normal
fit_ln  <- fitdist(dfexp1_r$NN_seeds, "lnorm")
ks.test(dfexp1_r$NN_seeds, "plnorm", meanlog = fit_ln$estimate[1], sdlog = fit_ln$estimate[2]) # Nope
summary(fit_ln)
fit_ln_sqrt  <- fitdist(dfexp1_r$NN_seeds_sqrt, "lnorm")
ks.test(dfexp1_r$NN_seeds_sqrt, "plnorm", meanlog = fit_ln$estimate[1], sdlog = fit_ln$estimate[2]) # Nope
summary(fit_ln_sqrt)

# Gamma
fit_g <- fitdist(dfexp1_r$NN_seeds, "gamma")
ks.test(dfexp1_r$NN_seeds, "pgamma", shape = fit_g$estimate[1], rate = fit_g$estimate[2]) # Nope
fit_g_sqrt  <- fitdist(dfexp1_r$NN_seeds_sqrt, "gamma")
ks.test(dfexp1_r$NN_seeds_sqrt, "pgamma", shape = fit_g$estimate[1], rate = fit_g$estimate[2]) # Nope


# Let's try with log10
fit_n_10 <- fitdist(dfexp1_r$NN_seeds_log10, "norm")
ks.test(dfexp1_r$NN_seeds_log10, "pnorm", mean = fit_n$estimate[1], sd = fit_n$estimate[2]) # nope

# Weibull
fit_w_10  <- fitdist(dfexp1_r$NN_seeds_log10, "weibull")
ks.test(dfexp1_r$NN_seeds_log10, "pweibull", shape = fit_w$estimate[1], scale = fit_w$estimate[2]) # Nope

# Log normal
fit_ln_10  <- fitdist(dfexp1_r$NN_seeds_log10, "lnorm")
ks.test(dfexp1_r$NN_seeds_log10, "plnorm", meanlog = fit_ln$estimate[1], sdlog = fit_ln$estimate[2]) # Nope
summary(fit_ln)

# Gamma
fit_g_10 <- fitdist(dfexp1_r$NN_seeds_log10, "gamma")
ks.test(dfexp1_r$NN_seeds_log10, "pgamma", shape = fit_g$estimate[1], rate = fit_g$estimate[2]) # Nope


# Nothing yet. Let's try other distributions

# fit_g <- MASS::fitdistr(dfexp1_r$NN_seeds, "chi-squared", df = 3)
# ks.test(dfexp1_r$NN_seeds, "pgamma", shape = fit_g$estimate[1], rate = fit_g$estimate[2]) # Nope
fit_exp <- fitdist(dfexp1_r$NN_seeds, "exp")
ks.test(dfexp1_r$NN_seeds, "pexp", rate = fit_g$estimate[1]) # Nope

fit_exp_sqrt <- fitdist(dfexp1_r$NN_seeds_sqrt, "exp")
ks.test(dfexp1_r$NN_seeds_sqrt, "pexp", rate = fit_g$estimate[1]) # Nope

fit_exp_10 <- fitdist(dfexp1_r$NN_seeds_log10, "exp")
ks.test(dfexp1_r$NN_seeds_log10, "pexp", rate = fit_g$estimate[1]) # Nope


# As I could not find any distribution, I'll plot them and infer visually


# Plot distrubtions

# With NN_seeds
par(mfrow=c(2,2))
plot.legend <- c("Normal", "Weibull", "lognormal", "gamma", "exp")
denscomp(list(fit_n, fit_w, fit_g, fit_ln, fit_exp), fitlwd = 2, legendtext = plot.legend)
cdfcomp (list(fit_n, fit_w, fit_g, fit_ln, fit_exp), fitlwd = 2, legendtext = plot.legend)
qqcomp  (list(fit_n, fit_w, fit_g, fit_ln, fit_exp), fitlwd = 2, legendtext = plot.legend)
ppcomp  (list(fit_n, fit_w, fit_g, fit_ln, fit_exp), fitlwd = 2, legendtext = plot.legend)

# This looks good enough to use gamma

# With sqrt NN_seeds
plot.legend <- c("Normal", "Weibull", "lognormal", "gamma", "exp")
denscomp(list(fit_n_sqrt, fit_w_sqrt, fit_g_sqrt, fit_ln_sqrt, fit_exp_sqrt), fitlwd = 2, legendtext = plot.legend)
cdfcomp (list(fit_n_sqrt, fit_w_sqrt, fit_g_sqrt, fit_ln_sqrt, fit_exp_sqrt), fitlwd = 2, legendtext = plot.legend)
qqcomp  (list(fit_n_sqrt, fit_w_sqrt, fit_g_sqrt, fit_ln_sqrt, fit_exp_sqrt), fitlwd = 2, legendtext = plot.legend)
ppcomp  (list(fit_n_sqrt, fit_w_sqrt, fit_g_sqrt, fit_ln_sqrt, fit_exp_sqrt), fitlwd = 2, legendtext = plot.legend)
# These ones are bad

# With log10 NN_seeds
plot.legend <- c("Normal", "Weibull", "lognormal", "gamma", "exp")
denscomp(list(fit_n_10, fit_w_10, fit_g_10, fit_ln_10, fit_exp_10), fitlwd = 2, legendtext = plot.legend)
cdfcomp (list(fit_n_10, fit_w_10, fit_g_10, fit_ln_10, fit_exp_10), fitlwd = 2, legendtext = plot.legend)
qqcomp  (list(fit_n_10, fit_w_10, fit_g_10, fit_ln_10, fit_exp_10), fitlwd = 2, legendtext = plot.legend)
ppcomp  (list(fit_n_10, fit_w_10, fit_g_10, fit_ln_10, fit_exp_10), fitlwd = 2, legendtext = plot.legend)
# even worse



# Let's see if we can find another test
library("actuar")
gofstat(list(fit_n, fit_w, fit_g, fit_ln), fitnames = c("Normal", "Weibull", "Gamma", "Lognormal"))
gofstat(list(fit_n_sqrt, fit_w_sqrt, fit_g_sqrt, fit_ln_sqrt), fitnames = c("Normal", "Weibull", "Gamma", "Lognormal"))
gofstat(list(fit_n_10, fit_w_10, fit_g_10, fit_ln_10), fitnames = c("Normal", "Weibull", "Gamma", "Lognormal"))

# As the absolute value of AIC says nothing and we can't compare different fitted models and the lognormal is 
# frequently ranked as the best distribution by the KS test, I will proceed by using the lognormal with the untransformed data
fit_ln  <- fitdist(dfexp1_r$NN_seeds, "lnorm")
ks.test(dfexp1_r$NN_seeds, "plnorm", meanlog = fit_ln$estimate[1], sdlog = fit_ln$estimate[2]) # Nope
summary(fit_ln)



# Fitting the GLM with log normal
# PS: we fit a log normal by transforming Y to log: https://stats.stackexchange.com/questions/21447/how-to-specify-a-lognormal-distribution-in-the-glm-family-argument-in-r
library("lme4")


dfexp1_r <- dfexp1_r %>% 
  rename(
    step_z = `[step]_z`
  )

dfexp1_r %>% glimpse()

glm1 <- glm(log(NN_seeds) ~ as.factor(density), family= gaussian(link='identity'), data=dfexp1_r)
glm2 <- glm(log(NN_seeds) ~ as.factor(density)+ fragment_size_num_z, family= gaussian(link='identity'), data=dfexp1_r)
glm3 <- glm(log(NN_seeds) ~ as.factor(density)+ fragment_size_num_z + as.factor(field.shape.factor), family= gaussian(link='identity'), data=dfexp1_r)

glm4 <- glm(log(NN_seeds) ~ as.factor(density)+ fragment_size_num_z + as.factor(field.shape.factor), family= gaussian(link='identity'), data=dfexp1_r)
glm5 <- glm(log(NN_seeds) ~ as.factor(density)+ fragment_size_num_z + as.factor(field.shape.factor) + step_z, family= gaussian(link='identity'), data=dfexp1_r)
glm6 <- glm(log(NN_seeds) ~ as.factor(density)+ fragment_size_num_z + as.factor(field.shape.factor) + step_z + NN_feeding_trees_z, family= gaussian(link='identity'), data=dfexp1_r)
# glm7 <- glm(log(NN_seeds) ~ as.factor(density)+ fragment_size_num_z + as.factor(field.shape.factor) + step_z + NN_feeding_trees_z, NN_sleeping_trees_z, family= gaussian(link='identity'), data=dfexp1_r)

glm8 <- glm(log(NN_seeds) ~ as.factor(density)+ fragment_size_num_z + as.factor(field.shape.factor) + step_z + NN_feeding_trees_z + #NN_sleeping_trees_z
              energy_stored_z, family= gaussian(link='identity'), data=dfexp1_r)
glm9 <- glm(log(NN_seeds) ~ as.factor(density)+ fragment_size_num_z + as.factor(field.shape.factor) + step_z + NN_feeding_trees_z + #NN_sleeping_trees_z
              energy_stored_z + DPL_z, family= gaussian(link='identity'), data=dfexp1_r)
glm10 <- glm(log(NN_seeds) ~ as.factor(density)+ fragment_size_num_z + as.factor(field.shape.factor) + step_z + NN_feeding_trees_z + #NN_sleeping_trees_z
               energy_stored_z + DPL_z + MR_sd_z, family= gaussian(link='identity'), data=dfexp1_r)

glm11 <- glm(log(NN_seeds) ~ as.factor(density) + fragment_size_num_z + as.factor(field.shape.factor) + step_z + NN_feeding_trees_z + #NN_sleeping_trees_z
               energy_stored_z + DPL_z + MR_sd_z + PT_z, family= gaussian(link='identity'), data=dfexp1_r)

glm12 <- glm(log(NN_seeds) ~ as.factor(density) + fragment_size_num_z + as.factor(field.shape.factor) + step_z + NN_feeding_trees_z + #NN_sleeping_trees_z
               energy_stored_z + DPL_z + MR_sd_z + PT_z + n_unvisited_trees_z, family= gaussian(link='identity'), data=dfexp1_r)
glm13 <- glm(log(NN_seeds) ~ as.factor(density) + fragment_size_num_z + as.factor(field.shape.factor) + step_z + NN_feeding_trees_z + #NN_sleeping_trees_z
               energy_stored_z + DPL_z + MR_sd_z + PT_z + n_unvisited_trees_z + p_feeding_z, family= gaussian(link='identity'), data=dfexp1_r)


glm14 <- glm(log(NN_seeds) ~ as.factor(density) + fragment_size_num_z + as.factor(field.shape.factor) + step_z + NN_feeding_trees_z + #NN_sleeping_trees_z
               energy_stored_z + DPL_z + MR_sd_z + PT_z + n_unvisited_trees_z + p_feeding_z + p_foraging_z, family= gaussian(link='identity'), data=dfexp1_r)
glm15 <- glm(log(NN_seeds) ~ as.factor(density) + fragment_size_num_z + as.factor(field.shape.factor) + step_z + NN_feeding_trees_z + #NN_sleeping_trees_z
               energy_stored_z + DPL_z + MR_sd_z + PT_z + n_unvisited_trees_z + p_feeding_z + p_resting_z + p_traveling_z, family= gaussian(link='identity'), data=dfexp1_r)


# Model selection
library("MuMIn")
AIC.table  <- MuMIn::model.sel(glm1, glm2, glm3, glm4, glm5, glm6, #glm7,
                               glm8, glm9, glm10, glm11, glm12, glm13,
                               glm14, glm15
                               )
AIC.table

AIC.table <- AIC.table[ , c("df", "logLik", "AICc", "delta")]
AIC.table # glm14 and 15 are equally probable

# Save table
# AIC.table %>% write.csv(here("Model_simulations", "Exp1_run", "AIC_table_NNseeds.csv"))
# AIC.table %>% write.xlsx(here("Model_simulations", "Exp1_run", "AIC_table_NNseeds.xlsx"))



# Summary
summary(glm14) # best

# Save summary as table
glm14_res <-summary.glm(glm14)$coefficients
# glm14_res %>% write.xlsx(here("Model_simulations", "Exp1_run", "Best_model_NNseeds.xlsx"))
# summary(glm12)
# summary(glm11)
# summary(glm10)

# Check VIF
car::vif(glm14) # test for multicolinearity (https://analises-ecologicas.com/cap7.html#regress%C3%A3o-linear-simples)
car::vif(glm14, type = "predictor") # Huge correlation!

# Plot model
library("sjPlot")
plot_model(glm14)
plot_model(glm14, transform = NULL)
plot_model(glm14, show.values = TRUE, value.offset = .4)
# par(mfrow=c(2,2))
plot_model(glm14, type = "est")
plot_model(glm14, type = "std")
p <- plot_model(glm14, type = "diag")
p <- gridExtra::grid.arrange(grobs=p)

# Save model plot
# ggsave(p, filename = here("Model_simulations", "Exp1_run", "Best_model_NNseeds_plot.png"),
#        dpi = 300, width = 30, height = 25, units = "cm")



plot_model(glm14, type = "res") # I don't know how to interpret this
p1 <- plot_model(glm14, type = "pred")







# Various models in a grid
plot_models(glm14, glm16, grid = TRUE)
