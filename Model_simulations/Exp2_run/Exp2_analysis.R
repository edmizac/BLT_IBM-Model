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


# pathexp2 <- "D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest/Experiment2/withoutSDD/"
pathexp2 <- "D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest/Experiment2/"

filesexp2 <- list.files(pathexp2, pattern = ".rds")

#initialize dfexp2
rm(i)
i <- 1
dfexp2 <- readRDS(paste0(pathexp2, filesexp2[i])) %>% getsim(., "simoutput") %>% as.data.frame()
dfexp2 <- dfexp2[ -1, ]

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

# dfexp2$n %>% as.factor() %>% unique() # 5 levels
# dfexp2$fragment_size %>% as.factor() %>% unique() # 5 levels
# dfexp2$field.shape.factor %>% as.factor() %>% unique() # 5 levels
# dfexp2$density %>% as.factor() %>% unique() # why are there 3 levels?

dfexp2 %>% dplyr::filter(density == 0.197) #only one observation lol
dfexp2 <- dfexp2 %>% 
  dplyr::filter(density != 0.197)


# dfexp2 %>% str()
  

# For ggplot to work with factor levels as shape:
dfexp2gg <- dfexp2 %>% 
  mutate(
    density = as.factor(density),
    fragment_size = as.factor(fragment_size)
  )


# General plots -----



## Seed dispersal ----

# Our first interest is seed dispersal. Let's see

# If distance between feeding trees generate longer dispersal distances
dfexp2gg %>% 
  mutate(n = as.factor(n)) %>% 
  ggplot() +
  aes(x = NN_feeding_trees, y = SDD
      , color = n
      , shape = density
  ) +
  geom_point(size = 2, alpha = 0.6) +
  # geom_smooth() +
  # scale_colour_viridis_d("magma") +
  # scale_color_manual(values = c("#922B21", "#D98880")) + # red densitty
  # scale_color_manual(values = c("#85C1E9", "#154360")) + # blue densitty
  scale_discrete_manual(aesthetics = "color", 
                        values = c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600", "#012333")) +
  # scale_color_gradient(low = "#ffc197", high = "#d23600") +
  facet_grid(cols = vars(fragment_size), rows = vars(field.shape.factor)) +
  # facet_wrap(vars(fragment_size, field.shape.factor)) +
  # rename axis:
  xlab("NN of fruiting trees") +
  ylab("SSD (m)") +
  # create secondary axis:
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Fragment shape\n", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Fragment size (ha)\n", breaks = NULL, labels = NULL))

# ggsave(filename = paste0(pathexp2, "SDDs.png"),
#        dpi = 300, width = 25, height = 18, units = "cm")


# If clumpiness of seeds is higher with increasing distance between feeding trees
dfexp2gg %>% 
  mutate(n = as.numeric(paste(n))) %>% 
  ggplot() +
  aes(x = NN_feeding_trees, y = NN_seeds
      , color = n
      , shape = density
  ) +
  geom_point(size = 2, alpha = 0.6) +
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

# ggsave(filename = paste0(pathexp2, "NN_feeding_trees.png"),
#        dpi = 300, width = 25, height = 18, units = "cm")


# If clumpiness of seeds is higher with increasing distance between sleeping trees
dfexp2gg %>% 
  mutate(n = as.numeric(paste(n))) %>% 
  ggplot() +
  aes(x = NN_sleeping_trees, y = NN_seeds
      , color = n
      , shape = density
  ) +
  geom_point(size = 2, alpha = 0.6) +
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

# ggsave(filename = paste0(pathexp2, "NN_sleeping_trees.png"),
#        dpi = 300, width = 25, height = 18, units = "cm")







# Liner regressions -----


# First lest transform all predictores with Z transf

dfexp2 <- dfexp2 %>%
  mutate(
    # fragment_size_num = as.numeric(as.character(fragment_size)),
    # field.shape.factor_num = field.shape.factor,
    # density_num = density,
    fragment_size = as.factor(fragment_size),
    field.shape.factor = as.factor(field.shape.factor),
    density = as.factor(density)
  )

dfexp2_num <- dfexp2 %>% 
  dplyr::select(c(`[step]`:n_unvisited_trees)) %>%
  dplyr::select(c(
                  # output variables:
                  -NN_seeds, -SDD, -SDD_sd,
                  # variables of interest (are not predictors):
                  - fragment_size, - field.shape.factor, -density, #-n,
                  # These does not matter:
                  -R_seeds_p, -R_seeds)
                ) %>% 
  dplyr::select_if(is.numeric)

# And z transform all predictors
scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

z_transf <- dfexp2_num %>% 
  # dplyr::select(
  #   fragment_size:n_unvisited_trees
  # ) %>% 
  mutate_all(
    ~scale(.)[, 1]
  ) %>% 
  rename_with(
    .fn = ~paste0(., "_z")
  )


## Corrplot -------------------------
M <- z_transf %>% 
  # dplyr::select(
    # output variables:
      # -NN_seeds, -SDD, -SDD_sd,
      # variables of interest (are not predictors):
      # -fragment_size, -field.shape.factor, -density,
      # These does not matter:
      # -R_seeds, -R_seeds_p
  # ) %>%
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
# , mar = c(0,0,0,0)
# )
# dev.off()


## Remove correlated variables -----
dim(z_transf)
colnames(z_transf)

hc = findCorrelation(M, cutoff=0.75) # putt any value as a "cutoff" 
hc = sort(hc)
hc

# Predictors to be dropped (high correlation)
colnames(z_transf)[hc]
colnames(z_transf)[3] # This is a predictor which I'm willing to keep, which correlates with n_visited_trees. Thus, I'll drop n_visited_trees
hc <- hc[2:length(hc)] # Take out n_z because I want it as predictor
hc <- hc[-5] # Take out PT_z because it correlates with p_feeding and I want it as predictor in place of p_feeding

dfexp2_r <-  z_transf[,-c(hc)] # dropped out variables
dim(dfexp2_r)
colnames(dfexp2_r)



# we want to understand the effect of multiple variables on SDD and NN_seeds, so we will produce one model for each
# while filtering out correlated variables

# But less first transform the area variables (forest size and home range size) and distance (SDD, NN) for when we use it as response variables

dfexp2_var <- dfexp2 %>% 
  mutate(
    fragment_size_log10 = log10(fragment_size_num),
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


dfexp2_r <- dfexp2_r %>% bind_cols(dfexp2_var)
dfexp2_r %>% colnames()

dfexp2_r <- dfexp2 %>% 
  dplyr::select(c(
    fragment_size, field.shape.factor, density, 
    NN_seeds, SDD, SDD_sd
  )) %>% # grab the predictors (as factors) 
  bind_cols(dfexp2_r)

# Remove NAs for guaranteed Lms
# dfexp2_r %>% anyNA()
# dfexp2_r %>% dim()
# dfexp2_r %>% na.omit() %>% dim() # 0 NAs
# dfexp2_r <- dfexp2_r %>% na.omit()
# a <- dfexp2_r %>% na.omit()

dfexp2_r %>% glimpse() 
# dfexp2_r %>% is.na() %>% table()
# dfexp2 %>% is.na() %>% table()

# dfexp2_r$DPL %>% is.na() %>% table()

# dfex1_r <- dfexp2_r %>% 
#   mutate(
#     fragment_size = as.factor(fragment_size),
#     density = as.factor(density),
#     field.shape.factor = as.factor(field.shape.factor)
#     
#   )


dfexp2_r <- dfexp2_r %>% 
  na.omit()

## REGRESSIONS -----
# We are not using LMM because we don't have run repetitions
# HR sizes are highly correlated with SDD, so we are not using it. As density is a predictor of home range size, we will use it instead


### SDD -----

# Check distributions #http://www.di.fc.ul.pt/~jpn/r/distributions/fitting.html
plotdist(dfexp2_r$SDD, histo = TRUE, demp = TRUE)

# Check ties as KS test complains about it. The other option is to use ks.boot instead (with permutations): # https://stats.stackexchange.com/questions/113032/ks-test-and-ks-boot-exact-p-values-and-ties
rank(dfexp2_r$SDD, ties.method = "max") # not really helpful

# Find distribution
descdist(dfexp2_r$SDD,  discrete = FALSE)
descdist(dfexp2_r$SDD,  discrete = FALSE, boot = 500)

# Looks like we can approach a lognormal, gamma or normal. Lets test them.

# Let's try the normal first
fit_n <- fitdist(dfexp2_r$SDD, "norm")
ks.test(dfexp2_r$SDD, "pnorm", mean = fit_n$estimate[1], sd = fit_n$estimate[2]) # nope

library("nortest") # https://stats.stackexchange.com/questions/232011/ties-should-not-be-present-in-one-sample-kolmgorov-smirnov-test-in-r
ad.test(dfexp2_r$SDD) # nope

# Weibull
fit_w  <- fitdist(dfexp2_r$SDD, "weibull")
ks.test(dfexp2_r$SDD, "pweibull", shape = fit_w$estimate[1], scale = fit_w$estimate[2]) # Nope
# fit_w  <- fitdist(dfexp2_r$SDD_sqrt, "weibull")
# ks.test(dfexp2_r$SDD_sqrt, "pweibull", shape = fit_w$estimate[1], scale = fit_w$estimate[2]) # Nope

# Log normal
fit_ln  <- fitdist(dfexp2_r$SDD, "lnorm")
ks.test(dfexp2_r$SDD, "plnorm", meanlog = fit_ln$estimate[1], sdlog = fit_ln$estimate[2]) # Yes!
summary(fit_ln)

# Gamma
fit_g <- fitdist(dfexp2_r$SDD, "gamma")
ks.test(dfexp2_r$SDD, "pgamma", shape = fit_g$estimate[1], rate = fit_g$estimate[2]) # nope


# Plot distrubtions
par(mfrow=c(2,2))
plot.legend <- c("Normal", "Weibull", "lognormal", "gamma")
denscomp(list(fit_n, fit_w, fit_g, fit_ln), fitlwd = 2, legendtext = plot.legend)
cdfcomp (list(fit_n, fit_w, fit_g, fit_ln), fitlwd = 2, legendtext = plot.legend)
qqcomp  (list(fit_n, fit_w, fit_g, fit_ln), fitlwd = 2, legendtext = plot.legend)
ppcomp  (list(fit_n, fit_w, fit_g, fit_ln), fitlwd = 2, legendtext = plot.legend)
par(mfrow=c(1,1))

# Let's see if we can find another test
library("actuar")
gofstat(list(fit_n, fit_w, fit_g, fit_ln), fitnames = c("Normal", "Weibull", "Gamma", "Lognormal"))


# As any of these disrtibutions were confirmed by the ks test, we will use the sqrt transformed SDD

# Check distributions #http://www.di.fc.ul.pt/~jpn/r/distributions/fitting.html
plotdist(dfexp2_r$SDD_sqrt, histo = TRUE, demp = TRUE)

# Find distribution
descdist(dfexp2_r$SDD_sqrt,  discrete = FALSE)
descdist(dfexp2_r$SDD_sqrt,  discrete = FALSE, boot = 500)

# Again, ooks like we can approach a normal, lognormal or gamma. Lets test them.

fit_n <- fitdist(dfexp2_r$SDD_sqrt, "norm")
ks.test(dfexp2_r$SDD_sqrt, "pnorm", mean = fit_n$estimate[1], sd = fit_n$estimate[2]) # nope

# Weibull
fit_w  <- fitdist(dfexp2_r$SDD_sqrt, "weibull")
ks.test(dfexp2_r$SDD_sqrt, "pweibull", shape = fit_w$estimate[1], scale = fit_w$estimate[2]) # Nope

# Log normal
fit_ln  <- fitdist(dfexp2_r$SDD_sqrt, "lnorm")
ks.test(dfexp2_r$SDD_sqrt, "plnorm", meanlog = fit_ln$estimate[1], sdlog = fit_ln$estimate[2]) # Yes!

# Gamma
fit_g <- fitdist(dfexp2_r$SDD_sqrt, "gamma")
ks.test(dfexp2_r$SDD_sqrt, "pgamma", shape = fit_g$estimate[1], rate = fit_g$estimate[2]) # nope (almost)


# Plot distrubtions
par(mfrow=c(2,2))
plot.legend <- c("Normal", "Weibull", "lognormal", "gamma")
denscomp(list(fit_n, fit_w, fit_g, fit_ln), fitlwd = 2, legendtext = plot.legend)
cdfcomp (list(fit_n, fit_w, fit_g, fit_ln), fitlwd = 2, legendtext = plot.legend)
qqcomp  (list(fit_n, fit_w, fit_g, fit_ln), fitlwd = 2, legendtext = plot.legend)
ppcomp  (list(fit_n, fit_w, fit_g, fit_ln), fitlwd = 2, legendtext = plot.legend)
par(mfrow=c(1,1))


# Let's see if we can find another test
library("actuar")
gofstat(list(fit_n, fit_w, fit_g, fit_ln), fitnames = c("Normal", "Weibull", "Gamma", "Lognormal"))


# We will follow up with a lognormal distribution error (thus with GLMs) and the non transformed SDD (because we also used it non transformed for analyzing Exp 1)
library("lme4")

dfexp2_r <- dfexp2_r %>% 
  rename(
    step_z = `[step]_z`
  )

dfexp2_r %>% glimpse()


# Not using p_feeding because it inflates (VIF) PT_z
# Not using p_traveling because it inflates (VIF) MR
# Dropping MR_z in favor of PT_z (less VIF)
# Dropping KE50 to see if it desinflates PT_z (less VIF) (and it is correlated with fragment size)
# Dropping MR_sd_z to see if it desinflates PT_z (less VIF)
# 
glm1 <- glm(log(SDD) ~ factor(density), family= gaussian(link='identity'), data=dfexp2_r)
glm2 <- glm(log(SDD) ~ factor(density)+ factor(fragment_size), family= gaussian(link='identity'), data=dfexp2_r)
glm3 <- glm(log(SDD) ~ factor(density)+ factor(fragment_size) + factor(field.shape.factor), family= gaussian(link='identity'), data=dfexp2_r)
glm4 <- glm(log(SDD) ~ factor(density)+ factor(fragment_size) + factor(field.shape.factor) + n_z, family= gaussian(link='identity'), data=dfexp2_r)
glm5 <- glm(log(SDD) ~ factor(density)+ factor(fragment_size) + factor(field.shape.factor) + n_z + step_z, family= gaussian(link='identity'), data=dfexp2_r)
# glm6 <- glm(log(SDD) ~ factor(density)+ factor(fragment_size) + factor(field.shape.factor) + n_z + step_z, family= gaussian(link='identity'), data=dfexp2_r)
glm7 <- glm(log(SDD) ~ factor(density)+ factor(fragment_size) + factor(field.shape.factor) + n_z + step_z + NN_sleeping_trees_z
              + energy_stored_z, family= gaussian(link='identity'), data=dfexp2_r)
# glm8 <- glm(log(SDD) ~ factor(density)+ factor(fragment_size) + factor(field.shape.factor) + n_z + step_z + NN_sleeping_trees_z
#             + energy_stored_z + KDE_50_z, family= gaussian(link='identity'), data=dfexp2_r)
# glm9 <- glm(log(SDD) ~ factor(density) + factor(fragment_size) + factor(field.shape.factor) + n_z + step_z + NN_sleeping_trees_z
#             + energy_stored_z + PT_z, family= gaussian(link='identity'), data=dfexp2_r)
glm10 <- glm(log(SDD) ~ factor(density) + factor(fragment_size) + factor(field.shape.factor) + n_z + step_z + NN_sleeping_trees_z
            + energy_stored_z + PT_z , family= gaussian(link='identity'), data=dfexp2_r)

# glm11 <- glm(log(SDD) ~ factor(density) + factor(fragment_size) + factor(field.shape.factor) + n_z + step_z + NN_sleeping_trees_z
#             + energy_stored_z + KDE_50_z + PT_z + MR_sd_z , family= gaussian(link='identity'), data=dfexp2_r)
glm12 <- glm(log(SDD) ~ factor(density) + factor(fragment_size) + factor(field.shape.factor) + n_z + step_z + NN_sleeping_trees_z
            + energy_stored_z + PT_z + p_resting_z, family= gaussian(link='identity'), data=dfexp2_r)
# glm13 <- glm(log(SDD) ~ factor(density) + factor(fragment_size) + factor(field.shape.factor) + n_z + step_z + NN_sleeping_trees_z
#             + energy_stored_z + PT_z + p_resting_z, family= gaussian(link='identity'), data=dfexp2_r)
glm14 <- glm(log(SDD) ~ factor(density) + factor(fragment_size) + factor(field.shape.factor) + n_z + step_z + NN_sleeping_trees_z
            + energy_stored_z + PT_z + p_resting_z + n_unvisited_trees_z, family= gaussian(link='identity'), data=dfexp2_r)


# Model selection
library("MuMIn")
AIC.table  <- MuMIn::model.sel(glm1, glm2, glm3, glm4, glm5, #glm6, 
                               glm7, #glm8, glm9, 
                               glm10, # glm11,
                               glm12, #glm13, 
                               glm14
                               )
AIC.table

AIC.table <- AIC.table[ , c("df", "logLik", "AICc", "delta")]
AIC.table

# Save table
# AIC.table %>% write.csv(here("Model_simulations", "Exp2_run", "AIC_table.csv"))
# AIC.table %>% write.xlsx(here("Model_simulations", "Exp2_run", "AIC_table.xlsx"))



# Summary
summary(glm14) # best

# Save summary as table
glm14_res <-summary.glm(glm14)$coefficients
# glm14_res %>% write.xlsx(here("Model_simulations", "Exp2_run", "Best_model_SDD.xlsx"))
# summary(glm12) 
# summary(glm11)
# summary(glm10)

# Check VIF
car::vif(glm14) # test for multicolinearity (https://analises-ecologicas.com/cap7.html#regress%C3%A3o-linear-simples)
car::vif(glm14, type = "predictor") # Huge correlation!

# Plot model
library("sjPlot")
plot_model(glm14)
# plot_model(glm14, prefix.labels = "none")
# par(mfrow=c(2,2))
plot_model(glm14, type = "diag")[1] # Ok, I think this is enough. MR is slighly above tolerable but if I take it out other predictors get inflated and there's no other movement parameter

p <- plot_model(glm14, type = "diag")
p <- gridExtra::grid.arrange(grobs=p)

# Save model plot
# ggsave(p, filename = here("Model_simulations", "Exp2_run", "Best_model_SDD_plot.png"),
#        dpi = 300, width = 30, height = 25, units = "cm")

plot_model(glm14, type = "res") # I don't know how to interpret this
p1 <- plot_model(glm14, type = "pred")


# # 
# # # Without controlled predictors (as they are used in Experiment 1)
# glm4 <- glm(SDD_sqrt ~ n_z, family= gaussian(link='log'), data=dfexp2_r)
# glm5 <- glm(SDD_sqrt ~ n_z + NN_feeding_trees_z, family= gaussian(link='log'), data=dfexp2_r)
# glm6 <- glm(SDD_sqrt ~ n_z + NN_feeding_trees_z +
#               + DPL_sd_z, family= gaussian(link='log'), data=dfexp2_r)
# glm7 <- glm(SDD_sqrt ~ n_z + NN_feeding_trees_z +
#               DPL_sd_z + MR_z + PT_z, family= gaussian(link='log'), data=dfexp2_r)
# glm8 <- glm(SDD_sqrt ~ n_z + NN_feeding_trees_z +
#               DPL_sd_z + MR_z +PT_z + PT_sd_z, family= gaussian(link='log'), data=dfexp2_r)
# glm9 <- glm(SDD_sqrt ~ n_z + NN_feeding_trees_z +
#               + DPL_sd_z + MR_z+ PT_z + PT_sd_z + n_visited_trees_z, family= gaussian(link='log'), data=dfexp2_r)
# 
# # Model selection
# library("MuMIn")
# AIC.table  <- MuMIn::model.sel(glm4, glm5, glm6, glm7, glm8, glm9)#, glm10, glm11, glm12, glm13)
# AIC.table
# 
# AIC.table <- AIC.table[ , c("df", "logLik", "AICc", "delta")]
# AIC.table
# 
# # # Save table
# # # AIC.table %>% write.csv(here("Model_simulations", "Exp2_run", "Table3_AIC_table.csv"))
# # # AIC.table %>% write.xlsx(here("Model_simulations", "Exp2_run", "Table3_AIC_table.xlsx"))
# # 
# # # Summary
# # summary(glm9) # best
# # 
# # # Save summary as table
# # glm9_res <-summary.glm(glm9)$coefficients
# # # glm9_res %>% write.xlsx(here("Model_simulations", "Exp2_run", "Table4_best_model.xlsx"))
# 
# 
# # Check VIF
# car::vif(glm9) # test for multicolinearity (https://analises-ecologicas.com/cap7.html#regress%C3%A3o-linear-simples)
# car::vif(glm9, type = "predictor") # Huge correlation!
# 
# # Plot model
# library("sjPlot")
# plot_model(glm9)
# # par(mfrow=c(2,2))
# plot_model(glm9, type = "diag")
# plot_model(glm9, type = "res") # This shows a non-linear response, which for sure argues for a GAM
# plot_model(glm9, type = "pred") 
# p1 <- plot_model(glm3, type = "pred") 


# IF THE RESPONSE HAD NORMALLY DISTRIBUTED ERRORS:
# *** PS: não consegui usar o DPL, diz que tem valores NA mesmo eu filtrando todo NA fora. Roda com DPL_sd
# *** PS: depois conferi e parece que é porque é super correlacionado (sendo tirado do dataset pelo findCorrelations())
# lm1 <- lm(SDD ~ density, data=dfexp2_r) 
# lm2 <- lm(SDD ~ density, fragment_size_log10, data=dfexp2_r)
# lm3 <- lm(SDD ~ density, fragment_size_log10 + field.shape.factor, data=dfexp2_r)
# lm4 <- lm(SDD ~ density, fragment_size_log10 + field.shape.factor + NN_feeding_trees, data=dfexp2_r)
# lm5 <- lm(SDD ~ density, fragment_size_log10 + field.shape.factor + n + NN_feeding_trees, data=dfexp2_r)
# lm6 <- lm(SDD ~ density, fragment_size_log10 + field.shape.factor + n + NN_feeding_trees + NN_sleeping_trees, data=dfexp2_r)
# lm7 <- lm(SDD ~ density, fragment_size_log10 + field.shape.factor + n + NN_feeding_trees + NN_sleeping_trees
#           + DPL, data=dfexp2_r)
# lm8 <- lm(SDD ~ density, fragment_size_log10 + field.shape.factor + n + NN_feeding_trees + NN_sleeping_trees
#           + DPL + DPL_sd, data=dfexp2_r)
# lm9 <- lm(SDD ~ density, fragment_size_log10 + field.shape.factor + n + NN_feeding_trees + NN_sleeping_trees
#           + MR, data=dfexp2_r)
# lm10 <- lm(SDD ~ density, fragment_size_log10 + field.shape.factor + n + NN_feeding_trees + NN_sleeping_trees
#           + MR + MR_sd, data=dfexp2_r)
# lm11 <- lm(SDD ~ density, fragment_size_log10 + field.shape.factor + n + NN_feeding_trees + NN_sleeping_trees
#           + MR + MR_sd + PT, data=dfexp2_r)
# lm12 <- lm(SDD ~ density, fragment_size_log10 + field.shape.factor + n + NN_feeding_trees + NN_sleeping_trees
#           + MR + MR_sd + PT + PT_sd, data=dfexp2_r)
# lm13 <- lm(SDD ~ density, fragment_size_log10 + field.shape.factor + n + NN_feeding_trees + NN_sleeping_trees
#            + MR + MR_sd + PT + PT_sd + n_visited_trees, data=dfexp2_r)


### NN Seeds -----

# Check distributions #http://www.di.fc.ul.pt/~jpn/r/distributions/fitting.html
plotdist(dfexp2_r$NN_seeds, histo = TRUE, demp = TRUE)

# Check ties as KS test complains about it. The other option is to use ks.boot instead (with permutations): # https://stats.stackexchange.com/questions/113032/ks-test-and-ks-boot-exact-p-values-and-ties
rank(dfexp2_r$NN_seeds, ties.method = "max") # not really helpful

# Find distribution
descdist(dfexp2_r$NN_seeds,  discrete = FALSE)
descdist(dfexp2_r$NN_seeds,  discrete = FALSE, boot = 500)

# Looks like we can approach a gamma. Lets test them.

# Let's try the normal first
fit_n <- fitdist(dfexp2_r$NN_seeds, "norm")
ks.test(dfexp2_r$NN_seeds, "pnorm", mean = fit_n$estimate[1], sd = fit_n$estimate[2]) # nope

library("nortest") # https://stats.stackexchange.com/questions/232011/ties-should-not-be-present-in-one-sample-kolmgorov-smirnov-test-in-r
ad.test(dfexp2_r$NN_seeds) # nope

# Weibull
fit_w  <- fitdist(dfexp2_r$NN_seeds, "weibull")
ks.test(dfexp2_r$NN_seeds, "pweibull", shape = fit_w$estimate[1], scale = fit_w$estimate[2]) # Nope
# fit_w  <- fitdist(dfexp2_r$NN_seeds_sqrt, "weibull")
# ks.test(dfexp2_r$NN_seeds_sqrt, "pweibull", shape = fit_w$estimate[1], scale = fit_w$estimate[2]) # Nope

# Log normal
fit_ln  <- fitdist(dfexp2_r$NN_seeds, "lnorm")
ks.test(dfexp2_r$NN_seeds, "plnorm", meanlog = fit_ln$estimate[1], sdlog = fit_ln$estimate[2]) # Nope
summary(fit_ln)

# Gamma
fit_g <- fitdist(dfexp2_r$NN_seeds, "gamma")
ks.test(dfexp2_r$NN_seeds, "pgamma", shape = fit_g$estimate[1], rate = fit_g$estimate[2]) # nope


# Plot distrubtions
par(mfrow=c(2,2))
plot.legend <- c("Normal", "Weibull", "lognormal", "gamma")
denscomp(list(fit_n, fit_w, fit_g, fit_ln), fitlwd = 2, legendtext = plot.legend)
cdfcomp (list(fit_n, fit_w, fit_g, fit_ln), fitlwd = 2, legendtext = plot.legend)
qqcomp  (list(fit_n, fit_w, fit_g, fit_ln), fitlwd = 2, legendtext = plot.legend)
ppcomp  (list(fit_n, fit_w, fit_g, fit_ln), fitlwd = 2, legendtext = plot.legend)
par(mfrow=c(1,1))

# Let's see if we can find another test
library("actuar")
gofstat(list(fit_n, fit_w, fit_g, fit_ln), fitnames = c("Normal", "Weibull", "Gamma", "Lognormal"))


# As any of these disrtibutions were confirmed by the ks test, we will use the sqrt transformed NN_seeds

# Check distributions #http://www.di.fc.ul.pt/~jpn/r/distributions/fitting.html
plotdist(dfexp2_r$NN_seeds_sqrt, histo = TRUE, demp = TRUE)

# Find distribution
descdist(dfexp2_r$NN_seeds_sqrt,  discrete = FALSE)
descdist(dfexp2_r$NN_seeds_sqrt,  discrete = FALSE, boot = 500)

# It looks like we can approach a gamma

fit_n <- fitdist(dfexp2_r$NN_seeds_sqrt, "norm")
ks.test(dfexp2_r$NN_seeds_sqrt, "pnorm", mean = fit_n$estimate[1], sd = fit_n$estimate[2]) # nope

# Weibull
fit_w  <- fitdist(dfexp2_r$NN_seeds_sqrt, "weibull")
ks.test(dfexp2_r$NN_seeds_sqrt, "pweibull", shape = fit_w$estimate[1], scale = fit_w$estimate[2]) # Nope

# Log normal
fit_ln  <- fitdist(dfexp2_r$NN_seeds_sqrt, "lnorm")
ks.test(dfexp2_r$NN_seeds_sqrt, "plnorm", meanlog = fit_ln$estimate[1], sdlog = fit_ln$estimate[2]) # Nope

# Gamma
fit_g <- fitdist(dfexp2_r$NN_seeds_sqrt, "gamma")
ks.test(dfexp2_r$NN_seeds_sqrt, "pgamma", shape = fit_g$estimate[1], rate = fit_g$estimate[2]) # nope


# Plot distrubtions
par(mfrow=c(2,2))
plot.legend <- c("Normal", "Weibull", "lognormal", "gamma")
denscomp(list(fit_n, fit_w, fit_g, fit_ln), fitlwd = 2, legendtext = plot.legend)
cdfcomp (list(fit_n, fit_w, fit_g, fit_ln), fitlwd = 2, legendtext = plot.legend)
qqcomp  (list(fit_n, fit_w, fit_g, fit_ln), fitlwd = 2, legendtext = plot.legend)
ppcomp  (list(fit_n, fit_w, fit_g, fit_ln), fitlwd = 2, legendtext = plot.legend)
par(mfrow=c(1,1))


# Let's see if we can find another test
library("actuar")
gofstat(list(fit_n, fit_w, fit_g, fit_ln), fitnames = c("Normal", "Weibull", "Gamma", "Lognormal"))



# As with Exp1, we did not find any distribution for NN_dist. We will follow up with a lognormal distribution error (thus with GLMs) and the non transformed NN_seeds (because we also used it non transformed for analyzing Exp 1)


# Let's try with log10 transformation

# Check distributions #http://www.di.fc.ul.pt/~jpn/r/distributions/fitting.html
plotdist(dfexp2_r$NN_seeds_log10, histo = TRUE, demp = TRUE)

# Find distribution
descdist(dfexp2_r$NN_seeds_log10,  discrete = FALSE)
descdist(dfexp2_r$NN_seeds_log10,  discrete = FALSE, boot = 500)

# It looks like we can approach a gamma, lognormal or normal

fit_n <- fitdist(dfexp2_r$NN_seeds_log10, "norm")
ks.test(dfexp2_r$NN_seeds_log10, "pnorm", mean = fit_n$estimate[1], sd = fit_n$estimate[2]) # nope

# Weibull
fit_w  <- fitdist(dfexp2_r$NN_seeds_log10, "weibull")
ks.test(dfexp2_r$NN_seeds_log10, "pweibull", shape = fit_w$estimate[1], scale = fit_w$estimate[2]) # Nope

# Log normal
fit_ln  <- fitdist(dfexp2_r$NN_seeds_log10, "lnorm")
ks.test(dfexp2_r$NN_seeds_log10, "plnorm", meanlog = fit_ln$estimate[1], sdlog = fit_ln$estimate[2]) # Nope

# Gamma
fit_g <- fitdist(dfexp2_r$NN_seeds_log10, "gamma")
ks.test(dfexp2_r$NN_seeds_log10, "pgamma", shape = fit_g$estimate[1], rate = fit_g$estimate[2]) # nope


# Plot distrubtions
par(mfrow=c(2,2))
plot.legend <- c("Normal", "Weibull", "lognormal", "gamma")
denscomp(list(fit_n, fit_w, fit_g, fit_ln), fitlwd = 2, legendtext = plot.legend)
cdfcomp (list(fit_n, fit_w, fit_g, fit_ln), fitlwd = 2, legendtext = plot.legend)
qqcomp  (list(fit_n, fit_w, fit_g, fit_ln), fitlwd = 2, legendtext = plot.legend)
ppcomp  (list(fit_n, fit_w, fit_g, fit_ln), fitlwd = 2, legendtext = plot.legend)
par(mfrow=c(1,1))


# Let's see if we can find another test
library("actuar")
gofstat(list(fit_n, fit_w, fit_g, fit_ln), fitnames = c("Normal", "Weibull", "Gamma", "Lognormal"))


# The KS test (and other tests) showed, in general that the lognormal fitted better, besides the KS test being < 0.05 (rejecting it came from a lognormal distribution)
# As Im not sure how to proceed, I'll continue using a lognormal with the non transformed NN_seeds, because I proceeded like this for Exp 1




library("lme4")

dfexp2_r <- dfexp2_r %>% 
  rename(
    step_z = `[step]_z`
  )

dfexp2_r %>% glimpse()


# MR_z droped because it overinflates the model (high VIF)
# taking p_feeding out to see if it drops PT_z VIF
# taking p_feeding out didn't work. Taking p_traveling out to see if it does
# It worked a little bit. Now I'll take NN_feeding_trees to see if it drops PT_z VIF
# It worked a little bit. Now I'll take KDE_50 to see if it drops PT_z VIF
# 

glm1 <- glm(log(NN_seeds) ~ factor(density), family= gaussian(link='identity'), data=dfexp2_r)
glm2 <- glm(log(NN_seeds) ~ factor(density)+ factor(fragment_size), family= gaussian(link='identity'), data=dfexp2_r)
glm3 <- glm(log(NN_seeds) ~ factor(density)+ factor(fragment_size) + factor(field.shape.factor), family= gaussian(link='identity'), data=dfexp2_r)
glm4 <- glm(log(NN_seeds) ~ factor(density)+ factor(fragment_size) + factor(field.shape.factor) + n_z, family= gaussian(link='identity'), data=dfexp2_r)
glm5 <- glm(log(NN_seeds) ~ factor(density)+ factor(fragment_size) + factor(field.shape.factor) + n_z + step_z, family= gaussian(link='identity'), data=dfexp2_r)
# glm6 <- glm(log(NN_seeds) ~ factor(density)+ factor(fragment_size) + factor(field.shape.factor) + n_z + step_z + NN_feeding_trees_z, family= gaussian(link='identity'), data=dfexp2_r)
glm7 <- glm(log(NN_seeds) ~ factor(density)+ factor(fragment_size) + factor(field.shape.factor) + n_z + step_z + NN_sleeping_trees_z
            + energy_stored_z, family= gaussian(link='identity'), data=dfexp2_r)
# glm8 <- glm(log(NN_seeds) ~ factor(density)+ factor(fragment_size) + factor(field.shape.factor) + n_z + step_z + NN_sleeping_trees_z
#             + energy_stored_z + KDE_50_z, family= gaussian(link='identity'), data=dfexp2_r)
# glm9 <- glm(log(NN_seeds) ~ factor(density) + factor(fragment_size) + factor(field.shape.factor) + n_z + step_z  + NN_sleeping_trees_z
#             + energy_stored_z + KDE_50_z, PT_z, family= gaussian(link='identity'), data=dfexp2_r) # PT_z not allowed?


# glm10 <- glm(log(NN_seeds) ~ factor(density) + factor(fragment_size) + factor(field.shape.factor) + n_z + step_z + NN_sleeping_trees_z
#              + energy_stored_z + KDE_50_z + PT_z + MR_z, family= gaussian(link='identity'), data=dfexp2_r)

glm11 <- glm(log(NN_seeds) ~ factor(density) + factor(fragment_size) + factor(field.shape.factor) + n_z + step_z + NN_sleeping_trees_z
            + energy_stored_z  + PT_z  + MR_sd_z, family= gaussian(link='identity'), data=dfexp2_r)
# glm12 <- glm(log(NN_seeds) ~ factor(density) + factor(fragment_size) + factor(field.shape.factor) + n_z + step_z + NN_sleeping_trees_z
#             + energy_stored_z + KDE_50_z + PT_z  + MR_sd_z + p_feeding_z, family= gaussian(link='identity'), data=dfexp2_r)

glm13 <- glm(log(NN_seeds) ~ factor(density) + factor(fragment_size) + factor(field.shape.factor) + n_z + step_z + NN_sleeping_trees_z
             + energy_stored_z  + PT_z + MR_sd_z + p_feeding_z, family= gaussian(link='identity'), data=dfexp2_r)
glm14 <- glm(log(NN_seeds) ~ factor(density) + factor(fragment_size) + factor(field.shape.factor) + n_z + step_z + NN_sleeping_trees_z
            + energy_stored_z  + PT_z  + MR_sd_z + p_feeding_z + p_resting_z, family= gaussian(link='identity'), data=dfexp2_r)
glm15 <- glm(log(NN_seeds) ~ factor(density) + factor(fragment_size) + factor(field.shape.factor) + n_z + step_z + NN_sleeping_trees_z
             + energy_stored_z  + PT_z + MR_sd_z + p_feeding_z + p_resting_z + n_unvisited_trees_z, family= gaussian(link='identity'), data=dfexp2_r)


# Model selection
library("MuMIn")
AIC.table  <- MuMIn::model.sel(glm1, glm2, glm3, glm4, glm5, #glm6, 
                               glm7, #glm8, #glm9, #glm10, 
                               glm11,
                               # glm12, 
                               glm13, 
                               glm14, glm15
)
AIC.table

AIC.table <- AIC.table[ , c("df", "logLik", "AICc", "delta")]
AIC.table

# Save table
# AIC.table %>% write.csv(here("Model_simulations", "Exp2_run", "AIC_table_NNseeds.csv"))
# AIC.table %>% write.xlsx(here("Model_simulations", "Exp2_run", "AIC_table_NNseeds.xlsx"))


# Summary
summary(glm14) # glm15 and 15 are equaly likely, but glm14 has less inflated factors. Both of them have slighly above PT_z inflation

# Save summary as table
glm14_res <-summary.glm(glm14)$coefficients
# glm14_res %>% write.xlsx(here("Model_simulations", "Exp2_run", "Best_model_NNseeds.xlsx"))
# summary(glm12) 
# summary(glm11)
# summary(glm10)

# Check VIF
car::vif(glm14) # test for multicolinearity (https://analises-ecologicas.com/cap7.html#regress%C3%A3o-linear-simples)
car::vif(glm14, type = "predictor") # Huge correlation!

# Plot model
library("sjPlot")
plot_model(glm14)
# plot_model(glm15, prefix.labels = "none")
# par(mfrow=c(2,2))
plot_model(glm14, type = "diag")[1] # Ok, I think this is enough. MR is slighly above tolerable but if I take it out other predictors get inflated and there's no other movement parameter

p <- plot_model(glm14, type = "diag")
p <- gridExtra::grid.arrange(grobs=p)

# Save model plot
# ggsave(p, filename = here("Model_simulations", "Exp2_run", "Best_model_NNseeds_plot.png"),
#        dpi = 300, width = 30, height = 25, units = "cm")

plot_model(glm14, type = "res") # I don't know how to interpret this
p1 <- plot_model(glm14, type = "pred")








# Various models in a grid
plot_models(glm14, glm16, grid = TRUE)
