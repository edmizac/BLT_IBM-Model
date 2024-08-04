# Script name: Exp1_analysis_PathAnalysis.R
# Script purpose: During my MSc thesis, I have used GLMs. They were a little
# bit hard to fit because I had too many variables. Furthermore, it didn't allow 
# me to group variables and imply some relationships between groups of variables.
# Thus, I decided to go for a path analysis. 
# Examples are: Fuzessy et al. 2017

# Date created: 24/07/2024
# Author: Eduardo Zanette

## Notes --------------------------- 


## Options -------------------------
# (plotting, memory limit, decimal digits)
# 

## Packages -------------------------
library("here")
library("tidyverse")
# install_github("gastonstat/plspm")
library("plspm")
library("scales")

theme_set(theme_bw(base_size = 16))

# pathexp1 <- "C:/Users/Dell/Desktop/PLS/"
pathexp1 <- "D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest_2024/Exp1/results/"


# PLS Walktrough (Chapter 1 PLS with R) -----
# data(spainfoot)
# 
# spainfoot %>% head()
# # GSH total number of goals scored at home
# # GSA total number of goals scored away
# # SSH percentage of matches with scores goals at home
# # SSA percentage of matches with scores goals away
# # GCH total number of goals conceded at home
# # GCA total number of goals conceded away
# # CSH percentage of matches with no conceded goals at home
# # CSA percentage of matches with no conceded goals away
# # WMH total number of won matches at home
# # WMA total number of won matches away
# # LWR longest run of won matches
# # LRWL longest run of matches without losing
# # YC total number of yellow cards
# # RC total number of red cards
# 
# 
# # Create inner model:
# # Required for plspm: 1) a data set, 2) an inner model, and 3) an outer model.
# 
# 
# ## 2) inner model -----
# # requires a lower triangular boolean matrix --> COLUMNS AFFECTING ROWS ******
# # In other words, it must be a square matrix (same number of rows and columns), 
# # the elements in the diagonal and above it must be zeros, and the elements below
# # the diagonal can be either zeros or ones.
# 
# Attack = c(0, 0, 0)
# Defense= c(0, 0, 0)
# Success = c(1, 1, 0)
# 
# foot_path <- rbind(Attack, Defense, Success)
# 
# colnames(foot_path) <- rownames(foot_path)
# 
# foot_path
# 
# # plot the path matrix
# innerplot(foot_path)
# 
# 
# 
# ## 3) outer model -----
# # define list of indicators: what variables are associated with what latent variables.
# # Thus, the first block corresponding to the latent variable Attack is # associated with 
# # the first four columns of the data set... The second block, associated to Defense, 
# # is formed by columns from 5 to 8. And Success is associated with columns from 9 to 12:
# foot_blocks = list(1:4, 5:8, 9:12)
# 
# 
# 
# # Set mode
# # all latent variables are measured in a reflective way ("mode A") -> reflexive
# foot_modes = c("A", "A", "A")
# 
# # Success in formative mode B -> formative
# foot_modes2 = c("A", "A", "B")
# 
# 
# # run plspm analysis
# foot_pls <- plspm(spainfoot, foot_path, foot_blocks, modes = foot_modes2)
# 
# foot_pls
# 
# 
# # path coefficients
# foot_pls$path_coefs
# 
# # inner model
# foot_pls$inner_model
# 
# # summarized results
# summary(foot_pls)
# 
# # plotting results (inner model)
# plot(foot_pls)
# 
# plot(foot_pls, what = "loadings", arr.width = 0.1)
# 
# # show me the first scores
# head(foot_pls$scores, n = 5)
# 
# 
# 
# # 




# Exp1 Data -----


## 1) Data -----

# Load
# dfexp1 <- xlsx::read.xlsx(paste0(pathexp1, "Exp1_results.xlsx"), 1)
dfexp1 <- read_csv(paste0(pathexp1, "Exp1_results.csv"))


colnames(dfexp1)
glimpse(dfexp1)

# Remove bad characters:
dfexp1 <- dfexp1 %>% 
  rename_with( ~stringr::str_replace_all(., '-', "_"))

# Make them numeric
# dfexp1 <- dfexp1 %>% 
#   mutate(
#     density = as.numeric(density),
#     p_foraging_while_traveling = as.numeric(p_foraging_while_traveling),
#     resource_pattern_R = as.numeric(resource_pattern_R),
#     resource_pattern_R_p = as.numeric(resource_pattern_R_p),
#     R_seeds = as.numeric(R_seeds),
#     R_seeds_p = as.numeric(R_seeds_p),
#     step_len_forage = as.numeric(step_len_forage),
#     PT_sd = as.numeric(PT_sd)
#   ) %>% 
#   mutate_at(vars(starts_with("p_")), as.numeric)


# Rename columns
dfexp1 <- dfexp1 %>% 
  rename(
    fragment_shape = field.shape.factor,
    hr_size = KDE_95,
    n_trees = n,
    p_frugivory = p_feeding,
    step = `[step]`
    # p_travel = p_travelling
  )

dfexp1 %>% glimpse()

# Normalize variables -----
normalize_it <- function(x, na.rm = TRUE) {
  return(x - min(x, na.rm = na.rm)) / max(x, na.rm) - min(x, na.rm)
}

# dfexp1 <- dfexp1 %>%
#   mutate_at(vars(
#     no_days:`[step]`,
#     fragment_size:p_visited_trees,
#     step_len_forage:max_rel_ang_forage_75q,
#     resource_pattern_R, resource_pattern_R_p# ncol(dfexp1)
#   ), .funs = normalize_it
#   )

glimpse(dfexp1)


hist(dfexp1$no_days)
hist(dfexp1$fragment_size)
hist(dfexp1$SDD)
hist(dfexp1$resource_pattern_R) # OH YEAH
hist(dfexp1$p_visited_trees)
hist(dfexp1$step)
hist(dfexp1$SDD)
hist(dfexp1$NN_seeds)


# Only the input variables can only be numerical and must be the only ones present in the data object:
dfexp1_clean <- dfexp1 %>% 
  dplyr::select(
    c(  "n_trees",
        "density",
        "PT",
        "aggregation_type",
        "NN_feeding_trees", "NN_sleeping_trees", # latent variable
        "fragment_size",
        "fragment_shape",
        "step",
        "p_frugivory",
        "p_traveling", # ? tirar
        "p_resting",
        "DPL",
        "hr_size",
        "SDD",
        "NN_seeds"
    )
  ) %>% 
  as.data.frame()

anyNA(dfexp1_clean) # no NAs. OK!

dfexp1_clean$n_trees_neg = -1 * dfexp1_clean$n_trees
dfexp1_clean$fragment_shape_neg = -1 * dfexp1_clean$fragment_shape
dfexp1_clean$p_resting_neg = -1 * dfexp1_clean$p_resting
dfexp1_clean$p_frugivory_neg = -1 * dfexp1_clean$p_frugivory

dfexp1_clean <- dfexp1_clean %>% 
  mutate(
    aggregation_type = case_when(
      aggregation_type == "clumped" ~ 0,
      aggregation_type == "random" ~ 1,
      aggregation_type == "ordered" ~ 2,
    )
  )

dfexp1_clean %>% glimpse()


# # PLS round 1 -----
# 
# # 1) Either input it manually with a matrix:
# # n_trees                <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
# # Resource_distribution  <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
# # fragment_size          <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
# # fragment_shape         <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
# # p_frugivory            <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
# # p_traveling            <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
# # p_resting              <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
# # DPL                    <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
# # Home_range_size        <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
# # SDD_NN                 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
# 
# # 2) Or use a spreadsheet:
# ma <- readxl::read_excel(paste0("D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest_2024/0_files/"
#                                 , "PLS_design.xlsx"),
#                          sheet = 5)
# 
# rown <- colnames(ma)
# rown <- rown[-1]
# 
# ma_path <- ma[ , -1] %>% as.matrix
# dim(ma_path)
# 
# 
# innerplot(ma_path,  box.prop = 0.25)
# 
# rownames(ma_path) <- colnames(ma_path)
# 
# ma_path
# 
# # # Tirar hr_size:
# # ma_path <- ma_path[-3, -3]
# # innerplot(ma_path,  box.prop = 0.25)
# 
# 
# ## 2) Inner model: Definir variáveis latentes ------
# dfexp1_clean %>% glimpse()
# 
# ma_blocks = list(
#   c("n_trees", "NN_feeding_trees", "NN_sleeping_trees"), # LV Resource distribution
#   # c("NN_feeding_trees"),
#   # c("NN_sleeping_trees"),
#   c("fragment_size"),
#   c("fragment_shape"),
#   c("p_frugivory", "p_resting"), # LV Diet
#   c("p_traveling", "step"), # LV Movement
#   c("DPL"),
#   c("hr_size"),
#   c("SDD")
# )
# 
# # ma_blocks = list(
# #   # c("NN_feeding_trees", "NN_sleeping_trees"), # LV Resource distribution
# #   c("p_frugivory", "p_resting"), # LV Diet
# #   c("p_traveling", "step"), # LV Movement
# # )
# 
# ma_blocks
# length(ma_blocks)
# 
# ## 3) Outer model ----
# ma_modes = c("A", "A", "A", "A", "A", "A", "A",
#              "A"
# ) # são 1 (=numero de variaveis latentes) e não 11 (numero de variáveis)
# 
# length(ma_modes)
# 
# 
# ## 4) Run plspm analysis -----
# 
# exp1_pls <- plspm(dfexp1_clean, ma_path, ma_blocks, modes = ma_modes)
# 
# exp1_pls
# summary(exp1_pls)
# 
# # GOODNESS-OF-FIT 
# # [1]  0.4071
# 
# 
# ## 5) Model evaluation -----
# 
# ## Reflexive measures: ----
# # Basically, we must evaluate three aspects of reective measures:
# #   * Unidimensionality of the indicators
# #   * Check that indicators are well explained by its latent variable
# #   * Assess the degree to which a given construct is diferent from other constructs
# exp1_pls$unidim # Resource_dist problematico (C.alpha e DG.rho < 0.7)
# 
# # Loadings
# plot(exp1_pls, what = "loadings")
# exp1_pls$outer_model
# subset(exp1_pls$outer_model, block == "DPL")
# subset(exp1_pls$outer_model, block == "Resource_dist")
# 
# # Weights
# plot(exp1_pls, what = "weights")
# 
# 
# 
# # PLS round 2 -----
# 
# ma <- readxl::read_excel(paste0("D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest_2024/0_files/"
#                                 , "PLS_design.xlsx"),
#                          sheet = 6)
# 
# rown <- colnames(ma)
# rown <- rown[-1]
# 
# ma_path <- ma[ , -1] %>% as.matrix
# dim(ma_path)
# 
# 
# innerplot(ma_path,  box.prop = 0.25)
# 
# rownames(ma_path) <- colnames(ma_path)
# 
# ma_path
# 
# ma_blocks2 = list(
#   
#   # LV Resource distribution:
#   c("n_trees", "NN_feeding_trees", "NN_sleeping_trees"),
#   
#   # LV Diet:
#   c("p_frugivory", "p_resting"),
#   
#   # LV Movement:
#   c("p_traveling", "step", "n_trees", "NN_feeding_trees", "NN_sleeping_trees"), 
#   
#   # LV DPL: 
#   c("p_traveling", "step", "n_trees", "NN_feeding_trees", "NN_sleeping_trees"
#     , "p_frugivory", "p_resting"
#     , "fragment_shape"
#   ), 
#   
#   # LV SDD:
#   c("p_traveling", "step", "n_trees", "NN_feeding_trees", "NN_sleeping_trees"
#     , "p_frugivory", "p_resting"
#     , "fragment_shape"
#     , "hr_size")
#   
# )
# 
# glimpse(dfexp1_clean)
# 
# 
# # ma_blocks_new = list(
# #   c(13, 2:3), # Resource distribution
# #   c(4, 12, 6:7), # DPL
# #   # c(4), # Home range size
# #   c(9:10) # SDD
# # )
# 
# # ma_blocks_new_str = list(
# #   c('n_trees_neg', 'NN_feeding_trees', 'NN_sleeping_trees'), # Resource distribution
# #   c('fragment_size', 'fragment_shape_neg', 'p_frugivory', 'p_traveling'
# #     # , 'p_resting'
# #     ), # DPL
# #   # c('fragment_size'), # Home range size
# #   c('DPL', 'hr_size') # SDD
# # )
# 
# ma_modes2 = rep("A", 5) # são 1 (=numero de variaveis latentes) e não 11 (numero de variáveis)
# 
# exp1_pls2 <- plspm(dfexp1_clean, ma_path, ma_blocks2, modes = ma_modes2)
# 
# exp1_pls2
# summary(exp1_pls2)
# 
# # GOODNESS-OF-FIT 
# # [1]  0.5667
# 
# # Loadings
# plot(exp1_pls2, what = "loadings")
# exp1_pls$outer_model
# subset(exp1_pls$outer_model, block == "DPL")
# subset(exp1_pls$outer_model, block == "resource_dist")
# 
# exp1_pls2$unidim
# 
# ## Loadings and Communalities
# # loadings are correlations between a latent variable and its indicators
# # communalities are squared correlations (loadings ^2 =~ R2); A loading grater than 0.7 means that more than 0:72 * 50% of the variablity in an indicator is captured by its latent construct
# exp1_pls2$outer_model
# 
# 
# 
# ## PLS round 3 ------
# 
# # # Make negatively correlated variables negative
# dfexp1_clean$n_trees_neg = -1 * dfexp1_clean$n_trees
# dfexp1_clean$fragment_shape_neg = -1 * dfexp1_clean$fragment_shape
# dfexp1_clean$p_resting_neg = -1 * dfexp1_clean$p_resting
# dfexp1_clean$p_frugivory_neg = -1 * dfexp1_clean$p_frugivory
# 
# 
# ma_blocks3 = list(
#   
#   # LV Resource distribution:
#   c("n_trees_neg", "NN_feeding_trees", "NN_sleeping_trees"),
#   
#   # LV Diet:
#   c("p_frugivory_neg", "p_resting_neg"),
#   
#   # LV Movement:
#   c("p_traveling", "step", "n_trees_neg", "NN_feeding_trees", "NN_sleeping_trees"), 
#   
#   # LV DPL: 
#   c("p_traveling", "step", "n_trees_neg", "NN_feeding_trees", "NN_sleeping_trees"
#     , "p_frugivory_neg", "p_resting_neg"
#     , "fragment_shape_neg"
#   ), 
#   
#   # LV SDD:
#   c("p_traveling", "step", "n_trees_neg", "NN_feeding_trees", "NN_sleeping_trees"
#     , "p_frugivory_neg", "p_resting_neg"
#     , "fragment_shape_neg"
#     , "hr_size")
#   
# )
# 
# glimpse(dfexp1_clean)
# 
# 
# ma_modes3 = rep("A", 5) # são 1 (=numero de variaveis latentes) e não 11 (numero de variáveis)
# 
# exp1_pls3 <- plspm(dfexp1_clean, ma_path, ma_blocks3, modes = ma_modes3)
# 
# exp1_pls3
# summary(exp1_pls3)
# 
# # GOODNESS-OF-FIT 
# # [1]  0.5667  # ----> Mesmo valor que o PLS round 2
# 
# # Unidimensionalidade:
# exp1_pls3$unidim # OK
# 
# # Loadings
# plot(exp1_pls2, what = "loadings")
# exp1_pls$outer_model
# # subset(exp1_pls$outer_model, block == "DPL")
# # subset(exp1_pls$outer_model, block == "Resource_dist")
# 
# 
# # PLS round 4 ------
# # let's drop variables with loadings < 0.7
# # The first one is n_trees because it has loadins = 0.15 in the resource distribution block
# 
# ma_blocks4 = list(
#   
#   # LV Resource distribution:
#   c("NN_feeding_trees", "NN_sleeping_trees"),
#   
#   # LV Diet:
#   c("p_frugivory_neg", "p_resting_neg"),
#   
#   # LV Movement:
#   c("p_traveling", "step", "n_trees_neg", "NN_feeding_trees", "NN_sleeping_trees"), 
#   
#   # LV DPL: 
#   c("p_traveling", "step", "n_trees_neg", "NN_feeding_trees", "NN_sleeping_trees"
#     , "p_frugivory_neg", "p_resting_neg"
#     , "fragment_shape_neg"
#   ), 
#   
#   # LV SDD:
#   c("p_traveling", "step", "n_trees_neg", "NN_feeding_trees", "NN_sleeping_trees"
#     , "p_frugivory_neg", "p_resting_neg"
#     , "fragment_shape_neg"
#     , "hr_size")
#   
# )
# 
# ma_modes4 = rep("A", 5) # são 1 (=numero de variaveis latentes) e não 11 (numero de variáveis)
# 
# exp1_pls4 <- plspm(dfexp1_clean, ma_path, ma_blocks4, modes = ma_modes4)
# 
# exp1_pls4
# summary(exp1_pls4)
# 
# # GOODNESS-OF-FIT 
# # [1]  0.5646  # ----> Um pouco menor que o PLS round 3
# 
# # Unidimensionalidade:
# exp1_pls4$unidim # Resource_dist ainda nao unidimensional
# 
# 
# # PLS Round 5 -----
# # Com NN_feeding + n_trees_neg formando o LV Resource_dist
# ma_blocks5 = list(
#   
#   # LV Resource distribution:
#   c("NN_feeding_trees", "n_trees_neg"),
#   
#   # LV Diet:
#   c("p_frugivory_neg", "p_resting_neg"),
#   
#   # LV Movement:
#   c("p_traveling", "step", "n_trees_neg", "NN_feeding_trees", "NN_sleeping_trees"), 
#   
#   # LV DPL: 
#   c("p_traveling", "step", "n_trees_neg", "NN_feeding_trees", "NN_sleeping_trees"
#     , "p_frugivory_neg", "p_resting_neg"
#     , "fragment_shape_neg"
#   ), 
#   
#   # LV SDD:
#   c("p_traveling", "step", "n_trees_neg", "NN_feeding_trees", "NN_sleeping_trees"
#     , "p_frugivory_neg", "p_resting_neg"
#     , "fragment_shape_neg"
#     , "hr_size")
#   
# )
# 
# ma_modes5 = rep("A", 5) # são 1 (=numero de variaveis latentes) e não 11 (numero de variáveis)
# 
# exp1_pls5 <- plspm(dfexp1_clean, ma_path, ma_blocks5, modes = ma_modes5)
# 
# exp1_pls5
# summary(exp1_pls5)
# 
# # GOODNESS-OF-FIT 
# # [1]  0.5754  # ----> Um pouco maior que o PLS round 3
# 
# # Unidimensionalidade:
# exp1_pls5$unidim # Resource_dist ainda nao unidimensional
# 
# # Loadings
# plot(exp1_pls5, what = "loadings")
# exp1_pls5$outer_model
# subset(exp1_pls5$outer_model, block == "Diet") # esse valor de NN_sleeping_trees tá pessimo.
# 
# 
# 
# # PLS Round 6 -----
# # Sem Resource distribution LV, Sem NN_sleeping_trees
# ma6 <- readxl::read_excel(paste0("D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest_2024/0_files/"
#                                  , "PLS_design.xlsx"),
#                           sheet = 7)
# 
# rown <- colnames(ma6)
# rown <- rown[-1]
# 
# ma_path6 <- ma6[ , -1] %>% as.matrix
# dim(ma_path6)
# 
# 
# innerplot(ma_path6,  box.prop = 0.25)
# 
# rownames(ma_path6) <- colnames(ma_path6)
# 
# ma_path6
# 
# ma_blocks6 = list(
#   
#   # LV Diet:
#   c("p_frugivory_neg", "p_resting_neg"),
#   
#   # LV Movement:
#   c("p_traveling", "step", "n_trees_neg", "NN_feeding_trees"), 
#   
#   # LV DPL: 
#   c("p_traveling", "step", "n_trees_neg", "NN_feeding_trees"
#     , "p_frugivory_neg", "p_resting_neg"
#     , "fragment_shape_neg"
#   ), 
#   
#   # LV SDD:
#   c("p_traveling", "step", "n_trees_neg", "NN_feeding_trees"
#     , "p_frugivory_neg", "p_resting_neg"
#     , "fragment_shape_neg"
#     , "hr_size")
#   
# )
# 
# ma_modes6 = rep("A", 4) # são 1 (=numero de variaveis latentes) e não 11 (numero de variáveis)
# 
# exp1_pls6 <- plspm(dfexp1_clean, ma_path6, ma_blocks6, modes = ma_modes6)
# 
# exp1_pls6
# summary(exp1_pls6)
# 
# # GOODNESS-OF-FIT 
# # [1]  0.7196 # ----> Best so far!!!
# 
# # Unidimensionalidade:
# exp1_pls6$unidim # Resource_dist ainda nao unidimensional
# 
# # Loadings
# plot(exp1_pls6, what = "loadings")
# exp1_pls6$outer_model
# subset(exp1_pls6$outer_model, block == "Diet") 
# # valor de NN_sleeping_trees tava pessimo em Diet, tirei de tudo.
# 
# 
# 
# # PLS Round 7 ----
# # valor de n_trees_neg tava pessimo em Diet e DPL, tirei
# 
# ma_blocks7 = list(
#   
#   # LV Diet:
#   c("p_frugivory_neg", "p_resting_neg"),
#   
#   # LV Movement:
#   c("p_traveling", "step", "NN_feeding_trees"), 
#   
#   # LV DPL: 
#   c("p_traveling", "step", "NN_feeding_trees"
#     , "p_frugivory_neg", "p_resting_neg"
#     , "fragment_shape_neg"
#   ), 
#   
#   # LV SDD:
#   c("p_traveling", "step", "NN_feeding_trees"
#     , "p_frugivory_neg", "p_resting_neg"
#     , "fragment_shape_neg"
#     , "hr_size")
#   
# )
# 
# ma_modes7 = rep("A", 4) # são 1 (=numero de variaveis latentes) e não 11 (numero de variáveis)
# 
# exp1_pls7 <- plspm(dfexp1_clean, ma_path6, ma_blocks7, modes = ma_modes7)
# 
# exp1_pls7
# summary(exp1_pls7)
# 
# # GOODNESS-OF-FIT 
# # [1]  0.76 # ----> Best so far!!!
# 
# # Unidimensionalidade:
# exp1_pls7$unidim # Resource_dist ainda nao unidimensional
# 
# # Loadings
# plot(exp1_pls7, what = "loadings")
# exp1_pls7$outer_model
# subset(exp1_pls7$outer_model, block == "Diet") 
# # valor de NN_feeding_trees e fragment_shape_neg pessimos em DPL, vou retirar
# 
# 
# # PLS Round 8 ----
# # valor de n_trees_neg tava pessimo em Diet e DPL, tirei
# 
# ma_blocks8 = list(
#   
#   # LV Diet:
#   c("p_frugivory_neg", "p_resting_neg"),
#   
#   # LV Movement:
#   c("p_traveling", "step"), 
#   
#   # LV DPL: 
#   c("p_traveling", "step"
#     , "p_frugivory_neg", "p_resting_neg"
#   ), 
#   
#   # LV SDD:
#   c("p_traveling", "step"
#     , "p_frugivory_neg", "p_resting_neg"
#     , "hr_size")
#   
# )
# 
# ma_modes8 = rep("A", 4) # são 1 (=numero de variaveis latentes) e não 11 (numero de variáveis)
# 
# exp1_pls8 <- plspm(dfexp1_clean, ma_path6, ma_blocks8, modes = ma_modes8)
# 
# exp1_pls8
# summary(exp1_pls8)
# 
# # GOODNESS-OF-FIT 
# # [1]  0.8755 # ----> Best so far!!!
# 
# # Unidimensionalidade:
# exp1_pls8$unidim # Resource_dist ainda nao unidimensional
# 
# # Loadings
# plot(exp1_pls8, what = "loadings")
# exp1_pls8$outer_model # hr_size tá com um péssimo loading. Vou tirar e ver o que dá
# 
# 
# 
# # PLS Round 9 ----
# 
# # Tirando hr_size da variável latente LV SDD
# 
# ma_blocks9 = list(
#   
#   # LV Diet:
#   c("p_frugivory_neg", "p_resting_neg"),
#   
#   # LV Movement:
#   c("p_traveling", "step"), 
#   
#   # LV DPL: 
#   c("p_traveling", "step"
#     , "p_frugivory_neg", "p_resting_neg"
#   ), 
#   
#   # LV SDD:
#   c("p_traveling", "step"
#     , "p_frugivory_neg", "p_resting_neg"
#     # , "DPL"
#     # , "SDD"
#     # , "hr_size"
#   )
#   
# )
# 
# ma_modes9 = rep("A", 4) # são 1 (=numero de variaveis latentes) e não 11 (numero de variáveis)
# 
# exp1_pls9 <- plspm(dfexp1_clean, ma_path6, ma_blocks9, modes = ma_modes9)
# 
# exp1_pls9
# summary(exp1_pls9)
# 
# # GOODNESS-OF-FIT 
# # [1]  0.9109 # ----> Best so far!!!
# 
# # Unidimensionalidade:
# exp1_pls9$unidim # Resource_dist ainda nao unidimensional
# 
# # Loadings
# plot(exp1_pls9, what = "loadings")
# exp1_pls9$outer_model # hr_size tá com um péssimo loading. Vou tirar e ver o que dá
# 
# 
# # PLS Round 10 ----
# 
# # Tirando hr_size da variável latente LV SDD
# 
# ma_blocks10 = list(
#   
#   # LV Diet:
#   c("p_frugivory_neg", "p_resting_neg"),
#   
#   # LV Movement:
#   c("p_traveling", "step"), 
#   
#   # LV DPL: 
#   c(
#     # "p_traveling", "step"
#     # , "p_frugivory_neg", "p_resting_neg"
#     "DPL"
#   ), 
#   
#   # LV SDD:
#   c(
#     # "p_traveling", "step"
#     # , "p_frugivory_neg", "p_resting_neg"
#     # , "DPL"
#     "SDD"
#     # , "hr_size"
#   )
#   
# )
# 
# ma_modes10 = rep("A", 4) # são 1 (=numero de variaveis latentes) e não 11 (numero de variáveis)
# 
# exp1_pls10 <- plspm(dfexp1_clean, ma_path6, ma_blocks10, modes = ma_modes10)
# 
# exp1_pls10
# summary(exp1_pls10)
# 
# # GOODNESS-OF-FIT 
# # [1]  0.5302 #
# 
# # Unidimensionalidade:
# exp1_pls10$unidim # Resource_dist ainda nao unidimensional
# 
# # Loadings
# plot(exp1_pls10, what = "loadings")
# exp1_pls10$outer_model 
# 
# 
# 
# # PLS Round 11 ----
# 
# # Incluindo variáveis de fragmento (com Erika)
# 
# ma8 <- readxl::read_excel(paste0("D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest_2024/0_files/"
#                                  , "PLS_design.xlsx"),
#                           sheet = 8)
# 
# rown <- colnames(ma8)
# rown <- rown[-1]
# 
# ma_path8 <- ma8[ , -1] %>% as.matrix
# dim(ma_path8)
# 
# 
# innerplot(ma_path8,  box.prop = 0.25)
# 
# rownames(ma_path8) <- colnames(ma_path8)
# 
# ma_path8
# 
# 
# ma_blocks11 = list(
#   
#   # LV Diet:
#   c("p_frugivory_neg", "p_resting_neg"),
#   
#   # LV Movement:
#   c("p_traveling", "step"), 
#   
#   # LV DPL: 
#   c(
#     # "p_traveling", "step"
#     # , "p_frugivory_neg", "p_resting_neg"
#     "DPL"
#   ), 
#   
#   # LV SDD:
#   c(
#     # "p_traveling", "step"
#     # , "p_frugivory_neg", "p_resting_neg"
#     # , "DPL"
#     "SDD"
#     # , "hr_size"
#   ),
#   
#   c('fragment_size'),
#   c('fragment_shape_neg'),
#   c('hr_size')
#   
# )
# 
# ma_modes11 = rep("A", 7) # são 1 (=numero de variaveis latentes) e não 11 (numero de variáveis)
# 
# exp1_pls11 <- plspm(dfexp1_clean, ma_path8, ma_blocks11, modes = ma_modes11)
# 
# exp1_pls11
# summary(exp1_pls11)
# 
# # OODNESS-OF-FIT 
# # [1]  0.5142 #
# 
# # Unidimensionalidade:
# exp1_pls10$unidim # Resource_dist ainda nao unidimensional
# 
# # Loadings
# plot(exp1_pls10, what = "loadings")
# exp1_pls10$outer_model 



# Recomeçando ------

# SDD Round 12 ----

# Falei com Erika e Gisela e percebi que talvez eu nao deveria ter
# nenhuma variável latente, só manifestas.
# Também notei que PT nao tava no modelo e que isso tava dando um monte 
# de problema entre DPL e HR

dfexp1 %>% glimpse()
dfexp1 %>% colnames()


ma12 <- readxl::read_excel(paste0("D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest_2024/0_files/"
                                          , "PLS_design.xlsx"),
                                   sheet = 10)

rown <- colnames(ma12)
rown <- rown[-1]

ma_path12 <- ma12[ , -1] %>% as.matrix
dim(ma_path12)


innerplot(ma_path12,  box.prop = 0.25)

rownames(ma_path12) <- colnames(ma_path12)

ma_path12


ma_blocks12 = list(
  
  c('density'),
  
  c("n_trees"),
  c("NN_feeding_trees"), 
  c("NN_sleeping_trees"),
  c('aggregation_type'),
  
  c('fragment_size'),
  c('fragment_shape'),

  # LV Diet:
  c("p_frugivory", "p_resting"),
  
  # LV Movement:
  c("p_traveling", "step"), 
  
  c("DPL"),
  c('hr_size'),
  # c('PT'),
  
  c("SDD")
)


ma_modes12 = rep("A", 12) # são 1 (=numero de variaveis latentes) e não 12 (numero de variáveis)

exp1_pls12 <- plspm(dfexp1_clean, ma_path12, ma_blocks12, modes = ma_modes12)

exp1_pls12
summary(exp1_pls12)

# GOODNESS-OF-FIT 
# [1] 0.7289 #

# Unidimensionalidade:
exp1_pls12$unidim # Resource_dist ainda nao unidimensional

# Loadings
plot(exp1_pls12, what = "loadings")
exp1_pls12$outer_model 






# Communalities ----
#  Communalities = loadings^2 and they measure
# the part of the variance between a latent variable and its indicator 
# that is common to both. It measures how much of a given manifest 
# variable’s variance is reproducible from the latent variable.

# *** Indicators with low communality are those for which the model is “not working” and the
# researcher may use this information to drop such variables from the analysis

# For instance, let’s check what’s going on with the Defense block. The manifest variable GCH
# has a loading of 0.4837 which is less than the recommended 0.7. In turn, its communality is
# small with a value of 0.2339.
# Although this indicator has some contribution to the Quality of
# Defense, it doesn’t seem to be very happy in the Defense block. If we don’t want to have
# an uncomfortable indicator, the best option is to remove it from the model. For convenience
# and illustration purposes I’m going to keep GCH in our exmple, but if you find yourself in
# the middle of a similar issue, you should talk to the experts and discuss whether to keep or
# remove unhappy reflective indicators that don’t reflect enough.

# Cross-loadings
# The reason for doing so is that we need to be sure that we don’t have traitor
# indicators. The data frame of cross-loadings are in $crossloadings

# cross-loadings
exp1_pls12$crossloadings

# GSH has a loading value of 0.9380. This value must be greater than any other value in that first row.
# The cross-loadings of GSH with Defense is 0.5159; the cross-loading of GSH with Success is
# 0.8977. Clearly, 0.9380 is greater than 0.5159 and 0.8977.

# An alternative way to examine the table of cross-loadings is by visualizing them with some
# bar-charts like those shown in the figure below:


library('tidyverse')
library('reshape')

# reshape crossloadings data.frame for ggplot
xloads = melt(exp1_pls12$crossloadings, id.vars = c("name", "block"),
              variable_name = "LV") %>% 
  mutate(
    loadings = value
  )

# bar-charts of crossloadings by block
ggplot(data = xloads,
       aes(y = name, x = loadings, fill = block)) +
  # add horizontal reference lines
  geom_vline(xintercept = 0, color = "gray75") +
  geom_vline(xintercept = 0.5, color = "gray70", linetype = 2) +
  # indicate the use of car-charts
  geom_bar(stat = 'identity', position = 'dodge') +
  # panel display (i.e. faceting)
  facet_grid(block ~ LV) +
  # tweaking some grahical elements
  theme(axis.text.x = element_text(angle = 90),
        line = element_blank(),
        plot.title = element_text(size = 12)) +
  # add title
  ggtitle("Crossloadings") +
  ylab("")


ggsave(paste0(pathexp1, "crossloadings_SDD_PLS12.png"), 
              height = 20, width = 20, dpi = 300)

# The whole idea is to verify that the shared variance between a
# construct and its indicators is larger than the shared variance with other constructs. In other
# words, no indicator should load higher on another construct than it does on the construct
# it intends to measure. *** Otherwise, it is a traitor indicator ***
# . If an indicator loads higher with
# other constructs than the one it is intended to measure, we might consider its appropriateness
# because it is not clear which construct or constructs it is actually reflecting

# Inner model ----
exp1_pls12$inner_model

# inner model summary
exp1_pls12$inner_summary

# High redundancy means high ability to predict. 
# The column R-square is only available for endogenous variables
# The average communality Av.Commu indicates how much of the block variability is reproducible by the latent variable.
# Let’s say that we are interested in checking how well the indepedent LVs predict values of endogenous indicators. In
# our example the average redundancy for Success represents that Attack and Defense predict
# 68% of the variability of Success indicators


# GoF index (pseudo goodnes of fit)
# A remarkable aspect is that no single criterion exists within the PLS framework to measure
# the overall quality of a model, so we cannot perform inferential statistical tests for goodness
# of fit. As an alternative, non-parametrical tests can be applied for the assessment of the
# structural model

# Since it takes in to account communality, this index is more applicable to reflective indicators than to formative indicators. 
# However, you can also use the GoF index in presence of formative blocks, in which case more importance
# will be given to the average R2.

# gof index
exp1_pls12$gof 

# GoF can be used a global criterion that helps us to evaluate the performance of the model in
# both the inner and the outer models
# Drawback: there is no threshold that allows us to determine its statistical significanc
# a GoF value of 0.78 could be interpreted as if the prediction power of the
# model is of 78%. The naive rule of thumb is: the higher, # the better. 
# Acceptable “good” values within the PLS-PM community are GoF >0.7


### 4.6 Validation ---------

# Since PLS-PM does not rest on any distributional assumptions, significance levels for the parameter estimates (based on normal theory) are not suitable. Instead, resampling procedures
# such as blindfolding, jackknifing, and bootstrapping are used to obtain information about
# the variability of the parameter estimates. plspm() uses the former approach to provide a
# means for validating results.

# running bootstrap validation
exp1_pls12 <- plspm(dfexp1_clean, ma_path12, ma_blocks12, modes = ma_modes12,
                    boot.val = TRUE, br = 200)

# bootstrap results
exp1_pls12$boot

summary(exp1_pls12)

exp1_pls12_inner <- exp1_pls12$inner_model %>% do.call(rbind.data.frame, .)
exp1_pls12_outer <- exp1_pls12$outer_model
exp1_pls12_innersummary <- exp1_pls12$inner_summary
exp1_pls12_effects <- exp1_pls12$effects
exp1_pls12_unidim <- exp1_pls12$unidim
exp1_pls12_crossloadings <- exp1_pls12$crossloadings
exp1_pls12_boot <- exp1_pls12$boot$total.efs
exp1_pls12_boot$relationships <- rownames(exp1_pls12_boot)

class(exp1_pls12_inner)

exp1_pls12_effects <- dplyr::left_join(exp1_pls12_effects, exp1_pls12_boot)

exp1_pls12_inner %>% 
  write.csv(paste0(pathexp1, "SDD_PLS12_inner.csv"))
exp1_pls12_outer %>% 
  write.csv(paste0(pathexp1, "SDD_PLS12_outer.csv"))
exp1_pls12_innersummary %>% 
  write.csv(paste0(pathexp1, "SDD_PLS12_innersummary.csv"))
exp1_pls12_effects %>% 
  write.csv(paste0(pathexp1, "SDD_PLS12_effects.csv"))
exp1_pls12_unidim %>% 
  write.csv(paste0(pathexp1, "SDD_PLS12_unidim.csv"))
exp1_pls12_crossloadings %>% 
  write.csv(paste0(pathexp1, "SDD_PLS12_crossloadings.csv"))




# SEED DISPERSION (NN DIST OF SEED DISPERSAL EVENTS) ------
  NN01 <- readxl::read_excel(paste0("D:/Data/Documentos/Study/Mestrado/Model_Documentation/build_forest_2024/0_files/"
                                  , "PLS_design.xlsx"),
                           sheet = 11)

# MESMO MODELO QUE SDD (SEM PT)

rown <- colnames(NN01)
rown <- rown[-1]

NN01_path <- NN01[ , -1] %>% as.matrix
dim(NN01_path)


innerplot(NN01_path,  box.prop = 0.25)

rownames(NN01_path) <- colnames(NN01_path)

NN01_path


NN01_blocks = list(
  
  c('density'),
  
  # LV Diet:
  c("p_frugivory", "p_resting"),
  
  # LV Movement:
  c("p_traveling", "step"), 
  
  c("n_trees"),
  c("NN_feeding_trees"), 
  c("NN_sleeping_trees"),
  c('aggregation_type'),
  
  c("DPL"),
  # c('PT'),
  
  c('fragment_size'),
  c('fragment_shape'),
  c('hr_size'),
  
  c("NN_seeds")
)


NN01_modes = rep("A", 12) # são 1 (=numero de variaveis latentes) e não 12 (numero de variáveis)

NN01_pls <- plspm(dfexp1_clean, NN01_path, NN01_blocks, modes = NN01_modes)

NN01_pls
summary(NN01_pls)

# GOODNESS-OF-FIT 
# [1]  0.7224

# Unidimensionalidade:
NN01_pls$unidim # Resource_dist ainda nao unidimensional

# Loadings
plot(NN01_pls, what = "loadings", lcol = 'grey50', box.prop = 0.25)
plot(NN01_pls, lcol = 'grey50', box.prop = 0.25, arr.pos = 0.25)
NN01_pls$outer_model 






# Communalities ----
#  Communalities = loadings^2 and they measure
# the part of the variance between a latent variable and its indicator 
# that is common to both. It measures how much of a given manifest 
# variable’s variance is reproducible from the latent variable.

# *** Indicators with low communality are those for which the model is “not working” and the
# researcher may use this information to drop such variables from the analysis

# For instance, let’s check what’s going on with the Defense block. The manifest variable GCH
# has a loading of 0.4837 which is less than the recommended 0.7. In turn, its communality is
# small with a value of 0.2339.
# Although this indicator has some contribution to the Quality of
# Defense, it doesn’t seem to be very happy in the Defense block. If we don’t want to have
# an uncomfortable indicator, the best option is to remove it from the model. For convenience
# and illustration purposes I’m going to keep GCH in our exmple, but if you find yourself in
# the middle of a similar issue, you should talk to the experts and discuss whether to keep or
# remove unhappy reflective indicators that don’t reflect enough.

# Cross-loadings
# The reason for doing so is that we need to be sure that we don’t have traitor
# indicators. The data frame of cross-loadings are in $crossloadings

# cross-loadings
NN01_pls$crossloadings

# GSH has a loading value of 0.9380. This value must be greater than any other value in that first row.
# The cross-loadings of GSH with Defense is 0.5159; the cross-loading of GSH with Success is
# 0.8977. Clearly, 0.9380 is greater than 0.5159 and 0.8977.

# An alternative way to examine the table of cross-loadings is by visualizing them with some
# bar-charts like those shown in the figure below:


library('tidyverse')
library('reshape')

# reshape crossloadings data.frame for ggplot
xloads = melt(NN01_pls$crossloadings, id.vars = c("name", "block"),
              variable_name = "LV") %>% 
  mutate(
    loadings = value
  )

# bar-charts of crossloadings by block
ggplot(data = xloads,
       aes(y = name, x = loadings, fill = block)) +
  # add horizontal reference lines
  geom_vline(xintercept = 0, color = "gray75") +
  geom_vline(xintercept = 0.5, color = "gray70", linetype = 2) +
  # indicate the use of car-charts
  geom_bar(stat = 'identity', position = 'dodge') +
  # panel display (i.e. faceting)
  facet_grid(block ~ LV) +
  # tweaking some grahical elements
  theme(axis.text.x = element_text(angle = 90),
        line = element_blank(),
        plot.title = element_text(size = 12)) +
  # add title
  ggtitle("Crossloadings") +
  ylab("")


ggsave(paste0(pathexp1, "crossloadings_NN01.png"), 
       height = 20, width = 20, dpi = 300)

# The whole idea is to verify that the shared variance between a
# construct and its indicators is larger than the shared variance with other constructs. In other
# words, no indicator should load higher on another construct than it does on the construct
# it intends to measure. *** Otherwise, it is a traitor indicator ***
# . If an indicator loads higher with
# other constructs than the one it is intended to measure, we might consider its appropriateness
# because it is not clear which construct or constructs it is actually reflecting

# Inner model ----
NN01_pls$inner_model

# inner model summary
NN01_pls$inner_summary

# High redundancy means high ability to predict. 
# The column R-square is only available for endogenous variables
# The average communality Av.Commu indicates how much of the block variability is reproducible by the latent variable.
# Let’s say that we are interested in checking how well the indepedent LVs predict values of endogenous indicators. In
# our example the average redundancy for Success represents that Attack and Defense predict
# 68% of the variability of Success indicators


# GoF index (pseudo goodnes of fit)
# A remarkable aspect is that no single criterion exists within the PLS framework to measure
# the overall quality of a model, so we cannot perform inferential statistical tests for goodness
# of fit. As an alternative, non-parametrical tests can be applied for the assessment of the
# structural model

# Since it takes in to account communality, this index is more applicable to reflective indicators than to formative indicators. 
# However, you can also use the GoF index in presence of formative blocks, in which case more importance
# will be given to the average R2.

# gof index
NN01_pls$gof 

# GoF can be used a global criterion that helps us to evaluate the performance of the model in
# both the inner and the outer models
# Drawback: there is no threshold that allows us to determine its statistical significanc
# a GoF value of 0.78 could be interpreted as if the prediction power of the
# model is of 78%. The naive rule of thumb is: the higher, # the better. 
# Acceptable “good” values within the PLS-PM community are GoF >0.7


### 4.6 Validation ---------

# Since PLS-PM does not rest on any distributional assumptions, significance levels for the parameter estimates (based on normal theory) are not suitable. Instead, resampling procedures
# such as blindfolding, jackknifing, and bootstrapping are used to obtain information about
# the variability of the parameter estimates. plspm() uses the former approach to provide a
# means for validating results.

# running bootstrap validation
NN01_pls <- plspm(dfexp1_clean, NN01_path, NN01_blocks, modes = NN01_modes,
                    boot.val = TRUE, br = 200)

# bootstrap results
NN01_pls$boot

summary(NN01_pls)

NN01_pls_inner 


NN01_pls_inner <- NN01_pls$inner_model %>% do.call(rbind.data.frame, .)
NN01_pls_outer <- NN01_pls$outer_model
NN01_pls_innersummary <- NN01_pls$inner_summary
NN01_pls_effects <- NN01_pls$effects
NN01_pls_unidim <- NN01_pls$unidim
NN01_pls_crossloadings <- NN01_pls$crossloadings
NN01_pls_boot <- NN01_pls$boot$total.efs
NN01_pls_boot$relationships <- rownames(NN01_pls_boot)

class(NN01_pls_inner)

NN01_pls_effects <- dplyr::left_join(NN01_pls_effects, NN01_pls_boot)

NN01_pls_inner %>% 
  write.csv(paste0(pathexp1, "NN01_pls_inner.csv"))
NN01_pls_outer %>% 
  write.csv(paste0(pathexp1, "NN01_pls_outer.csv"))
NN01_pls_innersummary %>% 
  write.csv(paste0(pathexp1, "NN01_pls_innersummary.csv"))
NN01_pls_effects %>% 
  write.csv(paste0(pathexp1, "NN01_pls_effects.csv"))
NN01_pls_unidim %>% 
  write.csv(paste0(pathexp1, "NN01_pls_unidim.csv"))
NN01_pls_crossloadings %>% 
  write.csv(paste0(pathexp1, "NN01_pls_crossloadings.csv"))
