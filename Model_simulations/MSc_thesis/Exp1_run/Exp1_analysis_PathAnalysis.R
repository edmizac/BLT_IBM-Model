# Script name: Exp1_analysis_PathAnalysis.R
# Script purpose:

# Date created: 26/09/2023
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



# PLS Walktrough (Chapter 1 PLS with R) -----
data(spainfoot)

spainfoot %>% head()
# GSH total number of goals scored at home
# GSA total number of goals scored away
# SSH percentage of matches with scores goals at home
# SSA percentage of matches with scores goals away
# GCH total number of goals conceded at home
# GCA total number of goals conceded away
# CSH percentage of matches with no conceded goals at home
# CSA percentage of matches with no conceded goals away
# WMH total number of won matches at home
# WMA total number of won matches away
# LWR longest run of won matches
# LRWL longest run of matches without losing
# YC total number of yellow cards
# RC total number of red cards


# Create inner model:
# Required for plspm: 1) a data set, 2) an inner model, and 3) an outer model.


## 2) inner model -----
# requires a lower triangular boolean matrix --> COLUMNS AFFECTING ROWS ******
# In other words, it must be a square matrix (same number of rows and columns), 
# the elements in the diagonal and above it must be zeros, and the elements below
# the diagonal can be either zeros or ones.

Attack = c(0, 0, 0)
Defense= c(0, 0, 0)
Success = c(1, 1, 0)

foot_path <- rbind(Attack, Defense, Success)

colnames(foot_path) <- rownames(foot_path)

foot_path

# plot the path matrix
innerplot(foot_path)



## 3) outer model -----
# define list of indicators: what variables are associated with what latent variables.
# Thus, the first block corresponding to the latent variable Attack is # associated with 
# the first four columns of the data set... The second block, associated to Defense, 
# is formed by columns from 5 to 8. And Success is associated with columns from 9 to 12:
foot_blocks = list(1:4, 5:8, 9:12)



# Set mode
# all latent variables are measured in a reflective way ("mode A") -> reflexive
foot_modes = c("A", "A", "A")

# Success in formative mode B -> formative
foot_modes2 = c("A", "A", "B")


# run plspm analysis
foot_pls <- plspm(spainfoot, foot_path, foot_blocks, modes = foot_modes)

foot_pls


# path coefficients
foot_pls$path_coefs

# inner model
foot_pls$inner_model

# summarized results
summary(foot_pls)

# plotting results (inner model)
plot(foot_pls)

plot(foot_pls, what = "loadings", arr.width = 0.1)

# show me the first scores
head(foot_pls$scores, n = 5)



# 




# Exp1 Data -----


## 1) Data -----

dfexp1_pa <- dfexp1 %>% 
  ungroup() %>% 
  dplyr::select(c("SDD", "NN_seeds", 23:54)) %>% 
  dplyr::select(-c(#`random-seed`, siminputrow
    #, `survived?`
    n                            # don't take this out on Exp2
    , R_seeds, R_seeds
    , n_visited_trees, n_unvisited_trees
    # , hr_size_final
    #, SDD
    , SDD_sd
  ))

colnames(dfexp1_pa)


## 2) inner model -----
# requires a lower triangular boolean matrix --> COLUMNS AFFECTING ROWS ******
# In other words, it must be a square matrix (same number of rows and columns), 
# the elements in the diagonal and above it must be zeros, and the elements below
# the diagonal can be either zeros or ones.

Attack = c(0, 0, 0)
Defense= c(0, 0, 0)
Success = c(1, 1, 0)

foot_path <- rbind(Attack, Defense, Success)

colnames(foot_path) <- rownames(foot_path)

foot_path

# plot the path matrix
innerplot(foot_path)



## 3) outer model -----
# define list of indicators: what variables are associated with what latent variables.
# Thus, the first block corresponding to the latent variable Attack is # associated with 
# the first four columns of the data set... The second block, associated to Defense, 
# is formed by columns from 5 to 8. And Success is associated with columns from 9 to 12:
foot_blocks = list(1:4, 5:8, 9:12)



# Set mode
# all latent variables are measured in a reflective way ("mode A") -> reflexive
foot_modes = c("A", "A", "A")

# Success in formative mode B -> formative
foot_modes2 = c("A", "A", "B")


# run plspm analysis
foot_pls <- plspm(spainfoot, foot_path, foot_blocks, modes = foot_modes)

foot_pls


# path coefficients
foot_pls$path_coefs

# inner model
foot_pls$inner_model

# summarized results
summary(foot_pls)

# plotting results (inner model)
plot(foot_pls)

plot(foot_pls, what = "loadings", arr.width = 0.1)

# show me the first scores
head(foot_pls$scores, n = 5)

