# Script name: Exp1_analysis_FeatureSelection.R
# Script purpose:

# Date created: 26/09/2023
# Author: Eduardo Zanette

## Notes --------------------------- 
# Created after the course on ML
# Uses dfexp1 from "Exp1_analysis.R" and 
# is based on script 'Aula 10 - Seleção de variáveis.R'

## Options -------------------------
# (plotting, memory limit, decimal digits)
# 

## Packages -------------------------
library("here")
library("tidyverse")
library("caret")
library("randomForest")


# Take out characters from names
dfexp1_fs <- dfexp1 %>% 
  rename_with(~stringr::str_replace_all(., c("-" = "_", "\\[" = "", "\\]" = "")))
  
  
dfexp1_fs <- dfexp1 %>% 
  ungroup() %>% 
  dplyr::select(c("SDD", 23:54)) %>% 
  dplyr::select(-c(#`random-seed`,
                   #, `survived?`
                   n # don't take this out on Exp2
                   , siminputrow, `survived?`
                   , R_seeds, R_seeds, NN_seeds
                   , n_visited_trees, n_unvisited_trees
                   # , hr_size_final
                   #, SDD
                   , SDD_sd
                   ))

colnames(dfexp1_fs)



colnames(dfexp1_fs)


# 1) Selecting features based on randomForest algorithm -----
# - Does not require normalization

set.seed(10)

ctrl <- caret::rfeControl(functions = rfFuncs,      # usa random forest
                          method = "repeatedcv",
                          repeats = 5,
                          verbose = FALSE
)

lmProfile <- caret::rfe(y = dfexp1_fs$SDD, x = dfexp1_fs[ , -1], #-SDD
                        data = dfexp1_fs,
                        # sizes = subsets, ### check
                        rfeControl = ctrl)

lmProfile

# Nome das variaveis
predictors(lmProfile)

# Ver o modelo
lmProfile$fit
# lmProfile$pred

# 
lmProfile$optVariables

# Plotar o grafico
trellis.par.set(caretTheme())
plot(lmProfile, type = c("g", "o"))

# Ver importancia
varimp_data <- data.frame(feature = row.names(varImp(lmProfile))[1:8],
                          importance = varImp(lmProfile)[1:8, 1])

ggplot(data = varimp_data, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  theme_bw() + theme(legend.position = "none")


# Simulate new data
preds <- dfexp1_fs %>% 
  dplyr::select(-SDD)

cols <- colnames(preds)
vals <- rep(NA, ncol(preds))
names(vals) <- colnames(preds)
vals

str(vals)
vals <- vals %>% t() %>% as.data.frame()

newdata <- preds[0, ]

set.seed(124)

for(n in 1:1000) {
  i <- 1
  for(col in cols) {
    obs <- sample(preds[[col]], size = 1)
    vals[i] <- obs
    i <- i + 1
  }
  newdata <- bind_rows(newdata, vals)
}

newdata
  

# Predict 
predict(lmProfile, newdata = newdata)

lmProfile


# 
# # 2) Selecting features based on lm -----
# # - requires normalization
# 
# # Normalization
# normalization <- preProcess(x)
# x <- predict(normalization, x)
# x <- as.data.frame(x)
# subsets <- c(1:25)
# 
# ctrl <- caret::rfeControl(functions = lmFuncs,    # usa regressão simples (lm)
#                           method = "repeatedcv",
#                           repeats = 5,
#                           verbose = FALSE
# )
# 
# lmProfile <- caret::rfe(y = SDD, x = .,
#                         data = dfexp1_fs,
#                         # sizes = subsets,
#                         rfeControl = ctrl)
# 
# lmProfile
# 
# 
# # 3) Selecting features based on genetic algorithm -----
# # - requires normalization





# Selecting features with a ROC curve (Aula 12 ML)
#use roc_curve area as score
var(dados4)
temp<-ifelse(dados4$y=="benign",1,2)
roc_imp <- filterVarImp(x = dados4[,c(1:8)], y = temp)

roc_imp

#sort the score in decreasing order
roc_imp <- data.frame(cbind(variable = rownames(roc_imp), score = roc_imp[,1]))
roc_imp$score <- as.double(roc_imp$score)
roc_imp$Percentage<-(100*roc_imp$score)/sum(roc_imp$score) # porcentagem de explicação?
roc_imp[order(roc_imp$Percentage,decreasing = TRUE),]
variaveis_manter<-roc_imp[roc_imp$Percentage > 11, 1] # manter as variáveis que explicam mais que 11%

# normalmente se usa 5% ao invés de 11%. Nesse caso as variáveis explicam muito

dados5<-dados4[,colnames(dados4) %in% variaveis_manter]
dados5$y<-dados4$y
head(dados5)