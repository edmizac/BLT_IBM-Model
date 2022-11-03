library(here)
library(tidyverse)

dat.gua <- read.csv2(here("Data", "Trees-Guarei", "Data_Guarei_FelipeBufalo_EMZ1_trees_all.csv"),
                     stringsAsFactors = TRUE)
colnames(dat.gua)[1] <- "x"
dat.gua <- dat.gua %>% filter(behavior %in% c("sleeping", "resting",  "feed_fruits"))
# aux <- dat.gua %>% filter(behavior == "resting")
# aux <- dat.gua %>% filter(behavior == "sleeping")
# aux <- dat.gua %>% filter(behavior == "feed_fruits")


# filter by month, get unique and save to distinct .csv files:
dat.gua[!duplicated(dat.gua[ , 1:2]), ] %>% 
  write.csv2(here("Data", "Trees-Guarei","Guarei_trees_unique_all.csv"),
             row.names = FALSE)

gua.aug <- dat.gua %>% filter(id_month == "may")
gua.aug[!duplicated(gua.aug[ , 1:2]), ] %>% 
  write.csv2(here("Data", "Trees-Guarei","Guarei_trees_unique_aug.csv"),
             row.names = FALSE)

gua.jul <- dat.gua %>% filter(id_month == "july")
gua.jul[!duplicated(gua.jul[ , 1:2]), ] %>% 
  write.csv2(here("Data", "Trees-Guarei","Guarei_trees_unique_jul.csv"),
             row.names = FALSE)

gua.jun <- dat.gua %>% filter(id_month == "june")
gua.jun[!duplicated(gua.jun[ , 1:2]), ] %>% 
  write.csv2(here("Data", "Trees-Guarei","Guarei_trees_unique_jun.csv"),
             row.names = FALSE)

gua.may <- dat.gua %>% filter(id_month == "may")
gua.may[!duplicated(gua.may[ , 1:2]), ] %>% 
  write.csv2(here("Data", "Trees-Guarei","Guarei_trees_unique_may.csv"),
             row.names = FALSE)


# filter sleeping sites, make unique and write it.csv files:
gua.slp <- dat.gua %>% filter(behavior == "sleeping")
gua.slp[!duplicated(gua.slp[ , 1:2]), ] %>% 
  write.csv2(here("Data", "Trees-Guarei","Guarei_trees_unique_slp.csv"),
             row.names = FALSE)

