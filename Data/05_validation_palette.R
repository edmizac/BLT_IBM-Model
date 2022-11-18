library("here")


# 1) Color palette https://www.learnui.design/tools/data-color-picker.html#palette
pal1_5C <- c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600")
pal1b_8C <- c("#003f5c","#2f4b7c","#665191","#a05195","#d45087","#f95d6a","#ff7c43","#ffa600") # 8 colors

# 2) Color palette Tritanomaly blind https://coolors.co/351d34-f5ccb5-159ea2-d05d6c-ec4934
## Get 5 tonned down colors to compare with simulated places
pal2_5C <- c("#351d34", "#f5ccb5", "#159ea2", "#d05d6c", "#ec4934")


# 3) Only God Forgives http://colormind.io/
pal3_5C <- c("#5D6049", "#282C26", "#5B80A0", "#76A3B6", "#D3DADC")

# 4) Spectral with 10 colors https://loading.io/color/feature/Spectral-10/
## *CHECK OTHER WITH 10 COLORS: https://loading.io/color/random/
pal4_10C <- c("#9e0142", "#d53e4f", "#f6693d", "#fea752", "#faec86", "#efff9a", "#abdda4", "#66c2a5", "#3288bd", "#5e4fa2")


# Get objects
ls()
mget(ls())
rm(list)

# for ( obj in ls() ) { 
  list <- append(list, mget(ls()))
  # }
  
# ls(pattern = "([p])\w+") https://paulvanderlaken.files.wordpress.com/2017/08/r-regular-expression-cheetsheat.pdf and https://regexr.com/

# Save 
saveRDS(list, file = here("Data", "palletes.rds"))
# save(list, file = here("Data", "palletes.RData")) # does not work

# Check it
palletes <- readRDS(here("Data", "palletes.rds")) #%>% unlist()
# palletes <- load(here("Data", "palletes.RData")) %>% unlist()  # does not work


