library("here")
library("nlrx")
library("ggplot2")

path <- here("Model_analysis", "Sensitivity-analysis",
             "v1.1_November2022", "temp")

nl <- readRDS(here("Model_analysis", "Sensitivity-analysis",
                    "v1.1_November2022", "temp", "v1.1_Taquara_Jan_simple-379681892_tempRDS.Rdata"))
                    # "v1.1_November2022", "temp", "v1.1_Guareí_Aug_simple33642352_tempRDS.Rdata"))
db  <-  unnest_simoutput(nl)



seeds <- db %>% 
  dplyr::filter(breed == "seeds") %>% 
  droplevels()


seeds %>% 
  ggplot() +
  geom_point(aes(x = x_UTM, y = y_UTM, color = species, 
                 # shape = `disp-day`,
                 size = SDD),
             alpha = 0.3)


ggplot(db) +
  geom_point(data = subset(db, breed == "seeds"), 
             aes(x = x_UTM, y = y_UTM, color = species, shape = `disp-day`)) #https://stackoverflow.com/questions/60817705/filtering-values-in-ggplot2
  


# Grep files
nls_to_df <- list.files(here("Model_analysis", "Sensitivity-analysis",
                             "v1.1_November2022", "temp"), pattern = "tempRDS") #%>% # .RData does not work; "v1.1" works.
  # as.factor() 
  
i <- 1
for (f in nls_to_df) {
  
  # nl_file <- paste0("nl_", i)
  nl_file <- readRDS(paste0(path, "/", f))
  
  # paste0("df_", i)
  if (i == 1) {
    db <- unnest_simoutput(nl_file)
  }
  
  db <- dplyr::bind_rows(db, unnest_simoutput(nl_file))
  
  i <- i + 1
}

# Write csv
db %>%
  write.csv(paste0(path, "/", "02_Simoutput-simple.csv"),
            row.names = FALSE)

db %>% 
  dplyr::filter(
    
      study_area == "Suzano"            & 
      `feeding-trees-scenario` == "Dec" & 
      `random-seed` ==	-1386311883     &
        breed == "seeds"
      
  ) %>% 
  
ggplot() +
  geom_point(aes(x = x_UTM, y = y_UTM, color = species, shape = `disp-day`)) + #https://stackoverflow.com/questions/60817705/filtering-values-in-ggplot2
  theme(legend.position = "none")
