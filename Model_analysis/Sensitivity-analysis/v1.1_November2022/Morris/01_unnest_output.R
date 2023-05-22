library("here")
library("nlrx")
library("ggplot2")
library("dplyr")
library("stringr")
library("stringi")
library("vctrs")
library("nlrx")
library("purrr")



path <- here("Model_analysis", "Sensitivity-analysis",
             "v1.1_November2022", "temp")

nl <- readRDS(here("Model_analysis", "Sensitivity-analysis",
                    "v1.1_November2022", "temp", 
                   # "v1.1_Guareí_Aug_simple778458464_tempRDS.Rdata"))
                    "v1.1_Taquara_Jan_simple-609482361_tempRDS.Rdata"))
                    # "v1.1_November2022", "temp", "v1.1_Guareí_Aug_simple33642352_tempRDS.Rdata"))
db1  <-  unnest_simoutput(nl)
eval_simoutput(nl)
nl@simdesign@siminput

db1 %>% str()
db1$turn_ang_sd%>% str()

db1 <- db1 %>% 
  dplyr::select(
    -c(1:6, 
       agent, 
       siminputrow
       )
    ) # agent is always 'turtles', siminputrow is always 1

seeds <- db1 %>% 
  dplyr::filter(breed == "seeds") %>% 
  droplevels()


seeds %>% 
  ggplot() +
  geom_point(aes(x = x_UTM, y = y_UTM, color = species, 
                 # shape = `disp-day`,
                 size = SDD),
             alpha = 0.3)


ggplot(db1) +
  geom_point(data = subset(db1, breed == "seeds"), 
             aes(x = x_UTM, y = y_UTM, color = species, shape = `disp-day`)) #https://stackoverflow.com/questions/60817705/filtering-values-in-ggplot2
  


# Grep files
nls_to_df <- list.files(here("Model_analysis", "Sensitivity-analysis",
                             "v1.1_November2022", "temp"), pattern = "tempRDS") #%>% # .RData does not work; "v1.1" works.
  # as.factor() 
  
i <- 1
for (f in nls_to_df) {
  
  # nl_file <- paste0("nl_", i)
  nl_file <- readRDS(paste0(path, "/", f))
  # nl_file <- readRDS(paste0(path, "/", "v1.1_Taquara_Jan_simple1671135962_tempRDS.Rdata"))
  # nl_file <- readRDS(paste0(path, "/", "v1.1_Suzano_Sep_simple453130432_tempRDS.Rdata"))
  
  # paste0("df_", i)
  if (i == 1) {
   db <- unnest_simoutput(nl_file) #%>% 
     # mutate(
       # turn_ang_sd = as.character(turn_ang_sd)
     #   turn_ang_sd = case_when(turn_ang_sd == "false" ~ NA), # turn angles are "false" in Guareí Aug (value from amt package)
     #   turn_ang_sd = as.numeric(turn_ang_sd)
     # )
   db$breed %>% unique()
  }
  
  db <- dplyr::bind_rows(db, unnest_simoutput(nl_file) #%>%
                           # mutate(
                             # turn_ang_sd = as.character(turn_ang_sd)
                         
                         #     turn_ang_sd = case_when(turn_ang_sd == "false" ~ NA),
                         #     turn_ang_sd = as.numeric(turn_ang_sd)
                           # )
                         )
  
  i <- i + 1
}


db %>% str()
db$turn_ang_sd %>% unique()
db$study_area %>% unique()

### Repair data
db$species %>% as.factor() %>% levels()

# db$KDE_values

db$`random-seed`

# db1 <- db %>% dplyr::filter(breed == "seeds" & `random-seed` == "-1934277811" )
# db1$x_UTM
# db1$x_UTM.x
# db1$x_UTM.y

# stringr::str_replace_all(db$species, c("\\[" = "", "\\]" = ""))

db1 <- db %>% 
  dplyr::rename_all(~str_replace_all(., c("-" = "_", "\\s+" = "_"))) %>% # Remove - and space characters.

  # fix species names
  mutate(species = stringr::str_replace_all(species, c("\\[" = "", "\\]" = ""))) %>%
  mutate(species = case_when(is.na(species.x) ~ species,
                             TRUE ~ species.x)) %>% 
  dplyr::select(-species.x) %>%
  
  mutate(species = case_when(species == "Abutaselloana" ~ "Abuta selloana",             
                             species == "Allophylusedulis" ~ "Allophylus edulis",          
                             species == "Campomanesiaxanthocarpa" ~ "Campomanesia xanthocarpa",   
                             species == "Caseariasylvestris" ~ "Casearia sylvestris",       
                             species == "Celtisfluminensis" ~ "Celtis fluminensis",         
                             species == "Celtisiguanaea" ~ "Celtis iguanaea",            
                             species == "Cissussulcicaulis" ~ "Cissus sulcicaulis",         
                             species == "Cordiasellowiana" ~ "Cordia sellowiana",         
                             species == "Dyospirosinconstans" ~ "Dyospiros inconstans",       
                             species == "Eugeniaaff.ramboi" ~ "Eugenia aff. ramboi",         
                             species == "Eugeniabrasiliensis" ~ "Eugenia brasiliensis",       
                             species == "Eugeniapunicifolia" ~ "Eugenia punicifolia",       
                             species == "Eugeniasp." ~ "Eugenia sp.",                
                             species == "Ficusenormis" ~ "Ficus enormis",              
                             species == "Myrceugeniaovata" ~ "Myrceugenia ovata",         
                             species == "Myrciariacuspidata" ~ "Myrciaria cuspidata",        
                             species == "Myrciasplendens(Sw.)DC." ~ "Myrcia splendens",    
                             species == "Myrsineumbellata" ~ "Myrsine umbellata",          
                             species == "Peraglabrata" ~ "Pera glabrat",              
                             species == "Pereskiaaculeata" ~ "Pereskia aculeata",          
                             species == "Philodendronspp." ~ "Philodendron spp.",          
                             species == "Phoradendronquadrangulare" ~ "Phoradendron quadrangulare",
                             species == "Pliniatrunciflora" ~ "Plinia trunciflora",         
                             species == "Psidiumlongipetiolatum" ~ "Psidium longipetiolatum",    
                             species == "Psidiummyrtoides" ~ "Psidium myrtoides",          
                             species == "Randiaarmata" ~ "Randia armata",             
                             species == "Rhipsaliscereuscula" ~ "Rhipsalis cereuscula",       
                             species == "Rhipsalisteres" ~ "Rhipsalis teres",            
                             species == "Soroceabonplandii" ~ "Sorocea bonplandii",         
                             species == "Syagrusromanzoffiana" ~ "Syagrus romanzoffiana",     
                             species == "Trichiliacatigua" ~ "Trichilia catigua",          
                             species == "Xylopiabrasiliensis" ~ "Xylopia brasiliensis",
                             species == "inch_pass" ~ "Unidentified",
                             # species == "NA" ~ "NA",
                             is.na(species)  ~ "NA"
                             # TRUE ~ species
                             ))  %>% 

  tidyr::unite(x_UTM, x_UTM.x, x_UTM.y, col = "x", remove = TRUE, na.rm = TRUE) %>% 
  tidyr::unite(y_UTM, y_UTM.x, y_UTM.y, col = "y", remove = TRUE, na.rm = TRUE)  %>% 
  tidyr::unite(visitations.x, visitations.y, col = "visitations", remove = TRUE, na.rm = TRUE)  %>% 
  tidyr::unite(id_tree.x, id_tree.y, col = "id_tree", remove = TRUE, na.rm = TRUE) %>% 
  
  dplyr::select(-c("USER", `feedingbout_on?`, `step_model_param?`, `gtt_param?`, `p_forage_param?`)) %>% 
  dplyr::select(-c(siminputrow, "[run_number]"))
  # dplyr::select(-c("breed.x", "breed.y")) %>% 
  

db1 %>% str()
db1$species %>% as.factor() %>% levels() 
db$study_area %>% unique()
# a <- db1 %>% dplyr::filter(species == "Syagrus romanzoffiana")
# db1 <- db1
# a <- db1 %>% dplyr::filter_if(!is.na(SDD) & species != "Syagrus romanzoffiana")

db1 <- db1 %>% 
  mutate(
    x = as.numeric(x),
    y = as.numeric(y)
  ) %>% 
  rename(
    month = feeding_trees_scenario,
    group = study_area
  ) %>% 
  mutate(
    source = "simulated"
  )

db1 %>% str()
db1$group %>% as.factor() %>% levels()

# Write data (all)
# db1 %>%
#   saveRDS(paste0(path, "/", "02_Simoutput-simple.rds"))

# # Write csv (all)
# db1 %>%
#   write.csv(paste0(path, "/", "02_Simoutput-simple.csv"),
#             row.names = FALSE)



# Split into tables

# # # Write csv (plants)
# db1 %>%
#   dplyr::select(-c("energy":"PT_sd")) %>%
#   dplyr::filter(breed != "monkeys") %>%
#   write.csv(paste0(path, "/", "02_Simoutput-simple_plants.csv"),
#             row.names = FALSE)
# db1 %>%
#   dplyr::select(-c("energy":"PT_sd")) %>%
#   dplyr::filter(breed != "monkeys") %>%
#   saveRDS(paste0(path, "/", "02_Simoutput-simple_plants.rds"))


db_monkeys <- db1 %>%
  dplyr::filter(breed == "monkeys") %>%
  dplyr::select(-c("x":disp_day)) %>%
  mutate_all(~stringr::str_replace_all(., c("\\[" = "", "\\]" = "")))

db_monkeys$group %>% as.factor() %>% levels()  

# a <- db1 %>% 
#   # dplyr::filter(group == "Taquara") %>%
#   dplyr::filter(breed == "monkeys")

# Option 1: gather all daily values of DPL
# b <- db_monkeys$DPL_d %>%
#   str_extract_all(., pattern = ".{0,4}[\\.].{0,2}")
# db_monkeys$DPL_d <- b
#   
# db_monkeys <- db_monkeys %>% 
#   # map_dbl(., as.numeric) #%>% 
#   tidyr::unnest_longer(DPL_d)
# 
# db_monkeys <- db_monkeys %>% 
#   mutate(DPL_d = as.numeric(DPL_d))
# Add day id:
# db_monkeys <- db_monkeys %>% 
#   group_by(random_seed) %>% 
#   mutate(
#     date = row_number()
#   )

# # Option 2: use only mean and sd
db_monkeys <- db_monkeys %>%
  dplyr::select(-DPL_d)
# db_monkeys$DPL_d # THIS IS STILL WRONG
# db_monkeys$DPL_d %>% str()
# db_monkeys %>% str()
# Drop this down when separating DPL from strings is done correctly:
# db_monkeys <- db_monkeys %>% 
#   dplyr::filter(DPL_d > 600) # some values are smaller than 700, which is not realistic (it seems like it is a regex problem)
  

db_monkeys %>% str()
foo <- function(x, na.rm = TRUE) (x = as.numeric(x))
# db1_mv$MR %>% foo

db_monkeys %>% colnames()

db_monkeys <- db_monkeys %>% 
  dplyr::select(-timestep) %>%  # it is always 0 when nlrx valuates only in the end of the run
  mutate(
    across(
      c("no_days", "simulation_time", "[step]", "day",
        "n_visited_trees", "n_unvisited_trees", 
        "R_seeds", "R_seeds_p", "NN_seeds",
        "energy", "DPL", "DPL_sd", "KDE_95", "KDE_50",
        "p_feeding", "p_foraging","p_traveling",  "p_resting",
        "step_length_mean", "step_length_sd", "turn_ang_sd", 
        "MR", "MR_sd", "PT", "PT_sd"
        ), 
      foo
    )
  ) %>% 
  mutate_if(is.character, as.factor)

db_monkeys %>% str()

  

# # # Write data
# db_monkeys %>%
#   saveRDS(paste0(path, "/", "02_Simoutput-simple_monkeys_long.rds"))

# db_monkeys %>%
#   write.csv(paste0(path, "/", "02_Simoutput-simple_monkeys.csv"),
#             row.names = FALSE)
















# mutate(DPL_d = str_split(abilities, ";"))
  

# tidyr::separate_rows(DPL_d, )


a <- db_monkeys$DPL_d

str_replace(a, pattern = "[.{[:digit:]}]+", "X")

str_replace(a, pattern = "[.[:digit:]{}}]+", "X")

str_split(a, pattern = ".{0,3}\\.{0,5}", "X")
str_split(a, pattern = ".{3}\\.{2}")

str_split(a, pattern = "\\.", simplify = TRUE)
str_split(a, pattern = "\\.{2}", simplify = TRUE)
str_split(a, pattern = "\\.+", simplify = TRUE)
str_split(a, pattern = "[\\.{3}]", simplify = TRUE)

str_split(a, pattern = "[{2}\\.{3}]", simplify = TRUE) #melhor
str_split(a, pattern = ".{0,4}[\\.].{0,2}", simplify = TRUE) # dá certo no regex https://regex101.com/r/hMM7dV/1
str_split(a, pattern = ".{0,4}[\\.].{0,2}")

str_extract_all(a, pattern = ".{0,4}[\\.].{0,2}") # PQP FUNCIONOU


# str_split(a, pattern = "\\d(?=\\.)", simplify = TRUE) #melhor
# str_split(a, pattern = "\\d{3}(?=\\.(?<=\\.)\\d{2})", simplify = TRUE) #quase

# a
# str_subset(a, pattern = "\\d+(?=\\.(?<=\\.)\\d{2})") #quase
# str_extract_all(a, pattern = "\\d+(?=\\.(?<=\\.)\\d{2})") #quase
# # str_extract_all(a, pattern = "\\d+(?=\\.(?<=\\.)\\d{2})") %>% mean(., na.rm = TRUE) #quase
# 
# str_subset(a, pattern = "\\d+(?=(?<=\\.)\\d{2})")
# 
# 
# 
# str_subset(a, pattern = "(?<=\\.)\\d{2})") #melhor
# 
# a
# 
# 
# str_split(a, pattern = "[^.d{2}]+")
# str_split(a, pattern = '[\\d+\\.\\d*^+]')
# b <- str_split(a, pattern = '\\.')
# 
# 
# 
# b %>% str_split(pattern = "{3}")
# b %>% str_split(pattern = "^[:digit:]{2}")









# Methods for Home range (as it was before):

a <- db1_mv$KDE_values %>% str_replace_all(., c("\\[" = "", "\\]" = ""))#(pattern = "\\[", simplify = TRUE)
a <- a %>% str_split(pattern = "_") #, simplify = TRUE)
# a <- a %>% str_split(pattern = "_", simplify = TRUE)

a <- a %>% map(as.numeric)


db1_mv$KDE_values <- a
# teste <- db1_mv %>% tidyr::nest(data="KDE_values") %>% unnest("data")



db1_mv %>% group_nest() %>% 
  unnest_longer(data)

db1_mv_HR  <- db1_mv %>% unnest_legacy()

KDE_levels <- rep(c("KDE95", "KDE50", "drop"), nrow(db1_mv))
db1_mv_HR$KDE_levels  <- KDE_levels 

db1_mv_HR %>% str()

db1_mv_HR$KDE_levels  <-  as.factor(db1_mv_HR$KDE_levels)


db1_mv_HR <- db1_mv_HR %>% 
  filter(KDE_levels != 'drop') %>% 
  pivot_wider(names_from = "KDE_levels", values_from = "KDE_values",
              values_fn = "list")

# line 1 is still nested
# db1_mv_HR <- db1_mv_HR %>% 
#   dplyr::filter(row_number()==1) %>% 
#   as_tibble()
db1_mv_HR <- db1_mv_HR[-1, ]


db1_mv_HR %>% str()

# Divide HR by 10000 (ha)
db1_mv_HR <- db1_mv_HR %>% 
  # unlist as numeric
  mutate(
    KDE95 = unlist(KDE95),
    KDE50 = unlist(KDE50)
  ) %>% 
  
  mutate(
    KDE95 = KDE95 / 100000,
    KDE50 = KDE50 / 100000
  )

db1_mv_HR$KDE50 %>% str()
)





