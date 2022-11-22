library("here")
library("nlrx")
library("ggplot2")
library("dplyr")
library("stringr")
library("vctrs")


path <- here("Model_analysis", "Sensitivity-analysis",
             "v1.1_November2022", "temp")

nl <- readRDS(here("Model_analysis", "Sensitivity-analysis",
                    "v1.1_November2022", "temp", "v1.1_Taquara_Jan_simple-816064956_tempRDS.Rdata"))
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
  
  db <- dplyr::left_join(db, unnest_simoutput(nl_file))
  
  i <- i + 1
}


### Repair data
db$species %>% as.factor() %>% levels()
db %>% str()

db$KDE_values

db <- db %>% 
  # dplyr::rename_all(~str_replace_all(., c("-" = "_", "\\s+" = "_"))) %>% # Remove - and space characters.
  mutate(species = case_when(species == "[Abutaselloana]" ~ "Abuta selloana",             
                             species == "[Allophylusedulis]" ~ "Allophylus edulis",          
                             species == "[Campomanesiaxanthocarpa]" ~ "Campomanesia xanthocarpa",   
                             species == "[Caseariasylvestris]" ~ "Casearia sylvestris",       
                             species == "[Celtisfluminensis]" ~ "Celtis fluminensis",         
                             species == "[Celtisiguanaea]" ~ "Celtis iguanaea",            
                             species == "[Cissussulcicaulis]" ~ "Cissus sulcicaulis",         
                             species == "[Cordiasellowiana]" ~ "Cordia sellowiana",         
                             species == "[Dyospirosinconstans]" ~ "Dyospiros inconstans",       
                             species == "[Eugeniaaff.ramboi]" ~ "Eugenia aff. ramboi",         
                             species == "[Eugeniabrasiliensis]" ~ "Eugenia brasiliensis",       
                             species == "[Eugeniapunicifolia]" ~ "Eugenia punicifolia",       
                             species == "[Eugeniasp.]" ~ "Eugenia sp.",                
                             species == "[Ficusenormis]" ~ "Ficus enormis",              
                             species == "[Myrceugeniaovata]" ~ "Myrceugenia ovata",         
                             species == "[Myrciariacuspidata]" ~ "Myrciaria cuspidata",        
                             species == "[Myrciasplendens(Sw.)DC.]" ~ "Myrcia splendens",    
                             species == "[Myrsineumbellata]" ~ "Myrsine umbellata",          
                             species == "[Peraglabrata]" ~ "Pera glabrat",              
                             species == "[Pereskiaaculeata]" ~ "Pereskia aculeata",          
                             species == "[Philodendronspp.]" ~ "Philodendron spp.",          
                             species == "[Phoradendronquadrangulare]" ~ "Phoradendron quadrangulare",
                             species == "[Pliniatrunciflora]" ~ "Plinia trunciflora",         
                             species == "[Psidiumlongipetiolatum]" ~ "Psidium longipetiolatum",    
                             species == "[Psidiummyrtoides]" ~ "Psidium myrtoides",          
                             species == "[Randiaarmata]" ~ "Randia armata",             
                             species == "[Rhipsaliscereuscula]" ~ "Rhipsalis cereuscula",       
                             species == "[Rhipsalisteres]" ~ "Rhipsalis teres",            
                             species == "[Soroceabonplandii]" ~ "Sorocea bonplandii",         
                             species == "[Syagrusromanzoffiana]" ~ "Syagrus romanzoffiana",     
                             species == "[Trichiliacatigua]" ~ "Trichilia catigua",          
                             species == "[Xylopiabrasiliensis]" ~ "Xylopia brasiliensis",
                             species == "[inch_pass]" ~ "Unidentified",
                             species == "[NA]" ~ "NA",
                             is.na(species)  ~ "NA",
                             TRUE ~ species
                             )) %>% 
  dplyr::mutate(
    SDD = SDD * 10  # SDD is in patches and 1 patch = 10 m
  )

db <- db %>% 
  tidyr::unite(x_UTM, x_UTM.x, x_UTM.y, remove = TRUE, na.rm = TRUE) %>% 
  tidyr::unite(y_UTM, y_UTM.x, y_UTM.y, remove = TRUE, na.rm = TRUE)  %>% 
  tidyr::unite(visitations.x, visitations.y, remove = TRUE, na.rm = TRUE)  %>% 
  tidyr::unite(id_tree.x, id_tree.y, remove = TRUE, na.rm = TRUE)  %>% 
  
  # mutate(x = case_when(is.na(x_UTM) ~ x_UTM.x
  #                      TRUE  ~ x_UTM))
  
  dplyr::rename(
    x = x_UTM,
    y = y_UTM,
    visitations = visitations.x,
    id_tree = id_tree.x
  ) %>% 
  
  dplyr::select(-c("breed.x", "breed.y")) %>% 
  dplyr::select(-c("USER", `feedingbouton?`, `step_model_param?`, `gtt_param?`, `p_forage_param?`))


# Write csv
# db %>%
#   write.csv(paste0(path, "/", "02_Simoutput-simple.csv"),
#             row.names = FALSE)

  

# Split into tables
db_monkeys <- db %>% 
  dplyr::filter(breed == "monkeys") %>% 
  dplyr::select(-c("x":`disp-day`)) %>% 
  mutate_all(~stringr::str_replace_all(., c("\\[" = "", "\\]" = "")))
  

mutate(DPL_d = str_split(abilities, ";"))
  

tidyr::separate_rows(DPL_d, )


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
str_split(a, pattern = ".{0,3}[\\.].{0,2}", simplify = TRUE) # dá certo no regex https://regex101.com/r/hMM7dV/1


str_split(a, pattern = "\\d(?=\\.)", simplify = TRUE) #melhor
str_split(a, pattern = "\\d{3}(?=\\.(?<=\\.)\\d{2})", simplify = TRUE) #quase












a
str_subset(a, pattern = "\\d+(?=\\.(?<=\\.)\\d{2})") #quase
str_extract_all(a, pattern = "\\d+(?=\\.(?<=\\.)\\d{2})") #quase

str_subset(a, pattern = "\\d+(?=(?<=\\.)\\d{2})")



str_subset(a, pattern = "(?<=\\.)\\d{2})") #melhor

a














str_split(a, pattern = "[^.d{2}]+")
str_split(a, pattern = '[\\d+\\.\\d*^+]')
b <- str_split(a, pattern = '\\.')
  


b %>% str_split(pattern = "{3}")
b %>% str_split(pattern = "^[:digit:]{2}")


# Write csv
db %>%
  write.csv(paste0(path, "/", "02_Simoutput-simple.csv"),
            row.names = FALSE)


