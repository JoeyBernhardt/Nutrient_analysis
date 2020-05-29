### Feb 20 2018 Goal here is to to get the latest datasets in order


## ok these are the most up-to-datasets
percentages <- read_csv("data-processed/percentages.csv") 



## Feb 20 2018. Here I'm recreating the cleaning code in 28_rdi_counts to go from trait_data to the percentages.csv file. 
## I'm re-creating this because I want to create a new clean dataset that also includes the data from the traditional foods dataset
trait_data <- read_csv("data-processed/n.long_lat3.csv")

length(unique(trait_data3b$species_name))

trait_data2 <- trait_data %>% 
  filter(!grepl("^Mohanty", ref_info)) %>% 
  filter(seanuts_id2 != 1737) 
trait_data3b <- trait_data2 %>% 
  distinct(species_name, food_name_clean, ref_info, seanuts_id2, nutrient, concentration, .keep_all = TRUE) %>% 
  spread(key = nutrient, value = concentration) %>% 
  mutate(protein = protcnt_g) %>% 
  mutate(protein = ifelse(is.na(protein), protein_g, protein)) %>% 
  mutate(protein = ifelse(is.na(protein), prot_g, protein)) %>% 
  # select(species_name, seanuts_id2, protein, fat_g, dha, epa, ca_mg, zn_mg, fe_mg, subgroup) %>% 
  rename(fat = fat_g,
         calcium = ca_mg, 
         zinc = zn_mg,
         iron = fe_mg) %>%
  select(-X13) 


### Now I see there are multiple columns for latitude. Need to pick which latitude to use.

trait_data4 <- trait_data3b %>% 
  mutate(latitude = ifelse(!is.na(latitude.y), latitude.y, latitude.x)) %>% 
  select(contains('lat'), everything()) %>% 
  select(-latitude.x) %>% 
  select(-latitude.y) %>% 
  mutate(seanuts_id2 = as.character(seanuts_id2))
  # gather(key = nutrient, value = concentration, 3:9) %>% 
  # filter(!is.na(concentration))

### ok now bring in the traditional animal foods data

cnuts <- read_csv("data-processed/cnuts-trad-foods-culture.csv")

cnuts2 <- cnuts %>% 
  rename(fat = fat_g,
         calcium = ca_mg, 
         zinc = zn_mg,
         iron = fe_mg,
         protein = protein_g,
         species_name = latin_name) %>% 
  mutate(cnuts_id = rownames(.)) %>% 
  mutate(cnuts_id2 = paste("cnuts", cnuts_id, sep = "_")) %>% ## make a new column for a unique ID
  mutate(seanuts_id2 = cnuts_id2) %>% 
  mutate(subgroup = NA) %>% 
  mutate(subgroup = ifelse(level_2 %in% c("Bivalves", "Sea Snails","Primitive Mollusks", "Cephalopods"), "mollusc", subgroup)) %>% 
  mutate(subgroup = ifelse(level_2 %in% c("Freshwater Fish", "Saltwater Fish", "Searun Fish"), "finfish", subgroup)) %>% 
  mutate(subgroup = ifelse(level_2 %in% c("Crustaceans"), "crustacean", subgroup)) %>% 
  mutate(subgroup = ifelse(level_2 %in% c("Echinoderms"), "echinoderm", subgroup))


seadiv <- bind_rows(trait_data4, cnuts2) %>% 
  filter(!is.na(species_name))




write_csv(seadiv, "data-processed/seadiv.csv") ### update May 29 2020
seadiv <- read_csv("data-processed/seadiv.csv")
mean_seadiv <- seadiv %>% 
  group_by(species_name, subgroup) %>% 
  summarise(calcium = mean(calcium, na.rm = TRUE),
            zinc = mean(zinc, na.rm = TRUE), 
            iron = mean(iron, na.rm = TRUE),
            epa = mean(epa, na.rm = TRUE),
            dha = mean(dha, na.rm = TRUE),
            protein = mean(protein, na.rm = TRUE),
            fat = mean(fat, na.rm = TRUE)) %>% 
  filter(!is.na(calcium), !is.na(zinc), !is.na(iron), !is.na(epa), !is.na(dha))


write_csv(mean_seadiv, "data-processed/mean_seadiv.csv")
