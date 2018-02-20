### Feb 20 2018 Goal here is to to get the latest datasets in order


## ok these are the most up-to-datasets
percentages <- read_csv("data-processed/percentages.csv") 
cnuts <- read_csv("data-processed/cnuts-trad-foods-culture.csv")


## Feb 20 2018. Here I'm recreating the cleaning code in 28_rdi_counts to go from trait_data to the percentages.csv file. 
## I'm re-creating this because I want to create a new clean dataset that also includes the data from the traditional foods dataset
trait_data <- read_csv("data-processed/n.long_lat3.csv")

trait_data2 <- trait_data %>% 
  filter(!grepl("^Mohanty", ref_info))
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
trait_data3b %>% 
  select(contains('lat'), everything()) %>% 
  # gather(key = nutrient, value = concentration, 3:9) %>% 
  # filter(!is.na(concentration))

sum(!is.na(trait_data3b$latitude.x))
sum(!is.na(trait_data3b$latitude.y))

mean_nuts <- trait_data %>% 
  filter(!grepl("^Mohanty", ref_info)) %>%
  spread(nutrient, concentration) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(calcium = mean(ca_mg, na.rm = TRUE),
            zinc = mean(zn_mg, na.rm = TRUE), 
            iron = mean(fe_mg, na.rm = TRUE),
            epa = mean(epa, na.rm = TRUE),
            dha = mean(dha, na.rm = TRUE)) %>% 
  filter(!is.na(calcium), !is.na(zinc), !is.na(iron), !is.na(epa), !is.na(dha)) %>% 
  ungroup()