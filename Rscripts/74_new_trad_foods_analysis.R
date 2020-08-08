
library(tidyverse)
library(janitor)
library(cowplot)
theme_set(theme_cowplot())

nt <- read_csv("data-processed/trad-foods-cleaned-2020.csv")

nt_species <- nt %>% 
  distinct(latin_name_cleaned, common_name)

write_csv(nt_species, "data-processed/nt_species.csv")



new_trad_search_raw <- read_csv("data-processed/new-extracted-trad-foods.csv") %>% 
  clean_names() 

nt1 <- new_trad_search_raw %>% 
  rename(epa = f20d5n3_g) %>% 
  rename(dha = f22d6n3_g) %>% 
  mutate(epa = str_replace(epa,"[\\[]", "")) %>% 
  mutate(epa = str_replace(epa, "[\\]]", "")) %>% 
  mutate(dha = str_replace(dha,"[\\[]", "")) %>% 
  mutate(dha = str_replace(dha, "[\\]]", "")) %>% 
  mutate(ca_mg = str_replace(ca_mg,"[\\[]", "")) %>% 
  mutate(ca_mg = str_replace(ca_mg, "[\\]]", "")) %>% 
  mutate(fe_mg = str_replace(fe_mg,"[\\[]", "")) %>% 
  mutate(fe_mg = str_replace(fe_mg, "[\\]]", "")) %>% 
  mutate(zn_mg = str_replace(zn_mg,"[\\[]", "")) %>% 
  mutate(zn_mg = str_replace(zn_mg, "[\\]]", "")) %>% 
  mutate(zn_mg = as.numeric(zn_mg)) %>% 
  mutate(ca_mg = as.numeric(ca_mg)) %>% 
  mutate(fe_mg = as.numeric(fe_mg)) %>% 
  mutate(epa = as.numeric(epa)) %>% 
  mutate(dha = as.numeric(dha)) 

str(nt1)

trad_foods_key <- read_csv("data-processed/nt_species_edited.csv")

names(new_trad_search)


nt1 %>% 
  gather(22:26, key = nutrient, value = concentration) %>% 
  ggplot(aes(x = concentration)) + geom_histogram() +
  facet_wrap( ~ nutrient, scales = "free")


#### ok now let's bring in the cine data, cadillac



updated_ref <- read_csv("data-processed/CINE-nutrients-fish-references-annotated.csv")

keep_refs <- updated_ref %>% 
  filter(grepl("yes", use_in_analysis)) 
View(keep_refs)
nt_keep <- nt %>% 
  filter(reference %in% c(keep_refs$ref_number))

nt2 <- nt_keep %>% 
  select(latin_name_cleaned, common_name, part, reference, cine_id, ca_mg, fe_mg, zn_mg, epa, dha, page_id) %>% 
  left_join(., trad_foods_key) %>% 
  rename(genus_species = latin_name_cleaned)


nt3 <- nt1 %>% 
  select(trad_food_id, subgroup, asfis_scientific_name, biblio_id, ca_mg, fe_mg, zn_mg, epa, dha) %>% 
  rename(genus_species = asfis_scientific_name)

all_trad <- bind_rows(nt2, nt3)

at2 <- all_trad %>% 
  filter(!cine_id %in% c("549", "548")) %>% 
  mutate(genus_species = ifelse(genus_species == "Catostomus commersoni", "Catostomus commersonii", genus_species)) %>% 
  group_by(trad_food_id, genus_species) %>% 
  summarise(calcium = mean(ca_mg, na.rm = TRUE),
            zinc = mean(zn_mg, na.rm = TRUE), 
            iron = mean(fe_mg, na.rm = TRUE),
            epa = mean(epa, na.rm = TRUE),
            dha = mean(dha, na.rm = TRUE)) %>% 
  filter(!is.na(calcium), !is.na(iron), !is.na(zinc), !is.na(epa), !is.na(dha)) 

View(at2)
library(readxl)
species_numbers <- read_csv("data-processed/species_numbers.csv")
culture_foods <- read_excel("~/Documents/traditional-foods/culture-foods.xlsx") %>% 
  filter(culture %in% species_numbers$culture)


trad_nuts_mean_raw_old <- read_csv("data-processed/trad-foods-mean.csv") %>% 
  select(-culture) %>% 
  distinct()  ### old trad foods

### histogram of nutrient distributions
at2 %>% 
  gather(3:7, key = nutrient, value = concentration) %>% 
  ggplot(aes(x = concentration)) + geom_histogram() +
  facet_wrap( ~ nutrient, scales = "free")


### get page_id, so we can match with culture

food_key <- nt2 %>% 
  select(page_id, trad_food_id) %>% 
  distinct(trad_food_id, .keep_all = TRUE)

at3 <- at2 %>% 
  left_join(., food_key) 

View(culture_foods)
View(at3)

tdata <- culture_foods %>% 
  left_join(., at3) %>% 
  filter(!is.na(genus_species)) ### ok this is the cleaned up trad foods with cultures
write_csv(tdata, "data-processed/tdata.csv")
