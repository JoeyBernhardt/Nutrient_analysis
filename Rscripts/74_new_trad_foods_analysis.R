
library(tidyverse)
library(janitor)
library(cowplot)
theme_set(theme_cowplot())

nt <- read_csv("data-processed/trad-foods-cleaned-2020.csv")

nt_species <- nt %>% 
  distinct(latin_name_cleaned, common_name)

write_csv(nt_species, "data-processed/nt_species.csv")

cine_ids <- nt %>% 
  select(cine_id, page_id)

new_trad_search_raw <- read_csv("data-processed/new-extracted-trad-foods.csv") %>% 
  clean_names() 

## ok this is new trad foods data, non-cine
nt1 <- new_trad_search_raw %>% 
  rename(epa = f20d5n3_g) %>% 
  rename(dha = f22d6n3_g) %>% 
  # mutate(epa = str_replace(epa,"[\\[]", "")) %>%
  # mutate(epa = str_replace(epa, "[\\]]", "")) %>%
  # mutate(dha = str_replace(dha,"[\\[]", "")) %>%
  # mutate(dha = str_replace(dha, "[\\]]", "")) %>%
  # mutate(ca_mg = str_replace(ca_mg,"[\\[]", "")) %>%
  # mutate(ca_mg = str_replace(ca_mg, "[\\]]", "")) %>%
  # mutate(fe_mg = str_replace(fe_mg,"[\\[]", "")) %>%
  # mutate(fe_mg = str_replace(fe_mg, "[\\]]", "")) %>%
  # mutate(zn_mg = str_replace(zn_mg,"[\\[]", "")) %>%
  # mutate(zn_mg = str_replace(zn_mg, "[\\]]", "")) %>%
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
library(readxl)
### update august 28 2020; ok i think this is the latest version of the cine data
cine_parts_edited <- read_excel("data-processed/nt_keep_bio_edited_with27.xlsx") %>% 
  mutate(biblio_id = as.character(biblio_id)) %>% 
  left_join(., cine_ids, by = "cine_id")

### ok it looks like I need to grab the cine id and page id from the nt file. come back to this. 

updated_ref <- read_csv("data-processed/CINE-nutrients-fish-references-annotated.csv")

# keep_refs <- updated_ref %>%
#   filter(grepl("yes", use_in_analysis))
# View(keep_refs)
# nt_keep <- nt %>%
#   filter(reference %in% c(keep_refs$ref_number))

nt2 <- cine_parts_edited %>% 
  select(genus_species, common_name, part_edited, biblio_id, cine_id, ca_mg, fe_mg, zn_mg, epa, dha, page_id) 

# %>% 
#   left_join(., trad_foods_key)


nt3 <- nt1 %>% 
  select(trad_food_id, subgroup, asfis_scientific_name, biblio_id, ca_mg, fe_mg, zn_mg, epa, dha) %>% 
  rename(genus_species = asfis_scientific_name)

all_trad <- bind_rows(nt2, nt3)

## ok let's grab the page ids from all_trad and use them to fill in the missing page_ids for the non-cine data

culture_key <- all_trad %>% 
  select(genus_species, page_id) %>% 
  distinct() %>% 
  filter(!is.na(page_id))

### ok which are the species that are not matched by name?
all_trad2 <- all_trad %>% 
  left_join(., culture_key, by = "genus_species") %>% 
  mutate(culture_page_id = ifelse(is.na(page_id.x), page_id.y, page_id.x)) 



trad_mismatch_species <- all_trad2 %>%
  filter(is.na(culture_page_id)) %>%
  mutate(genus_species = ifelse(genus_species == "Catostomus commersoni", "Catostomus commersonii", genus_species)) %>% 
  mutate(genus_species = ifelse(genus_species == "Clupea pallasii", "Clupea pallasii pallasii", genus_species)) %>% 
    mutate(genus_species = ifelse(genus_species == "Lepomis spp.", "Lepomis spp.", genus_species)) %>% 
    mutate(genus_species = ifelse(genus_species == "Oncorhynchus gorbusha", "Oncorhynchus gorbuscha", genus_species)) 
    

all_trad3 <- all_trad %>% 
  mutate(genus_species = ifelse(genus_species == "Catostomus commersoni", "Catostomus commersonii", genus_species)) %>% 
  mutate(genus_species = ifelse(genus_species == "Clupea pallasii", "Clupea pallasii pallasii", genus_species)) %>% 
  mutate(genus_species = ifelse(genus_species == "Clupea pallasi", "Clupea pallasii pallasii", genus_species)) %>% 
  mutate(genus_species = ifelse(genus_species == "Lepomis gibbosus", "Lepomis spp.", genus_species)) %>% 
  mutate(genus_species = ifelse(genus_species == "Oncorhynchus gorbusha", "Oncorhynchus gorbuscha", genus_species)) %>% 
  left_join(., culture_key, by = "genus_species") %>% 
  mutate(culture_page_id = ifelse(is.na(page_id.x), page_id.y, page_id.x)) 




at2 <- all_trad3 %>% 
  filter(!cine_id %in% c("549", "548")) %>% 
  # mutate(genus_species = ifelse(genus_species == "Catostomus commersoni", "Catostomus commersonii", genus_species)) %>% 
  group_by(genus_species) %>% 
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
  filter(culture %in% species_numbers$culture) %>% 
  distinct()


at3 <- all_trad3 %>% 
  filter(!cine_id %in% c("549", "548")) %>% ## this is getting rid of fatty acids expressed in percent not concentration
  filter(!is.na(culture_page_id)) %>% 
  full_join(., culture_foods, by = c("culture_page_id" = 'page_id')) %>%
  filter(!is.na(culture)) %>% 
  mutate(ca_na = ifelse(is.na(ca_mg), 1, 0)) %>% 
  mutate(fe_na = ifelse(is.na(fe_mg), 1, 0)) %>% 
  mutate(zn_na = ifelse(is.na(zn_mg), 1, 0)) %>% 
  mutate(epa_na = ifelse(is.na(epa), 1, 0)) %>% 
  mutate(dha_na = ifelse(is.na(dha), 1, 0)) %>% 
  mutate(nutrient_na = ca_na + fe_na + zn_na + epa_na + dha_na) %>% 
  filter(nutrient_na < 5) %>% 
  select(cine_id, genus_species, ca_mg, fe_mg, zn_mg, epa, dha, biblio_id, culture, culture_page_id)

write_csv(at3, "data-processed/traditional_foods_nutrients_cultures.csv")

View(at3)

trad_nuts_mean_raw_old <- read_csv("data-processed/trad-foods-mean.csv") %>% 
  select(-culture) %>% 
  distinct()  ### old trad foods

### histogram of nutrient distributions
at3 %>% 
  gather(3:7, key = nutrient, value = concentration) %>% 
  ggplot(aes(x = concentration)) + geom_histogram() +
  facet_wrap( ~ nutrient, scales = "free")


### get page_id, so we can match with culture -- JOEY come back here. (September 7 2020)

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
