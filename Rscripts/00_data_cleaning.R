## Infoods data cleaning
## October 6 2016
## Joey Bernhardt



# load packages -----------------------------------------------------------

library(tidyverse)
library(stringr)
library(janitor)
library(readxl)

# read in data ------------------------------------------------------------

nuts <- read_csv("data/INF_fish.csv", na = c("", "NA")) %>% 
  clean_names() 


fatty_acids_raw <- read_csv("data/fatty_acids.csv") %>%
  remove_empty_rows() %>% 
  remove_empty_cols() %>%
  clean_names() %>% 
  slice(3:n()) %>% 
  filter(is.na(type) | type == "W") %>% 
  filter(processing == "r")

# names_col <- names(fatty_acids_raw)
# 
# str_subset(names_col, "d5n3")


fatty_acids <- fatty_acids_raw %>%
  select(1:20, fat_g, fatce_g, fat_g_2, 27:28, 30:40, f20d5n3_g, f22d6n3_g) %>% 
  remove_empty_cols() 

glimpse(fatty_acids)
  

# get rid of brackets!! in an ugly way ------------------------------------



fatty_acids_cleaned <- fatty_acids %>% 
  mutate(fat_g= str_replace(fat_g,"[\\[]", "")) %>% 
  mutate(fat_g = str_replace(fat_g, "[\\]]", "")) %>% 
  mutate(fat_g = as.numeric(fat_g)) %>% 
  mutate(fatce_g= str_replace(fatce_g,"[\\[]", "")) %>% 
  mutate(fatce_g = str_replace(fatce_g, "[\\]]", "")) %>% 
  mutate(fatce_g = as.numeric(fatce_g)) %>% 
  mutate(fat_g_2 = str_replace(fat_g_2,"[\\[]", "")) %>% 
  mutate(fat_g_2 = str_replace(fat_g_2, "[\\]]", "")) %>% 
  mutate(fat_g_2 = as.numeric(fat_g_2)) %>% 
  mutate(fapu_g  = str_replace(fapu_g ,"[\\[]", "")) %>% 
  mutate(fapu_g  = str_replace(fapu_g , "[\\]]", "")) %>% 
  mutate(fapu_g  = as.numeric(fapu_g )) %>% 
  mutate(faun_g   = str_replace(faun_g,"[\\[]", "")) %>% 
  mutate(faun_g   = str_replace(faun_g, "[\\]]", "")) %>% 
  mutate(faun_g   = as.numeric(faun_g)) %>% 
  mutate(facid_g   = str_replace(facid_g,"[\\[]", "")) %>% 
  mutate(facid_g   = str_replace(facid_g, "[\\]]", "")) %>% 
  mutate(facid_g   = as.numeric(facid_g)) %>% 
  mutate(fapun3_g = str_replace(fapun3_g,"[\\[]", "")) %>% 
  mutate(fapun3_g = str_replace(fapun3_g, "[\\]]", "")) %>% 
  mutate(fapun3_g = as.numeric(fapun3_g)) %>%
  mutate(fapun6_g = str_replace(fapun6_g,"[\\[]", "")) %>% 
  mutate(fapun6_g = str_replace(fapun6_g, "[\\]]", "")) %>% 
  mutate(fapun6_g = as.numeric(fapun6_g)) %>%
  mutate(f20d5n3_g = str_replace(f20d5n3_g,"[\\[]", "")) %>% 
  mutate(f20d5n3_g = str_replace(f20d5n3_g, "[\\]]", "")) %>% 
  mutate(f20d5n3_g = as.numeric(f20d5n3_g)) %>%
  mutate(f22d6n3_g = str_replace(f22d6n3_g,"[\\[]", "")) %>% 
  mutate(f22d6n3_g = str_replace(f22d6n3_g, "[\\]]", "")) %>% 
  mutate(f22d6n3_g = as.numeric(f22d6n3_g))
  
glimpse(fatty_acids_cleaned)
  
write_csv(fatty_acids_cleaned, "data-processed/fatty_acids_cleaned.csv")



# how many species names are not in fishes? -------------------------------
fishbase_names <- read_csv("data-processed/fishbase_species_names.csv")
fishbase_names <- unique(fishbase_names$species_name)
fatty_acid_names <- unique(fatty_acids$asfis_scientific_name)

setdiff(fatty_acid_names, fishbase_names)


# do some column renaming etc ---------------------------------------------

nuts2 <- nuts %>% 
 select(1:18, 29:39, 48:60, 65, 66, 69) %>% 
  remove_empty_cols() %>% 
  filter(is.na(type_farmed_wild ) | type_farmed_wild  == "W") %>% 
  filter(processing == "r")

inf_species_info <- nuts2 %>% 
  filter(!is.na(fat_g) | !is.na(prot_g) | !is.na(protcnt_g) |!is.na(ca_mg) | !is.na(zn_mg) | !is.na(fe_mg) | !is.na(mg_mg) | !is.na(mn_mg) |!is.na(hg_mcg) | !is.na(pb_mcg) | !is.na(vita_mcg)) %>% 
  select(1:12)

write_csv(inf_species_info, "data-processed/infoods_species_info.csv")



# compare the fats and the nuts datasets ----------------------------------

fat_species <- unique(fatty_acids$asfis_scientific_name)
inf_species <- unique(inf_species_info$asfis_scientific_name)


setdiff(fat_species, inf_species)



## just pull out the minerals and elements
minerals <- nuts2 %>% 
  select(1:18, 29:43)
  

# clean up the character data ---------------------------------------------


glimpse(minerals)
unique(minerals$zn_mg)

minerals <- minerals %>% 
  mutate(ca_mg = str_replace(ca_mg,"[\\[]", "")) %>% 
  mutate(ca_mg = str_replace(ca_mg, "[\\]]", "")) %>% 
  mutate(ca_mg = as.numeric(ca_mg)) %>% 
  mutate(fe_mg = str_replace(fe_mg,"[\\[]", "")) %>% 
  mutate(fe_mg = str_replace(fe_mg, "[\\]]", "")) %>% 
  mutate(fe_mg = as.numeric(fe_mg)) %>% 
  mutate(zn_mg = str_replace(zn_mg,"[\\[]", "")) %>% 
  mutate(zn_mg = str_replace(zn_mg, "[\\]]", "")) %>% 
  mutate(zn_mg = as.numeric(zn_mg)) %>% 
  mutate(hg_mcg = str_replace(hg_mcg,"[\\[]", "")) %>% 
  mutate(hg_mcg = str_replace(hg_mcg, "[\\]]", "")) %>% 
  mutate(hg_mcg = as.numeric(hg_mcg)) %>% 
  mutate(mg_mg = str_replace(mg_mg,"[\\[]", "")) %>% 
  mutate(mg_mg = str_replace(mg_mg, "[\\]]", "")) %>% 
  mutate(mg_mg = as.numeric(mg_mg)) %>% 
  mutate(mn_mg = str_replace(mn_mg,"[\\[]", "")) %>% 
  mutate(mn_mg = str_replace(mn_mg, "[\\]]", "")) %>% 
  mutate(mn_mg = as.numeric(mn_mg)) 

glimpse(minerals)
  
hist(minerals$mn_mg)

write_csv(minerals, "data-processed/minerals_inf.csv")

