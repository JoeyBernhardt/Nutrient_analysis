## Infoods data cleaning
## October 6 2016
## Joey Bernhardt



# load packages -----------------------------------------------------------

library(tidyverse)
library(stringr)
library(janitor)

# read in data ------------------------------------------------------------

nuts <- read_csv("data/INF_fish.csv", na = c("", "NA")) %>% 
  clean_names() 

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

## just pull out the minerals and elements
minerals <- nuts2 %>% 
  select(1:18, 30, 31, 42, 43, 35, 36)
  

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

## get the species for which we have minerals data

minerals <- read_csv("data-processed/minerals_inf.csv")


minerals_species <- unique(minerals$asfis_scientific_name)

