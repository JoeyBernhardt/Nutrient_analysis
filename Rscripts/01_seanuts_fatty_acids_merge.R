## goal: merge the infoods nutrient datasheet with the infoods fatty acids datasheet


# packages ----------------------------------------------------------------

library(janitor)
library(tidyverse)
library(stringr)


# read in data ------------------------------------------------------------
inf <- read_csv("data-processed/inf_species_info_in_progress.csv")
inf_nuts_raw <- read_csv("data/INF_fish.csv") %>% 
  clean_names()
inf_fatty_acids <- read_csv("data-processed/fatty_acids_cleaned_in_progress_googled.csv")

inf_fatty_acids <- inf_fatty_acids %>% 
  rename(fat_g_3 = fat_g) ## rename the fat_g column to avoid merge problems later




# clean the minerals data in the inf_nuts ---------------------------------

inf_nuts <- inf_nuts_raw %>% 
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


## get the raw data into a trimmed form that only includes what we want (i.e. raw and wild)
inf_nutrients_selected_raw_wild <- inf_nuts %>% 
  filter(is.na(type_farmed_wild ) | type_farmed_wild  == "W") %>% 
  filter(processing == "r") %>% 
  filter(!is.na(fat_g) | !is.na(prot_g) | !is.na(protcnt_g) |!is.na(ca_mg) | !is.na(zn_mg) | !is.na(fe_mg) | !is.na(mg_mg) | !is.na(mn_mg) |!is.na(hg_mcg) | !is.na(pb_mcg) | !is.na(vita_mcg)) 
  
write_csv(inf_nutrients_selected_raw_wild, "data-processed/inf_nutrients_selected_raw_wild.csv")

inf_nutrients <- left_join(inf_nutrients_selected_raw_wild, inf, by = "food_item_id")


# now merge together the fatty acid sheet with the rest of the nut --------

glimpse(inf_fatty_acids)
glimpse(inf_nutrients_selected_raw_wild)

all_nuts <- full_join(inf_fatty_acids, inf_nutrients, by = "food_item_id")

write_csv(all_nuts, "data-processed/all_nuts.csv")


