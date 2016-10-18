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

## get the raw data into a trimmed form that only includes what we want (i.e. raw and wild)
inf_temp <- inf_nuts_raw %>% 
  filter(is.na(type_farmed_wild ) | type_farmed_wild  == "W") %>% 
  filter(processing == "r") %>% 
  filter(!is.na(fat_g) | !is.na(prot_g) | !is.na(protcnt_g) |!is.na(ca_mg) | !is.na(zn_mg) | !is.na(fe_mg) | !is.na(mg_mg) | !is.na(mn_mg) |!is.na(hg_mcg) | !is.na(pb_mcg) | !is.na(vita_mcg)) 
  


temp <- left_join(inf_temp, inf, by = "food_item_id")

