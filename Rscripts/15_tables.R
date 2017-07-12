### tables for the nutrient paper

library(stringr)
library(janitor)
library(broom)
library(forcats)
library(tidyverse)
library(stargazer)
library(xtable)
library(dplyr)
library(tidyr)
library(tibble)



trait_data <- read_csv("data-processed/n.long_lat3.csv")

str(trait_data)

trait_data %>% 
  group_by(nutrient) %>% 
  distinct(species_name) %>%
  count() %>% View

trait_select <- trait_data %>% 
  dplyr::select(species_name, subgroup, nutrient, concentration) %>% 
  as_tibble() %>% 
  mutate(nutrient = str_replace(nutrient, "prot_g", "protein")) %>% 
  mutate(nutrient = str_replace(nutrient, "protcnt_g", "protein")) %>% 
  mutate(nutrient = str_replace(nutrient, "protein_g", "protein"))
  

## table s1
trait_select %>% 
  unite(subgroup_species, subgroup, species_name, remove = FALSE) %>% 
  crosstab(subgroup_species, nutrient) %>% 
  separate(subgroup_species, into = c("subgroup", "species"), sep = "_") %>% 
  rename(calcium = ca_mg) %>%
  rename(DHA = dha) %>% 
  rename(EPA = epa) %>% 
  rename(`polyunsaturated fatty acids` = fapun_all_g) %>% 
  rename(`n-3 polyunsaturated fatty acids` = fapun3) %>% 
  rename(fat = fat_g) %>% 
  rename(iron = fe_mg) %>% 
  rename(zinc = zn_mg) %>% 
  dplyr::select(subgroup, species, protein, fat, contains("fatty"), EPA, DHA, calcium, iron, zinc) %>% 
  xtable(type = "latex")
  
## table 1
trait_select %>% 
  crosstab(subgroup, nutrient) %>% 
  rename(calcium = ca_mg) %>%
  rename(DHA = dha) %>% 
  rename(EPA = epa) %>% 
  rename(`polyunsaturated fatty acids` = fapun_all_g) %>% 
  rename(`n-3 polyunsaturated fatty acids` = fapun3) %>% 
  rename(fat = fat_g) %>% 
  rename(iron = fe_mg) %>% 
  rename(zinc = zn_mg) %>% 
  dplyr::select(subgroup, protein, fat, contains("fatty"), EPA, DHA, calcium, iron, zinc) %>% 
  xtable(type = "latex", digits = 0)


## table s2
mod_all <- trait_data %>% 
  filter(concentration > 0) %>% 
  mutate(anacat = ifelse(subgroup != "finfish", "non-migratory", anacat)) %>% 
  filter(!is.na(bulk_max_length), !is.na(bulk_trophic_level), !is.na(feeding_level), !is.na(feeding_mode), !is.na(abs_lat)) %>% 
  mutate(log_length = log(bulk_max_length),
         log_concentration = log(concentration)) %>% 
  filter(!grepl("^Mohanty, B. P.,", ref_info)) %>% 
  filter(nutrient %in% c("ca_mg", "zn_mg", "fe_mg", "fapun3", "protcnt_g", "fat_g", "epa", "dha"))



mod_all %>% 
  filter(subgroup == "finfish") %>% 
  group_by(nutrient) %>% 
  do(glance(lm(log_concentration ~ abs_lat + log_length + feeding_mode + feeding_level + bulk_trophic_level, data = .))) %>% 
  dplyr::select(1, 3, 5, 6, 7, 11, 12) %>% 
  xtable() %>% 
  print(type = "html")

