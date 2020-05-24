
library(tidyverse)
library(readxl)
library(janitor)
library(stringr)

nuts_raw <- read_excel("~/Documents/traditional-foods/nutrients-finfish.xlsx")
nuts_raw_inverts <- read_excel("~/Documents/traditional-foods/marine-inverts.xlsx")
nuts_6449 <- read_excel("~/Documents/traditional-foods/nutrients-6449.xlsx")
nuts_6433 <- read_excel("~/Documents/traditional-foods/nutrients-6433.xlsx")
nuts_6447 <- read_excel("~/Documents/traditional-foods/nutrients-6447.xlsx")

names(nuts_raw)


# begin cleaning, again :) ------------------------------------------------


nuts <- clean_names(nuts_raw) %>% 
  dplyr::select(- contains("sfa"))
nuts_inverts <- clean_names(nuts_raw_inverts) 

nuts_all <- bind_rows(nuts, nuts_inverts)


### ok let's clean this up!

nuts_all %>% 
  mutate(ca_mg = str_replace(ca_mg_100g, " (.*)", "")) %>%
  mutate(ca_mg = as.numeric(str_replace(ca_mg, ",", "."))) %>%
  select(ca_mg, everything()) %>% 
  distinct(ca_mg) %>% View
  

nuts2 <- nuts_all %>% 
  mutate(ca_mg = str_replace(ca_mg_100g, " (.*)", "")) %>%
  mutate(ca_mg = as.numeric(str_replace(ca_mg, ",", "."))) %>%
  mutate(zn_mg = str_replace(zn_mg_100g, " (.*)", "")) %>%
  mutate(zn_mg = as.numeric(str_replace(zn_mg, ",", "."))) %>%
  mutate(fe_mg = str_replace(fe_mg_100g, " (.*)", "")) %>%
  mutate(fe_mg = as.numeric(str_replace(fe_mg, ",", "."))) %>%
  mutate(mn_mg = str_replace(mn_mg_100g, " (.*)", "")) %>% 
  mutate(mn_mg = as.numeric(str_replace(mn_mg, ",", "."))) %>%
  mutate(mg_mg = str_replace(mg_mg_100g, " (.*)", "")) %>% 
  mutate(mg_mg = as.numeric(str_replace(mg_mg, ",", "."))) %>%
  mutate(epa = str_replace(omega_3_20_5n3_g_100g, " (.*)", "")) %>% 
  mutate(epa = as.numeric(str_replace(epa, ",", "."))) %>%
  mutate(dha = str_replace(omega_3_22_6n3_g_100g, " (.*)", "")) %>% 
  mutate(dha = as.numeric(str_replace(dha, ",", "."))) %>%
  mutate(protein_g = str_replace(protein_g_100g, " (.*)", "")) %>%
  mutate(protein_g = str_replace(protein_g, ",", ".")) %>%
  mutate(fat_g = str_replace(fat_g_100g, " (.*)", "")) %>%
  mutate(fat_g = str_replace(fat_g, "trace", "")) %>%
  mutate(fat_g = as.numeric(str_replace(fat_g, ",", "."))) %>%
  mutate(fapun_all_g = str_replace(total_pufa_g_100g, " (.*)", "")) %>% 
  mutate(fapun_all_g = str_replace(fapun_all_g, ",", ".")) %>%
  mutate(fapun3 = str_replace(total_omega_3_g_100g, " (.*)", "")) %>% 
  mutate(fapun3 = str_replace(fapun3, ",", ".")) %>%
  dplyr::select(1:8, ca_mg, zn_mg, fe_mg, protein_g, fat_g, fapun3, fapun_all_g, mn_mg, mg_mg, epa, dha)


nutsraw <- nuts2 %>% 
  filter(preparation == "raw")

unique(nuts2$preparation)

nuts_raw_parts <- nutsraw %>% 
  mutate(part = str_replace(part, "fillet", "muscle")) %>% 
  mutate(part = str_replace(part, "meat", "muscle")) %>%
  mutate(part = str_replace(part, "flesh + skin", "muscle + skin")) %>%
  mutate(part = str_replace(part, "meat + skin", "muscle + skin")) %>%
  mutate(part = str_replace(part, "flesh", "muscle")) %>%
  mutate(part = str_replace(part, "middle cut", "middle")) %>%
  mutate(part = str_replace(part, "roe", "eggs")) %>% 
  mutate(part = str_replace(part, "grease", "oil")) %>% 
  mutate(part = str_replace(part, "tail cut", "muscle")) %>% 
  mutate(part = str_replace(part, "middle", "muscle")) %>%
  mutate(part = str_replace(part, "tail end", "muscle")) %>% 
  mutate(part = str_replace(part, "head end", "muscle")) %>% 
  filter(reference != "88") %>% # get rid of USDA sources
  filter(reference != "63") %>% # get rid of USDA sources
  filter(!grepl("and", latin_name)) %>% ## get rid of ambigous species
  filter(!grepl(",", latin_name)) %>% 
  mutate(species1 = latin_name) %>% 
  mutate(old_species1 = species1) %>% 
  mutate(species1 = ifelse(common_name == "Ninespine Stickleback", "Pungitius pungitius", species1)) %>%
  mutate(species1 = ifelse(common_name == "Menhaden", "Brevoortia tyrannus", species1)) %>%
  mutate(species1 = ifelse(common_name == "Bluefin Tuna", "Thunnus thynnus", species1)) %>%
  mutate(species1 = ifelse(species1 == "Acipenser fulvenscens","Acipenser fulvescens", species1)) %>% 
  mutate(species1 = ifelse(species1 == "Acipenser oxyrhyncus", "Acipenser oxyrinchus", species1)) %>% 
  mutate(species1 = ifelse(species1 == "Clupea pallasii", "Clupea pallasii pallasii", species1)) %>% 
  mutate(species1 = ifelse(species1 == "Morone saxatillis", "Morone saxatilis", species1)) %>% 
  mutate(species1 = ifelse(species1 == "Oncorhynchus mykiss irideus", "Oncorhynchus mykiss", species1)) %>% 
  mutate(species1 = ifelse(species1 == "Theragra chalcogramma", "Gadus chalcogrammus", species1)) %>% 
  mutate(species1 = ifelse(species1 == "Centropristes striata", "Centropristis striata", species1)) %>% 
  mutate(species1 = ifelse(species1 == "Coregonus autumnalis and Coregonus sardinella", "Coregonus autumnalis", species1)) %>% 
  mutate(species1 = ifelse(species1 == "Centropristes striata", "Centropristis striata", species1)) %>% 
  rename(latin_name_cleaned = species1) %>% 
  arrange(latin_name_cleaned) %>% 
  mutate(cine_id = rownames(.))


write_csv(nuts_raw_parts, "data-processed/trad-foods-cleaned-2020.csv")


