
library(tidyverse)
library(readxl)
library(janitor)
library(stringr)

nuts_raw <- read_excel("~/Documents/traditional-foods/nutrients-finfish.xlsx")
nuts_raw_inverts <- read_excel("~/Documents/traditional-foods/marine-inverts.xlsx")
nuts_6449 <- read_excel("~/Documents/traditional-foods/nutrients-6449.xlsx")
nuts_6433 <- read_excel("~/Documents/traditional-foods/nutrients-6433.xlsx")

thing1 <- nuts_raw %>% 
  filter(page_id == 6433) %>%
  clean_names() %>% 
  mutate(ca_mg = as.numeric(str_replace(ca_mg_100g, " (.*)", ""))) %>%
  select(page_id, common_name, ca_mg)

thing2 <- nuts_6433 %>% 
  filter(page_id == 6433) %>%
  clean_names() %>% 
  mutate(ca_mg = as.numeric(str_replace(ca_mg_100g, " (.*)", ""))) %>%
  select(page_id, common_name, ca_mg)

identical(thing1, thing2)
  

nuts <- clean_names(nuts_raw) %>% 
  select(- contains("sfa"))
nuts_inverts <- clean_names(nuts_raw_inverts) 

nuts_all <- bind_rows(nuts, nuts_inverts)

summary(nuts$sfa_8_0_g_100g)
summary(nuts_inverts$sfa_8_0_g_100g)


nuts2 <- nuts_all %>% 
  mutate(ca_mg = as.numeric(str_replace(ca_mg_100g, " (.*)", ""))) %>% 
  mutate(zn_mg = as.numeric(str_replace(zn_mg_100g, " (.*)", ""))) %>%
  mutate(fe_mg = as.numeric(str_replace(fe_mg_100g, " (.*)", ""))) %>% 
  mutate(mn_mg = as.numeric(str_replace(mn_mg_100g, " (.*)", ""))) %>% 
  mutate(mg_mg = as.numeric(str_replace(mg_mg_100g, " (.*)", ""))) %>% 
  mutate(epa = as.numeric(str_replace(omega_3_20_5n3_g_100g, " (.*)", ""))) %>% 
  mutate(dha = as.numeric(str_replace(omega_3_22_6n3_g_100g, ",", "."))) %>% 
  mutate(dha = as.numeric(str_replace(dha, " (.*)", ""))) %>% 
  mutate(protein_g = as.numeric(str_replace(protein_g_100g, " (.*)", ""))) %>%
  mutate(fat_g = as.numeric(str_replace(fat_g_100g, " (.*)", ""))) %>%
  mutate(fapun_all_g = as.numeric(str_replace(total_pufa_g_100g, " (.*)", ""))) %>% 
  mutate(fapun3 = as.numeric(str_replace(total_omega_3_g_100g, " (.*)", ""))) %>% 
  dplyr::select(1:8, ca_mg, zn_mg, fe_mg, protein_g, fat_g, fapun3, fapun_all_g, mn_mg, mg_mg, epa, dha)


nutsraw <- nuts2 %>% 
  filter(preparation == "raw")


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
  mutate(part = str_replace(part, "head end", "muscle"))


write_csv(nuts_raw_parts, "data-processed/trad-foods-cleaned.csv")

