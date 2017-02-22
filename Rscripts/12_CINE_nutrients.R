library(tidyverse)
library(stringr)
library(janitor)
library(plotrix)



CINE_raw_data <- read_csv("data/CINE-raw-fish.csv")
refs <- read_csv("data/CINE-nutrients-fish-references.csv")
trait_data <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data-processed/n.long_lat3.csv")


unique(trait_data$nutrient)


CINE <- CINE_raw_data %>% 
  clean_names() 


length(unique(CINE$latin_name))
unique(CINE$part)
names(CINE)

CINE <- CINE %>% 
  select(-contains("sample_size"))

str(CINE)

unique(CINE$ca_mg_100g)

CINE_rename <- CINE %>% 
  mutate(ca_mg = as.numeric(str_replace(ca_mg_100g, " (.*)", ""))) %>% 
  mutate(zn_mg = as.numeric(str_replace(zn_mg_100g, " (.*)", ""))) %>%
  mutate(fe_mg = as.numeric(str_replace(fe_mg_100g, " (.*)", ""))) %>% 
  mutate(mn_mg = as.numeric(str_replace(mn_mg_100g, " (.*)", ""))) %>% 
  mutate(mg_mg = as.numeric(str_replace(mg_mg_100g, " (.*)", ""))) %>% 
  mutate(protein_g = as.numeric(str_replace(protein_g_100g, " (.*)", ""))) %>%
  mutate(fat_g = as.numeric(str_replace(fat_g_100g, " (.*)", ""))) %>%
  mutate(fapun_all_g = as.numeric(str_replace(total_pufa_g_100g, " (.*)", ""))) %>% 
  mutate(fapun3 = as.numeric(str_replace(total_omega_3_g_100g, " (.*)", ""))) %>% 
  dplyr::select(1:8, ca_mg, zn_mg, fe_mg, protein_g, fat_g, fapun3, fapun_all_g, mn_mg, mg_mg) 
  
  
  
write_csv(CINE_rename, "data-processed/CINE-fish-nutrients-processed.csv")



CINE_rename %>% 
  gather(key = nutrient, value = concentration, 9:16) %>% 
  filter(!is.na(concentration)) %>% 
  filter(nutrient == "ca_mg") %>% View
  ggplot(aes(x = part, y = concentration)) + geom_boxplot() + 
  facet_wrap( ~ nutrient, scales = "free")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


CINE_long <- CINE_rename %>% 
  gather(key = nutrient, value = concentration, 9:17) %>% 
  filter(!is.na(concentration))

CINE_merge <- CINE_long %>% 
  mutate(part = str_replace(part, "fillet", "muscle")) %>% 
  mutate(part = str_replace(part, "meat", "muscle")) %>%
  mutate(part = str_replace(part, "flesh + skin", "muscle + skin")) %>%
  mutate(part = str_replace(part, "meat + skin", "muscle + skin")) %>%
  mutate(part = str_replace(part, "flesh", "muscle")) %>%
  mutate(part = str_replace(part, "middle cut", "middle")) %>%
  mutate(part = str_replace(part, "roe", "eggs")) %>% 
  mutate(part = str_replace(part, "grease", "oil")) 


CINE_merge %>% 
  ggplot(aes(x = part, y = concentration)) + geom_boxplot() + 
  facet_wrap( ~ nutrient, scales = "free")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

CINE_merge %>% 
  filter(nutrient %in% c("ca_mg", "zn_mg", "fe_mg", "mn_mg", "fat_g", "protein_g", "fapun_all_g")) %>% 
  # filter(part %in% c("whole", "muscle")) %>% 
  group_by(nutrient, part) %>% 
  summarise_each(funs(mean, std.error), concentration) %>%
  arrange(mean) %>% 
  ggplot(aes(x = reorder(part, mean), y = mean)) + geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error), width = 0.2) +
  facet_wrap( ~ nutrient, scales = "free")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  