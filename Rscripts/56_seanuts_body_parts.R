

all_traits <- read_csv("data-processed/all-traits-nuts.csv")
refs <- read_csv("data-processed/trait_data_refs.csv")
mod_all <- read_csv("data-processed/mod_all.csv")


modall2 <- mod_all %>% 
  select(seanuts_id2, food_name_clean, species_name) %>% 
  distinct(seanuts_id2, .keep_all = TRUE) %>% 
  filter(!is.na(food_name_clean))

refs2 <- refs %>% 
  select(seanuts_id2, food_name_clean, species_name) %>% 
  distinct(seanuts_id2, .keep_all = TRUE) %>% 
  filter(!is.na(food_name_clean))

write_csv(refs2, "data-processed/seanuts-body-parts.csv")


seanuts_traits <- read_csv("data-processed/all-traits-nuts.csv") %>% 
  filter(!is.na(seanuts_id2)) %>% 
  select(seanuts_id2, Species, subgroup) %>% 
  distinct(seanuts_id2, Species, subgroup)


sea2 <- full_join(seanuts_traits, refs2)

sea3 <- sea2 %>% 
  filter(!is.na(food_name_clean)) %>% 
  filter(!is.na(Species)) %>% 
  mutate(part = NA) %>% 
  mutate(part = case_when(grepl("whole", food_name_clean) ~ "whole",
                        grepl("skinless", food_name_clean) ~ "muscle-skinless",
                          grepl("fillet", food_name_clean) ~ "muscle",
                          grepl("muscle", food_name_clean) ~ "muscle",
                          grepl("meat", food_name_clean) ~ "muscle",
                          grepl("flesh", food_name_clean) ~ "muscle",
                          grepl("hepatopancreas", food_name_clean) ~ "liver",
                          TRUE ~ "NA")) %>% 
  mutate(part = ifelse(part == "NA", NA, part)) %>% 
  mutate(part = ifelse(is.na(part), "muscle", part))
 



write_csv(sea3, "data-processed/seanuts_parts.csv")

  