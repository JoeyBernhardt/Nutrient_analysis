

all_traits <- read_csv("data-processed/all-traits-nuts.csv")
refs <- read_csv("data-processed/trait_data_refs.csv")
mod_all <- read_csv("data-processed/mod_all.csv")
a25_food_names <- read_csv("data-processed/a25_food_names.csv")

modall2 <- mod_all %>% 
  select(seanuts_id2, food_name_clean, species_name) %>% 
  distinct(seanuts_id2, .keep_all = TRUE) %>% 
  filter(!is.na(food_name_clean))

refs2 <- refs %>% 
  dplyr::select(seanuts_id2, food_name_clean, species_name) %>% 
  distinct(seanuts_id2, .keep_all = TRUE) %>% 
  filter(!is.na(food_name_clean))

write_csv(refs2, "data-processed/seanuts-body-parts.csv")


seanuts_traits <- read_csv("data-processed/all-traits-nuts.csv") %>% 
  filter(!is.na(seanuts_id2)) %>% 
  dplyr::select(seanuts_id2, Species, subgroup) %>% 
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


### fix mistakes in body part

parts <- read_csv("data-processed/seanuts_parts.csv")
traits_raw_ids <- read_csv("data-processed/nutrients-traits-for-pgls.csv") %>% 
  select(seanuts_id2, reference)

non_muscles <- parts %>% 
  left_join(., traits_raw_ids, by = "seanuts_id2") %>% 
  mutate(food_name_clean = ifelse(grepl("Belinsky", reference), paste(food_name_clean, "muscle"), food_name_clean)) %>% 
  mutate(food_name_clean = ifelse(grepl("Nurhasan", reference), paste(food_name_clean, "muscle_with_skin"), food_name_clean)) %>% 
  filter(!str_detect(food_name_clean, "fillet|muscle|whole")) %>% 
  distinct(seanuts_id2, .keep_all = TRUE) %>% 
  dplyr::select(seanuts_id, species_name, food_name_clean, part, reference)

write_csv(non_muscles, "data-processed/non-parts-to-be-cleaned.csv")

### after filling in from looking at papers May 23 2020

parts2 <- read_csv("data-processed/non-parts-to-be-cleaned-filled-in.csv") %>% 
  mutate(part_corrected = food_name_checked_in_paper) %>% 
  dplyr::select(part_corrected, seanuts_id2)

write_csv(parts2, "data-processed/seanuts_parts2.csv")

### now merge the corrected parts with the parts dataset

parts3 <- left_join(parts, parts2, by = "seanuts_id2") %>% 
  mutate(part = ifelse(!is.na(part_corrected), part_corrected, part)) %>% 
  dplyr::select(-part_corrected)


write_csv(parts3, "data-processed/seanuts_parts3.csv")

  