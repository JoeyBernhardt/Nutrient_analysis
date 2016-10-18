## ok starting over, to get the raw asfis data!!

original_fish_data <- read_csv("data/INF_fish.csv") %>% 
  clean_names() %>% 
  select(food_item_id, asfis_scientific_name)


new_fish_data <- read_csv("data-processed/inf_species_info_in_progress.csv")


original_fish_data <- original_fish_data %>% 
 mutate(original_asfis_scientific_name = asfis_scientific_name)

all_fish_names <- left_join(new_fish_data, original_fish_data, by = "food_item_id")

write_csv(all_fish_names, "data-processed/all_fish_names.csv")






