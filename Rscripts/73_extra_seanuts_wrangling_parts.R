
#### ok new plan is to grab the body parts from the original a25 file


### ok original seanuts from infoods goes to seanuts_id2 = 1854, new data pulled in after that is
### this file has the new data added after infoods: 
### seanuts_new5 <- read_csv("data-processed/seanuts_new5.csv")
a25 <- read_csv("data-processed/all_nuts_working25.csv")


a25$species_name[a25$asfis_scientific_name.x.x == "Strombus gracilior"] <- "Strombus gracilior"


sum(is.na(a25$species_name))

sum(is.na(a25$seanuts_id2))

a25$seanuts_id2 <- row.names(a25)

a25 %>% 
  dplyr::select(seanuts_id2, food_item_id, everything()) %>% 
  mutate(seanuts_id2 = as.numeric(seanuts_id2)) %>% View

a25_food_names <- a25 %>% 
  clean_names() %>% 
  mutate(biblioid_y = ifelse(is.na(biblioid_y), biblioid_x, biblioid_y)) %>% 
  dplyr::select(food_item_id, food_name_in_english_x_x, seanuts_id2, biblioid_y) 

write_csv(a25_food_names, "data-processed/a25_food_names.csv")



#### now merge these food names with the percentages data

percentages <- read_csv("data-processed/percentages_refs.csv") 

perc2 <- percentages %>% 
  mutate(seanuts_id2 = as.character(seanuts_id2)) %>% 
  left_join(a25_food_names, by = "seanuts_id2") %>% 
  mutate(seanuts_id2 = as.numeric(seanuts_id2))


perc2 %>% 
  filter(seanuts_id2 == 288) %>% View
 ### pulling in parts data 

parts1 <- read_csv("data-processed/seanuts_parts3.csv")
parts2 <- read_csv("data-processed/all-seanuts-may-24-2020.csv")

parts2 %>% 
  filter(is.na(part_corr)) %>% View
