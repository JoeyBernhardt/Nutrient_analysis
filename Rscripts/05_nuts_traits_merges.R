## working with traits and nutrient data
## October 12
## Joey Bernhardt
## goal: get all the fishbase traits and the nutrient concentrations in one dataframe by merging and joining
## Last updated Nov 16 with the giant, messy seanuts database
## Nov 16: "data-processed/seanuts_ecology.csv" is the latest output from this file

## Nov 21, goal is to update with the a26 working file
### Nov 27 now updating with the weirdly different new results from the fishbase calls
## Nov 29: "data-processed/seanuts_ecology2.csv" is the latest output from this file

# load pacakges -----------------------------------------------------------

library(tidyverse)
library(janitor)



# read in data ------------------------------------------------------------

inf_ecology_raw <- read_csv("data-processed/inf_ecology.csv")
# inf_species_info_raw <- read_csv("data-processed/inf_species_info_in_progress.csv")
species_inf <- read_csv("data-processed/species_inf.csv")

inf_ecology2 <- read_csv("data-processed/inf_ecology2.csv")
species_inf2 <- read_csv("data-processed/species_inf_2.csv")



ecology2 <- bind_rows(inf_ecology_raw_2, inf_ecology2)


### mods to inf_ecology_* to deal with the merge conflicts
inf_ecology_raw_2 <- inf_ecology_raw %>% 
  select(-TS) 

inf_ecology2 %>% 
  select(TS) %>% View

ecology2 <- bind_rows(inf_ecology_raw_2, inf_ecology2)

species_inf_mod <- species_inf %>% 
  select(-AuthorRef) %>% 
  select(-TS)


species2 <- bind_rows(species_inf_mod, species_inf2)

### OK so now ecology2 and species2 are the ones to merge!



inf_ecology <- inf_ecology_raw %>% 
  clean_names() %>% 
  remove_empty_cols()

# inf_species_info <- inf_species_info_raw %>% 
#   clean_names()

inf_nuts_raw <- read_csv("data/INF_fish.csv")
inf_minerals <- read_csv("data-processed/minerals_inf.csv") %>% 
  clean_names() %>% 
  remove_empty_cols()

inf_nuts <- inf_nuts_raw %>% 
  clean_names() %>% 
  remove_empty_cols()

## update Nov 16
seanuts <- read_csv("data-processed/all_nuts_working18.csv")
## update Nov 21
seanuts <- read_csv("data-processed/all_nuts_working27_subset.csv")

# begin the joins!! -------------------------------------------------------

inf_all <- left_join(seanuts, ecology2, by = c("species_name" = "sciname"))
inf_all2 <- left_join(inf_all, species2, by = c("species_name" = "sciname"))



# inf_nuts_all <- left_join(inf_nuts, inf_all) %>% 
  remove_empty_cols()

# write_csv(inf_nuts_all, "data-processed/seanuts_ecology.csv")
write_csv(inf_all2, "data-processed/seanuts_ecology2.csv")

inf_minerals_all <- left_join(inf_minerals, inf_all) %>% 
  remove_empty_cols()

write_csv(inf_minerals_all, "data-processed/seanuts_minerals_ecology.csv")

                       