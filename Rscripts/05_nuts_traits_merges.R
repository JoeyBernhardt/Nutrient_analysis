## working with traits and nutrient data
## October 12
## Joey Bernhardt
## goal: get all the fishbase traits and the nutrient concentrations in one dataframe by merging and joining

# load pacakges -----------------------------------------------------------

library(tidyverse)
library(janitor)



# read in data ------------------------------------------------------------

inf_ecology_raw <- read_csv("data-processed/inf_ecology.csv")
inf_species_info_raw <- read_csv("data-processed/inf_species_info_in_progress.csv")

inf_ecology <- inf_ecology_raw %>% 
  clean_names() %>% 
  remove_empty_cols()

inf_species_info <- inf_species_info_raw %>% 
  clean_names()

inf_nuts_raw <- read_csv("data/INF_fish.csv")

inf_nuts <- inf_nuts_raw %>% 
  clean_names() %>% 
  remove_empty_cols()


# begin the joins!! -------------------------------------------------------

inf_all <- left_join(inf_species_info, inf_ecology, by = c("asfis_scientific_name_fishbase_swap" = "sciname"))

inf_nuts_all <- left_join(inf_nuts, inf_all)

glimpse(inf_nuts_all)

                       