## compiling traits table from fishbase



# load packages -----------------------------------------------------------

library(tidyverse)
library(rfishbase)

# read in species list  ---------------------------------------------------

inf_species_raw <- read_csv("data-processed/inf_species_info_in_progress.csv")
fishbase_species_list <- read_csv("data-processed/fishbase_species_names.csv")
seanuts_species <- read_csv("data-processed/seanuts_species.csv") %>% 
  rename(species_name = `unique(a20$species_name)`)


seanuts_species <- seanuts_species$species_name
inf_species <- inf_species_raw$asfis_scientific_name_fishbase_swap
fb_species <- fishbase_species_list$species_name



### here's where we create our magic list of species we can extract fishbase data for!
inf_fb_species <- intersect(seanuts_species, fb_species)
length(inf_fb_species)
length(setdiff(seanuts_species, fb_species)) ### ok, so there are still 184 species mismatches between fishbase and the seanuts species! ugh.


# make a vector of unique species names -----------------------------------

inf_species <- unique(inf_fb_species)
inf_species


(length_fields <- list_fields("Length"))


#### Use stocks function to get the temp min and temp max ####
temps <- stocks(inf_species, c("TempMin", "TempMax", "StockDefs"))
write.csv(temps, "data-processed/inf_temps.csv")

#### grab the diet data ####


diet <- ecology(inf_species,
                     fields=c("SpecCode", "FoodTroph", "FoodSeTroph", "DietTroph", "DietSeTroph", "FishLength"))
write.csv(diet, "data-processed/inf_diet.csv")

##### Here I pull out the 'ecology' tables for all the ntbl species in fb. ####
ecology_inf <- ecology(inf_species)

write.csv(ecology_inf, "data-processed/inf_ecology.csv")

#### Here I pull out the 'species' table

species_inf <- species(inf_species)
write_csv(species_inf, "data-processed/species_inf.csv")
