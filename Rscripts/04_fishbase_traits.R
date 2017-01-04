## compiling traits table from fishbase

## last updated Dec 4 to get traits from newly acquired nutrient data (seanuts_new3)

# load packages -----------------------------------------------------------

library(tidyverse)
library(rfishbase)
library(stringr)

# read in species list  ---------------------------------------------------

inf_species_raw <- read_csv("data-processed/inf_species_info_in_progress.csv")
fishbase_species_list <- read_csv("data-processed/fishbase_species_names.csv")


a26 <- read_csv("data-processed/all_nuts_working26.csv") ### read in latest dataset
seanuts_species <- unique(a26$species_name)


inf_species <- inf_species_raw$asfis_scientific_name_fishbase_swap
fb_species <- fishbase_species_list$species_name



### here's where we create our magic list of species we can extract fishbase data for!
inf_fb_species <- intersect(seanuts_species, fb_species)

str_subset(inf_fb_species, "Mytilus")

length(inf_fb_species)
length(setdiff(seanuts_species, fb_species)) ### ok, so there are still 116 species mismatches between fishbase and the seanuts species! ugh.


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
write.csv(ecology_inf, "data-processed/inf_ecology2.csv")

#### Here I pull out the 'species' table

species_inf <- species(inf_species)
write_csv(species_inf, "data-processed/species_inf.csv")
write_csv(species_inf, "data-processed/species_inf_2.csv")

#### Now onto sealifebase to get the inverts data

inf_ecology <- read_csv("data-processed/inf_ecology.csv")

options(FISHBASE_API = "http://fishbase.ropensci.org/sealifebase")
inf_species

ecology_inf_inverts <- ecology(inf_species)
write.csv(ecology_inf_inverts, "data-processed/inf_ecology_inverts.csv")


invs_species <- unique(ecology_inf_inverts$sciname)
ninvs_species <- unique(ecology_inf$sciname)

length(invs_species)
length(ninvs_species)
setdiff(invs_species, ninvs_species)

### why does inf_ecology have more species now??
inf_ecology_raw <- read_csv("data-processed/inf_ecology.csv")
invs1_species <- unique(inf_ecology_raw$sciname)
length(invs1_species)

intersect(invs1_species, ninvs_species)


### Dec 4 updates

fishbase_species_list <- read_csv("data-processed/fishbase_species_names.csv")
seanuts_new3 <- read_csv("data-processed/seanuts_new3.csv")

new_species <- unique(intersect(fishbase_species_list$species_name, seanuts_new3$species_name))

ecology_seanuts_new <- ecology(new_species)
species_seanuts_new <- species(new_species)

write_csv(ecology_seanuts_new, "data-processed/ecology_seanuts_new.csv")
write_csv(species_seanuts_new, "data-processed/species_seanuts_new.csv")
