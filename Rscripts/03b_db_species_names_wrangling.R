## Nov 16 2016
## goal of this script is to deal with the species names that are in species_name that aren't in fishbase



# libraries ---------------------------------------------------------------

library(fuzzyjoin)
library(tidyverse)
library(stringr)
library(rfishbase)


# read in data ------------------------------------------------------------

a20 <- read_csv("data-processed/all_nuts_working20.csv")

# November 16 2016 dealing with the new seanuts data -----------------------------------


seanuts_species <- read_csv("data-processed/seanuts_species.csv") %>% 
  rename(species_name = `unique(a20$species_name)`)

seanuts_species <- unique(seanuts_species$species_name)


fishbase_species_list <- read_csv("data-processed/fishbase_species_names.csv")
fishbase_list <- unique(fishbase_species_list$species_name)


setdiff(seanuts_species, fishbase_list)


str_subset(fishbase_list, "Helostoma temmincki")

str_subset(fishbase_list, "Anadara broughtonii")


#### let's play around with the fuzzyjoin stuff

seanuts_species_clip <- seanuts_species[1:100, ]
fishbase_species_list_clip <- fishbase_species_list[1:2000, ]

sum(is.na(seanuts_species$species_name))
sum(is.na(fishbase_species_list$species_name))

seanuts_species <- seanuts_species %>% 
  filter(!is.na(species_name))



match_list1 <- seanuts_species %>% 
  stringdist_inner_join(fishbase_species_list, max_dist = 1)

match_list1 %>% 
  filter(species_name.x != species_name.y) %>% View

### now try with map values to get rid of the crappy names?

oldvalues2 <- as.vector(match_list1$species_name.x)
newvalues2 <- as.vector(match_list1$species_name.y)



a20$species_name <- plyr::mapvalues(a20$species_name, oldvalues2, newvalues2)


## check again
seanuts_species <- unique(a20$species_name)
fishbase_list <- unique(fishbase_species_list$species_name)


un_matched <- as.data.frame(setdiff(seanuts_species, fishbase_list)) %>% 
  rename(species_name = `setdiff(seanuts_species, fishbase_list)`) %>% 
  filter(!is.na(species_name))

sum(is.na(un_matched$species_name))


match_list2 <- un_matched %>%
  stringdist_inner_join(fishbase_species_list, max_dist = 2) 

match_list2 %>% 
  filter(species_name.x != species_name.y) %>% View ### OK this swap looks acceptable! Let's implement it!
  
oldvalues3 <- as.vector(match_list2$species_name.x)
newvalues3 <- as.vector(match_list2$species_name.y)



a20$species_name <- plyr::mapvalues(a20$species_name, oldvalues3, newvalues3)


## check again 
seanuts_species <- unique(a20$species_name)
fishbase_list <- unique(fishbase_species_list$species_name)

setdiff(seanuts_species, fishbase_list)


un_matched2 <- as.data.frame(setdiff(seanuts_species, fishbase_list)) %>% 
  rename(species_name = `setdiff(seanuts_species, fishbase_list)`) %>% 
  filter(!is.na(species_name))

match_list3 <- un_matched2 %>%
  stringdist_inner_join(fishbase_species_list, max_dist = 3) 

match_list4 <- match_list3 %>% 
  filter(species_name.x != species_name.y) %>%
  filter(species_name.y != "Anadara lienosa")

### let's go in by hand here to make some changes

match_list4$species_name.y[match_list4$species_name.x == "Barbodes altus"] <- "Barbonymus altus"
match_list4$species_name.y[match_list4$species_name.x == "Barbus intermedius"] <- "Labeobarbus intermedius"
match_list4$species_name.y[match_list4$species_name.x == "Penaeus indicus"] <- "Penaeus semisulcatus"
match_list4$species_name.y[match_list4$species_name.x == "Penaeus indicus"] <- "Penaeus semisulcatus"
match_list4$species_name.y[match_list4$species_name.x == "Puntius ticto"] <- "Puntius sophore"
match_list4$species_name.y[match_list4$species_name.x == "Sepia aculeata"] <- "Sepia aculeata"

oldvalues4 <- as.vector(match_list4$species_name.x)
newvalues4 <- as.vector(match_list4$species_name.y)

a20$species_name <- plyr::mapvalues(a20$species_name, oldvalues4, newvalues4)

## check again 
seanuts_species <- unique(a20$species_name)
fishbase_list <- unique(fishbase_species_list$species_name)

setdiff(seanuts_species, fishbase_list)

### let's write out the latest version of the datasheet

write_csv(a20, "data-processed/all_nuts_working21.csv")

##### Ok now an attempt to convert the english, common name to sci name using rfishbase


un_matched3 <- as.data.frame(setdiff(unique(a20$species_name), fishbase_list)) %>% 
  rename(species_name = `setdiff(unique(a20$species_name), fishbase_list)`) %>% 
  filter(!is.na(species_name))


non_matches <- a20 %>% 
  filter(!is.na(species_name)) %>% 
  stringdist_anti_join(fishbase_species_list, max_dist = 1) 


common_names_list <- as.vector(non_matches$asfis_english_name.x)

common_names_list
common_to_sci(common_names_list) ### this doesn't seem to be working.
