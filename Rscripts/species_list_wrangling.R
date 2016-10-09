## rfishbase

library(rfishbase)
library(janitor)
library(tidyverse)


# load in the fishbase species data ---------------------------------------

fishbase <- fishbase

GenusSpecies <- unite(fishbase, GenusSpecies, Genus, Species, sep = " ")
FishSpecies <- tbl_df(GenusSpecies$GenusSpecies)

## the sealifebase species
my_sealifebase <- sealifebase

SL_GenusSpecies <- unite(my_sealifebase, GenusSpecies, Genus, Species, sep = " ")
SL_FishSpecies <- tbl_df(SL_GenusSpecies$GenusSpecies)


fb_species_all <- bind_rows(SL_FishSpecies, FishSpecies) %>% 
  rename(species_name = value)

species_name <- unique(fb_species_all$species_name)


# load in seanuts species -------------------------------------------------

inf_fish <- read_csv("data/INF_fish.csv")
minerals <- read_csv("data-processed/minerals_inf.csv")


sn_species <- unique(seanuts_species$scientific_name)
sn_asfis_species <- unique(seanuts_species$asfis_scientific_name)


fb_species <- intersect(sn_species, species_name) ## find matching species
nfb_species <- setdiff(minerals_species, species_name) ## find unmatched species (there are about 71)
nfb_asfis_species <- setdiff(minerals_species, species_name) ## find unmatched species (there are about 71)




# rename species that don’t match after googling --------------------------




### After googling, import new csv with all the re-matches ####

fb.names <- read_csv("data/ntbl.FB.renames.csv")
inf_species_info <- read_csv("data-processed/infoods_species_info.csv")
fb_matches <- read_csv("data-processed/FB_clean_names.csv")
fb_matches <- read_csv("data-processed/dirty_species_names_complete.csv")


## getting rid of parentheses

inf_species_info <- inf_species_info %>% 
  mutate(asfis_scientific_name = str_replace(asfis_scientific_name, "[(]", "")) %>% 
  mutate(asfis_scientific_name = str_replace(asfis_scientific_name, "[)]", ""))



fb.names <- fb.names %>% 
  rename(dirty_names = ntbl.nfb.species,
         fishbase_names = fishbase.names)

fb_matches <- fb_matches %>% 
  rename(dirty_names = dirty_inf_name,
         fishbase_names = clean_fishbase_name)

all_renames <- bind_rows(fb_matches, fb.names)


#### Renaming to match fb names ####
oldvalues <- as.vector(all_renames$dirty_names)
newvalues <- as.vector(all_renames$fishbase_names) 

 
## try with mapvalues
inf_species_info$asfis_scientific_name <- plyr::mapvalues(inf_species_info$asfis_scientific_name, oldvalues, newvalues)


current_species <- unique(inf_species_info$asfis_scientific_name)
current_species

dirty_species_names <- as.data.frame(setdiff(current_species, species_name))

write_csv(dirty_species_names, "data-processed/dirty_species_names.csv")

current_species <- unique(inf_species_info$asfis_scientific_name)

setdiff(current_species, species_name)



  

## after googling, bring in new matches


dirty_names_match <- read_csv("data-processed/dirty_species_names_complete.csv")

dirty_names_match <- dirty_names_match %>% 
  filter(!is.na(clean_fishbase_name))
  

#### Renaming to match fb names ####
oldvalues2 <- as.vector(dirty_names_match$dirty_inf_name)
newvalues2 <- as.vector(dirty_names_match$clean_fishbase_name) 
length(oldvalues2)

## try with mapvalues

inf_species_info_copy <- inf_species_info
inf_species_info_copy$asfis_scientific_name <- plyr::mapvalues(inf_species_info_copy$asfis_scientific_name, oldvalues2, newvalues2)


current_species_2 <- unique(inf_species_info_copy$asfis_scientific_name)
unique(current_species_2)


## at this point, we only have 244 unique species?
setdiff(current_species_2, species_name)
  

  
spp_species <- str_subset(current_species_2, "spp")
spp_species

str_subset(species_name, "Alepes kleinii")  

spp_match <- str_c(spp_species, collapse = "|")

## these are the species in asfis scientific name that are only to spp
sci_in_spp <- inf_species_info_copy$scientific_name[str_detect(inf_species_info_copy$asfis_scientific_name, spp_match)]
length(sci_in_spp)

as_sci_in_spp <- inf_species_info_copy$asfis_scientific_name[str_detect(inf_species_info_copy$asfis_scientific_name, spp_match)]
length(as_sci_in_spp)

inf_species_info_copy$asfis_scientific_name <- plyr::mapvalues(inf_species_info_copy$asfis_scientific_name, as_sci_in_spp, sci_in_spp)

current_species_2 <- unique(inf_species_info_copy$asfis_scientific_name)


## at this point, now we're down to 57 mismatching species....getting better!!?
setdiff(current_species_2, species_name)

