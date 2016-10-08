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
SL_FishSpecies <- tbl_df(GenusSpecies$GenusSpecies)


fb_species_all <- bind_rows(SL_FishSpecies, FishSpecies) %>% 
  rename(species_name = value)

species_name <- unique(fb_species_all$species_name)


# load in seanuts species -------------------------------------------------

inf_fish <- read_csv("data/INF_fish.csv")
minerals <- read_csv("data-processed/minerals_inf.csv")

minerals_species <- unique(minerals$asfis_scientific_name)

seanuts_species <- inf_fish %>% 
  clean_names() %>%
  select(scientific_name, asfis_scientific_name, food_name_in_english) %>% 
  distinct()

sn_species <- unique(seanuts_species$scientific_name)
sn_asfis_species <- unique(seanuts_species$asfis_scientific_name)


fb_species <- intersect(sn_species, species_name) ## find matching species
nfb_species <- setdiff(minerals_species, species_name) ## find unmatched species (there are about 71)
nfb_asfis_species <- setdiff(minerals_species, species_name) ## find unmatched species (there are about 71)

?intersect


fb_species
nfb_species
nfb_asfis_species

sn_asfis_species

sn_asfis_species[str_detect(sn_asfis_species, "Penaeus brasiliensis/Penaeus paulensis")]
str_subset(sn_asfis_species, "Pinirampus pinirampu")


# rename species that don’t match after googling --------------------------

### After googling, import new csv with all the re-matches ####

fb.names <- read_csv("data/ntbl.FB.renames.csv")
inf_species_info <- read_csv("data-processed/infoods_species_info.csv")


#### Renaming to match fb names ####
oldvalues <- as.vector(fb.names$ntbl.nfb.species)
newvalues <- as.vector(fb.names$fishbase.names) 
length(oldvalues)
length(newvalues)
 
## try with mapvalues
inf_species_info$asfis_scientific_name <- plyr::mapvalues(inf_species_info$asfis_scientific_name, oldvalues, newvalues)


current_species <- unique(inf_species_info$asfis_scientific_name)
current_species

setdiff(current_species, species_name)

str_subset(current_species, "[)]")

## getting rid of parentheses

inf_species_info <- inf_species_info %>% 
  mutate(asfis_scientific_name = str_replace(asfis_scientific_name, "[(]", "")) %>% 
  mutate(asfis_scientific_name = str_replace(asfis_scientific_name, "[)]", "")) %>% 
  mutate(asfis_scientific_name = str_replace(asfis_scientific_name, "Loricariidae", "Lophius piscatorius")) %>% 
  mutate(asfis_scientific_name = str_replace(asfis_scientific_name, "Salminus maxillosus", "Salminus brasiliensis")) %>% 
  mutate(asfis_scientific_name = str_replace(asfis_scientific_name,  "Salvelinus alpinus", "Salvelinus alpinus alpinus"))
  

current_species <- unique(inf_species_info$asfis_scientific_name)

setdiff(current_species, species_name)

str_subset(species_name, "Salvelinus alpinus alpinus")


  
  
  
  
  
  
  
  
