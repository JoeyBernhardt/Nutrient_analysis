## rfishbase

library(rfishbase)
library(janitor)
library(tidyverse)
library(stringr)


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
fatty_acids <- read_csv("data-processed/fatty_acids_cleaned.csv")

sn_species <- unique(seanuts_species$scientific_name)
sn_asfis_species <- unique(seanuts_species$asfis_scientific_name)

sn_asfis_species <- unique(fatty_acids$asfis_scientific_name)

fb_species <- intersect(sn_species, species_name) ## find matching species

nfb_species <- setdiff(sn_asfis_species, species_name) ## find unmatched species (there are about 71)
nfb_asfis_species <- setdiff(sn_asfis_species, species_name) ## find unmatched species (there are about 71)

nfb_species


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

fatty_acids <- fatty_acids %>% 
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

## write out the modified inf_species_info_copy to file

write_csv(inf_species_info_copy, "data-processed/inf_species_info_in_progress.csv")
write_csv(as.data.frame(species_name), "data-processed/fishbase_species_names.csv") ## write out the fishbase species names list



# oct 11 continuing -------------------------------------------------------

fishbase_list <- read_csv("data-processed/fishbase_species_names.csv")
dirty_names_match <- read_csv("data-processed/fishbase_names_to_replace.csv")
inf_species_info <- read_csv("data-processed/inf_species_info_in_progress.csv")
dirty_names_match <- dirty_names_match %>% 
  filter(!is.na(clean_fishbase_name))


#### Renaming to match fb names ####
oldvalues2 <- as.vector(dirty_names_match$dirty_inf_name)
newvalues2 <- as.vector(dirty_names_match$clean_fishbase_name) 
length(oldvalues2)

## try with mapvalues

inf_species_info_copy <- inf_species_info
# inf_species_info_copy$asfis_scientific_name <- plyr::mapvalues(inf_species_info_copy$asfis_scientific_name, oldvalues2, newvalues2)

fishbase_list <- fishbase_list$species_name

current_species_2 <- unique(inf_species_info_copy$asfis_scientific_name_fishbase_swap)

setdiff(current_species_2, fishbase_list)

str_subset(fishbase_list, "Hypostomus watwata")

write_csv(inf_species_info_copy, "data-processed/inf_species_info_in_progress.csv")

## OK, we are now down to 14 non matching species -- great progress!!
length(unique(inf_species_info_copy$asfis_scientific_name_fishbase_swap))



# October 18, now renaming fatty acid data ------------------------------------

inf_species_info <- read_csv("data-processed/all_fish_names.csv")
fatty_acids <- read_csv("data-processed/fatty_acids_cleaned.csv")

fatty_acid_species <- fatty_acids$asfis_scientific_name ## crappy fatty acid species
new_species <- inf_species_info$asfis_scientific_name_fishbase_swap ## good fish base swap species


oldvalues2 <- as.vector(inf_species_info$original_asfis_scientific_name)
newvalues2 <- as.vector(inf_species_info$asfis_scientific_name_fishbase_swap) 




fatty_acids$asfis_scientific_name_fishbase_swap <- fatty_acids$asfis_scientific_name

View(fatty_acids)

?plyr::mapvalues


fatty_acids$asfis_scientific_name_fishbase_swap <- plyr::mapvalues(fatty_acids$asfis_scientific_name_fishbase_swap, oldvalues2, newvalues2)


fishbase_list <- fishbase_list$species_name

current_species_2 <- unique(fatty_acids$asfis_scientific_name_fishbase_swap)

setdiff(current_species_2, fishbase_list)

## write out the new fatty acid data sheet to csv

write_csv(fatty_acids, "data-processed/fatty_acids_cleaned_in_progress.csv")


## now read in the new csv, after googling for species matches

fatty_acids_post_google <- read_csv("data-processed/fatty_acids_cleaned_in_progress_googled.csv")

current_species <- unique(fatty_acids_post_google$asfis_scientific_name_fishbase_swap_in_progress)

setdiff(current_species, fishbase_list)




# November 16 2016 dealing with the new seanuts data -----------------------------------


seanuts_species <- read_csv("data-processed/seanuts_species.csv") %>% 
  rename(species_name = `unique(a20$species_name)`)

seanuts_species <- unique(seanuts_species$species_name)


fishbase_species_list <- read_csv("data-processed/fishbase_species_names.csv")
fishbase_list <- unique(fishbase_species_list$species_name)


setdiff(seanuts_species, fishbase_list)


str_subset(fishbase_list, "Helostoma temmincki")

anti_join(seanuts_working, fishbase_species_list) %>% View
fishbase_list

str_subset(fishbase_list, "Anadara broughtonii")


