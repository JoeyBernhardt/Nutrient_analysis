## looking at the seanuts data
### trimming down to get a more manageable dataframe
## latest df is "data-processed/seanuts_select2.csv"

# load packages -----------------------------------------------------------

library(tidyverse)
library(stringr)



# read in data ------------------------------------------------------------

seanuts_raw <- read_csv("data-processed/seanuts_ecology2.csv")


# trim down the giant table to just the variables we want moving forward --------


cols <- names(seanuts_raw)
str_subset(cols, "reference")

seanuts_select <- seanuts_raw %>% 
  select(seanuts_id2, food_item_id_2, database, biblioid, biblioid.x, biblioid.y, biblioid_y, nutrient_ref, species_name, subgroup, food_name_clean, prot_g, protcnt_g, protein_g, fat_g, epa, dha, ca_mg, zn_mg, fe_mg, tl,
         length_from_study, length_source, abs_lat, latitude,
         slmax, slmax_nov28, slmax_source, lwa, lwb,
         country_region, isscaap_cat, isscaap,
         season, asfis_scientific_name_fishbase_swap_in_progress, season, season.x, season.y,
         fapun3, fapun_all_g, Herbivory2, HerbivoryRef, FeedingType, FeedingTypeRef, FoodTroph, FoodSeTroph, StockCode, SpecCode.x, SpecCode.y,
         Genus, Species, FBname, Subfamily, DemersPelag, AnaCat, DepthRangeShallow, DepthRangeDeep, starts_with("LType"), Length, starts_with("CommonLength"), contains("Weight"),
         Sp2000_HierarchyCode)


write_csv(seanuts_select, "data-processed/seanuts_select.csv")




# trimming and reordering the dataset now! ----------------------------------------------

seanuts_select <- read_csv("data-processed/seanuts_select.csv")

seanuts_select2 <- seanuts_select %>% 
  unite(genus_species, Genus, Species, sep = " ", remove = FALSE) %>% 
  filter(genus_species == "NA NA" | genus_species == species_name) %>% 
  select(seanuts_id2, food_item_id_2, food_name_clean, database,
         starts_with("biblio"), nutrient_ref, species_name, genus_species, subgroup,
         prot_g, protcnt_g, protein_g, fat_g, contains("fapun"), epa, dha, ca_mg, zn_mg, fe_mg, everything()) %>% 
  select(-biblioid.y)


seanuts_select2 <- seanuts_select2 %>% 
  unite(biblioid2, biblioid.x, biblioid_y, remove = FALSE) %>% 
  select(biblioid2, everything()) %>%
  mutate(biblioid2 = str_replace(biblioid2, "NA", "")) %>%
  mutate(biblioid2 = str_replace(biblioid2, "_", "")) %>%
  select(-biblioid.x) %>% 
  select(-biblioid_y) %>%
  select(-biblioid)
    

write_csv(seanuts_select2, "data-processed/seanuts_select2.csv")


# exploring the dataset now! ----------------------------------------------


seanuts_select2 <- read_csv("data-processed/seanuts_select2.csv")

no_ref <- seanuts_select2 %>% 
  filter(is.na(biblioid2), is.na(database))


## noticing that there are some rows with no biblioid, let's see if we can track that down

aquatic_inverts_micronutrients <- read_csv("data/aquatic_inverts_micronutrients.csv")
names(aquatic_inverts_micronutrients)



### OK so let's just add back in these observations with the reference info

aq_inv <- aquatic_inverts_micronutrients %>% 
  rename(protein_g = protein, 
         dha = dha_g, 
         fat_g = fat,
         epa = epa_g,
         species_name = species,
         protein_g = protein) %>% 
  mutate(ca_mg = as.numeric(ca_mg))






aq_ref <- aquatic_inverts_micronutrients %>% 
  select(Reference, species)

no_ref_sel <- no_ref %>% 
  select(species_name, biblioid2)

dplyr::left_join(no_ref_sel, aq_ref, by = c("species_name" = "species")) %>% 
  select(Reference, everything()) %>%
  filter(!is.na(Reference)) %>% 
  distinct(species_name, Reference) %>% View

?left_join

### OK somehow we need to pull the reference info from the aquatic_inverts_micronutrients.csv and plop it into seanuts_select2

aquatic_inverts_micronutrients %>% 
  

seanuts_select2


sum(is.na(seanuts_select2$subgroup))
unique(seanuts_select2$species_name)

length(unique(seanuts_select2$ca_mg))

sapply(seanuts_select2, function(x) length(unique(x)))
