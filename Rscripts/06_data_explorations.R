## looking at the seanuts data
### trimming down to get a more manageable dataframe
## latest df is "data-processed/seanuts_select2.csv"
## latest df is write_csv(seanuts_select_4, "data-processed/seanuts_select_4.csv")
# ### Nov 30: after fixing the latest missing subgroup data:
# write_csv(seanuts_select_5, "data-processed/seanuts_select_5.csv")
### Nov 30: after fixing the mcg/mg issues in the Anthony data points, 
## write_csv(seanuts_select_6, "data-processed/seanuts_select_6.csv")
## Dec 1: pulled in a few more traits from Fishbase

# load packages -----------------------------------------------------------

library(tidyverse)
library(stringr)



# read in data ------------------------------------------------------------

seanuts_raw <- read_csv("data-processed/seanuts_ecology2.csv")


# trim down the giant table to just the variables we want moving forward --------


cols <- names(seanuts_raw)
cols[75:length(cols)]
str_subset(cols, "reference")

seanuts_select <- seanuts_raw %>% 
  dplyr::select(seanuts_id2, food_item_id_2, database, biblioid, biblioid.x, biblioid.y, biblioid_y, nutrient_ref, species_name, subgroup, food_name_clean,
         prot_g, protcnt_g, protein_g, fat_g, epa, dha, ca_mg, zn_mg, fe_mg, tl,
         length_from_study, length_source, abs_lat, latitude,
         slmax, slmax_nov28, slmax_source, lwa, lwb,
         country_region, isscaap_cat, isscaap,
         season, asfis_scientific_name_fishbase_swap_in_progress, season, season.x, season.y,
         fapun3, fapun_all_g, Herbivory2, HerbivoryRef, FeedingType, FeedingTypeRef,
         contains("Troph"), StockCode, SpecCode.x, SpecCode.y,
         Genus, Species, FBname, Subfamily, DemersPelag, AnaCat, DepthRangeShallow, DepthRangeDeep, starts_with("LType"), Length, starts_with("CommonLength"), contains("Weight"),
         Sp2000_HierarchyCode, reference, Fresh, BrackishWater, Brack, contains("Length"))


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
  select(reference, everything()) %>% 
  filter(is.na(biblioid2), is.na(database), is.na(reference)) %>% View

### OK now, the next step is to go find the actual references for all the biblioids!

has_biblio <- seanuts_select2 %>% 
  filter(!is.na(biblioid2)) 



bibliography <- read_csv("data/seanuts_bibliography.csv") %>% 
  filter(!is.na(BiblioID))

left_join(has_biblio, bibliography, by = c("biblioid2" = "BiblioID")) %>%
  select(biblioid2, Bibliography, everything()) %>%
  filter(!is.na(biblioid2), is.na(Bibliography)) ## OK this looks good. I think I can join the bibliography table with the seanuts table



seanuts_select3 <- left_join(seanuts_select2, bibliography, by = c("biblioid2" = "BiblioID")) %>% 
  select(Bibliography, everything()) 

seanuts_select3 %>% 
  filter(is.na(biblioid2)) %>%
  select(reference, everything()) %>% View


seanuts_select_4 <- seanuts_select3 %>% 
  mutate(ref_info = Bibliography) %>% 
  mutate(ref_info = ifelse(is.na(Bibliography), nutrient_ref, ref_info)) %>% 
  mutate(ref_info = ifelse(is.na(ref_info), reference, ref_info)) %>%
  select(ref_info, everything()) 
  
write_csv(seanuts_select_4, "data-processed/seanuts_select_4.csv")



# now exploring! ----------------------------------------------------------

seanuts <- read_csv("data-processed/seanuts_select_4.csv")

seanuts_select_5 %>% 
  filter(is.na(subgroup)) %>% 
  distinct(species_name) %>% View### OK it looks like there are some missing subgroup data, let's go ahead and fill that in

seanuts_select_5 <- seanuts %>% 
  mutate(subgroup = ifelse(species_name == "Penaeus semisulcatus", "Crustacean", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Metapenaeus monoceros", "Crustacean", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Aristeus antennatus", "Crustacean", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "penaeid and pandalid shrimps", "Crustacean", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Penaeus monodon", "Crustacean", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Litopenaeus vannamei", "Crustacean", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Xiphopenaeus kroyeri", "Crustacean", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Macrobrachium rosenbergii", "Crustacean", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Farfantepenaeus brasiliensis", "Crustacean", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Litopenaeus schmitti", "Crustacean", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Maja brachydactyla", "Crustacean", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Hexaplex trunculus", "Molluscs", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Scylla paramamosain", "Crustacean", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Portunus trituberculatus", "Crustacean", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Eriocheir sinensis", "Crustacean", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Mytilus galloprovincialis", "Molluscs", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Mytilus chilensis", "Molluscs", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Perunytitus purpuratus", "Molluscs", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Nacella deaurata", "Molluscs", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Acanthina monodon", "Molluscs", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Aulacomya ater", "Molluscs", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Fissurella picta", "Molluscs", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Meretrix lusoria", "Molluscs", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Chamelea gallina", "Molluscs", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Donax trunculus", "Molluscs", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Carcinus maenus", "Crustacean", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Mercenaria mercenaria", "Molluscs", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Callinectes sapidus", "Crustacean", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Crassostrea virginica", "Molluscs", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Placopectens magellanicus", "Molluscs", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Homarus americanus", "Crustacean", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Homarus gammarus", "Crustacean", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Portunus pelagicus", "Crustacean", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Ostrea edulis", "Molluscs", subgroup)) %>%
  mutate(subgroup = ifelse(species_name == "Pinctada radiata", "Molluscs", subgroup))


### after fixing the latest missing subgroup data:
write_csv(seanuts_select_5, "data-processed/seanuts_select_5.csv")


#### Nov 30, after realizing that there were some data entry errors in Anthony, Jane E., et al. "Yields, proximate composition and mineral content of finfish and shellfish." Journal of Food Science 48.1 (1983): 313-314.
seanuts_select_5 <- read_csv("data-processed/seanuts_select_5.csv")

seanuts_select_5 %>% 
 filter(grepl("Anthony, Jane E., et al.", ref_info)) %>% View


seanuts_select_6 <- seanuts_select_5 %>% 
  mutate(fe_mg = if_else(grepl("Anthony, Jane E., et al.", ref_info) & species_name == "Crassostrea virginica", 7.65, fe_mg)) %>%
  mutate(zn_mg = if_else(grepl("Anthony, Jane E., et al.", ref_info) & species_name == "Crassostrea virginica", 82.46, zn_mg)) %>%
  mutate(fe_mg = if_else(grepl("Anthony, Jane E., et al.", ref_info) & species_name == "Mercenaria mercenaria", 2.46, fe_mg)) %>% 
  mutate(zn_mg = if_else(grepl("Anthony, Jane E., et al.", ref_info) & species_name == "Mercenaria mercenaria", 2.50, zn_mg)) %>%
  mutate(ca_mg = if_else(grepl("Anthony, Jane E., et al.", ref_info) & species_name == "Mercenaria mercenaria", 32.8, ca_mg)) %>%
  mutate(ca_mg = if_else(grepl("Anthony, Jane E., et al.", ref_info) & species_name == "Callinectes sapidus", 34.4, ca_mg)) 

## now get rid of the data points that look like they should have never been there in first place
seanuts_select_6$fe_mg[grepl("Anthony, Jane E., et al.", seanuts_select_6$ref_info) & seanuts_select_6$species_name == "Callinectes sapidus"] <- NA
seanuts_select_6$zn_mg[grepl("Anthony, Jane E., et al.", seanuts_select_6$ref_info) & seanuts_select_6$species_name == "Callinectes sapidus"] <- NA


seanuts_select_6 %>% 
  filter(grepl("Anthony, Jane E., et al.", ref_info)) %>% View


write_csv(seanuts_select_6, "data-processed/seanuts_select_6.csv")


### Dec 1 2016

seanuts_select_6 <- read_csv("data-processed/seanuts_select_6.csv")
seanuts_select_6 %>% 
  filter(grepl("Anthony, Jane E., et al.", ref_info)) %>% View



ecology <- read_csv("data-processed/inf_ecology2.csv")

ecology1 <- read_csv("data-processed/inf_ecology.csv")

ecology2 <- read_csv("data-processed/ecology2.csv")
species2 <- read_csv("data-processed/species2.csv")



##### EXTRA CODE


sum(is.na(seanuts_select2$subgroup))
unique(seanuts_select2$species_name)

length(unique(seanuts_select2$ca_mg))

sapply(seanuts_select_4, function(x) length(unique(x)))
