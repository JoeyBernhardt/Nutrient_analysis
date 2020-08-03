## looking at the seanuts data
### trimming down to get a more manageable dataframe
## latest df is "data-processed/seanuts_select2.csv"
## latest df is write_csv(seanuts_select_4, "data-processed/seanuts_select_4.csv")
# ### Nov 30: after fixing the latest missing subgroup data:
# write_csv(seanuts_select_5, "data-processed/seanuts_select_5.csv")
### Nov 30: after fixing the mcg/mg issues in the Anthony data points, 
## write_csv(seanuts_select_6, "data-processed/seanuts_select_6.csv")
## Dec 1: pulled in a few more traits from Fishbase
## write_csv(seanuts_select_8, "data-processed/seanuts_select_8.csv")
## Jan 4 2017 updated with new nutrient data, 
# write_csv(seanuts_select_8, "data-processed/seanuts_select_8.csv")
## Jan 16, updated with new size data, 
## write_csv(seanuts_select_9, "data-processed/seanuts_select_9.csv")

# load packages -----------------------------------------------------------

library(tidyverse)
library(stringr)
library(janitor)



# read in data ------------------------------------------------------------

seanuts_raw <- read_csv("data-processed/seanuts_ecology2.csv")

snames <- names(seanuts_raw)
str_subset(snames, "Marine")

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
         Sp2000_HierarchyCode, reference, Fresh, Marine, BrackishWater, Brack, contains("Length"))


snames2 <- names(seanuts_select)
str_subset(snames2, "Brack")
write_csv(seanuts_select, "data-processed/seanuts_select.csv")




# trimming and reordering the dataset now! ----------------------------------------------

seanuts_select <- read_csv("data-processed/seanuts_select.csv")
View(seanuts_select)

seanuts_select2 <- seanuts_select %>% 
  unite(genus_species, Genus, Species, sep = " ", remove = FALSE) %>% 
  filter(genus_species == "NA NA" | genus_species == species_name) %>% 
  dplyr::select(seanuts_id2, food_item_id_2, food_name_clean, database,
         starts_with("biblio"), nutrient_ref, species_name, genus_species, subgroup,
         prot_g, protcnt_g, protein_g, fat_g, contains("fapun"), epa, dha, ca_mg, zn_mg, fe_mg, everything()) %>% 
  dplyr::select(-biblioid.y)


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




# now bring in new (Dec4) data --------------------------------------------
# Updated Jan 4 2017 with new data --------------------------------------------

# seanuts_new4_all <- read_csv("data-processed/seanuts_new4_all.csv")
seanuts_new4_all <- read_csv("data-processed/seanuts_new6_all.csv") ## this is now the file with the Jan 4 2017 updated data
seanuts_select_6 <- read_csv("data-processed/seanuts_select_6.csv")


seanuts_new5all <- seanuts_new4_all %>% 
  dplyr::select(1:34, 
                seanuts_id2, species_name, subgroup, food_name_clean,
                                protein_g, fat_g, epa, dha, ca_mg, zn_mg, fe_mg, tl,
                                length_from_study, length_source, abs_lat, latitude,
                                slmax,
                                country_region, isscaap_cat,
                                season, season, 
                                fapun3, fapun_all_g, Herbivory2, HerbivoryRef, FeedingType, FeedingTypeRef,
                                contains("Troph"), StockCode, SpecCode.x, SpecCode.y,
                                Genus, Species, FBname, Subfamily, DemersPelag, AnaCat, DepthRangeShallow, DepthRangeDeep, starts_with("LType"), Length, starts_with("CommonLength"), contains("Weight"),
                                Fresh, Brack, contains("Length"))




seanuts_select_6 <- seanuts_select_6 %>% 
  mutate(protein_g = as.numeric(protein_g))

seanuts_select_7 <- bind_rows(seanuts_select_6, seanuts_new5all)

write_csv(seanuts_select_7, "data-processed/seanuts_select_7.csv")

seanuts_select_7 <- read_csv("data-processed/seanuts_select_7.csv")


seanuts_select_8 <- seanuts_select_7 %>% 
  mutate(subgroup = str_replace(subgroup, "Molluscs", "mollusc")) %>% 
  mutate(subgroup = str_replace(subgroup, "Crustacean", "crustacean")) %>% 
  mutate(subgroup = str_replace(subgroup, "Finfish", "finfish")) %>% 
  mutate(subgroup = ifelse(food_name_clean %in% c("red scorpion fish", "brown scorpion fish", "european hake", "angler fish", 
                                                  "turbotfish", "John Dory", "Greater weever fish", "Tub gurnard", "Pike"), "finfish", subgroup)) %>% 
  mutate(subgroup = ifelse(food_name_clean %in% c("Warty crab", "shamefaced crab", "shovelnosed lobster", "spiny lobster", "norway lobster", "shrimp"), "crustacean", subgroup)) %>%
  mutate(subgroup = ifelse(food_name_clean %in% c("broadtail shorfin squid", "european squid", "common octopus", "common cuttlefish", "sea snail", "pecten"), "mollusc", subgroup))

seanuts_select_8 %>% 
  filter(is.na(subgroup)) %>%
  dplyr::select(food_name_clean)



unique(seanuts_select_8$subgroup)


write_csv(seanuts_select_8, "data-processed/seanuts_select_8.csv")


# Jan 14 2017, bring in new trait data from online searching --------------

seanuts_ecology <- read_csv("data-processed/seanuts_select_8.csv")

## ok let's just get rid of the one super outlier ca and fe measurement for now

seanuts_ecology$ca_mg[seanuts_ecology$ca_mg == 41206.00000] <- NA
seanuts_ecology$fe_mg[seanuts_ecology$fe_mg > 40939.00000] <- NA

seanuts_2 <- seanuts_ecology %>% 
  rowwise() %>% 
  mutate(avg_length = mean(c(Length, slmax), na.rm = TRUE)) %>% 
  clean_names() %>% 
  select(avg_length, length, slmax, foodtroph, feedingtype, everything())


no_length_withdata <- read_csv("data-processed/no_length_withdata.csv")


new_lengths2 <- no_length_withdata %>% 
  select(seanuts_id2, avg_length, length, slmax, foodtroph, feedingtype, feeding_habit, length_source, length_source_1, accepted_name, species_name) %>% 
  rename(herbivory2 = feedingtype)

str(new_lengths2)

new_lengths2b <- new_lengths2 %>% 
  select(-seanuts_id2)

seanuts_3 <- left_join(seanuts_2, new_lengths2, by = "seanuts_id2")
seanuts_3b <- left_join(seanuts_2, new_lengths2b, by = "species_name")
str(seanuts_3b)

seanuts_4 <- seanuts_3b %>% 
  select(contains(".x"), contains(".y"), everything()) %>% 
  rowwise() %>% 
  mutate(mean_length = mean(c(avg_length.x, avg_length.y, length.x, length.y, avg_length.y), na.rm = TRUE)) %>% 
  select(mean_length, everything())

seanuts_4 %>% 
  select(mean_length, length.y, everything()) %>% View

seanuts5 <- seanuts_4 %>% 
  rowwise %>% 
  mutate(max_length = mean(c(slmax.x, slmax.y, slmax_nov28), na.rm = TRUE))
  
seanuts6 <- seanuts5 %>% 
  mutate(feeding_level = ifelse(is.na(herbivory2.x), herbivory2.y, herbivory2.x)) %>% 
  select(feeding_level, contains("feeding"), everything()) %>% 
  mutate(feeding_mode = ifelse(is.na(feedingtype), feeding_habit, feedingtype)) %>% 
  select(feeding_mode, contains("feeding"), everything()) %>%
  mutate(trophic_level = ifelse(is.na(foodtroph.x), foodtroph.y, foodtroph.x)) %>% 
  select(trophic_level, foodtroph.x, foodtroph.y, everything()) %>% 
  mutate(max_length = mean(c(slmax.x, slmax.y, slmax_nov28), na.rm = TRUE)) %>%
  select(max_length, contains("slmax"), everything())



seanuts_select_9 <- seanuts6

write_csv(seanuts_select_9, "data-processed/seanuts_select_9.csv")

seanuts_select_9 <- read_csv("data-processed/seanuts_select_9.csv")

str(seanuts6)
lengths <- seanuts6 %>%
  select(species_name, mean_length, max_length, trophic_level) %>% 
  group_by(species_name) %>% 
  summarise_all(.funs = mean, na.rm = TRUE) %>% 
  rename(bulk_mean_length = mean_length,
         bulk_max_length = max_length,
         bulk_trophic_level = trophic_level)
  
seanuts_select_10 <- left_join(seanuts_select_9, lengths, by = "species_name")



write_csv(seanuts_select_10, "data-processed/seanuts_select_10.csv")

seanuts_select_10 <- read_csv("data-processed/seanuts_select_10.csv")

seanuts_select_10 <- seanuts_select_10 %>% 
  select(species_name, contains("feeding"), contains("herbivory"), contains("length"), everything()) %>% 
  arrange(species_name)
write_csv(seanuts_select_10, "data-processed/seanuts_select_10.csv")

### OK so what I need to fix here is to get the values for size to fill in where there are 
### repeats for the species, but no concordant trait values. Maybe make a litle look up table that will fill in the missing values??

## Jan 16 update: so I uploaded the select_10 file as a csv to google sheets and went and filled in the missing values

seanuts_select_11 <- read_csv("data-processed/seanuts_select_11.csv")
View(seanuts_select_11)     

##### EXTRA CODE


sum(is.na(seanuts_select2$subgroup))
unique(seanuts_select2$species_name)

length(unique(seanuts_select2$ca_mg))

sapply(seanuts_select_4, function(x) length(unique(x)))
