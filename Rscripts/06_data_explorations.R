## looking at the seanuts data


# load packages -----------------------------------------------------------

library(tidyverse)
library(stringr)



# read in data ------------------------------------------------------------

seanuts_raw <- read_csv("data-processed/seanuts_ecology2.csv")



# being exploring! --------------------------------------------------------

glimpse(seanuts_raw)

summary(seanuts_raw$Length)

sum(!is.na(seanuts_raw$Weight))

seanuts <- seanuts_raw %>% 
  filter(ca_mg < 1000) 



sapply(seanuts_raw, function(x) sum(is.na(x)))


summary(seanuts_raw$seanuts_id2)


# trim down the giant table to just the variables we want moving forward --------


cols <- names(seanuts_raw)
str_subset(cols, "fapun")

seanuts_select <- seanuts_raw %>% 
  select(seanuts_id2, food_item_id_2, database, biblioid, biblioid.x, biblioid.y, biblioid_y, nutrient_ref, species_name, subgroup, food_name_clean, prot_g, protcnt_g, protein_g, fat_g, epa, dha, ca_mg, zn_mg, fe_mg, tl,
         length_from_study, length_source, abs_lat, latitude,
         slmax_nov28, slmax_source, lwa, lwb,
         country_region, isscaap_cat, isscaap,
         season, asfis_scientific_name_fishbase_swap_in_progress, season, season.x, season.y,
         fapun3, fapun_all_g, Herbivory2, HerbivoryRef, FeedingType, FeedingTypeRef, FoodTroph, FoodSeTroph, StockCode, SpecCode.x, SpecCode.y,
         Genus, Species, FBname, Subfamily, DemersPelag, AnaCat, DepthRangeShallow, DepthRangeDeep, starts_with("LType"), Length, starts_with("CommonLength"), contains("Weight"),
         Sp2000_HierarchyCode)


write_csv(seanuts_select, "data-processed/seanuts_select.csv")

