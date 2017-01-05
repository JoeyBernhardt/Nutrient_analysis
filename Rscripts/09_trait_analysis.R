### Seanuts traits analysis


# load libraries ----------------------------------------------------------

library(tidyverse)
library(stringr)
library(purrr)


# load data ---------------------------------------------------------------

seanuts_ecology <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data-processed/seanuts_select_8.csv")

## ok let's just get rid of the one super outlier ca and fe measurement for now

seanuts_ecology$ca_mg[seanuts_ecology$ca_mg == 41206.00000] <- NA
seanuts_ecology$fe_mg[seanuts_ecology$fe_mg > 40939.00000] <- NA


names_seanuts <- names(seanuts_ecology)
str_subset(names_seanuts, "length")
str_subset(names_seanuts, "Length")
str_subset(names_seanuts, "sl")
str_subset(names_seanuts, "BrackishWater")

sum(!is.na(seanuts_ecology$Length))
sum(!is.na(seanuts_ecology$slmax))
sum(!is.na(seanuts_ecology$length_from_study))

seanuts_ecology <- seanuts_ecology %>% 
  rename(foodtroph = FoodTroph)

n.long <- seanuts_ecology %>% 
  dplyr::select(species_name, subgroup, prot_g, protcnt_g, epa, dha, ca_mg, fat_g,
                zn_mg, fe_mg, slmax, seanuts_id2, tl, food_item_id_2,
                Length, abs_lat, Herbivory2, DemersPelag, contains("Brack"), Marine, Fresh, contains("troph"), contains("length"), contains("weight")) %>% 
  gather(key = "nutrient", value = "concentration", prot_g, protcnt_g, epa, dha, ca_mg, fat_g, zn_mg, fe_mg) %>% 
  filter(!is.na(concentration)) 

ggplot(aes(x = foodtroph, y = tl), data = n.long) + geom_point()