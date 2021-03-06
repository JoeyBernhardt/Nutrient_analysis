### tables for the nutrient paper

library(stringr)
library(janitor)
library(broom)
library(forcats)
library(tidyverse)
library(stargazer)
library(xtable)
library(dplyr)
library(tidyr)
library(tibble)
library(readxl)

## practice

trait_data <- read_csv("data-processed/n.long_lat3.csv")

str(trait_data)

trait_data %>% 
  group_by(nutrient) %>% 
  distinct(species_name) %>%
  count() %>% View

trait_select <- trait_data %>% 
  dplyr::select(species_name, subgroup, nutrient, concentration) %>% 
  as_tibble() %>% 
  mutate(nutrient = str_replace(nutrient, "prot_g", "protein")) %>% 
  mutate(nutrient = str_replace(nutrient, "protcnt_g", "protein")) %>% 
  mutate(nutrient = str_replace(nutrient, "protein_g", "protein"))
  

## table s1
trait_select %>% 
  unite(subgroup_species, subgroup, species_name, remove = FALSE) %>% 
  crosstab(subgroup_species, nutrient) %>% 
  separate(subgroup_species, into = c("subgroup", "species"), sep = "_") %>% 
  rename(calcium = ca_mg) %>%
  rename(DHA = dha) %>% 
  rename(EPA = epa) %>% 
  rename(`polyunsaturated fatty acids` = fapun_all_g) %>% 
  rename(`n-3 polyunsaturated fatty acids` = fapun3) %>% 
  rename(fat = fat_g) %>% 
  rename(iron = fe_mg) %>% 
  rename(zinc = zn_mg) %>% 
  dplyr::select(subgroup, species, protein, fat, contains("fatty"), EPA, DHA, calcium, iron, zinc) %>% View
  xtable(type = "latex")
  
## table 1
trait_select %>% 
  crosstab(subgroup, nutrient) %>% 
  rename(calcium = ca_mg) %>%
  rename(DHA = dha) %>% 
  rename(EPA = epa) %>% 
  rename(`polyunsaturated fatty acids` = fapun_all_g) %>% 
  rename(`n-3 polyunsaturated fatty acids` = fapun3) %>% 
  rename(fat = fat_g) %>% 
  rename(iron = fe_mg) %>% 
  rename(zinc = zn_mg) %>% 
  dplyr::select(subgroup, protein, fat, contains("fatty"), EPA, DHA, calcium, iron, zinc) %>% 
  xtable(type = "latex", digits = 0)


## table s2
mod_all <- trait_data %>% 
  filter(concentration > 0) %>% 
  mutate(anacat = ifelse(subgroup != "finfish", "non-migratory", anacat)) %>% 
  filter(!is.na(bulk_max_length), !is.na(bulk_trophic_level), !is.na(feeding_level), !is.na(feeding_mode), !is.na(abs_lat)) %>% 
  mutate(log_length = log(bulk_max_length),
         log_concentration = log(concentration)) %>% 
  filter(!grepl("^Mohanty, B. P.,", ref_info)) %>% 
  filter(nutrient %in% c("ca_mg", "zn_mg", "fe_mg", "fapun3", "protcnt_g", "fat_g", "epa", "dha"))



mod_all %>% 
  filter(subgroup == "finfish") %>% 
  group_by(nutrient) %>% 
  do(glance(lm(log_concentration ~ abs_lat + log_length + feeding_mode + feeding_level + bulk_trophic_level, data = .))) %>% 
  dplyr::select(1, 3, 5, 6, 7, 11, 12) %>% 
  filter(nutrient != "fapun3") %>% 
  arrange(desc(adj.r.squared)) %>% View
  xtable() %>% 
  print(type = "latex")

## table S1 as of July 22 2017

data <- read_csv("data-processed/mean_nuts.csv")

percentage <- 0.1
data %>% 
  mutate(RDI.CA = ifelse(calcium > (1200*percentage), 1, 0)) %>% 
  mutate(RDI.FE = ifelse(iron > (18*percentage), 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(zinc > (11*percentage), 1, 0)) %>%
  mutate(RDI.EPA = ifelse(epa > (1*percentage), 1, 0)) %>% 
  mutate(RDI.DHA = ifelse(dha > (1*percentage), 1, 0)) %>% 
  ungroup() %>% 
  mutate(RDI.micro.tot = rowSums(.[8:12])) %>% 
  filter(!is.na(RDI.micro.tot)) %>% 
  group_by(subgroup, RDI.micro.tot) %>% 
  tally() %>% 
  group_by(subgroup) %>% 
  mutate(total = cumsum(n)) %>% 
  mutate(total_spp = NA) %>% 
  mutate(total_spp = ifelse(subgroup == "finfish", 78, total_spp)) %>% 
  mutate(total_spp = ifelse(subgroup == "mollusc", 12, total_spp)) %>% 
  mutate(total_spp = ifelse(subgroup == "crustacean", 6, total_spp)) %>% 
  mutate(proportion_that_reach_RDI = n*100/total_spp) %>% View
  xtable(type = "latex", digits = 0)
  
  
  trait_data <- read_csv("data-processed/n.long_lat3.csv")
  
  trait_data2 <- trait_data %>% 
    filter(!grepl("^Mohanty", ref_info))
  
  write_csv(trait_data2, "data-processed/trait_data_refs.csv")
  
  wide <- trait_data2 %>% 
    select(species_name, subgroup, seanuts_id2, nutrient, concentration, ref_info) %>% 
    distinct(species_name, nutrient, concentration, .keep_all = TRUE) %>% 
    spread(key = nutrient, value = concentration) 
  
  wide2 <- wide %>% 
    group_by(species_name, subgroup) %>% 
    summarise(mean.CA = mean(ca_mg, na.rm = TRUE),
              mean.ZN = mean(zn_mg, na.rm = TRUE), 
              mean.FE = mean(fe_mg, na.rm = TRUE),
              mean.EPA = mean(epa, na.rm = TRUE),
              mean.DHA = mean(dha, na.rm = TRUE), 
              mean.protein = mean(protcnt_g, na.rm = TRUE),
              mean.fat = mean(fat_g, na.rm = TRUE)) 
  
  
  rdis <- wide2 %>% 
    mutate(RDI.CA = ifelse(mean.CA > (1200*percentage), 1, 0)) %>% 
    mutate(RDI.FE = ifelse(mean.FE > (18*percentage), 1, 0)) %>% 
    mutate(RDI.ZN = ifelse(mean.ZN > (11*percentage), 1, 0)) %>%
    mutate(RDI.EPA = ifelse(mean.EPA > (1*percentage), 1, 0)) %>% 
    mutate(RDI.DHA = ifelse(mean.DHA > (1*percentage), 1, 0)) %>%
    mutate(RDI.fat = ifelse(mean.fat > (70*percentage), 1, 0)) %>%
    mutate(RDI.protein = ifelse(mean.protein > (56*percentage), 1, 0)) %>% 
    ungroup() %>% 
    mutate(RDI.micro.tot = rowSums(.[10:14])) 
  
  
  ### proportion that reach RDI targets
tableS1 <-   rdis %>% 
    gather(key = nutrient, value = rdi, 10:16) %>% 
    filter(!is.na(rdi)) %>% 
    select(-contains("mean")) %>% 
    group_by(nutrient, rdi, subgroup) %>% 
    tally() %>% 
    group_by(nutrient, subgroup) %>% 
    mutate(total = cumsum(n)) %>% 
    filter(rdi == 1) %>% 
    mutate(proportion_that_reach_RDI = n*100/total) %>% View
    select(subgroup, proportion_that_reach_RDI, nutrient) %>% View
    spread(key = subgroup, value = proportion_that_reach_RDI) %>%
    ungroup() %>% 
    mutate(nutrient = ifelse(nutrient == "RDI.CA", "calcium", nutrient)) %>% 
    mutate(nutrient = ifelse(nutrient == "RDI.FE", "iron", nutrient)) %>% 
    mutate(nutrient = ifelse(nutrient == "RDI.ZN", "zinc", nutrient)) %>% 
    mutate(nutrient = ifelse(nutrient == "RDI.EPA", "EPA", nutrient)) %>% 
    mutate(nutrient = ifelse(nutrient == "RDI.DHA", "DHA", nutrient)) %>% 
    mutate(nutrient = ifelse(nutrient == "RDI.fat", "fat", nutrient)) %>% 
    mutate(nutrient = ifelse(nutrient == "RDI.protein", "protein", nutrient)) %>% 
  arrange(desc(crustacean)) %>% 
  xtable(type = "latex", digits = 0)


write_csv(tableS1 , "tables/tableS1.csv")

### table S9, indigenous cultures

cultures <- read_csv("data-processed/species_numbers.csv")
cultures_details <- read_xlsx("data/Cultures_details.xlsx") %>% 
  mutate(Culture = ifelse(Culture == "Montagnais-Naskapi (Innu)", "Montagnais-Naskapi", Culture))

cultures_list <- cultures$culture

cultures_all <- left_join(cultures, cultures_details, by = c("culture" = "Culture"))


tableS9 <- cultures_all %>% 
  select(1:5) %>% 
  select(-Language) %>% 
  mutate(dataset = culture) %>% 
  mutate(dataset = str_replace(dataset, "Inuit-Inupiaq", "II")) %>% 
  mutate(dataset = str_replace(dataset, "Central Salish", "CS")) %>% 
  mutate(dataset = str_replace(dataset, "Wampanoag", "WA")) %>% 
  mutate(dataset = str_replace(dataset, "Cree", "CR")) %>%
  mutate(dataset = str_replace(dataset, "Nootkan", "NO")) %>% 
  mutate(dataset = str_replace(dataset, "Bella Coola", "BC")) %>% 
  mutate(dataset = str_replace(dataset, "Tlingit", "TL")) %>%
  mutate(dataset = str_replace(dataset, "Haida", "HA")) %>%
  mutate(dataset = str_replace(dataset, "Tsimshian", "TS")) %>% 
  mutate(dataset = str_replace(dataset, "Montagnais-Naskapi", "MN")) %>%
  mutate(dataset = str_replace(dataset, "Yupik", "YU")) %>% 
  mutate(dataset = str_replace(dataset, "Abenaki", "AB")) %>%
  mutate(dataset = str_replace(dataset, "Micmac", "MI")) %>%
  mutate(dataset = str_replace(dataset, "Kwakiutl", "KW")) %>% 
  rename(Abbreviation = dataset,
         Culture = culture,
         `Number of species in diet` = n_species) %>%
  select(Culture, Abbreviation, Region, Location, everything()) %>%
  # xtable(type = "latex", digits = 0) %>% 
  writexl::write_xlsx(., "tables/tableS9.xlsx")

write_csv(tableS9 , "tables/tableS9.csv") 

## table s10, species in the local diets

nuts_trad <- read_csv("data-processed/trad-foods-cleaned.csv")
trad_nuts_mean <- read_csv("data-processed/trad-foods-mean.csv")
cultures <- read_csv("data-processed/species_numbers.csv")


trad_nuts_mean %>% 
  filter(culture %in% cultures$culture) %>% 
  select(culture, latin_name) %>%
  write_csv(., "tables/tableS10_cultures_species.csv")


## update table S7
percentages <- read_csv("data-processed/percentages.csv")
trait_data <- read_csv("data-processed/n.long_lat3.csv")

nutrients <- trait_data %>% 
  filter(!grepl("^Mohanty", ref_info)) %>% 
  as_tibble() %>% 
  mutate(nutrient = str_replace(nutrient, "prot_g", "protein")) %>% 
  mutate(nutrient = str_replace(nutrient, "protcnt_g", "protein")) %>% 
  mutate(nutrient = str_replace(nutrient, "protein_g", "protein")) %>% 
  select(species_name, subgroup, nutrient, concentration) %>%
  group_by(subgroup, species_name, nutrient) %>% 
  summarise(concentration = mean(concentration)) %>% 
  spread(key =nutrient, value = concentration) %>% 
  select(subgroup, species_name, ca_mg, zn_mg, fe_mg, epa, dha, protein, fat_g) %>%
  rename(calcium = ca_mg,
         zinc = zn_mg, 
         iron = fe_mg,
         fat = fat_g)



percentages_mean %>% 
  filter(nutrient == "protein") %>% View

percentages_mean <- percentages %>% 
  group_by(subgroup, species_name, nutrient) %>%
  summarise(dri_per = mean(dri_per))

reaches <- percentages_mean %>% 
  mutate(reaches = ifelse(dri_per > 10, 1, 0)) %>% 
  group_by(subgroup, nutrient) %>% 
  summarise(number_reaching = sum(reaches))

View(reaches)
View(totals)


totals <- percentages_mean %>% 
  group_by(subgroup, nutrient) %>% 
  tally()


prop_reaches_dri <- left_join(totals, reaches) %>% 
  mutate(proportion_reaching_dri = (number_reaching/n)*100) %>% 
  mutate(proportion_reaching_dri = round(proportion_reaching_dri, digits = 2)) %>% 
  select(subgroup, nutrient, proportion_reaching_dri) %>% 
  spread(key = subgroup, value = proportion_reaching_dri)

prop_reaches_dri %>% 
  mutate(avg_prop = (crustacean+ mollusc+finfish)/3) %>% View


n_reaches_dri <- left_join(totals, reaches) %>% 
  select(subgroup, nutrient, n) %>% 
  spread(key = subgroup, value = n) %>% 
  rename(crustacean_n = crustacean,
         mollusc_n = mollusc,
         finfish_n = finfish)

all_props <- left_join(prop_reaches_dri, n_reaches_dri) %>% 
  select(nutrient, starts_with("crustacean"), starts_with("finfish"), starts_with("mollusc"))



percentages_mean_all <- percentages %>% 
  group_by(species_name, nutrient) %>%
  summarise(dri_per = mean(dri_per))

reaches_all <- percentages_mean_all %>% 
  mutate(reaches = ifelse(dri_per > 10, 1, 0)) %>% 
  group_by(nutrient) %>% 
  summarise(number_reaching = sum(reaches))

totals_all <- percentages_mean %>% 
  group_by(nutrient) %>% 
  tally()


prop_reaches_dri_all <- left_join(totals_all, reaches_all) %>% 
  mutate(proportion_reaching_dri = (number_reaching/n)*100) %>% 
  mutate(proportion_reaching_dri = round(proportion_reaching_dri, digits = 2)) %>% 
  select(nutrient, proportion_reaching_dri, n) %>% 
  rename(all = proportion_reaching_dri)

all_props2 <- left_join(all_props, prop_reaches_dri_all) %>% 
  rename(all_species = all,
         all_species_n = n) %>% 
  xtable(type = "latex", digits = 0)



writexl::write_xlsx(all_props2, "tables/tableS7.xlsx")

length(unique(percentages$species_name))



# references --------------------------------------------------------------

traits <- read_csv("data-processed/trait_data_refs.csv")


traits %>% 
  filter(seanuts_id2 == 59) %>% View

"Brand J.C., Rae C., McDonnell J., Lee A., Cherikoff V. and Truswell A.S. (1983) The Nutritional composition of Austrialian Aborriginal bushfoods. Food Technology in Austrilia, 35(6): 293-298."

traits2 <- traits %>% 
  # select(species_name, ref_info, updated_ref_info) %>% 
  distinct(species_name, ref_info, updated_ref_info, .keep_all = TRUE) %>% 
  mutate(reference = ifelse(is.na(updated_ref_info), ref_info, updated_ref_info))


traits3$abs_lat[traits3$seanuts_id2 == 168]

traits3 <- traits2

traits4 <- traits3 %>% 
  mutate(reference = ifelse(abs_lat == traits3$abs_lat[traits3$seanuts_id2 == 168], "Stansby M.E. (1976). Chemical characteristics of fish caught in the Northeast Pacific Ocean. Marine Fisheries Review 38(9): 1-11", reference)) %>% 
  mutate(reference = ifelse(is.na(reference), ref_info, reference)) %>% 
  mutate(reference = ifelse(seanuts_id2 == 59, "Brand J.C., Rae C., McDonnell J., Lee A., Cherikoff V. and Truswell A.S. (1983) The Nutritional composition of Austrialian Aborriginal bushfoods. Food Technology in Austrilia, 35(6): 293-298.", reference)) %>% 
  # filter(is.na(reference)) %>%
  select(species_name, reference)

write_csv(traits4, "tables/references.csv")


writexl::write_xlsx(traits4, "tables/references.xlsx")



# write out trait data file for analysis ----------------------------------

n.long_lat3 <- read_csv("data-processed/n.long_lat3.csv")

mod_all <- n.long_lat3 %>% 
  filter(concentration > 0) %>% 
  mutate(nutrient = str_replace(nutrient, "prot_g", "protein")) %>% 
  mutate(nutrient = str_replace(nutrient, "protcnt_g", "protein")) %>% 
  mutate(nutrient = str_replace(nutrient, "protein_g", "protein")) %>% 
  mutate(anacat = ifelse(subgroup != "finfish", "non-migratory", anacat)) %>% 
  filter(!is.na(bulk_max_length), !is.na(bulk_trophic_level), !is.na(feeding_level), !is.na(feeding_mode), !is.na(abs_lat)) %>% 
  mutate(log_length = log(bulk_max_length),
         log_concentration = log(concentration)) %>% 
  filter(!grepl("^Mohanty, B. P.,", ref_info)) %>% 
  mutate(reference = ifelse(is.na(updated_ref_info), ref_info, updated_ref_info)) %>% 
  mutate(reference = ifelse(abs_lat == traits3$abs_lat[traits3$seanuts_id2 == 168], "Stansby M.E. (1976). Chemical characteristics of fish caught in the Northeast Pacific Ocean. Marine Fisheries Review 38(9): 1-11", reference)) %>% 
  filter(!is.na(reference)) %>% 
  dplyr::select(seanuts_id2, species_name, subgroup, log_concentration, log_length, bulk_trophic_level, feeding_mode, feeding_level, reference, abs_lat, nutrient)

write_csv(mod_all, "data-processed/traits_for_analysis.csv")
