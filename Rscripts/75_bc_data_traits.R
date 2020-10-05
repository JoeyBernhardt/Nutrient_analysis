library(readxl)
library(tidyverse)
library(janitor)

#### biofood comp data for traits


# bring in biocomp data ---------------------------------------------
### come back to deal with values reported as range, and plus minus, and trace

bc_raw_sheet1 <- read_excel("data/BioFoodComp4.0_read_only.xlsx", sheet = "09 Fish & Shellfish", col_names = TRUE, guess_max = 2) %>% 
  clean_names() %>% 
  filter(!is.na(food_item_id)) ## gets rid of second header row and any unidentified samples

bc_raw_sheet2 <- read_excel("data/BioFoodComp4.0_read_only.xlsx", sheet = "09 Fish & Shellfish_Fatty acids", col_names = TRUE, guess_max = 2) %>% 
  clean_names() %>% 
  filter(!is.na(food_item_id)) %>% ## gets rid of second header row and any unidentified samples
  rename(epa = f20d5n3_g) %>% 
  rename(dha = f22d6n3_g) %>% 
  dplyr::dplyr::select(food_item_id, epa, dha)


bc_all <- bc_raw_sheet1 %>% 
  left_join(., bc_raw_sheet2, by = "food_item_id") %>% ### here we join the datasheet with the minerals, protein and fat with the fatty acids
  dplyr::dplyr::select(food_item_id, biblio_id, subgroup, country_region, food_name_in_english, processing, scientific_name, asfis_english_name, asfis_scientific_name, season, other, comments_on_data_processing_methods, publication_year, contains("prot"), contains("fat"), ca_mg, fe_mg, zn_mg, epa, dha) %>% 
  mutate(bc_id = paste0("bc", rownames(.)))


WriteXLS(bc_all, "data-processed/bc_all.xlsx")

bc_all <- read_excel("data-processed/bc_all.xlsx")
# read in part key edited in excel ----------------------------------------
parts_edited <- read_excel("data-processed/bc_all_edited.xlsx") %>% 
  clean_names() %>% 
  dplyr::dplyr::select(food_item_id, part_edited)

bc_all2 <- bc_all %>% 
  left_join(., parts_edited, by = "food_item_id")


# bc_raw <- read_excel("data-processed/biocomp-raw-macro-micro.xlsx")

bc_raw1b <- bc_all2 %>% 
  dplyr::dplyr::select(-protcnp_g) %>% 
  mutate(protein = ifelse(is.na(prot_g), protcnt_g, prot_g)) %>% ### these rows merge the various methods of measuring protein and fat
  mutate(fat = ifelse(is.na(fat_g), fatce_g, fat_g)) %>% 
  mutate(fat = ifelse(is.na(fat), fat_g_2, fat)) %>% 
  separate(ca_mg, into = c("ca_mg", "extra_ca"), sep =  "±") %>% ### these rows get rid of the plus minus ranges, and only retain the mean, which is what we want
  separate(epa, into = c("epa", "extra_epa"), sep =  "±") %>% 
  separate(dha, into = c("dha", "extra_dha"), sep =  "±") %>% 
  separate(fe_mg, into = c("fe_mg", "extra_fe"), sep =  "±") %>% 
  separate(zn_mg, into = c("zn_mg", "extra_zn"), sep =  "±") %>% 
  separate(fat, into = c("fat", "extra_fat"), sep =  "±") %>% 
  separate(protein, into = c("protein", "extra_protein"), sep =  "±") %>% 
  # dplyr::select(food_item_id, ca_mg, fe_mg, zn_mg, epa, dha, fat, protein, everything()) %>% 
  dplyr::dplyr::select(-contains("extra")) %>% 
  filter(!is.na(food_item_id)) %>% 
  mutate(zn_mg = as.numeric(zn_mg)) %>% ### these 'as.numeric' rows get rid of the entries with ranges and that are in brackets, which are poor quality data
  mutate(ca_mg = as.numeric(ca_mg)) %>% 
  mutate(fe_mg = as.numeric(fe_mg)) %>% 
  mutate(epa = as.numeric(epa)) %>% 
  mutate(dha = as.numeric(dha)) %>% 
  mutate(fat = as.numeric(fat)) %>% 
  mutate(protein = as.numeric(protein)) %>%
  rename(location = country_region) 


bc_raw1c <- bc_raw1b %>% 
  dplyr::dplyr::select(-prot_g, - fatce_g, -fatrn_g, -fat_g, -fat_g_2, -protcnt_g) %>% ## get rid of columns we no longer need
  filter(processing == "r") %>%  ### only keep raw samples
  mutate(publication_year = as.character(publication_year))
 


# new body parts ----------------------------------------------------------
  

  fillet_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  filter(grepl("fillet",food_name_in_english)) %>% 
  filter(!grepl("bones",food_name_in_english)) %>% 
  filter(!grepl("muscle",food_name_in_english)) %>% 
  filter(!grepl("skinless",food_name_in_english)) %>% 
  mutate(part_edited = "muscle") %>% 
  dplyr::select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english)

muscle_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  filter(grepl("muscle",food_name_in_english)) %>% 
  filter(!grepl("bones",food_name_in_english)) %>% 
  filter(!grepl("fillet",food_name_in_english)) %>% 
  filter(!grepl("skinless",food_name_in_english)) %>% 
  mutate(part_edited = "muscle") %>% 
  dplyr::select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english)

muscle_skinless_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  # filter(grepl("fillet",food_name_in_english)) %>% 
  # filter(!grepl("bones",food_name_in_english)) %>% 
  filter(grepl("skinless",food_name_in_english)) %>% 
  mutate(part_edited = "muscle_skinless") %>% 
  dplyr::select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english)

muscle_skinned_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  # filter(!grepl("fillet",food_name_in_english)) %>% 
  filter(!grepl("bones",food_name_in_english)) %>% 
  filter(grepl("skinned",food_name_in_english)) %>% 
  mutate(part_edited = "muscle_skinless") %>% 
  dplyr::select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english) 

muscle_bones_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  filter(grepl("fillet",food_name_in_english)) %>% 
  filter(grepl("bones",food_name_in_english)) %>% 
  filter(!grepl("skinless",food_name_in_english)) %>% 
  mutate(part_edited = "muscle_bones") %>% 
  dplyr::select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english)

muscle_organs_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  filter(!grepl("fillet",food_name_in_english)) %>% 
  filter(!grepl("whole",food_name_in_english)) %>% 
  filter(!grepl("flesh",food_name_in_english)) %>% 
  filter(grepl("cleaned",food_name_in_english)) %>%
  mutate(part_edited = "muscle_organs") %>% 
  dplyr::select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english)

muscle_other_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  filter(!grepl("fillet",food_name_in_english)) %>% 
  filter(!grepl("whole",food_name_in_english)) %>%
  filter(!grepl("skinless",food_name_in_english)) %>%
  filter(!grepl("skinned",food_name_in_english)) %>%
  filter(!grepl("muscle",food_name_in_english)) %>%
  mutate(part_edited = "muscle_organs") %>% 
  dplyr::select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english) 

meat_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>%
  filter(grepl("meat",food_name_in_english)) %>% 
  filter(!grepl("fillet",food_name_in_english)) %>% 
  filter(!grepl("whole",food_name_in_english)) %>%
  filter(!grepl("skinless",food_name_in_english)) %>%
  filter(!grepl("skinned",food_name_in_english)) %>%
  filter(!grepl("skin",food_name_in_english)) %>% 
  filter(!grepl("muscle",food_name_in_english)) %>%
  mutate(part_edited = "muscle") %>% 
  dplyr::select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english)

flesh_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>%
  filter(grepl("flesh",food_name_in_english)) %>% 
  filter(!grepl("fillet",food_name_in_english)) %>% 
  filter(!grepl("whole",food_name_in_english)) %>%
  filter(!grepl("skinless",food_name_in_english)) %>%
  filter(!grepl("skinned",food_name_in_english)) %>%
  filter(!grepl("skin",food_name_in_english)) %>% 
  filter(!grepl("muscle",food_name_in_english)) %>%
  mutate(part_edited = "muscle") %>% 
  dplyr::select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english) 

whole_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  filter(!grepl("fillet",food_name_in_english)) %>% 
  filter(grepl("whole",food_name_in_english)) %>% 
  mutate(part_edited = "whole") %>% 
  dplyr::select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english) 

liver_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  filter(grepl("liver",food_name_in_english)) %>% 
  mutate(part_edited = "liver") %>% 
  dplyr::select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english) 

egg_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  filter(grepl("egg",food_name_in_english)) %>% 
  mutate(part_edited = "egg") %>% 
  dplyr::select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english)

roe_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  filter(grepl("roe",food_name_in_english)) %>% 
  mutate(part_edited = "egg") %>% 
  dplyr::select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english) 

caviar_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  filter(grepl("caviar",food_name_in_english)) %>% 
  mutate(part_edited = "egg") %>% 
  dplyr::select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english) 

oil_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  filter(grepl("oil",food_name_in_english)) %>% 
  mutate(part_edited = "oil") %>% 
  dplyr::select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english) 

all_parts <- bind_rows(fillet_samples, muscle_samples, flesh_samples, meat_samples, muscle_skinned_samples, muscle_skinless_samples, muscle_organs_samples, muscle_other_samples, 
                       oil_samples, liver_samples, caviar_samples, egg_samples, roe_samples, whole_samples, muscle_bones_samples) 



bc_finfish <- bc_raw1c %>% 
  filter(subgroup == "Finfish") 

bc_raw1d <- bc_raw1c %>% 
  left_join(., all_parts, by = "food_item_id")

# Bibliography info -------------------------------------------------------


bc_refs <- read_excel("data-processed/biocom-refs.xlsx") %>% 
  clean_names() %>% 
  rename(biblio_id = biblioid)
# 
# bc_raw2 <- bc_raw1 %>% 
#   left_join(., bc_refs) 
# 
# bc_references <- bc_raw2 %>% 
#   dplyr::select(biblio_id, bibliography) 




# Old cleaning ------------------------------------------------------------


# bc <- bc_raw1 %>% 
#   # mutate(fat = str_replace(fat,"[\\[]", "")) %>% 
#   # mutate(fat = str_replace(fat, "[\\]]", "")) %>% 
#   # mutate(protein = str_replace(protein,"[\\[]", "")) %>% 
#   # mutate(protein = str_replace(protein, "[\\]]", "")) %>% 
#   # mutate(epa = str_replace(epa,"[\\[]", "")) %>% 
#   # mutate(epa = str_replace(epa, "[\\]]", "")) %>% 
#   # mutate(dha = str_replace(dha,"[\\[]", "")) %>% 
#   # mutate(dha = str_replace(dha, "[\\]]", "")) %>% 
#   # mutate(ca_mg = str_replace(ca_mg,"[\\[]", "")) %>% 
#   # mutate(ca_mg = str_replace(ca_mg, "[\\]]", "")) %>% 
#   # mutate(fe_mg = str_replace(fe_mg,"[\\[]", "")) %>% 
#   # mutate(fe_mg = str_replace(fe_mg, "[\\]]", "")) %>% 
#   # mutate(zn_mg = str_replace(zn_mg,"[\\[]", "")) %>% 
#   # mutate(zn_mg = str_replace(zn_mg, "[\\]]", "")) %>% 
#   mutate(zn_mg = as.numeric(zn_mg)) %>% 
#   mutate(ca_mg = as.numeric(ca_mg)) %>% 
#   mutate(fe_mg = as.numeric(fe_mg)) %>% 
#   mutate(epa = as.numeric(epa)) %>% 
#   mutate(dha = as.numeric(dha)) %>% 
#   mutate(fat = as.numeric(fat)) %>% 
#   mutate(protein = as.numeric(protein)) %>% 
#   # rename(reference = biblio_id) %>% 
#   dplyr::select(food_item_id, bc_id, asfis_english_name, asfis_scientific_name, scientific_name, subgroup, country_region, food_name_in_english, biblio_id, ca_mg, fe_mg, zn_mg, epa, dha, fat, protein) %>% 
#   rename(location = country_region)

View(bc)
unique(bc$reference)

unique(bc$food_name_in_english)


# categorizing body parts -------------------------------------------------


fillet_samples <- bc %>% 
  filter(grepl("fillet",food_name_in_english)) %>% 
  filter(!grepl("bones",food_name_in_english)) %>% 
  dplyr::select(bc_id)

muscle_samples <- bc %>% 
  filter(grepl("muscle",food_name_in_english)) %>% 
  filter(!grepl("bones",food_name_in_english)) %>% 
  dplyr::select(bc_id)

View(muscle_samples)

egg_samples <- bc %>% 
  # filter(grepl("roe",food_name_in_english)) %>% 
  filter(grepl(c("egg"),food_name_in_english)) %>% 
  filter(!grepl("bones",food_name_in_english)) %>% 
  dplyr::select(bc_id)

roe_samples <- bc %>% 
  # filter(grepl("roe",food_name_in_english)) %>% 
  filter(grepl(c("roe"),food_name_in_english)) %>% 
  filter(!grepl("bones",food_name_in_english)) %>% 
  dplyr::select(bc_id)

whole_samples <- bc %>% 
  # filter(grepl("roe",food_name_in_english)) %>% 
  filter(grepl(c("whole"),food_name_in_english)) %>% 
  dplyr::select(bc_id)

skin_samples <- bc %>% 
  # filter(grepl("roe",food_name_in_english)) %>% 
  filter(grepl(c("skin"),food_name_in_english)) %>% 
  filter(!grepl("skinless",food_name_in_english)) %>% 
  filter(!grepl("whole",food_name_in_english)) %>% 
  filter(!grepl("skin-on",food_name_in_english)) %>% 
  filter(!grepl("muscle",food_name_in_english)) %>% 
  filter(!grepl("meat",food_name_in_english)) %>% 
  filter(!grepl("fillet",food_name_in_english)) %>% 
  filter(!grepl("skinned",food_name_in_english)) %>% 
  dplyr::select(bc_id)
  
  liver_samples <- bc %>% 
    # filter(grepl("roe",food_name_in_english)) %>% 
    filter(grepl(c("liver"),food_name_in_english)) %>% 
    dplyr::select(bc_id)
  
  head_samples <- bc %>% 
    # filter(grepl("roe",food_name_in_english)) %>% 
    filter(grepl(c("head"),food_name_in_english)) %>% 
    dplyr::select(bc_id)

muscle_only <- bind_rows(fillet_samples, muscle_samples) %>% 
  distinct()

unique(muscle_samples$food_name_in_english)

muscle_only_data <- bc %>% 
  filter(bc_id %in% muscle_only$bc_id)

unique(muscle_only_data$asfis_scientific_name)


# bring in AnFoods --------------------------------------------------------

af_raw_sheet1 <- read_excel("data/AnFood2.0_read_only.xlsx", sheet = "09 Fish & Shellfish", col_names = TRUE) %>% 
  clean_names() %>% 
  filter(!is.na(food_item_id)) ## gets rid of second header row and any unidentified samples

af_raw_sheet2 <- read_excel("data/AnFood2.0_read_only.xlsx", sheet = "09 Fish & Shellfish_fatty acids", col_names = TRUE) %>% 
  clean_names() %>% 
  filter(!is.na(food_item_id)) %>% ## gets rid of second header row and any unidentified samples
  rename(epa = f20d5n3_g) %>% 
  rename(dha = f22d6n3_g) %>% 
  dplyr::dplyr::select(food_item_id, epa, dha) 


af_all <- af_raw_sheet1 %>% 
  left_join(., af_raw_sheet2, by = "food_item_id") %>%
  dplyr::dplyr::select(epa, dha, ca_mg, zn_mg, fe_mg, everything()) %>% ### here we join the datasheet with the minerals, protein and fat with the fatty acids
  # dplyr::select(food_item_id, biblio_id, subgroup, country_region, food_name_in_english, processing, scientific_name, asfis_english_name, asfis_scientific_name, season, other, comments_on_data_processing_methods, publication_year, contains("prot"), contains("fat"), ca_mg, fe_mg, zn_mg, epa, dha) %>% 
  mutate(af_id = paste0("af", rownames(.))) %>% 
  filter(processing == "r") %>% 
  mutate(farmed_finfish = ifelse(grepl("farmed", food_name_in_english) & subgroup == "Finfish", "farmed_finfish", "other")) %>%
  mutate(farmed_crustacean = ifelse(grepl("farmed", food_name_in_english) & subgroup == "Crustacean", "farmed_crustacean", "unfarmed_crustacean")) %>%
  filter(farmed_finfish != "farmed_finfish") %>% 
  dplyr::dplyr::select(farmed_crustacean, everything()) %>% 
  filter(farmed_crustacean != "farmed_crustacean") %>% 
  filter(subgroup == "Molluscs" | grepl("wild", food_name_in_english) | biblio_id %in% c("fi195", "fi159", "fi188", "fi203", "fi45")) %>%  ### this is the step
  filter(!is.na(food_item_id)) %>% 
  dplyr::dplyr::select(food_item_id, af_id, biblio_id, type, subgroup, country_region, food_name_in_english, processing, scientific_name, asfis_english_name, asfis_scientific_name, season, other, comments_on_data_processing_methods, publication_year, contains("prot"), contains("fat"), ca_mg, fe_mg, zn_mg, epa, dha)
  


WriteXLS(af_all, "data-processed/af_all.xlsx")

af_parts_edited <- read_excel("data-processed/af_all_edited.xlsx") %>% 
  clean_names() %>% 
  dplyr::select(food_item_id, part_edited)

af_all2 <- af_all %>% 
  left_join(., af_parts_edited, by = "food_item_id")


af_raw1b <- af_all2 %>% 
  dplyr::select(-protcnp_g) %>% 
  mutate(protein = ifelse(is.na(prot_g), protcnt_g, prot_g)) %>% ### these rows merge the various methods of measuring protein and fat
  mutate(fat = ifelse(is.na(fat_g), fatce_g, fat_g)) %>% 
  mutate(fat = ifelse(is.na(fat), fat_g_2, fat)) %>% 
  separate(ca_mg, into = c("ca_mg", "extra_ca"), sep =  "±") %>% ### these rows get rid of the plus minus ranges, and only retain the mean, which is what we want
  separate(epa, into = c("epa", "extra_epa"), sep =  "±") %>% 
  separate(dha, into = c("dha", "extra_dha"), sep =  "±") %>% 
  separate(fe_mg, into = c("fe_mg", "extra_fe"), sep =  "±") %>% 
  separate(zn_mg, into = c("zn_mg", "extra_zn"), sep =  "±") %>% 
  separate(fat, into = c("fat", "extra_fat"), sep =  "±") %>% 
  separate(protein, into = c("protein", "extra_protein"), sep =  "±") %>% 
  # dplyr::select(food_item_id, ca_mg, fe_mg, zn_mg, epa, dha, fat, protein, everything()) %>% 
  dplyr::select(-contains("extra")) %>% 
  filter(!is.na(food_item_id)) %>% 
  mutate(zn_mg = as.numeric(zn_mg)) %>% ### these 'as.numeric' rows get rid of the entries with ranges and that are in brackets, which are poor quality data
  mutate(ca_mg = as.numeric(ca_mg)) %>% 
  mutate(fe_mg = as.numeric(fe_mg)) %>% 
  mutate(epa = as.numeric(epa)) %>% 
  mutate(dha = as.numeric(dha)) %>% 
  mutate(fat = as.numeric(fat)) %>% 
  mutate(protein = as.numeric(protein)) %>%
  rename(location = country_region) 


af_raw1c <- af_raw1b %>% 
  dplyr::select(-prot_g, - fatce_g, -fatrn_g, -fat_g, -fat_g_2, -protcnt_g) %>% ## get rid of columns we no longer need
  filter(processing == "r") %>% 
  mutate(publication_year = as.character(publication_year))### only keep raw samples



# View(af_raw1c)

# af_raw <- read_excel("data-processed/AnFood-macro-micro.xlsx") %>% 
#   clean_names() %>% 
#   # filter(subgroup == "Crustacean") %>%
#   mutate(farmed_finfish = ifelse(grepl("farmed", food_name_in_english) & subgroup == "Finfish", "farmed_finfish", "other")) %>%
#   mutate(farmed_crustacean = ifelse(grepl("farmed", food_name_in_english) & subgroup == "Crustacean", "farmed_crustacean", "unfarmed_crustacean")) %>% 
#   filter(farmed_finfish != "farmed_finfish") %>% 
#   filter(farmed_crustacean != "farmed_crustacean") %>% 
#   filter(subgroup == "Molluscs" | grepl("wild", food_name_in_english) | biblio_id %in% c("fi195", "fi159", "fi188", "fi203")) %>% 
#   filter(!is.na(food_item_id_1)) %>% 
#   mutate(af_id = paste0("af", rownames(.)))



af_refs <- read_excel("data-processed/anfood-refs.xlsx") %>% 
  clean_names() 


# ### come back to anfoods data
# af2 <- af_raw %>% 
#   left_join(., af_refs) %>% 
#   rename(epa = f20d5n3_g) %>% 
#   rename(dha = f22d6n3_g) %>% 
#   mutate(protein = ifelse(is.na(prot_g), protcnt_g, prot_g)) %>% 
#   mutate(fat = ifelse(is.na(fat_g), fatce_g, fat_g)) %>% 
#   mutate(fat = ifelse(is.na(fat), fat_g_2, fat)) %>% 
#   mutate(fat = str_replace(fat,"[\\[]", "")) %>% 
#   mutate(fat = str_replace(fat, "[\\]]", "")) %>% 
#   mutate(protein = str_replace(protein,"[\\[]", "")) %>% 
#   mutate(protein = str_replace(protein, "[\\]]", "")) %>% 
#   mutate(epa = str_replace(epa,"[\\[]", "")) %>% 
#   mutate(epa = str_replace(epa, "[\\]]", "")) %>% 
#   mutate(dha = str_replace(dha,"[\\[]", "")) %>% 
#   mutate(dha = str_replace(dha, "[\\]]", "")) %>% 
#   mutate(ca_mg = str_replace(ca_mg,"[\\[]", "")) %>% 
#   mutate(ca_mg = str_replace(ca_mg, "[\\]]", "")) %>% 
#   mutate(fe_mg = str_replace(fe_mg,"[\\[]", "")) %>% 
#   mutate(fe_mg = str_replace(fe_mg, "[\\]]", "")) %>% 
#   mutate(zn_mg = str_replace(zn_mg,"[\\[]", "")) %>% 
#   mutate(zn_mg = str_replace(zn_mg, "[\\]]", "")) %>% 
#   mutate(zn_mg = as.numeric(zn_mg)) %>% 
#   mutate(ca_mg = as.numeric(ca_mg)) %>% 
#   mutate(fe_mg = as.numeric(fe_mg)) %>% 
#   mutate(epa = as.numeric(epa)) %>% 
#   mutate(dha = as.numeric(dha)) %>% 
#   mutate(fat = as.numeric(fat)) %>% 
#   mutate(protein = as.numeric(protein)) %>% 
#   dplyr::select(af_id, asfis_english_name, scientific_name, asfis_scientific_name, subgroup, country_region, food_name_in_english, biblio_id, bibliography, ca_mg, fe_mg, zn_mg, epa, dha, fat, protein) %>% 
#   rename(location = country_region)


# af_refs <- unique(af2$biblio_id)
# bc_refs <- unique(bc_references$biblio_id)
# overlapping <- intersect(af_refs, bc_refs) ## ok there are 12 overlapping refs between anfood and biocomp

### ok now merge with CINE

updated_ref <- read_csv("data-processed/CINE-nutrients-fish-references-annotated.csv")

keep_refs_bio <- updated_ref %>% 
  filter(grepl("yes", use_in_analysis)) %>% 
  filter(!grepl("y", already_in_biocomp)) 
# %>% 
  # filter(ref_number != 27)

nt <- read_csv("data-processed/trad-foods-cleaned-2020.csv")
nt_keep_bio <- nt %>% 
  mutate(part = ifelse(common_name == "Ninespine Stickleback", "whole", part)) %>% ## fixing data entry mistake
  rename(subgroup = level_1) %>% 
  filter(reference %in% c(keep_refs_bio$ref_number)) %>% 
  rename(genus_species = latin_name_cleaned) %>% 
  rename(fat = fat_g) %>% 
  rename(protein = protein_g) %>% 
  mutate(reference = as.character(reference)) %>% 
  dplyr::select(cine_id, subgroup, genus_species, common_name, reference, part, ca_mg, fe_mg, zn_mg, epa, dha, protein, fat, reference) %>% 
  rename(biblio_id = reference)

WriteXLS(nt_keep_bio, "data-processed/nt_keep_bio.xlsx")


cine_parts_edited <- read_excel("data-processed/nt_keep_bio_edited_with27.xlsx") %>% 
  mutate(biblio_id = as.character(biblio_id))
### ok this file has the corrected sidwell refs; use this one!
  # clean_names() %>% 
  # dplyr::select(cine_id, part_edited)

# cine_all2 <- nt_keep_bio %>% 
#   left_join(., cine_parts_edited, by = "cine_id") ### or just use cine_parts_edited


# unique(nt_keep_bio$part)
# View(nt_keep_bio)

### bring in Reksten! coming back to say no to this, since it's past 2019

# reksten <- read_excel("data/reksten-2020.xlsx") %>% 
#   mutate(species_part = paste(species_name, part, sep = "_")) %>% 
#   dplyr::select(-species_name, - part) %>% 
#   group_by(species_part, nutrient) %>%
#   summarise_each(funs(mean), concentration) %>% 
#   # mutate(part = ifelse(part == "fillet", "muscle", part)) %>% 
#   # mutate(reksten_id = paste0("reksten",rownames(.))) %>% 
#   spread(key = nutrient, value = concentration) %>% 
#   separate(species_part, into = c("genus_species", "part"), sep = "_") %>% 
#   mutate(subgroup = "Finfish") %>% 
#   # rename(calcium = ca_mg,
#   #        zinc = zn_mg,
#   #        iron = fe_mg) %>% 
#   mutate(reference = "Reksten, A.M., Somasundaram, T., Kjellevold, M., Nordhagen, A., BÃ¸kevoll, A., Pincus, L.M., Rizwan, A.A.M., Mamun, A., Thilsted, S.H., Htut, T. and Aakre, I., 2020. Nutrient composition of 19 fish species from Sri Lanka and potential contribution to food and nutrition security.Â Journal of Food Composition and Analysis, p.103508.")

trait_data4_edited <- read_csv("data-processed/trait_data4_edited.csv") %>% 
  filter(keep == "yes") %>% 
  dplyr::dplyr::select(ref_info, seanuts_id2, species_name, nutrient, concentration, subgroup, location) %>% 
  rename(genus_species = species_name) %>% 
  mutate(subgroup = ifelse(subgroup == "finfish", "Finfish", subgroup)) %>% 
  mutate(subgroup = ifelse(subgroup == "mollusc", "Mollusc", subgroup)) %>% 
  mutate(subgroup = ifelse(subgroup == "crustacean", "Crustacean", subgroup)) %>% 
  filter(nutrient %in% c("epa", "dha", "ca_mg", "fe_mg", "zn_mg", "protein_g", "fat_g")) %>% 
  spread(key = nutrient, value = concentration) %>% 
  rename(fat = fat_g,
         protein = protein_g) %>% 
  rename(bibliography = ref_info)

# write_csv(trait_data4_edited, "data-processed/new_refs_contribution_to_seanuts.csv")

### going to check on the accuracy of these data and adding info on which parts are included

trait_data5_edited <- read_excel("data-processed/new_refs_contribution_to_seanuts_edited.xlsx") %>% 
  filter(is.na(exclude_for_wrong_units)) %>% 
  mutate(sea_id = paste0("seadata", rownames(.))) %>% 
  mutate_at(10:14, as.numeric) 

View(trait_data5_edited)
#### bind all datasets
all_data_traits <- bind_rows(cine_parts_edited, bc_raw1c, af_raw1c, trait_data5_edited) 

names(all_data_traits)
length(unique(all_data_traits$bibliography))
new_refs <- unique(all_data_traits$bibliography)


#### August 9 2020
### check out these refs, bring in ones that we don't already have.
# missing_refs_new_data <- data.frame(diffs = setdiff(old_refs, new_refs))
# write_csv(missing_refs_new_data, "data-processed/missing_refs_new_data.csv")
# 
# 
# 
# unique(trait_data4_edited$subgroup)
# missing_data <- trait_data2 %>% 
#   filter(ref_info %in% missing_refs_new_data)

  



all_data_traits_inverts <- all_data_traits %>% 
  filter(subgroup == "Marine Invertebrates") 

write_csv(all_data_traits_inverts, "data-processed/all_data_traits_inverts2.csv")

unique(all_data_traits$subgroup)

inverts_cat <- read_csv("data-processed/all_data_traits_inverts_edited2.csv") %>% 
  dplyr::select(cine_id, subgroup2)

View(inverts_cat)

### note observation of blackfish, obs_id c211 should be whole, not muscle -- fix this!!

all_data_traits2 <- all_data_traits %>% 
  left_join(., inverts_cat, by = "cine_id") %>% 
  mutate(subgroup = ifelse(is.na(subgroup2), subgroup, subgroup2)) %>% 
  mutate(subgroup = ifelse(subgroup == "Fish", "Finfish", subgroup)) %>% 
  mutate(subgroup = ifelse(subgroup == "Molluscs", "Mollusc", subgroup)) %>% 
  mutate(genus_species = ifelse(is.na(genus_species), asfis_scientific_name, genus_species)) %>% 
  mutate(genus_species = ifelse(is.na(genus_species), scientific_name, genus_species)) %>% 
  mutate(food_name_in_english = ifelse(is.na(food_name_in_english), paste(common_name, part, sep = ", "), food_name_in_english)) %>% 
  # mutate(bc_id = ifelse(!is.na(bc_id), paste0("bc", bc_id), bc_id)) %>% 
  # mutate(af_id = ifelse(!is.na(af_id), paste0("af", af_id), af_id)) %>% 
  mutate(cine_id = ifelse(!is.na(cine_id), paste0("c", cine_id), cine_id)) %>% 
  mutate(obs_id = bc_id) %>% 
  mutate(obs_id = ifelse(is.na(bc_id), af_id, bc_id)) %>% 
  mutate(obs_id = ifelse(is.na(obs_id), cine_id, obs_id)) %>% 
  mutate(obs_id = ifelse(is.na(obs_id), sea_id, obs_id)) %>% 
  mutate(common_name = ifelse(is.na(common_name), asfis_english_name, common_name)) %>% 
  dplyr::select(obs_id, subgroup, genus_species, common_name, part, food_name_in_english, location, ca_mg, fe_mg, zn_mg, epa, dha, protein, fat, everything()) %>% 
  mutate(part = ifelse(grepl("skinless", food_name_in_english), "muscle", part)) %>% 
  mutate(part = ifelse(grepl("muscle fillet", food_name_in_english), "muscle", part)) %>% 
  mutate(part_edited = ifelse(obs_id == "c211", "whole", part_edited))
 



View(all_data_traits2)


# clean species names -----------------------------------------------------


all_data_traits3 <- all_data_traits2 %>% 
  dplyr::select(obs_id, subgroup, genus_species, asfis_scientific_name, scientific_name, everything()) %>% 
  mutate(genus_species = str_replace(genus_species,"[\\(]", "")) %>% 
  mutate(genus_species = str_replace(genus_species,"[\\)]", "")) 

library(taxize)
specieslist <- unique(all_data_traits3$genus_species)

hot <- classification(specieslist, db = 'itis')
hot2 <- classification(result.short$matched_name2, db = 'itis')
outputlst2 <- hot2
outputlst <- hot

# Parse out the taxonomy levels that you require
taxdata <- data.frame()
for(x in 1:length(outputlst)){
  tryCatch({
    kingdom=filter(outputlst[[x]], rank =="kingdom")$name
    phylum=filter(outputlst[[x]], rank =="phylum")$name
    class=filter(outputlst[[x]], rank =="class")$name
    order=filter(outputlst[[x]], rank =="order")$name
    family=filter(outputlst[[x]], rank =="family")$name
    genus=filter(outputlst[[x]], rank =="genus")$name
    species=filter(outputlst[[x]], rank =="species")$name
    
    row <- data.frame(cbind(kingdom = kingdom, phylum=phylum,class=class,order=order,family=family,genus=genus, species = species))
    taxdata <- bind_rows(taxdata, row)    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

View(taxdata)

gnr_datasources() %>% View

src <- c("EOL", "NCBI", "ITIS")
subset(gnr_datasources(), title %in% src) %>% View

length(specieslist)
library(taxize)
result.long <- specieslist %>%
  gnr_resolve(data_source_ids = c(3,4, 177, 9, 155), 
              with_canonical_ranks=T)

result.short <- result.long %>%
  dplyr::select(user_supplied_name, submitted_name, matched_name2, score)%>%
  distinct()
write.table(result.short,
            "data-processed/species_names_resolved.txt", 
            sep="\t", row.names = F, quote = F)

result.itis <- specieslist %>%
  gnr_resolve(data_source_ids = c(3), 
              with_canonical_ranks=T)

itis_tax_output <- classification(result.itis$matched_name2, db = 'itis')
outputlst <- itis_tax_output

taxdata2 <- data.frame()
for(x in 1:length(outputlst)){
  tryCatch({
    kingdom=filter(outputlst[[x]], rank =="kingdom")$name
    phylum=filter(outputlst[[x]], rank =="phylum")$name
    class=filter(outputlst[[x]], rank =="class")$name
    order=filter(outputlst[[x]], rank =="order")$name
    family=filter(outputlst[[x]], rank =="family")$name
    genus=filter(outputlst[[x]], rank =="genus")$name
    species=filter(outputlst[[x]], rank =="species")$name
    
    row <- data.frame(cbind(kingdom = kingdom, phylum=phylum,class=class,order=order,family=family,genus=genus, species = species))
    taxdata2 <- bind_rows(taxdata2, row)    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}





result.short <- result.long %>%
  dplyr::select(user_supplied_name, submitted_name, matched_name2, score)%>%
  distinct()

library(taxize)
result.fishbase <- specieslist %>%
  gnr_resolve(data_source_ids = c(155), 
              with_canonical_ranks=T)

write_csv(result.fishbase, "data-processed/fishbase-species2.csv")


# write_csv(all_data_traits2, "data-processed/seanuts-rebuild.csv") ### this is the complete seanuts dataset, after rebuilding in August 2020
library(WriteXLS)
WriteXLS(all_data_traits3, "data-processed/seanuts-rebuild-aug14.xlsx") ### ok this is the new dataset! next need to taxize it.

# seanuts_parts <- read_excel("data-processed/seanuts-rebuild-edited.xlsx") %>% 
#   filter(biblio_id != "fi151") ### this file contains the edited parts
# View(seanuts_parts)

library(plotrix)
library(cowplot)
theme_set(theme_cowplot())

all_data_traits3 <- read_excel("data-processed/seanuts-rebuild-aug14.xlsx")

species_names_resolved <- read_excel("data-processed/species_names_resolved_edited.xlsx") %>% 
  filter(is.na(exclude))

all4 <- all_data_traits3 %>% 
  left_join(., species_names_resolved, by = c("genus_species" = "user_supplied_name")) %>% 
  rename(taxize_name = matched_name2)
WriteXLS(all4, "data-processed/seanuts-rebuild-aug14-taxized.xlsx") ### ok this is the new dataset, taxized

all4 <- read_excel("data-processed/seanuts-rebuild-aug14-taxized.xlsx")

all5 <- all4 %>% 
  dplyr::select(obs_id, taxize_name, genus_species, asfis_scientific_name, scientific_name, submitted_name, subgroup, common_name, part_edited,
                food_name_in_english, ca_mg, fe_mg, zn_mg, epa, dha, protein, fat, biblio_id, location, season, comments_on_data_processing_methods, publication_year, food_item_id) %>% 
  mutate(part_edited = ifelse(part_edited == "muscle_bones", "muscle_organs", part_edited)) %>% 
  rename(body_part = part_edited) %>% 
  mutate(taxize_name = ifelse(obs_id == "c428", "Mercenaria mercenaria", taxize_name)) %>% 
  mutate(taxize_name = ifelse(obs_id == "bc864", "Scomber colias", taxize_name)) %>% 
  mutate(taxize_name = ifelse(obs_id %in% c("af563", "af564"), "Panopea generosa", taxize_name)) %>% 
  mutate(taxize_name = ifelse(obs_id == "af802", "Spisula sachalinensis", taxize_name)) %>% 
  mutate(taxize_name = ifelse(obs_id == "af231", "Muraenesox talabon", taxize_name)) %>% 
  mutate(taxize_name = ifelse(obs_id == "af233", "Caranx sansun", taxize_name)) %>% 
  mutate(taxize_name = ifelse(obs_id == "af239", "Scoliodon sorrakowah", taxize_name)) %>% 
  mutate(taxize_name = ifelse(obs_id == "af240", "Tachysurus zona", taxize_name)) %>% 
  mutate(taxize_name = ifelse(obs_id %in% c("bc1439", "bc1504"), "Myoxocephalus scorpius", taxize_name)) %>% 
  mutate(taxize_name = ifelse(obs_id == "bc83", "Alepocephalus agassizii", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Aquapecten grandis", "Placopecten magellanicus", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Arius madagascariensis", "Arius madagascariensis", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Atherina lagunae", "Atherina lagunae", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Balistes capriscus", "Balistes capriscus", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Balsistes capriscus", "Balistes capriscus", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Barbodes altus", "Barbodes altus", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Barbodes gonionotus", "Barbonymus gonionotus", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Barbus intermedius", "Labeobarbus intermedius", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Brachidontes pharaonis", "Brachidontes pharaonis", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Brycon microlepis", "Brycon microlepis", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Caulolatilus intermedius", "Caulolatilus intermedius", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Brycon microlepis", "Brycon microlepis", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Chanda ranga", "Parambassis ranga", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Chionoecetes angulatus", "Chionoecetes angulatus", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Chionoecetes japonicus", "Chionoecetes japonicus", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Dermogenys pusilla", "Dermogenys pusilla", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Dermogenys pusilla juv.", "Dermogenys pusilla", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Esomus longilamus", "Esomus longilamus", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Egeria radiata", "Egeria radiata", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Euryglossa panoides", "Euryglossa panoides", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Evasterias troschelii", "Evasterias troschelii", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Galeorhinus australis", "Galeorhinus australis", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Haliotis laevigata", "Haliotis laevigata", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Hexaplex trunculus", "Hexaplex trunculus", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Hoplias malabaricus", "Hoplias malabaricus", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Leporinus elongatus", "Megaleporinus elongatus", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Liza carinata", "Liza carinata", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Maja brachydactyla", "Maja brachydactyla", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Megaloancistrus aculeatus", "Megaloancistrus aculeatus", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Serrasalmus marginatus", "Serrasalmus marginatus", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Stichopus californicus", "Stichopus californicus", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Mystus bleekeri", "Mystus bleekeri", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Nodipecten subnodosus", "Nodipecten subnodosus", taxize_name)) %>% 
  mutate(taxize_name = ifelse(scientific_name == "Nordotis discus", "Haliotis discus", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Notopterus chitala", "Notopterus chitala", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Pampus punctatissimus", "Pampus punctatissimus", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Parachela siamensis", "Parachela siamensis", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Parambassis wollfi", "Parambassis wolffii", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Paroctopus hongkongensis", "Octopus hongkongensis", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Peronidia venulosa", "Peronidia venulosa", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Pimelodus argenteus", "Pimelodus argenteus", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Pisaster ochraceus", "Pisaster ochraceus", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Pomacea canaliculata", "Pomacea canaliculata", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Puntius brevis", "Puntius brevis", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Puntius chola", "Puntius chola", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Pycnopodia helianthoides", "Pycnopodia helianthoides", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Rasbora borapetensis", "Rasbora borapetensis", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Salvelinus naresi", "Salvelinus naresi", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Scomber japonicus/colias", "Scomber japonicus", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Scopthalmus maeticus", "Scophthalmus maeoticus", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Sebastes alascanus", "Sebastolobus alascanus", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Sebastes rubrivincuts", "Sebastes rubrivinctus", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Spicara alcedo", "Spicara alcedo", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Spicara vulgaris", "Spicara smaris", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Sulculus diversicolor aquatieis", "Sulculus diversicolor", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Synodontis victoriae", "Synodontis victoriae", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Synodus foetens", "Synodus foetens", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Trichogaster microlepis", "Trichogaster microlepis", taxize_name)) %>%
  mutate(taxize_name = ifelse(scientific_name == "Venerupis japonica", "Venerupis japonica", taxize_name)) %>%
  mutate(taxize_name = ifelse(is.na(taxize_name), genus_species, taxize_name)) %>%
  filter(!grepl("spp", taxize_name)) %>% 
  filter(!grepl("[.]", taxize_name)) %>% 
  arrange(obs_id) %>% 
  dplyr::select(taxize_name, scientific_name, asfis_scientific_name, genus_species, everything()) 


unique(all5$taxize_name)

### c428 is Mercenaria mercenaria
## bc864 is Scomber colias
## af563, af564 is Panopea generosa
## af802 is Spisula sachalinensis
## bc1415, bc1424, bc 1426, bc1542, is not id'ed to species

WriteXLS(all5, "data-processed/seanuts-rebuild-aug26-taxized.xlsx") ### ok this is the new dataset, taxized; a few last taxized species names filled in
library(WriteXLS)

# read in complete dataset ------------------------------------------------


all5 <- read_excel("data-processed/seanuts-rebuild-aug26-taxized.xlsx")
View(all5)
all5 %>% 
  filter(is.na(taxize_name)) %>% View

unique(all5$part_edited)
length(unique(all4$taxize_name))

library(plotrix)
all5 %>% 
  group_by(body_part) %>% 
  filter(subgroup == "Finfish") %>% 
  dplyr::dplyr::select(ca_mg) %>% 
  filter(!is.na(ca_mg)) %>% 
  summarise(mean_ca = mean(ca_mg, na.rm = TRUE),
            stde_ca = std.error(ca_mg, na.rm = TRUE)) %>% 
  filter(!is.na(mean_ca)) %>% 
  ggplot(aes(x = reorder(body_part, mean_ca), y = mean_ca)) + geom_point() +
  geom_errorbar(aes(x = reorder(body_part, mean_ca), ymin = mean_ca - stde_ca, ymax = mean_ca + stde_ca), width = 0.1)


mod <- aov(zn_mg ~ body_part, data = filter(all5, subgroup == "Finfish")) 
summary(mod)
aov(mod)

unique(all_data_traits2$subgroup)
# View(all_data_traits)


percentages <- all5 %>% 
  rename(calcium = ca_mg,
         zinc = zn_mg,
         iron = fe_mg) %>% 
  filter(!is.na(taxize_name)) %>% 
  gather(8:14, key = nutrient, value = concentration) %>% 
  mutate(dri_per = NA) %>% 
  mutate(dri_per = ifelse(nutrient == "calcium", concentration/1200, dri_per)) %>% 
  mutate(dri_per = ifelse(nutrient == "iron", concentration/18, dri_per)) %>% 
  mutate(dri_per = ifelse(nutrient == "zinc", concentration/11, dri_per)) %>% 
  mutate(dri_per = ifelse(nutrient == "epa", concentration/1, dri_per)) %>%
  mutate(dri_per = ifelse(nutrient == "dha", concentration/1, dri_per)) %>% 
  mutate(dri_per = ifelse(nutrient == "protein", concentration/50, dri_per)) %>% 
  mutate(dri_per = ifelse(nutrient == "fat", concentration/70, dri_per)) %>% 
  mutate(dri_per = dri_per*100) %>% 
  filter(!is.na(concentration)) 

unique(percentages$nutrient)

percentages$nutrient <- factor(percentages$nutrient, levels = c("protein", "fat", "calcium", "zinc", "iron", "epa", "dha"))


perc2 <- percentages %>% 
  distinct(nutrient, concentration, taxize_name, common_name, food_name_in_english, body_part, dri_per, subgroup, .keep_all = TRUE)

perc2$nutrient <- factor(perc2$nutrient, levels = c("protein", "fat", "calcium", "zinc", "iron", "epa", "dha"))

write_csv(perc2, "data-processed/seafood-rda-percentages.csv")

perc2 %>% 
  ggplot(aes(x = dri_per, fill = subgroup)) + 
  geom_histogram(binwidth = 0.07) +
  scale_fill_brewer(type = "qual", palette = "Paired") +
  scale_x_log10(breaks = c(1, 10, 100)) +
  facet_grid(nutrient ~ subgroup, scales = "free_y", switch = "x") + theme_bw() + geom_vline(xintercept = 10) +
  xlab("Percentage of DRI in 100g edible portion") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text=element_text(family="Helvetica", size=12)) +
  theme(strip.background = element_blank()) +
  theme(legend.title=element_blank()) +
  theme(strip.text.y = element_text(size = 12)) +
  theme(legend.position="none") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 2))

# var. spp.: Mallotus, Oncorphyncus and Salmo

### joey come back here.
mean_nuts2 <- perc2 %>% 
  spread(nutrient, concentration) %>% 
  group_by(taxize_name, subgroup) %>% 
  summarise(calcium = mean(calcium, na.rm = TRUE),
            zinc = mean(zinc, na.rm = TRUE), 
            iron = mean(iron, na.rm = TRUE),
            epa = mean(epa, na.rm = TRUE),
            dha = mean(dha, na.rm = TRUE)) %>% 
  filter(!is.na(calcium), !is.na(zinc), !is.na(iron), !is.na(epa), !is.na(dha)) 

View(mean_nuts2)

write_csv(mean_nuts2, "data-processed/mean_nuts_aug2020.csv") ## ok this is the new mean_nuts, after rebuilding from infoods
write_csv(mean_nuts2, "data-processed/mean_nuts_aug-26-2020.csv") 
# write_csv(mean_nuts2, "data-processed/mean_nuts_aug2020b.csv")
mean_nuts3 <- perc2 %>% 
  spread(nutrient, concentration) %>% 
  group_by(taxize_name, subgroup) %>% 
  summarise(calcium = mean(calcium, na.rm = TRUE),
            zinc = mean(zinc, na.rm = TRUE), 
            protein = mean(protein, na.rm = TRUE), 
            fat = mean(fat, na.rm = TRUE), 
            iron = mean(iron, na.rm = TRUE),
            epa = mean(epa, na.rm = TRUE),
            dha = mean(dha, na.rm = TRUE)) %>% 
  filter(!is.na(calcium), !is.na(zinc), !is.na(iron), !is.na(epa), !is.na(dha), !is.na(protein)) %>% 
  ungroup()
write_csv(mean_nuts3, "data-processed/mean_nuts_aug2020_micro_macro.csv") ## ok this is the new mean_nuts, after rebuilding from infoods
write_csv(mean_nuts3, "data-processed/mean_nuts_aug-26-2020_micro_macro.csv")
View(mean_nuts3)

mean_nuts3 %>% 
  gather(3:9, key = nutrient, value = concentration) %>% 
  ggplot(aes(x = concentration)) + geom_density(fill = "grey") +
  facet_wrap( ~ nutrient, scales = "free")
