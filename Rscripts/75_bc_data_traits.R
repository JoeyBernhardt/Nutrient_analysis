library(readxl)

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
  select(food_item_id, epa, dha)


bc_all <- bc_raw_sheet1 %>% 
  left_join(., bc_raw_sheet2, by = "food_item_id") %>% ### here we join the datasheet with the minerals, protein and fat with the fatty acids
  select(food_item_id, biblio_id, subgroup, country_region, food_name_in_english, processing, scientific_name, asfis_english_name, asfis_scientific_name, season, other, comments_on_data_processing_methods, publication_year, contains("prot"), contains("fat"), ca_mg, fe_mg, zn_mg, epa, dha) %>% 
  mutate(bc_id = paste0("bc", rownames(.)))


WriteXLS(bc_all, "data-processed/bc_all.xlsx")


# read in part key edited in excel ----------------------------------------
parts_edited <- read_excel("data-processed/bc_all_edited.xlsx") %>% 
  clean_names() %>% 
  select(food_item_id, part_edited)

bc_all2 <- bc_all %>% 
  left_join(., parts_edited, by = "food_item_id")


# bc_raw <- read_excel("data-processed/biocomp-raw-macro-micro.xlsx")

bc_raw1b <- bc_all2 %>% 
  select(-protcnp_g) %>% 
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
  # select(food_item_id, ca_mg, fe_mg, zn_mg, epa, dha, fat, protein, everything()) %>% 
  select(-contains("extra")) %>% 
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
  select(-prot_g, - fatce_g, -fatrn_g, -fat_g, -fat_g_2, -protcnt_g) %>% ## get rid of columns we no longer need
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
  select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english)

muscle_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  filter(grepl("muscle",food_name_in_english)) %>% 
  filter(!grepl("bones",food_name_in_english)) %>% 
  filter(!grepl("fillet",food_name_in_english)) %>% 
  filter(!grepl("skinless",food_name_in_english)) %>% 
  mutate(part_edited = "muscle") %>% 
  select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english)

muscle_skinless_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  # filter(grepl("fillet",food_name_in_english)) %>% 
  # filter(!grepl("bones",food_name_in_english)) %>% 
  filter(grepl("skinless",food_name_in_english)) %>% 
  mutate(part_edited = "muscle_skinless") %>% 
  select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english)

muscle_skinned_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  # filter(!grepl("fillet",food_name_in_english)) %>% 
  filter(!grepl("bones",food_name_in_english)) %>% 
  filter(grepl("skinned",food_name_in_english)) %>% 
  mutate(part_edited = "muscle_skinless") %>% 
  select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english) 

muscle_bones_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  filter(grepl("fillet",food_name_in_english)) %>% 
  filter(grepl("bones",food_name_in_english)) %>% 
  filter(!grepl("skinless",food_name_in_english)) %>% 
  mutate(part_edited = "muscle_bones") %>% 
  select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english)

muscle_organs_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  filter(!grepl("fillet",food_name_in_english)) %>% 
  filter(!grepl("whole",food_name_in_english)) %>% 
  filter(!grepl("flesh",food_name_in_english)) %>% 
  filter(grepl("cleaned",food_name_in_english)) %>%
  mutate(part_edited = "muscle_organs") %>% 
  select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english)

muscle_other_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  filter(!grepl("fillet",food_name_in_english)) %>% 
  filter(!grepl("whole",food_name_in_english)) %>%
  filter(!grepl("skinless",food_name_in_english)) %>%
  filter(!grepl("skinned",food_name_in_english)) %>%
  filter(!grepl("muscle",food_name_in_english)) %>%
  mutate(part_edited = "muscle_organs") %>% 
  select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english) 

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
  select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english)

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
  select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english) 

whole_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  filter(!grepl("fillet",food_name_in_english)) %>% 
  filter(grepl("whole",food_name_in_english)) %>% 
  mutate(part_edited = "whole") %>% 
  select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english) 

liver_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  filter(grepl("liver",food_name_in_english)) %>% 
  mutate(part_edited = "liver") %>% 
  select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english) 

egg_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  filter(grepl("egg",food_name_in_english)) %>% 
  mutate(part_edited = "egg") %>% 
  select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english)

roe_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  filter(grepl("roe",food_name_in_english)) %>% 
  mutate(part_edited = "egg") %>% 
  select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english) 

caviar_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  filter(grepl("caviar",food_name_in_english)) %>% 
  mutate(part_edited = "egg") %>% 
  select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english) 

oil_samples <- bc_raw1c %>% 
  filter(subgroup == "Finfish") %>% 
  filter(grepl("oil",food_name_in_english)) %>% 
  mutate(part_edited = "oil") %>% 
  select(food_item_id, bc_id, part_edited, subgroup, food_name_in_english) 

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

bc_raw2 <- bc_raw1 %>% 
  left_join(., bc_refs) 

bc_references <- bc_raw2 %>% 
  select(biblio_id, bibliography) 




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
#   select(food_item_id, bc_id, asfis_english_name, asfis_scientific_name, scientific_name, subgroup, country_region, food_name_in_english, biblio_id, ca_mg, fe_mg, zn_mg, epa, dha, fat, protein) %>% 
#   rename(location = country_region)

View(bc)
unique(bc$reference)

unique(bc$food_name_in_english)


# categorizing body parts -------------------------------------------------


fillet_samples <- bc %>% 
  filter(grepl("fillet",food_name_in_english)) %>% 
  filter(!grepl("bones",food_name_in_english)) %>% 
  select(bc_id)

muscle_samples <- bc %>% 
  filter(grepl("muscle",food_name_in_english)) %>% 
  filter(!grepl("bones",food_name_in_english)) %>% 
  select(bc_id)

View(muscle_samples)

egg_samples <- bc %>% 
  # filter(grepl("roe",food_name_in_english)) %>% 
  filter(grepl(c("egg"),food_name_in_english)) %>% 
  filter(!grepl("bones",food_name_in_english)) %>% 
  select(bc_id)

roe_samples <- bc %>% 
  # filter(grepl("roe",food_name_in_english)) %>% 
  filter(grepl(c("roe"),food_name_in_english)) %>% 
  filter(!grepl("bones",food_name_in_english)) %>% 
  select(bc_id)

whole_samples <- bc %>% 
  # filter(grepl("roe",food_name_in_english)) %>% 
  filter(grepl(c("whole"),food_name_in_english)) %>% 
  select(bc_id)

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
  select(bc_id)
  
  liver_samples <- bc %>% 
    # filter(grepl("roe",food_name_in_english)) %>% 
    filter(grepl(c("liver"),food_name_in_english)) %>% 
    select(bc_id)
  
  head_samples <- bc %>% 
    # filter(grepl("roe",food_name_in_english)) %>% 
    filter(grepl(c("head"),food_name_in_english)) %>% 
    select(bc_id)

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
  select(food_item_id, epa, dha)


af_all <- af_raw_sheet1 %>% 
  left_join(., af_raw_sheet2, by = "food_item_id") %>% ### here we join the datasheet with the minerals, protein and fat with the fatty acids
  # select(food_item_id, biblio_id, subgroup, country_region, food_name_in_english, processing, scientific_name, asfis_english_name, asfis_scientific_name, season, other, comments_on_data_processing_methods, publication_year, contains("prot"), contains("fat"), ca_mg, fe_mg, zn_mg, epa, dha) %>% 
  mutate(af_id = paste0("af", rownames(.))) %>% 
  filter(processing == "r") %>% 
  mutate(farmed_finfish = ifelse(grepl("farmed", food_name_in_english) & subgroup == "Finfish", "farmed_finfish", "other")) %>%
  mutate(farmed_crustacean = ifelse(grepl("farmed", food_name_in_english) & subgroup == "Crustacean", "farmed_crustacean", "unfarmed_crustacean")) %>% 
  filter(farmed_finfish != "farmed_finfish") %>% 
  filter(farmed_crustacean != "farmed_crustacean") %>% 
  filter(subgroup == "Molluscs" | grepl("wild", food_name_in_english) | biblio_id %in% c("fi195", "fi159", "fi188", "fi203")) %>% 
  filter(!is.na(food_item_id)) %>% 
  select(food_item_id, af_id, biblio_id, type, subgroup, country_region, food_name_in_english, processing, scientific_name, asfis_english_name, asfis_scientific_name, season, other, comments_on_data_processing_methods, publication_year, contains("prot"), contains("fat"), ca_mg, fe_mg, zn_mg, epa, dha)
  


WriteXLS(af_all, "data-processed/af_all.xlsx")

af_parts_edited <- read_excel("data-processed/af_all_edited.xlsx") %>% 
  clean_names() %>% 
  select(food_item_id, part_edited)

af_all2 <- af_all %>% 
  left_join(., af_parts_edited, by = "food_item_id")


af_raw1b <- af_all2 %>% 
  select(-protcnp_g) %>% 
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
  # select(food_item_id, ca_mg, fe_mg, zn_mg, epa, dha, fat, protein, everything()) %>% 
  select(-contains("extra")) %>% 
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
  select(-prot_g, - fatce_g, -fatrn_g, -fat_g, -fat_g_2, -protcnt_g) %>% ## get rid of columns we no longer need
  filter(processing == "r") %>% 
  mutate(publication_year = as.character(publication_year))### only keep raw samples



View(af_raw1c)

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
#   select(af_id, asfis_english_name, scientific_name, asfis_scientific_name, subgroup, country_region, food_name_in_english, biblio_id, bibliography, ca_mg, fe_mg, zn_mg, epa, dha, fat, protein) %>% 
#   rename(location = country_region)


# af_refs <- unique(af2$biblio_id)
# bc_refs <- unique(bc_references$biblio_id)
# overlapping <- intersect(af_refs, bc_refs) ## ok there are 12 overlapping refs between anfood and biocomp

### ok now merge with CINE

updated_ref <- read_csv("data-processed/CINE-nutrients-fish-references-annotated.csv")

keep_refs_bio <- updated_ref %>% 
  filter(grepl("yes", use_in_analysis)) %>% 
  filter(!grepl("y", already_in_biocomp)) %>% 
  filter(ref_number != 27)

nt <- read_csv("data-processed/trad-foods-cleaned-2020.csv")
nt_keep_bio <- nt %>% 
  mutate(part = ifelse(common_name == "Ninespine Stickleback", "whole", part)) %>% ## fixing data entry mistake
  rename(subgroup = level_1) %>% 
  filter(reference %in% c(keep_refs_bio$ref_number)) %>% 
  rename(genus_species = latin_name_cleaned) %>% 
  rename(fat = fat_g) %>% 
  rename(protein = protein_g) %>% 
  mutate(reference = as.character(reference)) %>% 
  select(cine_id, subgroup, genus_species, common_name, reference, part, ca_mg, fe_mg, zn_mg, epa, dha, protein, fat, reference) %>% 
  rename(biblio_id = reference)

WriteXLS(nt_keep_bio, "data-processed/nt_keep_bio.xlsx")


cine_parts_edited <- read_excel("data-processed/nt_keep_bio_edited.xlsx") %>% 
  clean_names() %>% 
  select(cine_id, part_edited)

cine_all2 <- nt_keep_bio %>% 
  left_join(., cine_parts_edited, by = "cine_id")


# unique(nt_keep_bio$part)
# View(nt_keep_bio)

### bring in Reksten! coming back to say no to this, since it's past 2019

# reksten <- read_excel("data/reksten-2020.xlsx") %>% 
#   mutate(species_part = paste(species_name, part, sep = "_")) %>% 
#   select(-species_name, - part) %>% 
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
  select(ref_info, seanuts_id2, species_name, nutrient, concentration, subgroup, location) %>% 
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
all_data_traits <- bind_rows(cine_all2, bc_raw1c, af_raw1c, trait_data5_edited) 

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

# write_csv(all_data_traits_inverts, "data-processed/all_data_traits_inverts.csv")

unique(all_data_traits$subgroup)

inverts_cat <- read_csv("data-processed/all_data_traits_inverts_edited.csv") %>% 
  select(cine_id, subgroup2)

View(inverts_cat)

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
  select(obs_id, subgroup, genus_species, common_name, part, food_name_in_english, location, ca_mg, fe_mg, zn_mg, epa, dha, protein, fat, everything()) %>% 
  mutate(part = ifelse(grepl("skinless", food_name_in_english), "muscle", part)) %>% 
  mutate(part = ifelse(grepl("muscle fillet", food_name_in_english), "muscle", part))
 



View(all_data_traits2)


# write_csv(all_data_traits2, "data-processed/seanuts-rebuild.csv") ### this is the complete seanuts dataset, after rebuilding in August 2020
library(WriteXLS)
WriteXLS(all_data_traits2, "data-processed/seanuts-rebuild.xlsx")

# seanuts_parts <- read_excel("data-processed/seanuts-rebuild-edited.xlsx") %>% 
#   filter(biblio_id != "fi151") ### this file contains the edited parts
# View(seanuts_parts)



all_data_traits2 %>% 
  group_by(part_edited) %>% 
  filter(subgroup == "Finfish") %>% 
  select(ca_mg) %>% 
  filter(!is.na(ca_mg)) %>% 
  summarise(mean_ca = mean(ca_mg, na.rm = TRUE),
            stde_ca = std.error(ca_mg, na.rm = TRUE)) %>% 
  filter(!is.na(mean_ca)) %>% 
  ggplot(aes(x = reorder(part_edited, mean_ca), y = mean_ca)) + geom_point() +
  geom_errorbar(aes(x = reorder(part_edited, mean_ca), ymin = mean_ca - stde_ca, ymax = mean_ca + stde_ca), width = 0.1)


mod <- aov(fe_mg ~ part_edited, data = filter(all_data_traits2, subgroup == "Finfish")) 
summary(mod)
aov(mod)

unique(all_data_traits2$subgroup)
# View(all_data_traits)


percentages <- all_data_traits2 %>% 
  rename(calcium = ca_mg,
         zinc = zn_mg,
         iron = fe_mg) %>%
  gather(7:13, key = nutrient, value = concentration) %>% 
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
  mutate(genus_species = ifelse(is.na(genus_species), asfis_scientific_name, genus_species)) %>% 
  mutate(genus_species = ifelse(is.na(genus_species), scientific_name, genus_species)) %>% 
  mutate(food_name_in_english = ifelse(is.na(food_name_in_english), paste(common_name, part, sep = ", "), food_name_in_english)) %>% 
  distinct(nutrient, concentration, genus_species, common_name, food_name_in_english, asfis_scientific_name, part, dri_per, subgroup, .keep_all = TRUE)

perc2$nutrient <- factor(perc2$nutrient, levels = c("protein", "fat", "calcium", "zinc", "iron", "epa", "dha"))

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

### joey come back here.
mean_nuts2 <- perc2 %>% 
  filter(grepl(" ", genus_species)) %>% 
  mutate(genus_species = ifelse(genus_species == "Oreochromis (=Tilapia) spp", "Oreochromis niloticus", genus_species)) %>% 
  mutate(genus_species = ifelse(genus_species == "var. spp.: Boreogadus, Eleginus, Gadus, Microgadus", "Microgadus tomcod", genus_species)) %>% 
  mutate(genus_species = ifelse(genus_species == "tinca tinca", "Tinca tinca", genus_species)) %>% 
  spread(nutrient, concentration) %>% 
  group_by(genus_species, subgroup) %>% 
  summarise(calcium = mean(calcium, na.rm = TRUE),
            zinc = mean(zinc, na.rm = TRUE), 
            iron = mean(iron, na.rm = TRUE),
            epa = mean(epa, na.rm = TRUE),
            dha = mean(dha, na.rm = TRUE)) %>% 
  filter(!is.na(calcium), !is.na(zinc), !is.na(iron), !is.na(epa), !is.na(dha)) %>% 
  ungroup()

write_csv(mean_nuts2, "data-processed/mean_nuts_aug2020.csv") ## ok this is the new mean_nuts, after rebuilding from infoods
write_csv(mean_nuts2, "data-processed/mean_nuts_aug2020b.csv")
mean_nuts3 <- perc2 %>% 
  filter(grepl(" ", genus_species)) %>% 
  mutate(genus_species = ifelse(genus_species == "Oreochromis (=Tilapia) spp", "Oreochromis niloticus", genus_species)) %>% 
  mutate(genus_species = ifelse(genus_species == "var. spp.: Boreogadus, Eleginus, Gadus, Microgadus", "Microgadus tomcod", genus_species)) %>% 
  mutate(genus_species = ifelse(genus_species == "tinca tinca", "Tinca tinca", genus_species)) %>% 
  spread(nutrient, concentration) %>% 
  group_by(genus_species, subgroup) %>% 
  summarise(calcium = mean(calcium, na.rm = TRUE),
            zinc = mean(zinc, na.rm = TRUE), 
            protein = mean(protein, na.rm = TRUE), 
            fat = mean(fat, na.rm = TRUE), 
            iron = mean(iron, na.rm = TRUE),
            epa = mean(epa, na.rm = TRUE),
            dha = mean(dha, na.rm = TRUE)) %>% 
  filter(!is.na(calcium), !is.na(zinc), !is.na(iron), !is.na(epa), !is.na(dha), !is.na(protein), !is.na(fat)) %>% 
  ungroup()
write_csv(mean_nuts3, "data-processed/mean_nuts_aug2020_micro_macro.csv") ## ok this is the new mean_nuts, after rebuilding from infoods


mean_nuts3 %>% 
  gather(3:9, key = nutrient, value = concentration) %>% 
  ggplot(aes(x = concentration)) + geom_density(fill = "grey") +
  facet_wrap( ~ nutrient, scales = "free")
