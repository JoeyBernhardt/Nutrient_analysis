

#### biofood comp data for traits

bc_raw <- read_csv("data-processed/biofood-comp-data.csv") %>% 
  clean_names() %>% 
  filter(!is.na(food_item_id)) %>% 
  mutate(bc_id = rownames(.))

bc <- bc_raw %>% 
  rename(epa = f20d5n3_g) %>% 
  rename(dha = f22d6n3_g) %>% 
  mutate(epa = str_replace(epa,"[\\[]", "")) %>% 
  mutate(epa = str_replace(epa, "[\\]]", "")) %>% 
  mutate(dha = str_replace(dha,"[\\[]", "")) %>% 
  mutate(dha = str_replace(dha, "[\\]]", "")) %>% 
  mutate(ca_mg = str_replace(ca_mg,"[\\[]", "")) %>% 
  mutate(ca_mg = str_replace(ca_mg, "[\\]]", "")) %>% 
  mutate(fe_mg = str_replace(fe_mg,"[\\[]", "")) %>% 
  mutate(fe_mg = str_replace(fe_mg, "[\\]]", "")) %>% 
  mutate(zn_mg = str_replace(zn_mg,"[\\[]", "")) %>% 
  mutate(zn_mg = str_replace(zn_mg, "[\\]]", "")) %>% 
  mutate(zn_mg = as.numeric(zn_mg)) %>% 
  mutate(ca_mg = as.numeric(ca_mg)) %>% 
  mutate(fe_mg = as.numeric(fe_mg)) %>% 
  mutate(epa = as.numeric(epa)) %>% 
  mutate(dha = as.numeric(dha)) %>% 
  select(bc_id, asfis_english_name, asfis_scientific_name, subgroup, food_name_in_english, ca_mg, fe_mg, zn_mg, epa, dha)


unique(bc$food_name_in_english)

fillet_samples <- bc %>% 
  filter(grepl("fillet",food_name_in_english)) %>% 
  filter(!grepl("bones",food_name_in_english)) %>% 
  select(bc_id)

muscle_samples <- bc %>% 
  filter(grepl("muscle",food_name_in_english)) %>% 
  filter(!grepl("bones",food_name_in_english)) %>% 
  select(bc_id)

muscle_only <- bind_rows(fillet_samples, muscle_samples) %>% 
  distinct()

unique(muscle_samples$food_name_in_english)

muscle_only_data <- bc %>% 
  filter(bc_id %in% muscle_only$bc_id)

unique(muscle_only_data$asfis_scientific_name)


### ok now merge with CINE

updated_ref <- read_csv("data-processed/CINE-nutrients-fish-references-annotated.csv")

keep_refs_bio <- updated_ref %>% 
  filter(grepl("yes", use_in_analysis)) %>% 
  filter(!grepl("y", already_in_biocomp))

nt <- read_csv("data-processed/trad-foods-cleaned-2020.csv")
nt_keep_bio <- nt %>% 
  rename(subgroup = level_1) %>% 
  filter(reference %in% c(keep_refs_bio$ref_number)) %>% 
  rename(genus_species = latin_name_cleaned) %>% 
  select(cine_id, subgroup, genus_species, common_name, part, ca_mg, fe_mg, epa, dha, protein_g, fat_g, reference)

all_data_traits <- bind_rows(nt_keep_bio, bc)


