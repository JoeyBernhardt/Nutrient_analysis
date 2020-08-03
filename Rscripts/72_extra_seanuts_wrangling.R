

trait_data3b_wide <- trait_data2 %>% 
  distinct(species_name, food_name_clean, ref_info, seanuts_id2, nutrient, concentration, .keep_all = TRUE) %>% 
  spread(key = nutrient, value = concentration) %>% 
  mutate(protein = protcnt_g) %>% 
  mutate(protein = ifelse(is.na(protein), protein_g, protein)) %>% 
  mutate(protein = ifelse(is.na(protein), prot_g, protein)) %>% 
  select(species_name, seanuts_id2, protein, fat_g, dha, epa, ca_mg, zn_mg, fe_mg, subgroup, ref_info, food_name_clean) %>% 
  rename(fat = fat_g,
         calcium = ca_mg, 
         zinc = zn_mg,
         iron = fe_mg) 

references <- trait_data3b_wide %>% 
  distinct(ref_info) %>% 
  mutate(ref_id = paste0("s", rownames(.)))

write_csv(references, "data-processed/seanuts_references.csv")
