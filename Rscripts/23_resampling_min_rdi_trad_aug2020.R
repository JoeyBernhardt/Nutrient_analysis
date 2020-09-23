library(purrr)
library(tidyverse)


trad_data_old <- read_csv("data-processed/trad-foods-mean.csv")
trad_data1 <- read_csv("data-processed/tdata.csv") ### update with new extracted trad foods aug 8 2020

trad_data <- read_csv("data-processed/traditional_foods_nutrients_cultures.csv") %>% 
  gather(key = nutrient, value = concentration, ca_mg, fe_mg, zn_mg, epa, dha) %>% 
  group_by(culture, genus_species, nutrient) %>% 
  filter(!is.na(concentration)) %>% 
  summarise_each(funs(mean), concentration) %>% 
  spread(key = nutrient, value = concentration) %>% View
  select(-cine_id) %>% 
  distinct(., .keep_all = TRUE)

  unique(trad_data1$genus_species)
  unique(trad_data$genus_species)
  
  
  cultures_keep <- trad_data %>% 
  group_by(culture) %>% 
  mutate(number_species = length(unique(latin_name))) %>% 
  distinct(number_species) %>% 
  filter(number_species >= 25) 


trad_data2 <- trad_data1 %>% 
  filter(culture %in% cultures_keep$culture) 


trad_split <- trad_data %>% 
  split(.$culture)


dataset <- trad_data %>% 
  filter(culture == "Abenaki") 

nutrient_fishing_function <- function(sample_size, dataset) {
  ntbl_sub1 <- dataset %>% 
    sample_n(size = sample_size, replace = FALSE)
  
  sample_list <- NULL
  for (i in 1:nrow(ntbl_sub1) ) {
    output <- combn(nrow(ntbl_sub1), i, FUN=function(x) ntbl_sub1[x,], simplify = FALSE)
    output <- bind_rows(output, .id = "sample_id")
    subsample_size <- rep(i, nrow(output))
    output <- cbind(output, subsample_size)
    sample_list <- rbind(sample_list,output)
  }
  
  sample_list <- split(sample_list, f = sample_list$subsample_size)
  
  new_data_sub1 <- sample_list %>% 
    map_df(`[`, .id = "replicate")
  
  resampling_15 <- new_data_sub1 %>% 
    dplyr::rename(species_number = subsample_size) %>%
    group_by(species_number, sample_id) %>% 
    mutate(cal_total = (calcium/species_number)) %>% ## get the amount of calcium each species will contribute
    mutate(zinc_total = (zinc/species_number)) %>% 
    mutate(iron_total = (iron/species_number)) %>% 
    mutate(epa_total = (epa/species_number)) %>%
    mutate(dha_total = (dha/species_number)) %>%
    summarise_each(funs(sum), contains("total")) %>% ## sum up all of each of the nutrients
    mutate(cal_grams = (cal_total/(1200))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
    mutate(iron_grams = (iron_total/(18))) %>%
    mutate(zinc_grams = (zinc_total/(11))) %>% 
    mutate(epa_grams = (epa_total/(1))) %>%
    mutate(dha_grams = (dha_total/(1))) %>%
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no, sample_id) %>% 
    select(-contains("total")) %>% 
    gather(key = nutrient, value = concentration, 3:7) %>% 
    group_by(species_no, sample_id) %>% 
    summarise(min_percentage = min(concentration)) %>% 
    mutate(grams_required = 100/min_percentage) 
}


samples_rep <- rep(10, 10)

#1
yupik_data <- trad_data %>% 
  filter(culture == "Yupik") 
output_yupik <- samples_rep %>% 
  map_df(nutrient_fishing_function, dataset = yupik_data, .id = "run") %>% 
  mutate(culture = "Yupik")
#2
bella_data <- trad_data %>% 
  filter(culture == "Bella Coola") 
output_bella <- samples_rep %>% 
  map_df(nutrient_fishing_function, dataset = bella_data, .id = "run") %>% 
  mutate(culture = "Bella Coola")
#3
central_salish_data <- trad_data %>% 
  filter(culture == "Central Salish") 
output_central_salish <- samples_rep %>% 
  map_df(nutrient_fishing_function, dataset = central_salish_data, .id = "run") %>% 
  mutate(culture = "Central Salish")
#4
abenaki_data <- trad_data %>% 
  filter(culture == "Abenaki") 
output_abenaki <- samples_rep %>% 
  map_df(nutrient_fishing_function, dataset = abenaki_data, .id = "run") %>% 
  mutate(culture = "Abenaki")
#5
inuit_data <- trad_data %>% 
  filter(culture == "Inuit-Inupiaq") 
output_inuit <- samples_rep %>% 
  map_df(nutrient_fishing_function, dataset = inuit_data, .id = "run") %>% 
  mutate(culture = "Inuit-Inupiaq")
#6
tsimshian_data <- trad_data %>% 
  filter(culture == "Tsimshian") 
output_tsimshian <- samples_rep %>% 
  map_df(nutrient_fishing_function, dataset = tsimshian_data, .id = "run") %>% 
  mutate(culture = "Tsimshian")
#7
cree_data <- trad_data %>% 
  filter(culture == "Cree") 
output_cree <- samples_rep %>% 
  map_df(nutrient_fishing_function, dataset = cree_data, .id = "run") %>% 
  mutate(culture = "Cree")

#8
haida_data <- trad_data %>% 
  filter(culture == "Haida") 
output_haida <- samples_rep %>% 
  map_df(nutrient_fishing_function, dataset = haida_data, .id = "run") %>% 
  mutate(culture = "Haida")

#9
tlingit_data <- trad_data %>% 
  filter(culture == "Tlingit") 
output_tlingit <- samples_rep %>% 
  map_df(nutrient_fishing_function, dataset = tlingit_data, .id = "run") %>% 
  mutate(culture = "Tlingit")

#10
kwakiutl_data <- trad_data %>% 
  filter(culture == "Kwakiutl") 
output_kwakiutl <- samples_rep %>% 
  map_df(nutrient_fishing_function, dataset = kwakiutl_data, .id = "run") %>% 
  mutate(culture = "Kwakiutl")

#11
wampanoag_data <- trad_data %>% 
  filter(culture == "Wampanoag") 
output_wampanoag <- samples_rep %>% 
  map_df(nutrient_fishing_function, dataset = wampanoag_data, .id = "run") %>% 
  mutate(culture = "Wampanoag")

#12
nootkan_data <- trad_data %>% 
  filter(culture == "Nootkan") 
output_nootkan <- samples_rep %>% 
  map_df(nutrient_fishing_function, dataset = nootkan_data, .id = "run") %>% 
  mutate(culture = "Nootkan")
#13 
montaganais_data <- trad_data %>% 
  filter(culture == "Montagnais-Naskapi") 
output_montaganais <- samples_rep %>% 
  map_df(nutrient_fishing_function, dataset = montaganais_data, .id = "run") %>% 
  mutate(culture = "Montagnais-Naskapi")

#14 
micmac_data <- trad_data %>% 
  filter(culture == "Micmac") 
output_micmac <- samples_rep %>% 
  map_df(nutrient_fishing_function, dataset = micmac_data, .id = "run") %>% 
  mutate(culture = "Micmac")


all_trad_reps <- bind_rows(output_yupik, output_bella, output_central_salish, 
                          output_haida, output_inuit, output_abenaki,
                            output_micmac, output_wampanoag,
                          output_tsimshian, output_tlingit, output_kwakiutl, output_cree, output_nootkan, output_montaganais)

# write_csv(all_trad_reps, "data-processed/all_trad_reps.csv")
write_csv(all_trad_reps, "data-processed/all_trad_reps-aug2020.csv") ### update new trad data aug 2020


