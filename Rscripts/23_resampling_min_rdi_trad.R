

trad_data <- read_csv("data-processed/trad-foods-mean.csv")


trad_split <- trad_data %>% 
  split(.$culture)

trad_split[[2]] %>% View

dataset <- trad_data %>% 
  filter(culture == "Yupik") 

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


samples_rep <- rep(10, 100)

yupik_data <- trad_data %>% 
  filter(culture == "Yupik") 
output_yupik <- samples_rep %>% 
  map_df(nutrient_fishing_function, dataset = yupik_data, .id = "run")
output_yupik <- output_yupik %>% 
  mutate(culture = "Yupik")

bella_data <- trad_data %>% 
  filter(culture == "Bella Coola") 
output_bella <- samples_rep %>% 
  map_df(nutrient_fishing_function, dataset = bella_data, .id = "run")
output_bella <- output_bella %>% 
  mutate(culture = "Bella Coola")

central_salish_data <- trad_data %>% 
  filter(culture == "Central Salish") 
output_central_salish <- samples_rep %>% 
  map_df(nutrient_fishing_function, dataset = central_salish_data, .id = "run") %>% 
  mutate(culture = "Central Salish")

interior_salish_data <- trad_data %>% 
  filter(culture == "Interior Salish") 
output_interior_salish <- samples_rep %>% 
  map_df(nutrient_fishing_function, dataset = interior_salish_data, .id = "run") %>% 
  mutate(culture = "Interior Salish")

inuit_data <- trad_data %>% 
  filter(culture == "Inuit-Inupiaq") 
output_inuit <- samples_rep %>% 
  map_df(nutrient_fishing_function, dataset = inuit_data, .id = "run") %>% 
  mutate(culture = "Inuit-Inupiaq")

tsimshian_data <- trad_data %>% 
  filter(culture == "Tsimshian") 
output_tsimshian <- samples_rep %>% 
  map_df(nutrient_fishing_function, dataset = tsimshian_data, .id = "run") %>% 
  mutate(culture = "Tsimshian")


all_trad_reps <- bind_rows(output_yupik, output_bella, output_interior_salish, output_central_salish, output_inuit, output_tsimshian)

write_csv(all_trad_reps, "data-processed/all_trad_reps.csv")


