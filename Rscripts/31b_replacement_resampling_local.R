
library(plotrix)

### RDI accumulation for the global and local scales, with replacement design
trad_nuts_mean_raw <- read_csv("data-processed/trad-foods-mean.csv")
species_numbers <- read_csv("data-processed/species_numbers.csv")

trad_nuts_mean <- trad_nuts_mean_raw %>% 
  filter(culture %in% species_numbers$culture)

nutrient_fishing_function <- function(sample_size, culture_name) {
  ntbl_sub1 <- trad_nuts_mean %>% 
    filter(culture == culture_name) %>% 
    dplyr:: sample_n(size = sample_size, replace = FALSE)
  
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
    summarise_each(funs(sum), contains("total")) %>%   ## sum up all of each of the nutrients, to get total amount of nutrient per sample
    mutate(cal_grams = (cal_total/(1200*threshold))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
    mutate(iron_grams = (iron_total/(18*threshold))) %>%
    mutate(zinc_grams = (zinc_total/(11*threshold))) %>% 
    mutate(epa_grams = (epa_total/(1*threshold))) %>%
    mutate(dha_grams = (dha_total/(1*threshold))) %>%
    mutate(rdi_calcium = ifelse(cal_grams >= 1, 1, 0)) %>% ## if the amount of calcium exceeds 1 (i.e. reaches threshold), give it a value of 1, else 0
    mutate(rdi_iron = ifelse(iron_grams >= 1, 1, 0)) %>% 
    mutate(rdi_zinc = ifelse(zinc_grams >= 1, 1, 0)) %>% 
    mutate(rdi_epa = ifelse(epa_grams >= 1, 1, 0)) %>%
    mutate(rdi_dha = ifelse(dha_grams >= 1, 1, 0)) %>% 
    ungroup() %>% 
    mutate(rdi_micro_tot = rowSums(.[13:17])) %>%  ## add up all the targets reached in one sample
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no, sample_id) %>% 
    select(-contains("total")) %>% 
    mutate(threshold_level = threshold)
}


samples_rep <- rep(10, 100)

threshold <- 0.1
Abenaki_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Abenaki", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

Abenaki_10$dataset <- "AB"

BellaCoola_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Bella Coola", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

BellaCoola_10$dataset <- "BC"

CentralSalish_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Central Salish", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

CentralSalish_10$dataset <- "CS"

Cree_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Cree", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

Cree_10$dataset <- "CR"

Haida_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Haida", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

Haida_10$dataset <- "HA"

Inuit_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Inuit-Inupiaq", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

Inuit_10$dataset <- "II"

Kwakiutl_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Kwakiutl", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

Kwakiutl_10$dataset <- "KW"

Micmac_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Micmac", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

Micmac_10$dataset <- "MI"

Montagnais_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Montagnais-Naskapi", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

Montagnais_10$dataset <- "MN"

Nootkan_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Nootkan", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

Nootkan_10$dataset <- "NO"

Tlingit_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Tlingit", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

Tlingit_10$dataset <- "TL"

Tsimshian_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Tsimshian", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

Tsimshian_10$dataset <- "TS"

Wampanoag_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Wampanoag", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

Wampanoag_10$dataset <- "WA"

Yupik_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Yupik", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

Yupik_10$dataset <- "YU"


all_trad_accum <- bind_rows(Abenaki_10, Yupik_10, Wampanoag_10, Tlingit_10, Tsimshian_10,
                            BellaCoola_10, Haida_10, Nootkan_10, Montagnais_10, Micmac_10, 
                            Kwakiutl_10, Inuit_10, Cree_10, CentralSalish_10)

write_csv(all_trad_accum, "data-processed/trad_accumulation_replacement.csv")


all_trad_accum %>% 
  ggplot(aes(x = species_no, y = rdi_micro_tot_mean, group = run)) + 
  geom_line() + 
facet_wrap( ~ dataset)


all_trad_accum %>% 
  group_by(dataset, species_no) %>% 
  summarise(mean_rdi_count = mean(rdi_micro_tot_mean)) %>% 
  ggplot(aes(x = species_no, y = mean_rdi_count, color = dataset)) + geom_line(size = 2)
