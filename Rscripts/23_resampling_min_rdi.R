library(tidyverse)
library(purrr)
library(dplyr)
library(tidyr)
library(readr)

mean_nuts <- read.csv("data-processed/mean_nuts.csv")
trait_data_pro <- read.csv("data-processed/micronutrients-species-mean.csv")
most_common <- read_csv("data-processed/most_common_species.csv")
bang_data <- read.csv("data-processed/bangladesh-micronutrients.csv")

inuit_mean <- read_csv("data-processed/CINE-inuit-mean-nutrients.csv")
inuit <- inuit_mean %>% 
  dplyr::rename(species_name = latin_name)

mc <- most_common %>%
  filter(rowname < 57) %>% 
  select(species_name, subgroup, contains("mean")) %>% 
  rename(calcium = mean.CA, 
         zinc = mean.ZN, 
         iron = mean.FE, 
         epa = mean.EPA, 
         dha = mean.DHA)

trait_data_pro_no_moll <- trait_data_pro %>% 
  filter(subgroup == "finfish")


nutrient_fishing_function <- function(sample_size) {
ntbl_sub1 <- mc %>% 
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


samples_rep <- rep(10, 1000)

output_mc <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run")

write_csv(output, "data-processed/grams-required-10-spp-100reps-inuit.csv")
write.csv(output, "data-processed/grams-required-10-spp-1000reps.csv")
write_csv(output, "data-processed/grams-required-15-spp-1000reps.csv")
write_csv(output, "data-processed/grams-required-10-spp-1000reps-most-common.csv")
write_csv(output_mc, "data-processed/grams-required-10-spp-1000reps-10most-common.csv")

summaries <- output %>%
  group_by(species_no) %>% 
  summarise_each(funs(mean, min, max, median), grams_required)

output %>% 
  ggplot(aes(x = species_no, y = grams_required)) + geom_point(size = 2, alpha = 0.1, color = "black", stroke = 0.1) +
  geom_line(aes(x = species_no, y = min), color = "grey", size = 1, data = summaries) +
  geom_line(aes(x = species_no, y = median), color = "gray", size = 2, data = summaries) +
  geom_line(aes(x = species_no, y = max), color = "gray", size = 1, data = summaries) +
  theme_bw() +
  scale_y_log10() +
  ylab("grams required to meet 5 RDI targets") + xlab("species richness")
ggsave("figures/grams_required_15spp_1000reps.pdf")
ggsave("figures/grams_required_10spp_1000reps.png")

summaries  %>% 
  ggplot() +
  geom_line(aes(x = species_no, y = mean), color = "blue", size = 2, data = summaries) +
  theme_bw() +
  ylab("grams required to meet 5 RDI targets") + xlab("species richness")

mollusc <- read_csv("data-processed/mollusc_sampling_all.csv")


mollusc_processed <- mollusc %>% 
  rename(species_number = subsample_size) %>%
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
  rename(species_no = species_number) %>% 
  group_by(species_no, sample_id) %>% 
  select(-contains("total")) %>% 
  gather(key = nutrient, value = concentration, 3:7) %>% 
  group_by(species_no, sample_id) %>% 
  summarise(min_percentage = min(concentration)) %>% 
  mutate(grams_required = 100/min_percentage) 

summaries_mollusc <- mollusc_processed %>%
  group_by(species_no) %>% 
  summarise_each(funs(mean, min, max, median), grams_required)

summaries_mollusc  %>% 
  ggplot() +
  geom_line(aes(x = species_no, y = median), color = "green", size = 2, data = summaries_mollusc) +
  theme_bw() +
  ylab("grams required to meet 5 RDI targets") + xlab("species richness")

### now for the bangladesh data

sample_size <- 10
bang_data_complete <- bang_data %>% 
  filter(!is.na(total_vitamin_a)) %>% 
  filter(type == "SIS") %>% 
  filter(species_name != "Mola (cultured)")

bang_fishing_function <- function(sample_size) {
  ntbl_sub1 <- bang_data_complete %>% 
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
    mutate(total_vitamin_a_total = (total_vitamin_a/species_number)) %>%
    mutate(vitamin_b12_total = (vitamin_b12/species_number)) %>%
    summarise_each(funs(sum), contains("total")) %>% ## sum up all of each of the nutrients
    mutate(cal_grams = (cal_total/(1200))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
    mutate(iron_grams = (iron_total/(18))) %>%
    mutate(zinc_grams = (zinc_total/(11))) %>% 
    mutate(vitamin_a_grams = (total_vitamin_a_total/(900))) %>% 
    mutate(vitamin_b12_grams = (vitamin_b12_total/(2.4))) %>% 
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no, sample_id) %>% 
    select(-contains("total")) %>% 
    gather(key = nutrient, value = concentration, 3:7) %>% 
    group_by(species_no, sample_id) %>% 
    summarise(min_percentage = min(concentration)) %>% 
    mutate(grams_required = 100/min_percentage) 
}


samples_rep <- rep(10, 100)
output_bangladesh <- samples_rep %>% 
  map_df(bang_fishing_function, .id = "run")

write_csv(output_bangladesh, "data-processed/grams-required-10-spp-1000reps-bangladesh.csv")
