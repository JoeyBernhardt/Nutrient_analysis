

### making plots with resampling data
library(tidyverse)


reps100 <- read_csv("data-processed/grams-required-15-spp-1000reps.csv")
molluscs <- read_csv("data-processed/mollusc_sampling_all.csv")

summary <- reps100 %>%
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median), grams_required, grams_for_25_percent)

mollusc_processed <- molluscs %>% 
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

mollusc_summary <- mollusc_processed %>%
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median), grams_required, grams_for_25_percent)


reps100 %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  ggplot(aes(x = species_no, y = grams_for_25_percent, group = species_no)) + geom_violin(fill = "grey") +
  geom_point(aes(x = species_no, y = grams_for_25_percent_median), data = summary) +
  geom_point(aes(x = species_no, y = grams_for_25_percent_median), data = mollusc_summary, color = "red") +
  geom_hline(yintercept = 100) +
  scale_y_log10() +
  theme_bw() + xlab("species richness") + ylab("grams required to reach 5 RDI targets (10% RDI)")
ggsave("figures/violin_grams_req_with_moll_in_red.pdf")
