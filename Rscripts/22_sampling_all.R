library(tidyverse)
library(purrr)


trait_data <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data-processed/n.long_lat3.csv")

trait_data_pro <- trait_data %>% 
  filter(!grepl("^Mohanty", ref_info)) %>% 
  spread(nutrient, concentration) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(mean.CA = mean(ca_mg, na.rm = TRUE),
            mean.ZN = mean(zn_mg, na.rm = TRUE), 
            mean.FE = mean(fe_mg, na.rm = TRUE),
            mean.EPA = mean(epa, na.rm = TRUE),
            mean.DHA = mean(dha, na.rm = TRUE)) %>% 
  filter(!is.na(mean.CA)) %>% 
  filter(!is.na(mean.EPA)) %>% 
  filter(!is.na(mean.DHA)) %>%
  filter(!is.na(mean.FE)) %>% 
  filter(!is.na(mean.ZN)) %>%
  rename(calcium = mean.CA,
         epa = mean.EPA,
         dha = mean.DHA,
         zinc = mean.ZN,
         iron = mean.FE) %>% 
  ungroup() 

write_csv(trait_data_pro, "data-processed/micronutrients-species-mean.csv")

# start with subsamples ---------------------------------------------------

ntbl_sub1 <- trait_data_pro %>% 
  sample_n(size = 10, replace = FALSE)



sample_list <- NULL
to <- nrow(ntbl_sub1)
for (i in from:to ) {
  output <- combn(nrow(ntbl_sub1), i, FUN=function(x) ntbl_sub1[x,], simplify = FALSE)
  output <- bind_rows(output, .id = "sample_id")
  subsample_size <- rep(i, nrow(output))
  output <- cbind(output, subsample_size)
  sample_list <- rbind(sample_list,output)
}


sample_list <- split(sample_list, f = sample_list$subsample_size)


new_data_sub1 <- sample_list %>% 
  map_df(`[`)

### now with 15 of total dataset

resampling_15 <- new_data_sub1 %>% 
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


summary_15 <- resampling_15 %>%
  group_by(species_no) %>% 
  summarise_each(funs(mean, min, max, median), grams_required)


resampling_15  %>% 
  ggplot(aes(x = species_no, y = grams_required)) + geom_point(size = 2, alpha = 0.5) +
  geom_line(aes(x = species_no, y = median), color = "blue", size = 2, data = summary_15) +
  scale_y_log10() + 
  theme_bw() +
  ylab("grams required to meet 5 RDI targets") + xlab("species richness")


summary_15 %>% 
  ggplot() +
geom_line(aes(x = species_no, y = median), color = "blue", size = 2, data = summary_15) +
  theme_bw() +
  ylab("grams required to meet 5 RDI targets") + xlab("species richness")

### OK so, now we want to repeat this 1000 times







### compare this with the mollusc data set
## this was with starting with ntbl, where just molluscs were filtered out
# write_csv(new_data2, "data-processed/mollusc_sampling_all.csv")
# 
# new_data2 <- sample_list %>% 
#   map_df(`[`)

mollusc <- read_csv("data-processed/mollusc_sampling_all.csv")


 mollusc_resampling <- mollusc %>%
   rename(species_number = subsample_size) %>%
   group_by(species_number, sample_id) %>%
   mutate(cal_total = (calcium/species_number)) %>%  
   mutate(iron_total = (iron/species_number)) %>%
   mutate(zinc_total = (zinc/species_number)) %>%
   mutate(epa_total = (epa/species_number)) %>%
   mutate(dha_total = (dha/species_number)) %>%
   summarise_each(funs(sum), contains("total")) %>% #sum up all of each of the nutrients
   mutate(cal_grams = (cal_total/(1200))) %>%  #divide that total by the RDI, and into 100 to find out the number of grams required to reach target
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

 mollusc_summary <- mollusc_resampling %>%
   group_by(species_no) %>%
   summarise_each(funs(mean, min, max, median), grams_required)

 mollusc_resampling %>%
   ggplot(aes(x = species_no, y = grams_required)) + geom_point(size = 2, alpha = 0.5) +
   geom_line(aes(x = species_no, y = mean), color = "blue", size = 2, data = mollusc_summary) +
   theme_bw() +
   ylab("grams required to meet 5 RDI targets") + xlab("species richness")


mollusc_summary %>%
   ggplot(aes(x = species_no, y = median)) + geom_line() +
   theme_bw() + ylab("grams required to meet 5 RDI targets") + xlab("species richness")
