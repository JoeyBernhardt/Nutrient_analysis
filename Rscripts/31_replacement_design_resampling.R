

library(purrr)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(broom)
library(plotrix)

trait_data <- read_csv("data-processed/n.long_lat3.csv")



mean_nuts <- trait_data %>% 
  filter(!grepl("^Mohanty", ref_info)) %>% 
  filter(seanuts_id2 != 1737) %>% #### doing this because the units here were reported wrong (i.e. epa and dha should were in mg /100g when they should have been g / 100g)
  # filter(species_name != "Carcinus maenas") %>% 
  spread(nutrient, concentration) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(calcium = mean(ca_mg, na.rm = TRUE),
            zinc = mean(zn_mg, na.rm = TRUE), 
            iron = mean(fe_mg, na.rm = TRUE),
            epa = mean(epa, na.rm = TRUE),
            dha = mean(dha, na.rm = TRUE)) %>% 
  filter(!is.na(calcium), !is.na(zinc), !is.na(iron), !is.na(epa), !is.na(dha)) %>% 
  ungroup()


str(mean_nuts)
write_csv(mean_nuts, "data-processed/mean_nuts.csv") #### updating this May 29 2020 to account for issues with reporting in seanuts_id2 = 1737

max(mean_nuts$epa)



scaled <- mean_nuts %>% 
  mutate(calcium = calcium/1300,
         zinc = zinc/169,
         iron = iron/48, 
         epa = epa/154,
         dha = dha/115)


sample_size <- 10
threshold <- 5

# scale by the maximum
# total nutrients vary across species
# increase the threshold

nutrient_fishing_function <- function(sample_size) {
  ntbl_sub1 <- mean_nuts %>% 
    filter(subgroup != "mollusc") %>% 
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



samples_rep <- rep(12, 100)

threshold <- 1
output_100 <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run")

threshold <- 0.1
output_10 <-  samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run")

no_moll_10 <-  samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run") 

no_moll_10 <- no_moll_10 %>% 
  mutate(groups = "no_molluscs")
output_10 <- output_10 %>% 
  mutate(groups = "all")

all <- bind_rows(no_moll_10, output_10)

all_sum <- all %>% 
  group_by(species_no, groups) %>% 
  summarise_each(funs(mean, median, std.error, max), rdi_micro_tot) 
all_sum %>% 
  ggplot(aes(x = species_no, y = mean, color = groups)) + geom_line() +
  # geom_line(aes(x = species_no, y = max, color = groups), data = all_sum) +
  geom_ribbon(aes(ymin = mean - (std.error*1.96), ymax = mean + (std.error* 1.96), fill = groups), alpha = 0.5) +
  # ylim(1, 5) + 
  xlim(0, 10) +
  theme_bw()
  
  


threshold <- 0.25
output_25 <-  samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run")

threshold <- 0.50
output_50 <-  samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run")

threshold <- 0.7
output_70 <-  samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run")

threshold <- 0.6
output_60 <-  samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run")

threshold <- 0.9
output_90 <-  samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run")

threshold <- 5
output_500 <-  samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run")

threshold <- 15
output_1500 <-  samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run")

threshold <- 0.01
output_1 <-  samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run")


all_outputs <- bind_rows(output_10, output_25, output_50, output_60, output_70, output_90, output_100, output_500, output_1500, output_1)


all_outputs %>% 
  mutate(threshold_level = threshold_level * 100) %>% 
  ggplot(aes(x = species_no, y = rdi_micro_tot)) + geom_smooth(method = "lm") + 
  facet_wrap( ~ threshold_level) +
  theme_bw() + xlab("species richness") + ylab("number of DRI targets")

all_outputs %>% 
  group_by(threshold_level, species_no) %>%
  summarise_each(funs(mean), rdi_micro_tot)
  
  
all_outputs %>% 
  mutate(threshold_level = threshold_level * 100) %>% 
  group_by(threshold_level) %>% 
  do(tidy(lm(rdi_micro_tot ~ species_no, data = .), conf.int = TRUE)) %>%
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = threshold_level, y = estimate)) + geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + theme_bw() + geom_hline(yintercept = 0) + ylab("slope") + xlab("threshold level (percentage of DRI)")

output_10 %>% 
  ungroup() %>% 
  do(tidy(lm(rdi_micro_tot ~ species_no, data = .), conf.int = TRUE)) 

output_70 %>% 
  ungroup() %>% 
  ggplot(aes(x = species_no, y = rdi_micro_tot)) + geom_point() + geom_smooth(method = "lm") + theme_bw()

output_70 %>% 
  ungroup() %>% 
  do(tidy(lm(rdi_micro_tot ~ species_no, data = .), conf.int = TRUE)) %>% View 
  summary
  
  
  ### now with scaled
  
  threshold <- 0.1
  scaled_10 <-  samples_rep %>% 
    map_df(nutrient_fishing_function, .id = "run")
  
  threshold <- 0.15
  scaled_15 <-  samples_rep %>% 
    map_df(nutrient_fishing_function, .id = "run")
  
   threshold <- 0.25
  scaled_25 <-  samples_rep %>% 
    map_df(nutrient_fishing_function, .id = "run")
  
  threshold <- 0.30
  scaled_30 <-  samples_rep %>% 
    map_df(nutrient_fishing_function, .id = "run")
  
  threshold <- 0.35
  scaled_35 <-  samples_rep %>% 
    map_df(nutrient_fishing_function, .id = "run")
  
  threshold <- 0.40
  scaled_40 <-  samples_rep %>% 
    map_df(nutrient_fishing_function, .id = "run")
  threshold <- 0.50
  scaled_50 <-  samples_rep %>% 
    map_df(nutrient_fishing_function, .id = "run")
  
  threshold <- 0.7
  scaled_70 <-  samples_rep %>% 
    map_df(nutrient_fishing_function, .id = "run")
  
  threshold <- 0.6
  scaled_60 <-  samples_rep %>% 
    map_df(nutrient_fishing_function, .id = "run")
  
  threshold <- 0.9
  scaled_90 <-  samples_rep %>% 
    map_df(nutrient_fishing_function, .id = "run")
  
  all_scaled <- bind_rows(scaled_10, scaled_25, scaled_15, scaled_30, scaled_35, scaled_40, scaled_50, scaled_60, scaled_70, scaled_90)
  
  
  all_scaled %>% 
    mutate(threshold_level = threshold_level * 100) %>% 
    ggplot(aes(x = species_no, y = rdi_micro_tot)) + geom_smooth(method = "lm") + 
    facet_wrap( ~ threshold_level) +
    theme_bw() + xlab("species richness") + ylab("number of DRI targets")
  
  all_scaled %>% 
    group_by(threshold_level, species_no) %>%
    summarise_each(funs(mean), rdi_micro_tot)
  
  
  all_scaled %>% 
    mutate(threshold_level = threshold_level * 100) %>% 
    group_by(threshold_level) %>% 
    do(tidy(lm(rdi_micro_tot ~ species_no, data = .), conf.int = TRUE)) %>%
    filter(term != "(Intercept)") %>% 
    ggplot(aes(x = threshold_level, y = estimate)) + geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.7) + theme_bw() + geom_hline(yintercept = 0) + ylab("slope") + xlab("threshold level (percentage of DRI)")
  