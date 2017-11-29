library(tidyverse)
library(purrr)
library(here)
library(stringr)
library(cowplot)
library(broom)

new_global <- read_csv(here("data-processed", "new_global.csv"))




accumulate_global <- function(data, threshold) {
  ntbl.RDI.all <- data %>% 
    mutate(RDI.CA = ifelse(calcium > (1200*threshold), 1, 0)) %>% ## here we create a matrix of 0s and 1s, corresponding to whether the sample reaches DRI or not
    mutate(RDI.FE = ifelse(iron > (18*threshold), 1, 0)) %>% 
    mutate(RDI.ZN = ifelse(zinc > (11*threshold), 1, 0)) %>%
    mutate(RDI.EPA = ifelse(epa > (1*threshold), 1, 0)) %>%
    mutate(RDI.DHA = ifelse(dha > (1*threshold), 1, 0)) %>%
    ungroup() %>% 
    mutate(RDI.micro.tot = rowSums(.[7:11])) %>% 
    filter(!is.na(RDI.micro.tot)) 
  
  all_spa <- ntbl.RDI.all %>%
    dplyr::select(species_name, 7:12) %>% 
    mutate(subgroup = "all") %>% 
    split( .$subgroup) %>% 
    map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.EPA", "RDI.DHA")) %>%
    map(.f = specaccum, method = "random", permutations = 1000)
  
  
  accumulated_targets <- all_spa %>% 
    map(.f = `[`, "richness") %>% 
    unlist() %>% 
    as.data.frame()
  
  accumulated_targets_sd <- all_spa %>% 
    map(.f = `[`, "sd") %>% 
    unlist() %>% 
    as.data.frame()
  
  accumulated_targets$richness_level = rownames(accumulated_targets)
  colnames(accumulated_targets) <- c("number_of_targets", "richness_level")
  
  accumulated_targets_sd$sd = rownames(accumulated_targets_sd)
  colnames(accumulated_targets_sd) <- c("sd", "number_of_targets")
  
  accumulated_targets_sd <- accumulated_targets_sd %>% 
    separate(number_of_targets, into = c("subgroup", "number_of_species")) %>%
    mutate(number_of_species = str_replace(number_of_species, "sd", "")) %>%
    mutate(number_of_species = as.numeric(number_of_species))
  
  
  accumulated_targets <- accumulated_targets %>% 
    separate(richness_level, into = c("subgroup", "number_of_species")) %>%
    mutate(number_of_species = str_replace(number_of_species, "richness", "")) %>%
    mutate(number_of_species = as.numeric(number_of_species))
  
  accumulated_targets_all <- left_join(accumulated_targets, accumulated_targets_sd)
  accumulated_targets_all <- accumulated_targets_all %>% 
    mutate(se = sd / sqrt(number_of_species))
  return(accumulated_targets_all)
  
}



rep_acc_function <- function(x) accumulate_global(sample_n(new_global, size = x, replace = FALSE), threshold = 0.1)


samples <- rep(40, 1000)


repeat_global <- samples %>% 
  map_df(rep_acc_function, .id = "run")


mean_target <- repeat_global %>% 
  group_by(number_of_species) %>% 
  summarise_each(funs(mean), number_of_targets) %>% 
  filter(number_of_species < 11) 


sub <- repeat_global %>%
  filter(number_of_species < 11) 

sub %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets)) + geom_point(alpha = 0.1)
  
  
  mean_target %>% 
 ggplot(aes(x = number_of_species, y = number_of_targets), color = "red") + geom_line() +
  geom_line(aes(x = number_of_species, y = number_of_targets, group = run), data = sub, alpha = 0.2) +
  geom_line(aes(x = number_of_species, y = number_of_targets), data = mean_target, color = "cadetblue", size = 1) +
    ylim(0, 5)
  




write_csv(repeat_global, "data-processed/global_accumulation_null.csv")
