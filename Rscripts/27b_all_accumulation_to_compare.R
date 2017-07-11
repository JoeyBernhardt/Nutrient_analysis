library(purrr)
library(dplyr)
library(tidyr)
library(readr)
library(vegan)
library(stringr)
library(ggplot2)

trait_data <- read_csv("data-processed/n.long_lat3.csv")

accumulateq <- function(data, threshold) {
  ntbl.RDI.all <- data %>% 
    filter(!grepl("^Mohanty", ref_info)) %>% 
    spread(nutrient, concentration) %>% 
    group_by(species_name, subgroup) %>% 
    summarise(mean.CA = mean(ca_mg, na.rm = TRUE),
              mean.ZN = mean(zn_mg, na.rm = TRUE), 
              mean.FE = mean(fe_mg, na.rm = TRUE)) %>% 
    mutate(RDI.CA = ifelse(mean.CA > (1200*threshold), 1, 0)) %>% 
    mutate(RDI.FE = ifelse(mean.FE > (18*threshold), 1, 0)) %>% 
    mutate(RDI.ZN = ifelse(mean.ZN > (11*threshold), 1, 0)) %>%
    ungroup() %>%
    mutate(RDI.micro.tot = rowSums(.[6:8])) %>% 
    filter(!is.na(RDI.micro.tot))
  
  all_spa <- ntbl.RDI.all %>%
    dplyr::select(-RDI.micro.tot) %>%
    dplyr::select(-contains("mean")) %>% 
    dplyr::select(-species_name) %>%
    mutate(subgroup = "all") %>% 
    split( .$subgroup) %>% 
    map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN")) %>%
    map(.f = specaccum, method = "random", permutations = 100000)
  
  
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


finfish_accumulation_25 <- trait_data %>% 
  accumulateq(., threshold = 0.1) %>% 
  mutate(subgroup = "finfish") %>% 
  mutate(threshold = "10 percent")

finfish_accumulation_25 %>% 
  filter(number_of_species < 11) %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets)) + geom_line()
