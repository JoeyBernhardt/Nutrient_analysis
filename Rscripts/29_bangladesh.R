

library(tidyverse)
library(janitor)
library(vegan)
library(purrr)
library(stringr)
library(readr)
library(tidyr)
library(dplyr)

### June 6 next steps: get the other RDIs for the bangladesh micronutrients, (eg vitamins!)

## b12 is 2.4ug, vitamin A is 900ug, vitamin E is 1000mg

table3 <- read_csv("bogard-data/bogard_table3.csv")
table4 <- read_csv("bogard-data/bogard_table4.csv")
table6 <- read_csv("bogard-data/bogard_table6.csv")
table7 <- read_csv("bogard-data/bogard_table7.csv")


table3 <- table3 %>% 
  clean_names()

table4 <- table4 %>% 
  clean_names()

table6 <- table6 %>% 
  clean_names()

table7 <- table7 %>% 
  clean_names()


length(intersect(unique(table3$species_name), unique(table4$species_name)))


nutrients <- left_join(table3, table4)

nuts <- left_join(nutrients, table6)

write.csv(nuts, "data-processed/bangladesh-micronutrients.csv")

bang_data <- read.csv("data-processed/bangladesh-micronutrients.csv")

names(nuts)
threshold <- 0.1


accumulate <- function(data, threshold) {
  ntbl.RDI.all <- data %>% 
    mutate(RDI.CA = ifelse(calcium > (1200*threshold), 1, 0)) %>% 
    mutate(RDI.FE = ifelse(iron > (18*threshold), 1, 0)) %>% 
    mutate(RDI.ZN = ifelse(zinc > (11*threshold), 1, 0)) %>%
    mutate(RDI.VITE = ifelse(vitamin_e_α_tocopherol > (1000*threshold), 1, 0)) %>%
    mutate(RDI.VITA = ifelse(total_vitamin_a > (900*threshold), 1, 0)) %>%
    mutate(RDI.VITB = ifelse(vitamin_b12 > (2.4*threshold), 1, 0)) %>%
    ungroup() %>% 
    mutate(RDI.micro.tot = rowSums(.[31:36])) %>% 
    filter(!is.na(RDI.micro.tot)) 
  
  all_spa <- ntbl.RDI.all %>%
    dplyr::select(-RDI.micro.tot) %>%
    dplyr::select(-contains("mean")) %>% 
    dplyr::select(-species_name) %>%
    mutate(subgroup = "all") %>% 
    split( .$subgroup) %>% 
    map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.VITE", "RDI.VITA", "RDI.VITB")) %>%
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


accumulation <- nuts %>% 
  filter(type %in% c("SIS", "large")) %>% 
  accumulate(., threshold = 0.1) %>% 
  mutate(subgroup = "wild") %>% 
  mutate(threshold = "10 percent")


finfish_accumulation_25 <- finfish_accumulation_25 %>% 
  mutate(subgroup = str_replace(subgroup, "finfish", "full dataset"))

accumulation <- accumulation %>% 
  mutate(subgroup = str_replace(subgroup, "wild", "Bangladesh - wild caught freshwater species"))

all <- bind_rows(accumulation, finfish_accumulation_25)



accumulation %>% 
  filter(number_of_species < 11) %>% 
  group_by(subgroup) %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets, group = subgroup, color = subgroup)) + geom_line(size =1.5) +
  geom_ribbon(aes(ymin = number_of_targets - se, ymax = number_of_targets + se, color = subgroup, group = subgroup), alpha = 0.2, size = 0) +
  ylab("number of nutrient requirements fulfilled (10% DRI)") +
  xlab("number of species") + theme(text = element_text(size=14)) + 
  scale_color_grey(start = 0.01, end = 0.7) +
  theme_bw() +
  theme(legend.position = c(0.6, 0.2)) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  theme(legend.title=element_blank())



### now let's look at the correlation

