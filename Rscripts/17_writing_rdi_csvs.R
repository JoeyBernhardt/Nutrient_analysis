
library(stringr)
library(purrr)
library(tidyverse)
library(broom)
library(vegan)


trait_data <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data-processed/n.long_lat3.csv")



function1<- function(percentage, trait_data){
  ntbl.RDI <- trait_data %>% 
  spread(nutrient, concentration) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(mean.CA = mean(ca_mg, na.rm = TRUE),
            mean.ZN = mean(zn_mg, na.rm = TRUE), 
            mean.FE = mean(fe_mg, na.rm = TRUE),
            mean.EPA = mean(epa, na.rm = TRUE),
            mean.DHA = mean(dha, na.rm = TRUE)) %>% 
  mutate(RDI.CA = ifelse(mean.CA > (1200*percentage), 1, 0)) %>% 
  mutate(RDI.FE = ifelse(mean.FE > (18*percentage), 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(mean.ZN > (11*percentage), 1, 0)) %>%
  mutate(RDI.EPA = ifelse(mean.EPA > (1*percentage), 1, 0)) %>% 
  mutate(RDI.DHA = ifelse(mean.DHA > (1*percentage), 1, 0)) %>% 
  ungroup() %>% 
  mutate(RDI.micro.tot = rowSums(.[8:12])) %>% 
  filter(!is.na(RDI.micro.tot)) 
  
  #save file
  write_csv(ntbl.RDI, paste0("./data-processed-rdi/rdi_", percentage, ".csv") )
  
}


function2 <- function(rdi_data, datafilename){
  
all_spa <- rdi_data %>%
  dplyr::select(-RDI.micro.tot) %>%
  dplyr::select(-contains("mean")) %>% 
  dplyr::select(-species_name) %>%
  mutate(subgroup = "all") %>% 
  split( .$subgroup) %>% 
  map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.EPA", "RDI.DHA")) %>%
  map(.f = specaccum, method = "random")

accumulated_targets_full <- all_spa %>% 
  map(.f = `[`, "richness") %>% 
  unlist() %>% 
  as.data.frame()

accumulated_targets_full$richness_level = rownames(accumulated_targets_full)
colnames(accumulated_targets_full) <- c("number_of_targets", "richness_level")

accumulated_targets_full <- accumulated_targets_full %>% 
  separate(richness_level, into = c("subgroup", "number_of_species")) %>%
  mutate(number_of_species = str_replace(number_of_species, "richness", "")) %>%
  mutate(number_of_species = as.numeric(number_of_species)) %>% 
  mutate(threshold = i) %>% 
  dplyr::select(-subgroup)

write_csv(accumulated_targets_full, paste0("./data-processed-targets2/rdi_targets_", datafilename) )

}

##### Run these to get all the files
for(i in seq(0,1, 0.01)){
  
  function1(i, trait_data)
  print(i)
}

rdi_files <- list.files("./data-processed-rdi")
rdi_files

for (i in 1:length(rdi_files)){
  setwd("./data-processed-rdi/")
  data_round2 <- read_csv(rdi_files[i])
  setwd("../")
  function2(data_round2, rdi_files[i])
  
}


target_files <- list.files("./data-processed-targets2", full.names = TRUE)


names(target_files) <- target_files %>% 
  gsub(pattern = ".csv$", replacement = "")


#### Step 3: read in all the files!

all_targets <- map_df(target_files, read_csv, .id = "file_name")


targets <- all_targets %>% 
  mutate(file_name = str_replace(file_name, "./data-processed-targets2/rdi_targets_rdi_", "")) %>%
  mutate(file_name = as.numeric(file_name)) %>% 
  rename(threshold1 = file_name)

write_csv(targets, "data-processed/targets_richness.csv")

targets <- read_csv("data-processed/targets_richness.csv")

targets %>%
  filter(number_of_species < 30) %>% 
  mutate(threshold1 = threshold1*100) %>% 
  group_by(threshold1) %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets, group = threshold1, color = threshold1)) + geom_line() +
  theme_bw() +
  scale_color_gradient(name="Percent of DRI", low="blue", high="red") + xlab("species richness") + ylab("number of functions (distinct DRI targets reached)")
ggsave("figures/nutrient_accumulation_by_threshold.pdf")

targets %>% 
  filter(number_of_species < 15) %>% 
  group_by(threshold1) %>% 
  do(tidy(lm(log(number_of_targets) ~ log(number_of_species), data =.), conf.int = TRUE)) %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = threshold1, y = estimate)) + geom_point() + geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + theme_bw() +
  xlab("threshold (DRI percentage)") + ylab("effect of adding one species to diet (linear coefficient)")
ggsave("figures/effect_of_adding_one_species_by_threshold.pdf")



targets %>%
  # filter(number_of_species < 30) %>% 
  mutate(threshold1 = threshold1*100) %>% 
  filter(threshold1 == 25) %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets, group = threshold1, color = threshold1)) + geom_line() +
  theme_bw() +
  scale_color_gradient(name="Percent of DRI", low="blue", high="red") + xlab("species richness") + ylab("number of functions (distinct DRI targets reached)")


### ok now need to do the thing where I compare the accumulation curves to the best species at each richness level

