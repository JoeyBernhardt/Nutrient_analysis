


#### 

library(taxize)
library(readxl)
library(tidyverse)

?taxize
comm2sci("sea trout", db = "worms")


fda <- read_excel("data/fda-mercury.xlsx", skip = 4) %>% 
  clean_names() %>% 
  filter(!is.na(analyte_measured)) 

fda_species <- fda %>% 
  distinct(sample_description) %>% 
  mutate(sample_description = str_to_lower(sample_description)) %>% 
  filter(!grepl("canned", sample_description)) %>% 
  filter(!grepl("fzn", sample_description)) %>% 
  filter(!grepl("farmed", sample_description)) %>% 
  filter(grepl("[(]", sample_description)) 


fda_species <- fda %>% 
  distinct(sample_description) %>% 
  mutate(sample_description = str_to_lower(sample_description)) %>% 
  filter(!grepl("canned", sample_description)) %>% 
  filter(!grepl("farmed", sample_description)) %>% 
  filter(!grepl("fzn", sample_description)) %>% 
  mutate(sample_description = ifelse(sample_description == "grouper black (mycteroperca)", "black grouper", sample_description)) %>% 
  mutate(sample_description = ifelse(sample_description == "grouper red (epinephelus)", "red grouper", sample_description)) %>% 
  mutate(sample_description = ifelse(sample_description == "grouper snowy (epinephelus)", "snowy grouper", sample_description)) %>% 
  mutate(sample_description = ifelse(sample_description == "grouper strawberry(epinephelus)", "strawberry grouper", sample_description)) %>% 
  mutate(sample_description = ifelse(sample_description == "jack smelt (silverside)", "silverside", sample_description)) %>% 
  mutate(sample_description = ifelse(sample_description == "pollock (surimi)", "pollock", sample_description)) %>% 
  mutate(sample_description = ifelse(sample_description == "tilefish (atlantic)", "tilefish", sample_description)) %>% 
  mutate(sample_description = ifelse(sample_description == "tilefish golden (atlantic)", "tilefish", sample_description)) %>% 
  mutate(sample_description = ifelse(sample_description == "tilefish (gulf)", "tilefish", sample_description)) %>% 
  mutate(sample_description = ifelse(sample_description == "trout rainbow (freshwater)", "rainbow trout", sample_description)) %>% 
  mutate(sample_description = ifelse(sample_description == "weakfish (sea trout)", "sea trout", sample_description)) %>% 
  mutate(sample_description = ifelse(sample_description == "weakfish (sea trout grey)", "sea trout", sample_description)) %>% 
  mutate(sample_description = ifelse(sample_description == "weakfish (sea trout sand)", "sea trout", sample_description)) %>% 
  mutate(sample_description = ifelse(sample_description == "weakfish (sea trout speckled)", "sea trout", sample_description)) %>% 
  mutate(sample_description = ifelse(sample_description == "weakfish (sea trout spotted)", "sea trout", sample_description)) 

write_csv(fda_species, "data-processed/fda-mercury-species.csv")

fda_species_edited <- read_csv("data-processed/fda-mercury-species-edited.csv") %>% 
  mutate(sample_description_edited = ifelse(is.na(sample_description_edited), sample_description, sample_description_edited))
scis <- comm2sci(fda_species_edited$sample_description_edited, db = "worms")
scis


fda %>% 
  mutate(sample_description = str_to_lower(sample_description)) %>% 
  filter(!grepl("canned", sample_description)) %>%
  filter(!grepl("farmed", sample_description)) %>%
  filter(!grepl("fzn", sample_description)) %>%
  filter(analyte_measured == "Methylmercury") %>% 
  group_by(sample_description) %>% 
  summarise(mean_merc  = mean(conc_ppm)) %>% 
  ggplot(aes(x = mean_merc)) + geom_histogram(bins = 20) 

library(rfishbase)

common_to_sci("grey seatrout")


