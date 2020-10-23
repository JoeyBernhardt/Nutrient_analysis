
library(readxl)
library(tidyverse)


all_merc_raw <- read_csv("data-processed/mercury-data-raw-compiled.csv")

times_hundred <- function(x, na.rm = FALSE) (x*100)
tox <- read_excel("data/Hall-trace-elements.xlsx") %>% 
  clean_names() %>%
  mutate(species = str_to_lower(species)) %>% 
  mutate(area = str_to_lower(area)) %>% 
  mutate(part = str_to_lower(part)) %>% 
  filter(part != "liver") %>% 
  # select(6:20) %>% 
  filter(cadmium != 2.61) %>% 
  filter(nickel != 1.810) %>% 
dplyr::select(1:9, subgroup) %>% 
  mutate(arsenic = arsenic*100,
         lead = lead*100,
         cadmium = cadmium*100, 
         mercury = mercury*100) %>% 
  mutate(methylmercury = ifelse(subgroup == "mollusc", mercury*0.30, mercury*0.95)) %>% 
  rename(location_of_study = area) %>% 
  rename(genus_species = species) %>% 
  mutate(dataset = "hall") %>% 
  mutate(bibliography =  "Hall, R. A., E. G. Zook, and G. M. Meaburn. 1978. National Marine Fisheries Service Survey of Trace Elements in the Fishery Resource. https://spo.nmfs.noaa.gov/content/national-marine-fisheries-service-survey-trace-elements-fishery-resource.")

View(tox)

all_merc <- read_csv("data-processed/mercury-data-raw-compiled.csv") %>%
  filter(dataset != "hall") 


all_contaminants <- bind_rows(tox, all_merc)%>% 
  dplyr::select(-code, -mercury, -site) %>% 
  mutate(location_of_study = str_replace(location_of_study, "area", "US")) %>% 
  mutate(genus_species = str_to_lower(genus_species)) %>% 
  mutate(location_of_study = ifelse(dataset == "adams", "US-Florida", location_of_study)) %>% 
  dplyr::select(genus_species, subgroup, taxon_common_name, location_of_study, methylmercury, lead, arsenic, cadmium, everything()) %>% 
  arrange(genus_species) 

write_csv(all_contaminants, "data-processed/all-contaminant-raw.csv")
WriteXLS::WriteXLS(all_contaminants, "data-processed/all-contaminant-raw.xlsx")


# all_merc <- read_csv("data-processed/mercury-data-compiled.csv")
str(tox)

names_tox <- names(tox)

tox_sum <- tox %>% 
  group_by(species) %>% 
  summarise_at(c(names_tox[6:20]), mean) %>% 
  dplyr::select(species, 2:5) %>% 
  gather(key = nutrient, value = concentration, 2:5) %>% 
  mutate(concentration = ifelse(concentration == 0, 0.00001, concentration)) %>%
  spread(key= nutrient, value = concentration) %>% 
  dplyr::select(-species) %>% 
  mutate_all(.funs = log)


tox_sum4 <- tox %>% 
  group_by(subgroup, species) %>% 
  summarise_at(c(names_tox[6:20]), mean) %>% 
  gather(key = element, value = concentration, 3:6)

#### toxins to use: mercury, cadmium, lead, arsenic


tox2 <- tox %>% 
  group_by(species, subgroup) %>% 
  summarise_at(c(names_tox[6:20]), mean) %>% 
  mutate_at(c(names_tox[6:20]), times_hundred) %>% 
  dplyr::select(1:6) %>% 
  filter(arsenic < 50000) %>%
  filter(cadmium < 500) %>% 
  mutate(arsenic = arsenic / 150*100) %>%
  mutate(cadmium = cadmium / 70*100) %>%
  mutate(mercury = mercury / 16*100) %>%
  mutate(lead = lead / 250*100) %>%
  gather(key = contaminant, value = concentration, 3:6) %>%
  mutate(dataset = "hall")


allcon <- bind_rows(tox2, all_merc) %>% 
  filter(!is.na(concentration)) %>% 
  group_by(species, contaminant, dataset) %>% 
  summarise(concentration = mean(concentration))

length(unique(allcon$species))
write_csv(allcon, "data-processed/all-contaminant-means.csv")
