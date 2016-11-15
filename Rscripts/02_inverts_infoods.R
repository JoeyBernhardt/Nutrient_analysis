## playing around with original version of infoods


# load pacakges -----------------------------------------------------------

library(tidyverse)
library(janitor)
library(stringr)


# read in data ------------------------------------------------------------

anf_raw <- read_csv("/Users/Joey/Desktop/Nutrient_databases/AnFooD1.0_fish.csv")


anf <- anf_raw %>% 
  slice(2:n()) %>% 
  clean_names()


anf <- anf %>% 
  mutate(type = str_replace(type, "F", "farmed"))

anf %>% 
  filter(type != "farmed" | is.na(type)) %>% 
  # filter(is.na(type) | type == "W") %>% 
  filter(processing == "r") %>% View


unique(anf$asfis_scientific_name)

moll <- anf %>% 
  filter(subgroup == "Molluscs") %>%
  filter(processing == "r")

fm <- moll %>% 
  filter(type == "farmed") 


unique(fm$scientific_name)


%>% 
  summarize(species_no = n_distinct(scientific_name))

moll %>% 
  filter(type == "farmed") %>% 
n_distinct(scientific_name)
  
sum(is.na(anf_raw$Processing))
