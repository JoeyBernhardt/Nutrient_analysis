## looking at the seanuts data


# load packages -----------------------------------------------------------

library(tidyverse)
library(stringr)
library(ggplot2)


# read in data ------------------------------------------------------------

seanuts_raw <- read_csv("data-processed/seanuts_minerals_ecology.csv")
glimpse(seanuts_raw)

summary(seanuts_raw$Length)

sum(is.na(seanuts_raw$Length))

seanuts <- seanuts_raw %>% 
  filter(ca_mg < 1000) 



sapply(seanuts_raw, function(x) sum(is.na(x)))


str(seanuts)
  
hist(as.numeric(log(seanuts_raw$mn_mg)))

length(unique(seanuts$ca_mg))
length(unique(ntbl$species))

ntbl <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data/ntbl2.csv")
length(unique(ntbl$Food.Item.ID))
length(unique(seanuts_raw$food_item_id))
