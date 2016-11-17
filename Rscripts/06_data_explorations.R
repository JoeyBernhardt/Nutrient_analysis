## looking at the seanuts data


# load packages -----------------------------------------------------------

library(tidyverse)
library(stringr)
library(ggplot2)


# read in data ------------------------------------------------------------

seanuts_raw <- read_csv("data-processed/seanuts_ecology.csv")



# being exploring! --------------------------------------------------------

glimpse(seanuts_raw)

summary(seanuts_raw$Length)

sum(!is.na(seanuts_raw$Weight))

seanuts <- seanuts_raw %>% 
  filter(ca_mg < 1000) 



sapply(seanuts_raw, function(x) sum(is.na(x)))


