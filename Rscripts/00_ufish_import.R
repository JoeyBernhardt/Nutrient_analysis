## PLaying around with the uFISH data



# libraries ---------------------------------------------------------------

library(tidyverse)
library(stringr)
library(janitor)
library(fuzzyjoin)

ufish <- read_csv("data/uFiSh1.0_mins.csv")

ufish1 <- ufish %>% 
  clean_names() %>% 
  filter(row_number() != 1) %>% 
  filter(state_of_food == "r") %>% View

### Ok let's check to see if there are refs in the ufish dataset that I don't have yet in seanuts!

ufish_refs <- read_csv("data/uFiSh1.0_refs.csv")

ufish_refs_sci <- ufish_refs %>% 
  filter(str_detect(Type, "scientific")) 
  
seanuts <- read_csv("data-processed/seanuts_select_8.csv") 

seanuts_refs <- seanuts %>% 
  dplyr::select(biblioid2) %>% 
  distinct(biblioid2)

??fuzzy_join

ufish_refs2 <- ufish_refs_sci %>% 
  separate(RefID, into = c("letter", "number"), sep = 2, remove = FALSE) %>% 
  mutate(number = as.numeric(number)) %>%
  unite(biblioid2, letter, number, sep = "") 


u <- ufish_refs2$biblioid2
s <- seanuts_refs$biblioid2

setdiff(s, u)

not_in_seanuts <- anti_join(ufish_refs2, seanuts_refs)

write_csv(not_in_seanuts, "data-processed/references_not_in_seanuts.csv")

not_in_seanuts$Citation
