
library(tidyverse)
library(broom)



seadiv <- read_csv("data-processed/seadiv.csv")
### Goal here is to update stats for the paper, https://github.com/JoeyBernhardt/Nutrient_analysis/issues/28


# Result #1 ---------------------------------------------------------------

# R1 Species in the global dataset differed substantially in their micronutrient contents, but not their protein concentrations 
## need to figure out how to show that species differ in their micronutrient content, but not protein content

unique(seadiv$level_2)

nuts <- seadiv %>% 
  select(species_name, subgroup, calcium, iron, zinc, epa, dha, fat, protein) %>% 
  gather(key = nutrient, value = concentration, 3:9) %>% 
  filter(!is.na(concentration))


nuts %>% 
  group_by(nutrient) %>% 
  do(tidy(lm(concentration ~ species_name, data = .), conf.int = TRUE)) %>% View

nuts %>% 
  filter(nutrient == "calcium") %>% 
  lm(concentration ~ species_name, data = .) %>% 
  summary



# Result #2 ---------------------------------------------------------------

# R2 We found no effect of diversity on the protein benefits of seafood.



