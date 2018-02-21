
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

all_summaries <- read_csv("data-processed/all_summaries_BEF.csv")
all_summaries %>% 
  ungroup() %>% 
  mutate(nutrient = ifelse(nutrient == "all 5 micronutrients", "all", nutrient)) %>% 
  group_by(nutrient) %>% 
  do(tidy(nls(median ~ a * species_no^b, data =., start = c(a=10000, b=-0.7)), conf.int = TRUE)) %>% 
  filter(term == "b") %>% 
  mutate(estimate = estimate*-1) %>%
  mutate(conf.low = conf.low *-1) %>%
  mutate(conf.high = conf.high*-1) %>% 

protein <- all_summaries %>% 
  filter(nutrient == "protein") 


 pro_mod <- nls(median ~ a * species_no^b, start = c(a=10, b=-0.7), data = protein)
 summary(pro_mod)
 tidy(pro_mod, conf.int = TRUE)
 boot_ci <- nlstools::nlsBoot(pro_mod)
 as_data_frame(boot_ci$coefboot) %>% View
 as_data_frame(boot_ci$bootCI) %>% View

 
 

# Result #3 ---------------------------------------------------------------

### although the magnitude of the biodiversity effect was generally lower at the local scale than the global scale 
 
 
