
library(tidyverse)
library(broom)



seadiv <- read_csv("data-processed/seadiv.csv")
mean_nuts <- read_csv("data-processed/mean_nuts.csv")
### Goal here is to update stats for the paper, https://github.com/JoeyBernhardt/Nutrient_analysis/issues/28


# Result #1 ---------------------------------------------------------------

# R1 Species in the global dataset differed substantially in their micronutrient contents, but not their protein concentrations 
## need to figure out how to show that species differ in their micronutrient content, but not protein content

unique(seadiv$level_2)

nuts <- seadiv %>% 
  filter(species_name %in% mean_nuts$species_name) %>% 
  select(species_name, subgroup, calcium, iron, zinc, epa, dha, fat, protein) %>% 
  gather(key = nutrient, value = concentration, 3:9) %>% 
  filter(!is.na(concentration))


nuts %>% 
  group_by(nutrient) %>% 
  do(tidy(lm(concentration ~ species_name, data = .), conf.int = TRUE)) %>% View

nuts %>% 
  filter(nutrient == "protein") %>% 
  lm(concentration ~ species_name, data = .) %>% 
  summary

percentages <- read_csv("data-processed/percentages.csv") 

percentages %>% 
  group_by(subgroup, nutrient) %>% 
  summarise_each(funs(mean, std.error), dri_per) %>% 
  ggplot(aes(x = nutrient, y = dri_per_mean)) + geom_point() +
  geom_errorbar(aes(ymin = dri_per_mean - dri_per_std.error, ymax = dri_per_mean + dri_per_std.error)) +
  facet_wrap( ~ subgroup, scales = "free")

### CVs for protein and micronutrients
percentages %>% 
  filter(dri_per > 0) %>% 
  mutate(log_dri_per = log(dri_per)) %>% 
  group_by(nutrient) %>% 
  summarise_each(funs(mean, sd), log_dri_per) %>% 
  mutate(cv = log_dri_per_sd/log_dri_per_mean) %>% 
  mutate(log_cv = (exp(log_dri_per_sd^2)-1)^1/2) %>% View
 


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
 
 
 ### number of species that reach DRI targets
 
(.94*17 + 45*161 + .88*34)/(17+161+34)
 
 
