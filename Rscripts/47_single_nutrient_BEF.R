

### goal of this script is to generate the stats for the scaling parameters on the single nutrient vs. multinutrient efficiency urves


library(tidyverse)
library(cowplot)
library(plotrix)
library(broom)

output_all5 <- read_csv("data-processed/all_5_micronutrients_grams_required.csv")
output_calcium <- read_csv("data-processed/calcium_grams_required.csv")
output_iron <- read_csv("data-processed/iron_grams_required.csv")
output_zinc <- read_csv("data-processed/zinc_grams_required.csv")
output_dha <- read_csv("data-processed/dha_grams_required.csv")
output_epa <- read_csv("data-processed/epa_grams_required.csv")
output_protein <- read_csv("data-processed/protein_grams_required.csv")
output_iron_nuts <- read_csv("data-processed/iron_grams_required_nuts.csv")


all_g <- bind_rows(output_all5, output_calcium, output_dha, output_epa, output_iron, output_protein, output_zinc)

mods <- all_g %>% 
  group_by(nutrient, run) %>% 
  do(tidy(nls(formula = (grams_required ~ a * species_no^b),data = .,  start = c(a=1000, b=-0.5)))) 


mods %>% 
  filter(term == "b") %>% 
  ggplot(aes(x = estimate)) + geom_histogram() + 
  facet_wrap( ~ nutrient, scales = "free")

mod_sum <- mods %>% 
  filter(term == "b") %>% 
  group_by(nutrient) %>% 
  summarise_each(funs(mean, std.error), estimate)

allg_summ <- all_g %>% 
  group_by(nutrient, species_no) %>% 
  summarise_each(funs(mean, median), grams_required)


cal_med <- allg_summ %>% 
filter(nutrient == "calcium") %>% 
  do(tidy(nls(formula = (grams_required_median ~ a * species_no^b), data = .,  start = c(a=1000, b=-0.5)), conf.int = TRUE))

iron_med <- allg_summ %>% 
  filter(nutrient == "iron") %>% 
  do(tidy(nls(formula = (grams_required_median ~ a * species_no^b), data = .,  start = c(a=1000, b=-0.5)), conf.int = TRUE))


output_iron_nuts %>% 
  group_by(species_no) %>% 
  summarise_each(funs(mean, median), grams_required) %>% 
  do(tidy(nls(formula = (grams_required_median ~ a * species_no^b), data = .,  start = c(a=1000, b=-0.5)), conf.int = TRUE)) %>% View

zinc_med <- allg_summ %>% 
  filter(nutrient == "zinc") %>% 
  do(tidy(nls(formula = (grams_required_median ~ a * species_no^b), data = .,  start = c(a=1000, b=-0.5)), conf.int = TRUE))

epa_med <- allg_summ %>% 
  filter(nutrient == "epa") %>% 
  do(tidy(nls(formula = (grams_required_median ~ a * species_no^b), data = .,  start = c(a=1000, b=-0.5)), conf.int = TRUE))

dha_med <- allg_summ %>% 
  filter(nutrient == "dha") %>% 
  do(tidy(nls(formula = (grams_required_median ~ a * species_no^b), data = .,  start = c(a=1000, b=-0.5)), conf.int = TRUE))

all_med <- allg_summ %>% 
  filter(nutrient == "all_5_micronutrients") %>% 
  do(tidy(nls(formula = (grams_required_median ~ a * species_no^b), data = .,  start = c(a=1000, b=-0.5)), conf.int = TRUE))

all_med_estimates <- bind_rows(cal_med, iron_med, zinc_med, epa_med, dha_med, all_med)

all_med_estimates %>% 
  filter(term == "b") %>% 
  ggplot(aes(x = nutrient, y = estimate)) + geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1)


allg_summ %>% 
  ggplot(aes(x = species_no, y = grams_required_median, group = nutrient, color= nutrient)) + geom_line(size = 2)

lin_mods <- all_g %>% 
  group_by(nutrient, run) %>% 
  do(tidy(lm(log(grams_required) ~ log(species_no), data = .), conf.int = TRUE)) 

lin_mods %>% 
  filter(term == "log(species_no)") %>% 
  group_by(nutrient) %>% 
  summarise_each(funs(mean, std.error), estimate) %>% View
