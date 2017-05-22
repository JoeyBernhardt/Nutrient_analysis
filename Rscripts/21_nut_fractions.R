library(tidyverse)
library(stringr)
library(purrr)



trait_data <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data-processed/n.long_lat3.csv")

ntbl <- trait_data %>% 
  spread(nutrient, concentration) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(mean.CA = mean(ca_mg, na.rm = TRUE),
            mean.ZN = mean(zn_mg, na.rm = TRUE), 
            mean.FE = mean(fe_mg, na.rm = TRUE),
            mean.EPA = mean(epa, na.rm = TRUE),
            mean.DHA = mean(dha, na.rm = TRUE)) %>% 
  filter(!is.na(mean.CA)) %>% 
  filter(!is.na(mean.EPA)) %>% 
  filter(!is.na(mean.DHA)) %>%
  filter(!is.na(mean.FE)) %>% 
  filter(!is.na(mean.ZN)) %>%
  rename(calcium = mean.CA,
         epa = mean.EPA,
         dha = mean.DHA,
         zinc = mean.ZN,
         iron = mean.FE) %>% 
  ungroup()

write_csv(ntbl, "data-processed/ntbl.csv")

ntbl_sub <- ntbl %>% 
  ungroup() %>% 
  select(species_name, subgroup, calcium_g, zinc_g, iron_g, epa_g, dha_g)

### maybe do this for each target separately??

  
  ## ok here's an option, sum it up
  
grams_required <- function(species_number){
df <- ntbl_sub %>% 
  select(subgroup, species_name, calcium_g) %>% 
  sample_n(size = species_number, replace = TRUE) %>% 
  mutate(cal_total = (calcium_g/species_number)*108) %>% 
  mutate(total_calcium = cumsum(cal_total)) %>% 
  mutate(calcium_rdi = ifelse(total_calcium > (1200*0.75), 1, 0)) %>% 
  mutate(species_no = species_number) %>%
  group_by(species_no) %>% 
  summarise_each(funs(sum), contains("rdi")) %>% 
  mutate(cal_grams_required = species_no - calcium_rdi) %>%
  select(species_no, contains("required"), everything()) %>% 
  mutate(target_reached = ifelse(cal_grams_required == species_no, "no", "yes"))
return(df)
}


sample_number_rep <- rep(1:108, 50)
results_test <- sample_number_rep %>% 
  map_df(grams_required)

View(results_test)



### let's try the percentage way

ntbl <- ntbl %>% 
  select(-contains("g")) 


species_number <- 10

library(tidyverse)
library(purrr)

species_number <- 10

grams_req <- function(species_number){
  y <- species_number
  df <- ntbl %>% 
  sample_n(size = species_number, replace = FALSE) %>% ## sample n species
  mutate(cal_total = (calcium/species_number)) %>% ## get the amount of calcium each species will contribute
  mutate(zinc_total = (zinc/species_number)) %>% 
  mutate(iron_total = (iron/species_number)) %>% 
  mutate(epa_total = (epa/species_number)) %>%
  mutate(dha_total = (dha/species_number)) %>% 
  summarise_each(funs(sum), contains("total")) %>%  ## sum up all of each of the nutrients
  mutate(cal_grams = (cal_total/(1200))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
  mutate(iron_grams = (iron_total/(18))) %>%
  mutate(zinc_grams = (zinc_total/(11))) %>% 
  mutate(epa_grams = (epa_total/(1))) %>%
  mutate(dha_grams = (dha_total/(1))) %>%
  mutate(species_no = species_number) %>% 
  select(contains("grams")) %>%
  gather() %>%
  summarise(min_percentage = min(value)) %>% 
  mutate(grams_required = 100/min_percentage)
  combined_df <- data.frame(y, df)
  return(combined_df)
}


sample_number_rep <- rep(1:108, 10)

results_test <- sample_number_rep %>% 
  map_df(grams_req) ## iterate over the list of species pool numbers

results_test %>%
  rename(number_of_species = y) %>%
  group_by(number_of_species) %>% 
  summarise_each(funs(mean, min, max, median), grams_required) %>%
  ggplot(aes(x = number_of_species, y = min)) + geom_point()+
  scale_x_log10()

results_test %>% 
  ggplot(aes(x = species_no, y = dha_grams)) + geom_point()
