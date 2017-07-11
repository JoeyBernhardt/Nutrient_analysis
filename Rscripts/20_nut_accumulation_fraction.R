library(tidyverse)
library(vegan)
library(stringr)
library(purrr)
library(plotrix)
library(broom)

trait_data <- read_csv("data-processed/n.long_lat3.csv")


ntbl.RDI.30 <- trait_data %>% 
  filter(!grepl("^Mohanty", ref_info)) %>% 
  spread(nutrient, concentration) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(mean.CA = mean(ca_mg, na.rm = TRUE),
            mean.ZN = mean(zn_mg, na.rm = TRUE), 
            mean.FE = mean(fe_mg, na.rm = TRUE),
            mean.EPA = mean(epa, na.rm = TRUE),
            mean.DHA = mean(dha, na.rm = TRUE)) %>% 
  mutate(RDI.CA = ifelse(mean.CA > (1200*0.3), 1, 0)) %>% 
  mutate(RDI.FE = ifelse(mean.FE > (18*0.3), 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(mean.ZN > (11*0.3), 1, 0)) %>%
  mutate(RDI.EPA = ifelse(mean.EPA > (1*0.3), 1, 0)) %>% 
  mutate(RDI.DHA = ifelse(mean.DHA > (1*0.3), 1, 0)) %>% 
  ungroup() %>% 
  mutate(RDI.micro.tot = rowSums(.[8:12])) %>% 
  filter(!is.na(RDI.micro.tot)) 

nuts <- ntbl.RDI.30 %>% 
  dplyr::select(species_name, subgroup, contains("RDI")) %>% 
  mutate(ID = rownames(.))


ntbl <- trait_data %>% 
  filter(!grepl("^Mohanty", ref_info)) %>% 
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
  mutate(calcium_g = calcium/100,
         zinc_g = zinc/100,
         iron_g = iron/100,
         epa_g = epa/100,
         dha_g = dha/100)


### goal now, make the nutrient accumulation curve, but based on fractions of 100g portion, such that 
### when you have one species, the entire 100g is taken from that one species
### when you have two species, each species gets 100g/2 
### when you have three species, each species gets to contribute 100g/3 etc. 

total_nutrient <- function(species_number){
  
}


str(ntbl)

ntbl_sub <- ntbl %>% 
  ungroup() %>% 
  select(species_name, subgroup, calcium_g, zinc_g, iron_g, epa_g, dha_g)

### if the the target threshold is 25% of RDI
total_nutrient_25 <- function(species_number){
  y <- vector()
  x <- species_number
  y <- ntbl_sub %>% 
    sample_n(size = species_number, replace = TRUE) %>% 
    mutate(cal_total = (calcium_g/species_number)*108) %>% 
    mutate(zinc_total = (zinc_g/species_number)*108) %>% 
    mutate(iron_total = (iron_g/species_number)*108) %>% 
    mutate(epa_total = (epa_g/species_number)*108) %>%
    mutate(dha_total = (dha_g/species_number)*108) %>% 
    mutate(total_calcium = cumsum(cal_total)) %>% 
    mutate(total_zinc = cumsum(zinc_total)) %>% 
    mutate(total_iron = cumsum(iron_total)) %>% 
    mutate(total_epa = cumsum(epa_total)) %>%
    mutate(total_dha = cumsum(dha_total)) %>% 
    top_n(n = 1, wt = total_dha) %>% 
    mutate(calcium_rdi = ifelse(total_calcium > (1200*0.25), 1, 0)) %>% 
    mutate(zinc_rdi = ifelse(total_zinc > (11*0.25), 1, 0)) %>% 
    mutate(iron_rdi = ifelse(total_iron > (18*0.25), 1, 0)) %>% 
    mutate(epa_rdi = ifelse(total_epa > (1*0.25), 1, 0)) %>% 
    mutate(dha_rdi = ifelse(total_dha > (1*0.25), 1, 0)) %>% 
    mutate(species_no = species_number) %>% 
    select(18:23)
  df <- data.frame(x, y)
  combined_df <- rbind(df)
  return(combined_df)
}


sample_number_rep <- rep(1:108, 10)


results_25 <- sample_number_rep %>% 
  map_df(total_nutrient_25)

results_total_25_v2 <- results_25 %>% 
  mutate(number_of_targets = rowSums(.[2:6])) %>% 
  mutate(threshold = "25") 

results_total_25 <- results %>% 
  mutate(number_of_targets = rowSums(.[7:11])) 

results_total_25 %>% 
  ggplot(aes(x = species_no, y = number_of_targets)) + geom_point(size = 1) + theme_bw()

results_total_25_v2 %>% 
  ggplot(aes(x = species_no, y = number_of_targets)) + geom_point(size = 1) + theme_bw() + geom_smooth()

results_total_25_v2 %>% 
  filter(species_no < 11) %>% 
group_by(species_no) %>% 
  summarise_each(funs(mean, median), number_of_targets) %>% 
  ggplot(aes(x = species_no, y = median)) + geom_point(size = 1) + theme_bw() + geom_smooth()

### now if the threshold is 10% of RDI
total_nutrient_10 <- function(species_number){
  y <- vector()
  x <- species_number
  y <- ntbl_sub %>% 
    sample_n(size = species_number, replace = TRUE) %>% 
    mutate(cal_total = (calcium_g/species_number)*108) %>% 
    mutate(zinc_total = (zinc_g/species_number)*108) %>% 
    mutate(iron_total = (iron_g/species_number)*108) %>% 
    mutate(epa_total = (epa_g/species_number)*108) %>%
    mutate(dha_total = (dha_g/species_number)*108) %>% 
    mutate(total_calcium = cumsum(cal_total)) %>% 
    mutate(total_zinc = cumsum(zinc_total)) %>% 
    mutate(total_iron = cumsum(iron_total)) %>% 
    mutate(total_epa = cumsum(epa_total)) %>%
    mutate(total_dha = cumsum(dha_total)) %>% 
    top_n(n = 1, wt = total_dha) %>% 
    mutate(calcium_rdi = ifelse(total_calcium > (1200*.1), 1, 0)) %>% 
    mutate(zinc_rdi = ifelse(total_zinc > (11*.1), 1, 0)) %>% 
    mutate(iron_rdi = ifelse(total_iron > (18*.1), 1, 0)) %>% 
    mutate(epa_rdi = ifelse(total_epa > (1*.1), 1, 0)) %>% 
    mutate(dha_rdi = ifelse(total_dha > (1*.1), 1, 0)) %>% 
    mutate(species_no = species_number) %>% 
    select(18:23)
  df <- data.frame(x, y)
  combined_df <- rbind(df)
  return(combined_df)
}


sample_number_rep <- rep(1:10, 1000)
results_10_all <- sample_number_rep %>% 
  map_df(total_nutrient_10) %>% 
mutate(number_of_targets = rowSums(.[2:6])) %>% 
  mutate(threshold = "10") %>% 
  mutate(group = "all species")


### now if the threshold is 100% of RDI
total_nutrient_100 <- function(species_number){
  y <- vector()
  x <- species_number
  y <- ntbl_sub %>% 
    sample_n(size = species_number, replace = TRUE) %>% 
    mutate(cal_total = (calcium_g/species_number)*108) %>% 
    mutate(zinc_total = (zinc_g/species_number)*108) %>% 
    mutate(iron_total = (iron_g/species_number)*108) %>% 
    mutate(epa_total = (epa_g/species_number)*108) %>%
    mutate(dha_total = (dha_g/species_number)*108) %>% 
    mutate(total_calcium = cumsum(cal_total)) %>% 
    mutate(total_zinc = cumsum(zinc_total)) %>% 
    mutate(total_iron = cumsum(iron_total)) %>% 
    mutate(total_epa = cumsum(epa_total)) %>%
    mutate(total_dha = cumsum(dha_total)) %>% 
    top_n(n = 1, wt = total_dha) %>% 
    mutate(calcium_rdi = ifelse(total_calcium > (1200*1), 1, 0)) %>% 
    mutate(zinc_rdi = ifelse(total_zinc > (11*1), 1, 0)) %>% 
    mutate(iron_rdi = ifelse(total_iron > (18*1), 1, 0)) %>% 
    mutate(epa_rdi = ifelse(total_epa > (1*1), 1, 0)) %>% 
    mutate(dha_rdi = ifelse(total_dha > (1*1), 1, 0)) %>% 
    mutate(species_no = species_number) %>% 
    select(18:23)
  df <- data.frame(x, y)
  combined_df <- rbind(df)
  return(combined_df)
}


sample_number_rep <- rep(1:10, 1000)
results_100_all_species <- sample_number_rep %>% 
  map_df(total_nutrient_100) %>% 
  mutate(number_of_targets = rowSums(.[2:6])) %>% 
  mutate(threshold = "100") %>% 
  mutate(group = "all species")



### now for inverts only
total_nutrient_10_inverts <- function(species_number){
  y <- vector()
  x <- species_number
  y <- ntbl_sub %>%
    filter(subgroup != "finfish") %>%
    sample_n(size = species_number, replace = TRUE) %>% 
    mutate(cal_total = (calcium_g/species_number)*108) %>% 
    mutate(zinc_total = (zinc_g/species_number)*108) %>% 
    mutate(iron_total = (iron_g/species_number)*108) %>% 
    mutate(epa_total = (epa_g/species_number)*108) %>%
    mutate(dha_total = (dha_g/species_number)*108) %>% 
    mutate(total_calcium = cumsum(cal_total)) %>% 
    mutate(total_zinc = cumsum(zinc_total)) %>% 
    mutate(total_iron = cumsum(iron_total)) %>% 
    mutate(total_epa = cumsum(epa_total)) %>%
    mutate(total_dha = cumsum(dha_total)) %>% 
    top_n(n = 1, wt = total_dha) %>% 
    mutate(calcium_rdi = ifelse(total_calcium > (1200*.1), 1, 0)) %>% 
    mutate(zinc_rdi = ifelse(total_zinc > (11*.1), 1, 0)) %>% 
    mutate(iron_rdi = ifelse(total_iron > (18*.1), 1, 0)) %>% 
    mutate(epa_rdi = ifelse(total_epa > (1*.1), 1, 0)) %>% 
    mutate(dha_rdi = ifelse(total_dha > (1*.1), 1, 0)) %>% 
    mutate(species_no = species_number) %>% 
    select(18:23)
  df <- data.frame(x, y)
  combined_df <- rbind(df)
  return(combined_df)
}

sample_number_rep <- rep(1:10, 1000)
results_10_inverts <- sample_number_rep %>% 
  map_df(total_nutrient_10_inverts) %>% 
  mutate(number_of_targets = rowSums(.[2:6])) %>% 
  mutate(threshold = "10") %>% 
  mutate(group = "invertebrates")

results_10_inverts %>% 
  group_by(species_no) %>% 
  summarise_each(funs(mean, median, std.error), number_of_targets) %>% 
  ggplot(aes(x = species_no, y = mean)) + geom_point(size = 1) + theme_bw() +
  geom_line() +
  geom_ribbon(aes(ymin = mean - std.error, ymax = mean + std.error), fill = "grey", alpha = 0.5) +
  scale_x_continuous(breaks = c(1:10)) + ylim(0, 5)

### now for finfish only

total_nutrient_10_finfish <- function(species_number){
  y <- vector()
  x <- species_number
  y <- ntbl_sub %>%
    filter(subgroup == "finfish") %>%
    sample_n(size = species_number, replace = TRUE) %>% 
    mutate(cal_total = (calcium_g/species_number)*108) %>% 
    mutate(zinc_total = (zinc_g/species_number)*108) %>% 
    mutate(iron_total = (iron_g/species_number)*108) %>% 
    mutate(epa_total = (epa_g/species_number)*108) %>%
    mutate(dha_total = (dha_g/species_number)*108) %>% 
    mutate(total_calcium = cumsum(cal_total)) %>% 
    mutate(total_zinc = cumsum(zinc_total)) %>% 
    mutate(total_iron = cumsum(iron_total)) %>% 
    mutate(total_epa = cumsum(epa_total)) %>%
    mutate(total_dha = cumsum(dha_total)) %>% 
    top_n(n = 1, wt = total_dha) %>% 
    mutate(calcium_rdi = ifelse(total_calcium > (1200*.1), 1, 0)) %>% 
    mutate(zinc_rdi = ifelse(total_zinc > (11*.1), 1, 0)) %>% 
    mutate(iron_rdi = ifelse(total_iron > (18*.1), 1, 0)) %>% 
    mutate(epa_rdi = ifelse(total_epa > (1*.1), 1, 0)) %>% 
    mutate(dha_rdi = ifelse(total_dha > (1*.1), 1, 0)) %>% 
    mutate(species_no = species_number) %>% 
    select(18:23)
  df <- data.frame(x, y)
  combined_df <- rbind(df)
  return(combined_df)
}

sample_number_rep <- rep(1:10, 1000)
results_10_finfish <- sample_number_rep %>% 
  map_df(total_nutrient_10_finfish) %>% 
  mutate(number_of_targets = rowSums(.[2:6])) %>% 
  mutate(threshold = "10")%>% 
  mutate(group = "finfish")

### all of the 10 percent thresholds

all_10 <- bind_rows(results_10_finfish, results_10_inverts, results_10_all, results_100_all_species)

write_csv(all_10, "data-processed/DRI_accumulation_replacement_design.csv")

all_10 %>% 
  filter(species_no < 11, threshold == 10) %>% 
  group_by(group, species_no, threshold) %>% 
  summarise_each(funs(mean, median, std.error), number_of_targets) %>% 
  ggplot(aes(x = species_no, y = mean, color = group, shape = threshold)) + geom_line(size = 2) +
  theme_bw() + ylim(1, 5) + scale_x_continuous(breaks = c(0:10)) + ylab("mean number of distinct micronutrient DRI targets per 100g portion") +
  xlab("species richness") +
  stat_function(fun = all_power_fit)
  # + geom_ribbon(aes(ymin = mean - std.error*1.96, ymax = mean + std.error*1.96, fill = group), alpha = 0.2)
ggsave("figures/nutrient_accumulation_replacement.png")

## now let's try to fit power functions to these curves


  
all_sp_10 <- all_10 %>% 
  filter(group == "all species", threshold == 10) 


all_fit <-   nls(formula=(number_of_targets ~ a * species_no^b), data= all_sp_10, start = c(a=1, b=0.5))


summary(all_fit)
glance(all_fit)
tidy(all_fit, conf.int = TRUE)
cor(all_sp_10$number_of_targets, predict(all_fit))

all.b <- coef(all_fit)[["b"]]
all.a <- coef(all_fit)[["a"]]

all_power_fit <-  function(x) all.a*x^all.b

results_100 <- sample_number_rep %>% 
  map_df(total_nutrient_100)

results_100_finfish <- sample_number_rep %>% 
  map_df(total_nutrient_100_finfish)

results_total_100_finfish <- results_100_finfish %>% 
  mutate(number_of_targets = rowSums(.[2:6])) %>% 
  mutate(threshold = "100") %>% 
  filter(species_no < 5) %>% 
  ggplot(aes(x = species_no, y = number_of_targets)) + geom_point(size = 1) + theme_bw()

results_total_100 <- results_100 %>% 
  mutate(number_of_targets = rowSums(.[2:6])) %>% 
  mutate(threshold = "100") %>% 
  filter(species_no < 5) %>% 
  ggplot(aes(x = species_no, y = number_of_targets)) + geom_point(size = 1) + theme_bw()


results_total_100 %>% 
  ggplot(aes(x = species_no, y = number_of_targets)) + geom_point(size = 1) + theme_bw()

### now if the threshold is 50% of RDI
total_nutrient_50 <- function(species_number){
  y <- vector()
  x <- species_number
  y <- ntbl_sub %>% 
    sample_n(size = species_number, replace = TRUE) %>% 
    mutate(cal_total = (calcium_g/species_number)*108) %>% 
    mutate(zinc_total = (zinc_g/species_number)*108) %>% 
    mutate(iron_total = (iron_g/species_number)*108) %>% 
    mutate(epa_total = (epa_g/species_number)*108) %>%
    mutate(dha_total = (dha_g/species_number)*108) %>% 
    mutate(total_calcium = cumsum(cal_total)) %>% 
    mutate(total_zinc = cumsum(zinc_total)) %>% 
    mutate(total_iron = cumsum(iron_total)) %>% 
    mutate(total_epa = cumsum(epa_total)) %>%
    mutate(total_dha = cumsum(dha_total)) %>% 
    top_n(n = 1, wt = total_dha) %>% 
    mutate(calcium_rdi = ifelse(total_calcium > (1200*0.5), 1, 0)) %>% 
    mutate(zinc_rdi = ifelse(total_zinc > (11*0.5), 1, 0)) %>% 
    mutate(iron_rdi = ifelse(total_iron > (18*0.5), 1, 0)) %>% 
    mutate(epa_rdi = ifelse(total_epa > (1*0.5), 1, 0)) %>% 
    mutate(dha_rdi = ifelse(total_dha > (1*0.5), 1, 0)) %>% 
    mutate(species_no = species_number) %>% 
    select(18:23)
  df <- data.frame(x, y)
  combined_df <- rbind(df)
  return(combined_df)
}


sample_number_rep <- rep(1:108, 100)


results_50 <- sample_number_rep %>% 
  map_df(total_nutrient_50)


results_total_50 <- results_50 %>% 
  mutate(number_of_targets = rowSums(.[2:6])) %>% 
  mutate(threshold = "50")


results_total_50 %>% 
  ggplot(aes(x = species_no, y = number_of_targets)) + geom_point(size = 1) + theme_bw()

### now if the threshold is 75% of RDI
total_nutrient_75 <- function(species_number){
  y <- vector()
  x <- species_number
  y <- ntbl_sub %>% 
    sample_n(size = species_number, replace = TRUE) %>% 
    mutate(cal_total = (calcium_g/species_number)*108) %>% 
    mutate(zinc_total = (zinc_g/species_number)*108) %>% 
    mutate(iron_total = (iron_g/species_number)*108) %>% 
    mutate(epa_total = (epa_g/species_number)*108) %>%
    mutate(dha_total = (dha_g/species_number)*108) %>% 
    mutate(total_calcium = cumsum(cal_total)) %>% 
    mutate(total_zinc = cumsum(zinc_total)) %>% 
    mutate(total_iron = cumsum(iron_total)) %>% 
    mutate(total_epa = cumsum(epa_total)) %>%
    mutate(total_dha = cumsum(dha_total)) %>% 
    top_n(n = 1, wt = total_dha) %>% 
    mutate(calcium_rdi = ifelse(total_calcium > (1200*0.5), 1, 0)) %>% 
    mutate(zinc_rdi = ifelse(total_zinc > (11*0.75), 1, 0)) %>% 
    mutate(iron_rdi = ifelse(total_iron > (18*0.75), 1, 0)) %>% 
    mutate(epa_rdi = ifelse(total_epa > (1*0.75), 1, 0)) %>% 
    mutate(dha_rdi = ifelse(total_dha > (1*0.75), 1, 0)) %>% 
    mutate(species_no = species_number) %>% 
    select(18:23)
  df <- data.frame(x, y)
  combined_df <- rbind(df)
  return(combined_df)
}


sample_number_rep <- rep(1:108, 2)
results_test <- sample_number_rep %>% 
  map_df(total_nutrient_75) %>% 
  mutate(number_of_targets = rowSums(.[2:6])) %>% 
  mutate(threshold = "75") %>% 
  ggplot(aes(x = species_no, y = number_of_targets)) + geom_point(size = 1) + theme_bw() + geom_smooth()

results_75 <- sample_number_rep %>% 
  map_df(total_nutrient_75)


results_total_75 <- results_75 %>% 
  mutate(number_of_targets = rowSums(.[2:6])) %>% 
  mutate(threshold = "75")


results_total_75 %>% 
  ggplot(aes(x = species_no, y = number_of_targets)) + geom_point(size = 1) + theme_bw() + geom_smooth()



all_results <- bind_rows(results_total_25_v2, results_total_100, results_total_50, results_total_75)

write_csv(all_results, "data-processed/all_accumulation_by_fractions.csv")

all_results <- read_csv("data-processed/all_accumulation_by_fractions.csv")

all_results %>% 
  filter(species_no < 30) %>%
  mutate(threshold = as.numeric(threshold)) %>% 
  ggplot(aes(x = species_no, y = number_of_targets, group = threshold, color = threshold)) + geom_point(alpha = 0.5) + geom_smooth(color = "black", size = 2) +
  facet_wrap( ~ threshold) + theme_bw() + ylab("number of functions (DRI targets reached)") + xlab("species richness")


## let's get the average monoculture number of thresholds

monoculture_100 <- ntbl_sub %>% 
  summarise_each(funs(mean), 3:7) %>% 
  mutate_each(funs(.*108)) %>% 
    mutate(calcium_rdi = ifelse(calcium_g > (1200*1), 1, 0)) %>% 
    mutate(zinc_rdi = ifelse(zinc_g > (11*1), 1, 0)) %>% 
    mutate(iron_rdi = ifelse(iron_g > (18*1), 1, 0)) %>% 
    mutate(epa_rdi = ifelse(epa_g > (1*1), 1, 0)) %>% 
    mutate(dha_rdi = ifelse(dha_g > (1*1), 1, 0)) %>% 
  mutate(number_of_targets = rowSums(.[6:10])) %>% 
  mutate(threshold = "100") %>% 
  select(number_of_targets)

monoculture_75 <- ntbl_sub %>% 
  summarise_each(funs(mean), 3:7) %>% 
  mutate_each(funs(.*108)) %>% 
  mutate(calcium_rdi = ifelse(calcium_g > (1200*0.75), 1, 0)) %>% 
  mutate(zinc_rdi = ifelse(zinc_g > (11*0.75), 1, 0)) %>% 
  mutate(iron_rdi = ifelse(iron_g > (18*0.75), 1, 0)) %>% 
  mutate(epa_rdi = ifelse(epa_g > (1*0.75), 1, 0)) %>% 
  mutate(dha_rdi = ifelse(dha_g > (1*0.75), 1, 0)) %>% 
  mutate(number_of_targets = rowSums(.[6:10])) %>% 
  mutate(threshold = "75") %>% 
  select(number_of_targets)

monoculture_50 <- ntbl_sub %>% 
  summarise_each(funs(mean), 3:7) %>% 
  mutate_each(funs(.*108)) %>% 
  mutate(calcium_rdi = ifelse(calcium_g > (1200*0.5), 1, 0)) %>% 
  mutate(zinc_rdi = ifelse(zinc_g > (11*0.5), 1, 0)) %>% 
  mutate(iron_rdi = ifelse(iron_g > (18*0.5), 1, 0)) %>% 
  mutate(epa_rdi = ifelse(epa_g > (1*0.5), 1, 0)) %>% 
  mutate(dha_rdi = ifelse(dha_g > (1*0.5), 1, 0)) %>% 
  mutate(number_of_targets = rowSums(.[6:10])) %>% 
  mutate(threshold = "50") %>% 
  select(number_of_targets)
  

monoculture_25 <- ntbl_sub %>% 
  summarise_each(funs(mean), 3:7) %>% 
  mutate_each(funs(.*108)) %>% 
  mutate(calcium_rdi = ifelse(calcium_g > (1200*0.25), 1, 0)) %>% 
  mutate(zinc_rdi = ifelse(zinc_g > (11*0.25), 1, 0)) %>% 
  mutate(iron_rdi = ifelse(iron_g > (18*0.25), 1, 0)) %>% 
  mutate(epa_rdi = ifelse(epa_g > (1*0.25), 1, 0)) %>% 
  mutate(dha_rdi = ifelse(dha_g > (1*0.25), 1, 0)) %>% 
  mutate(number_of_targets = rowSums(.[6:10])) %>% 
  mutate(threshold = "25") %>% 
  select(number_of_targets)  


plot1 <- all_results %>% 
  filter(threshold == 75) %>% 
  filter(species_no < 30) %>%
  ggplot(aes(x = species_no, y = number_of_targets)) + geom_point() + geom_smooth() +
  geom_hline(yintercept = 2)

plot2 <- all_results %>% 
  filter(threshold == 75) %>% 
  filter(species_no < 50) %>%
  group_by(species_no) %>% 
  summarise_each(funs(mean), number_of_targets) %>% 
  ggplot(aes(x = species_no, y = number_of_targets)) + geom_point() + ylim(0,5) +
  geom_hline(yintercept = 2)

library(gridExtra)



plot1_100 <- all_results %>% 
  filter(threshold == 100) %>% 
  filter(species_no < 30) %>%
  ggplot(aes(x = species_no, y = number_of_targets)) + geom_point() + geom_smooth() +
  geom_hline(yintercept = 2)

plot2_100 <- all_results %>% 
  filter(threshold == 100) %>% 
  filter(species_no < 50) %>%
  group_by(species_no) %>% 
  summarise_each(funs(mean), number_of_targets) %>% 
  ggplot(aes(x = species_no, y = number_of_targets)) + geom_point() + ylim(0,5) +
  geom_hline(yintercept = 2)
plot_100 <- grid.arrange(plot1_100, plot2_100, ncol = 2)



plot_25 <- grid.arrange(plot1, plot2, ncol = 2)
plot_50 <- grid.arrange(plot1, plot2, ncol = 2)
plot_75 <- grid.arrange(plot1, plot2, ncol = 2)
plot_25


### next step instead of yeild in terms of distinct targets, 
# how about yields in terms of total progress towards a range of RDIs?

## also, let's try to get the figure that shows the amount of fish tissue required to reach a given number of RDI targets at the different levels of diversity
### and the figure that shows the 'null model' type expectations
## for how well the best monoculture would do at each level of fish tissue
## first step, find the best 'overall species', i.e. the one that has the most of all the nutrients



reps100 <- read_csv("data-processed/grams-required-15-spp-1000reps.csv")


reps_monoculture <- reps100 %>% 
  filter(species_no == 1)


View(trait_data)


subset <- trait_data %>% 
  select(species_name, nutrient, concentration) %>%
  filter(nutrient %in% c("ca_mg", "zn_mg", "fe_mg", "epa", "dha"))
  
unique(subset$nutrient)


trait_data_pro <- read_csv("data-processed/micronutrients-species-mean.csv")
## which species requires the least tissue to meet all 5 RDIs?

species_number <- 1

trait_data_pro %>% 
mutate(cal_total = (calcium/species_number)) %>% ## get the amount of calcium each species will contribute
  mutate(zinc_total = (zinc/species_number)) %>% 
  mutate(iron_total = (iron/species_number)) %>% 
  mutate(epa_total = (epa/species_number)) %>%
  mutate(dha_total = (dha/species_number)) %>% 
  mutate(cal_grams = (cal_total/(1200))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
  mutate(iron_grams = (iron_total/(18))) %>%
  mutate(zinc_grams = (zinc_total/(11))) %>% 
  mutate(epa_grams = (epa_total/(1))) %>%
  mutate(dha_grams = (dha_total/(1))) %>%
  select(-contains("total")) %>% 
  gather(key = nutrient, value = concentration, 3:7) %>% 
  group_by(species_name) %>% 
  mutate(grams_required = 100/concentration) %>% 
  group_by(species_name) %>% 
  mutate(total_grams_required = cumsum(grams_required)) %>% 
  group_by(species_name) %>% 
  top_n(n=1, wt = total_grams_required) %>% View

## ok so it looks like the best species is Fenneropenaeus indicus, in terms of reaching
## all 5 micronutrient targets in the least amount of tissue
## Puntius sophore is second best


trait_data_pro %>% 
  filter(species_name %in% c("Fenneropenaeus indicus", "Puntius sophore")) %>% View
