
library(purrr)
library(tidyverse)
library(broom)


trait_data <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data-processed/n.long_lat3.csv")

trait_data %>% 
  filter(species_name == "Hexaplex trunculus") %>% View

ntbl.RDI.25 <- trait_data %>% 
  spread(nutrient, concentration) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(mean.CA = mean(ca_mg, na.rm = TRUE),
            mean.ZN = mean(zn_mg, na.rm = TRUE), 
            mean.FE = mean(fe_mg, na.rm = TRUE),
            mean.EPA = mean(epa, na.rm = TRUE),
            mean.DHA = mean(dha, na.rm = TRUE)) %>% 
  mutate(RDI.CA = ifelse(mean.CA > (1200*0.25), 1, 0)) %>% 
  mutate(RDI.FE = ifelse(mean.FE > (18*0.25), 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(mean.ZN > (11*0.25), 1, 0)) %>%
  mutate(RDI.EPA = ifelse(mean.EPA > (1*0.25), 1, 0)) %>% 
  mutate(RDI.DHA = ifelse(mean.DHA > (1*0.25), 1, 0)) %>% 
  ungroup() %>% 
  mutate(RDI.micro.tot = rowSums(.[8:12])) %>% 
  filter(!is.na(RDI.micro.tot)) 


View(ntbl.RDI.25)

ntbl.RDI.50 <- trait_data %>% 
  spread(nutrient, concentration) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(mean.CA = mean(ca_mg, na.rm = TRUE),
            mean.ZN = mean(zn_mg, na.rm = TRUE), 
            mean.FE = mean(fe_mg, na.rm = TRUE),
            mean.EPA = mean(epa, na.rm = TRUE),
            mean.DHA = mean(dha, na.rm = TRUE)) %>% 
  mutate(RDI.CA = ifelse(mean.CA > (1200*0.50), 1, 0)) %>% 
  mutate(RDI.FE = ifelse(mean.FE > (18*0.50), 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(mean.ZN > (11*0.50), 1, 0)) %>%
  mutate(RDI.EPA = ifelse(mean.EPA > (1*0.50), 1, 0)) %>% 
  mutate(RDI.DHA = ifelse(mean.DHA > (1*0.50), 1, 0)) %>% 
  ungroup() %>% 
  mutate(RDI.micro.tot = rowSums(.[8:12])) %>% 
  filter(!is.na(RDI.micro.tot)) 

ntbl.RDI.75 <- trait_data %>% 
  spread(nutrient, concentration) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(mean.CA = mean(ca_mg, na.rm = TRUE),
            mean.ZN = mean(zn_mg, na.rm = TRUE), 
            mean.FE = mean(fe_mg, na.rm = TRUE),
            mean.EPA = mean(epa, na.rm = TRUE),
            mean.DHA = mean(dha, na.rm = TRUE)) %>% 
  mutate(RDI.CA = ifelse(mean.CA > (1200*0.75), 1, 0)) %>% 
  mutate(RDI.FE = ifelse(mean.FE > (18*0.75), 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(mean.ZN > (11*0.75), 1, 0)) %>%
  mutate(RDI.EPA = ifelse(mean.EPA > (1*0.75), 1, 0)) %>% 
  mutate(RDI.DHA = ifelse(mean.DHA > (1*0.75), 1, 0)) %>% 
  ungroup() %>% 
  mutate(RDI.micro.tot = rowSums(.[8:12])) %>% 
  filter(!is.na(RDI.micro.tot)) 



all_spa_25 <- ntbl.RDI.25 %>%
  dplyr::select(-RDI.micro.tot) %>%
  dplyr::select(-contains("mean")) %>% 
  dplyr::select(-species_name) %>%
  mutate(subgroup = "all") %>% 
  split( .$subgroup) %>% 
  map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.EPA", "RDI.DHA")) %>%
  map(.f = specaccum, method = "random")


accumulated_targets_full.25 <- all_spa_25 %>% 
  map(.f = `[`, "richness") %>% 
  unlist() %>% 
  as.data.frame()
accumulated_targets_full.25$richness_level = rownames(accumulated_targets_full.25)
colnames(accumulated_targets_full.25) <- c("number_of_targets", "richness_level")

accumulated_targets_full.25 <- accumulated_targets_full.25 %>% 
  separate(richness_level, into = c("subgroup", "number_of_species")) %>%
  mutate(number_of_species = str_replace(number_of_species, "richness", "")) %>%
  mutate(number_of_species = as.numeric(number_of_species)) %>% 
  mutate(threshold = 25) %>% 
  dplyr::select(-subgroup)



# now for 50% -------------------------------------------------------------

all_spa_50 <- ntbl.RDI.50 %>%
  dplyr::select(-RDI.micro.tot) %>%
  dplyr::select(-contains("mean")) %>% 
  dplyr::select(-species_name) %>%
  mutate(subgroup = "all") %>% 
  split( .$subgroup) %>% 
  map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.EPA", "RDI.DHA")) %>%
  map(.f = specaccum, method = "random")


accumulated_targets_full.50 <- all_spa_50 %>% 
  map(.f = `[`, "richness") %>% 
  unlist() %>% 
  as.data.frame()
accumulated_targets_full.50$richness_level = rownames(accumulated_targets_full.50)
colnames(accumulated_targets_full.50) <- c("number_of_targets", "richness_level")

accumulated_targets_full.50 <- accumulated_targets_full.50 %>% 
  separate(richness_level, into = c("subgroup", "number_of_species")) %>%
  mutate(number_of_species = str_replace(number_of_species, "richness", "")) %>%
  mutate(number_of_species = as.numeric(number_of_species)) %>% 
  mutate(threshold = 50) %>% 
  dplyr::select(-subgroup)


# now for 75% -------------------------------------------------------------

all_spa_75 <- ntbl.RDI.75 %>%
  dplyr::select(-RDI.micro.tot) %>%
  dplyr::select(-contains("mean")) %>% 
  dplyr::select(-species_name) %>%
  mutate(subgroup = "all") %>% 
  split( .$subgroup) %>% 
  map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.EPA", "RDI.DHA")) %>%
  map(.f = specaccum, method = "random")


accumulated_targets_full.75 <- all_spa_75 %>% 
  map(.f = `[`, "richness") %>% 
  unlist() %>% 
  as.data.frame()
accumulated_targets_full.75$richness_level = rownames(accumulated_targets_full.75)
colnames(accumulated_targets_full.75) <- c("number_of_targets", "richness_level")

accumulated_targets_full.75 <- accumulated_targets_full.75 %>% 
  separate(richness_level, into = c("subgroup", "number_of_species")) %>%
  mutate(number_of_species = str_replace(number_of_species, "richness", "")) %>%
  mutate(number_of_species = as.numeric(number_of_species)) %>% 
  mutate(threshold = 75) %>% 
  dplyr::select(-subgroup)


# now for 100 -------------------------------------------------------------

ntbl.RDI.100 <- trait_data %>% 
  spread(nutrient, concentration) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(mean.CA = mean(ca_mg, na.rm = TRUE),
            mean.ZN = mean(zn_mg, na.rm = TRUE), 
            mean.FE = mean(fe_mg, na.rm = TRUE),
            mean.EPA = mean(epa, na.rm = TRUE),
            mean.DHA = mean(dha, na.rm = TRUE)) %>% 
  mutate(RDI.CA = ifelse(mean.CA > (1200*1), 1, 0)) %>% 
  mutate(RDI.FE = ifelse(mean.FE > (18*1), 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(mean.ZN > (11*1), 1, 0)) %>%
  mutate(RDI.EPA = ifelse(mean.EPA > (1*1), 1, 0)) %>% 
  mutate(RDI.DHA = ifelse(mean.DHA > (1*1), 1, 0)) %>% 
  ungroup() %>% 
  mutate(RDI.micro.tot = rowSums(.[8:12])) %>% 
  filter(!is.na(RDI.micro.tot)) 

all_spa_100 <- ntbl.RDI.100 %>%
  dplyr::select(-RDI.micro.tot) %>%
  dplyr::select(-contains("mean")) %>% 
  dplyr::select(-species_name) %>%
  mutate(subgroup = "all") %>% 
  split( .$subgroup) %>% 
  map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.EPA", "RDI.DHA")) %>%
  map(.f = specaccum, method = "random")


accumulated_targets_full.100 <- all_spa_100 %>% 
  map(.f = `[`, "richness") %>% 
  unlist() %>% 
  as.data.frame()
accumulated_targets_full.100$richness_level = rownames(accumulated_targets_full.100)
colnames(accumulated_targets_full.100) <- c("number_of_targets", "richness_level")

accumulated_targets_full.100 <- accumulated_targets_full.100 %>% 
  separate(richness_level, into = c("subgroup", "number_of_species")) %>%
  mutate(number_of_species = str_replace(number_of_species, "richness", "")) %>%
  mutate(number_of_species = as.numeric(number_of_species)) %>% 
  mutate(threshold = 100) %>% 
  dplyr::select(-subgroup)


# now for 10 --------------------------------------------------------------

ntbl.RDI.10 <- trait_data %>% 
  spread(nutrient, concentration) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(mean.CA = mean(ca_mg, na.rm = TRUE),
            mean.ZN = mean(zn_mg, na.rm = TRUE), 
            mean.FE = mean(fe_mg, na.rm = TRUE),
            mean.EPA = mean(epa, na.rm = TRUE),
            mean.DHA = mean(dha, na.rm = TRUE)) %>% 
  mutate(RDI.CA = ifelse(mean.CA > (1200*0.1), 1, 0)) %>% 
  mutate(RDI.FE = ifelse(mean.FE > (18*0.1), 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(mean.ZN > (11*0.1), 1, 0)) %>%
  mutate(RDI.EPA = ifelse(mean.EPA > (1*0.1), 1, 0)) %>% 
  mutate(RDI.DHA = ifelse(mean.DHA > (1*0.1), 1, 0)) %>% 
  ungroup() %>% 
  mutate(RDI.micro.tot = rowSums(.[8:12])) %>% 
  filter(!is.na(RDI.micro.tot)) 

all_spa_10 <- ntbl.RDI.10 %>%
  dplyr::select(-RDI.micro.tot) %>%
  dplyr::select(-contains("mean")) %>% 
  dplyr::select(-species_name) %>%
  mutate(subgroup = "all") %>% 
  split( .$subgroup) %>% 
  map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.EPA", "RDI.DHA")) %>%
  map(.f = specaccum, method = "random")


accumulated_targets_full.10 <- all_spa_10 %>% 
  map(.f = `[`, "richness") %>% 
  unlist() %>% 
  as.data.frame()
accumulated_targets_full.10$richness_level = rownames(accumulated_targets_full.10)
colnames(accumulated_targets_full.10) <- c("number_of_targets", "richness_level")

accumulated_targets_full.10 <- accumulated_targets_full.10 %>% 
  separate(richness_level, into = c("subgroup", "number_of_species")) %>%
  mutate(number_of_species = str_replace(number_of_species, "richness", "")) %>%
  mutate(number_of_species = as.numeric(number_of_species)) %>% 
  mutate(threshold = 10) %>% 
  dplyr::select(-subgroup)

# now for 30 -------------------------------------------------------------

ntbl.RDI.30 <- trait_data %>% 
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

all_spa_30 <- ntbl.RDI.30 %>%
  dplyr::select(-RDI.micro.tot) %>%
  dplyr::select(-contains("mean")) %>% 
  dplyr::select(-species_name) %>%
  mutate(subgroup = "all") %>% 
  split( .$subgroup) %>% 
  map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.EPA", "RDI.DHA")) %>%
  map(.f = specaccum, method = "random")


accumulated_targets_full.30 <- all_spa_30 %>% 
  map(.f = `[`, "richness") %>% 
  unlist() %>% 
  as.data.frame()
accumulated_targets_full.30$richness_level = rownames(accumulated_targets_full.30)
colnames(accumulated_targets_full.30) <- c("number_of_targets", "richness_level")

accumulated_targets_full.30 <- accumulated_targets_full.30 %>% 
  separate(richness_level, into = c("subgroup", "number_of_species")) %>%
  mutate(number_of_species = str_replace(number_of_species, "richness", "")) %>%
  mutate(number_of_species = as.numeric(number_of_species)) %>% 
  mutate(threshold = 30) %>% 
  dplyr::select(-subgroup)

# now for 60 -------------------------------------------------------------

ntbl.RDI.60 <- trait_data %>% 
  spread(nutrient, concentration) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(mean.CA = mean(ca_mg, na.rm = TRUE),
            mean.ZN = mean(zn_mg, na.rm = TRUE), 
            mean.FE = mean(fe_mg, na.rm = TRUE),
            mean.EPA = mean(epa, na.rm = TRUE),
            mean.DHA = mean(dha, na.rm = TRUE)) %>% 
  mutate(RDI.CA = ifelse(mean.CA > (1200*0.6), 1, 0)) %>% 
  mutate(RDI.FE = ifelse(mean.FE > (18*0.6), 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(mean.ZN > (11*0.6), 1, 0)) %>%
  mutate(RDI.EPA = ifelse(mean.EPA > (1*0.6), 1, 0)) %>% 
  mutate(RDI.DHA = ifelse(mean.DHA > (1*0.6), 1, 0)) %>% 
  ungroup() %>% 
  mutate(RDI.micro.tot = rowSums(.[8:12])) %>% 
  filter(!is.na(RDI.micro.tot)) 

all_spa_60 <- ntbl.RDI.60 %>%
  dplyr::select(-RDI.micro.tot) %>%
  dplyr::select(-contains("mean")) %>% 
  dplyr::select(-species_name) %>%
  mutate(subgroup = "all") %>% 
  split( .$subgroup) %>% 
  map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.EPA", "RDI.DHA")) %>%
  map(.f = specaccum, method = "random")


accumulated_targets_full.60 <- all_spa_60 %>% 
  map(.f = `[`, "richness") %>% 
  unlist() %>% 
  as.data.frame()
accumulated_targets_full.60$richness_level = rownames(accumulated_targets_full.60)
colnames(accumulated_targets_full.60) <- c("number_of_targets", "richness_level")

accumulated_targets_full.60 <- accumulated_targets_full.60 %>% 
  separate(richness_level, into = c("subgroup", "number_of_species")) %>%
  mutate(number_of_species = str_replace(number_of_species, "richness", "")) %>%
  mutate(number_of_species = as.numeric(number_of_species)) %>% 
  mutate(threshold = 60) %>% 
  dplyr::select(-subgroup)

# now for 5 -------------------------------------------------------------

ntbl.RDI.5 <- trait_data %>% 
  spread(nutrient, concentration) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(mean.CA = mean(ca_mg, na.rm = TRUE),
            mean.ZN = mean(zn_mg, na.rm = TRUE), 
            mean.FE = mean(fe_mg, na.rm = TRUE),
            mean.EPA = mean(epa, na.rm = TRUE),
            mean.DHA = mean(dha, na.rm = TRUE)) %>% 
  mutate(RDI.CA = ifelse(mean.CA > (1200*0.05), 1, 0)) %>% 
  mutate(RDI.FE = ifelse(mean.FE > (18*0.05), 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(mean.ZN > (11*0.05), 1, 0)) %>%
  mutate(RDI.EPA = ifelse(mean.EPA > (1*0.05), 1, 0)) %>% 
  mutate(RDI.DHA = ifelse(mean.DHA > (1*0.05), 1, 0)) %>% 
  ungroup() %>% 
  mutate(RDI.micro.tot = rowSums(.[8:12])) %>% 
  filter(!is.na(RDI.micro.tot)) 

all_spa_5 <- ntbl.RDI.5 %>%
  dplyr::select(-RDI.micro.tot) %>%
  dplyr::select(-contains("mean")) %>% 
  dplyr::select(-species_name) %>%
  mutate(subgroup = "all") %>% 
  split( .$subgroup) %>% 
  map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.EPA", "RDI.DHA")) %>%
  map(.f = specaccum, method = "random")


accumulated_targets_full.5 <- all_spa_5 %>% 
  map(.f = `[`, "richness") %>% 
  unlist() %>% 
  as.data.frame()
accumulated_targets_full.5$richness_level = rownames(accumulated_targets_full.5)
colnames(accumulated_targets_full.5) <- c("number_of_targets", "richness_level")

accumulated_targets_full.5 <- accumulated_targets_full.5 %>% 
  separate(richness_level, into = c("subgroup", "number_of_species")) %>%
  mutate(number_of_species = str_replace(number_of_species, "richness", "")) %>%
  mutate(number_of_species = as.numeric(number_of_species)) %>% 
  mutate(threshold = 5) %>% 
  dplyr::select(-subgroup)

# now for 95 -------------------------------------------------------------

ntbl.RDI.95 <- trait_data %>% 
  spread(nutrient, concentration) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(mean.CA = mean(ca_mg, na.rm = TRUE),
            mean.ZN = mean(zn_mg, na.rm = TRUE), 
            mean.FE = mean(fe_mg, na.rm = TRUE),
            mean.EPA = mean(epa, na.rm = TRUE),
            mean.DHA = mean(dha, na.rm = TRUE)) %>% 
  mutate(RDI.CA = ifelse(mean.CA > (1200*0.95), 1, 0)) %>% 
  mutate(RDI.FE = ifelse(mean.FE > (18*0.95), 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(mean.ZN > (11*0.95), 1, 0)) %>%
  mutate(RDI.EPA = ifelse(mean.EPA > (1*0.95), 1, 0)) %>% 
  mutate(RDI.DHA = ifelse(mean.DHA > (1*0.95), 1, 0)) %>% 
  ungroup() %>% 
  mutate(RDI.micro.tot = rowSums(.[8:12])) %>% 
  filter(!is.na(RDI.micro.tot)) 

all_spa_95 <- ntbl.RDI.95 %>%
  dplyr::select(-RDI.micro.tot) %>%
  dplyr::select(-contains("mean")) %>% 
  dplyr::select(-species_name) %>%
  mutate(subgroup = "all") %>% 
  split( .$subgroup) %>% 
  map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.EPA", "RDI.DHA")) %>%
  map(.f = specaccum, method = "random")


accumulated_targets_full.95 <- all_spa_95 %>% 
  map(.f = `[`, "richness") %>% 
  unlist() %>% 
  as.data.frame()
accumulated_targets_full.95$richness_level = rownames(accumulated_targets_full.95)
colnames(accumulated_targets_full.95) <- c("number_of_targets", "richness_level")

accumulated_targets_full.95 <- accumulated_targets_full.95 %>% 
  separate(richness_level, into = c("subgroup", "number_of_species")) %>%
  mutate(number_of_species = str_replace(number_of_species, "richness", "")) %>%
  mutate(number_of_species = as.numeric(number_of_species)) %>% 
  mutate(threshold = 95) %>% 
  dplyr::select(-subgroup)

# now all together --------------------------------------------------------

all_targets <- bind_rows(accumulated_targets_full.95, accumulated_targets_full.5, accumulated_targets_full.60, accumulated_targets_full.30, accumulated_targets_full.10, accumulated_targets_full.25, accumulated_targets_full.50, accumulated_targets_full.75, accumulated_targets_full.100)


all_targets %>% 
  filter(number_of_species < 30) %>% 
  # filter(threshold == 100) %>% 
  ggplot(aes(x = log(number_of_species), y = log(number_of_targets), color = threshold, group = threshold)) + geom_line() + theme_bw() +
  geom_hline(yintercept = 2) +
  scale_color_gradient(name="Percent of DRI", low="blue", high="red") + xlab("log species richness") + ylab("log number of functions (distinct DRI targets reached)")


all_targets %>% 
  filter(number_of_species < 30) %>% 
  group_by(threshold) %>% 
  do(tidy(lm(log(number_of_targets) ~ log(number_of_species), data =.), conf.int = TRUE)) %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = threshold, y = estimate)) + geom_point() + geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + theme_bw() +
  xlab("threshold (DRI percentage)") + ylab("effect of adding one species to diet (linear coefficient)")


