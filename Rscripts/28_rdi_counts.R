library(tidyverse)
library(purrr)
library(janitor)
library(xtable)


data <- read_csv("data-processed/micronutrients-species-mean.csv")

percentage <- 0.1
data %>% 
  mutate(RDI.CA = ifelse(calcium > (1200*percentage), 1, 0)) %>% 
  mutate(RDI.FE = ifelse(iron > (18*percentage), 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(zinc > (11*percentage), 1, 0)) %>%
  mutate(RDI.EPA = ifelse(epa > (1*percentage), 1, 0)) %>% 
  mutate(RDI.DHA = ifelse(dha > (1*percentage), 1, 0)) %>% 
  ungroup() %>% 
  mutate(RDI.micro.tot = rowSums(.[8:12])) %>% 
  filter(!is.na(RDI.micro.tot)) %>% 
  group_by(subgroup, RDI.micro.tot) %>% 
  tally() %>% 
  group_by(subgroup) %>% 
  mutate(total = cumsum(n)) %>% 
  mutate(total_spp = NA) %>% 
  mutate(total_spp = ifelse(subgroup == "finfish", 78, total_spp)) %>% 
  mutate(total_spp = ifelse(subgroup == "mollusc", 12, total_spp)) %>% 
  mutate(total_spp = ifelse(subgroup == "crustacean", 6, total_spp)) %>% 
  mutate(proportion_that_reach_RDI = n*100/total_spp) %>% 
  xtable(type = "latex", digits = 0)


data %>% 
  mutate(RDI.CA = ifelse(calcium > (1200*percentage), 1, 0)) %>% 
  mutate(RDI.FE = ifelse(iron > (18*percentage), 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(zinc > (11*percentage), 1, 0)) %>%
  ungroup() %>% 
  mutate(RDI.micro.tot = rowSums(.[8:10])) %>% 
  # filter(!is.na(RDI.micro.tot)) %>% 
  group_by(subgroup, RDI.micro.tot) %>% 
  tally() %>% View

 

data %>% 
  filter(iron > 1.8) %>% 
  tally()

data %>% 
  mutate(RDI.CA = ifelse(calcium > (1200*percentage), 1, 0)) %>% 
  mutate(RDI.FE = ifelse(iron > (18*percentage), 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(zinc > (11*percentage), 1, 0)) %>%
  mutate(RDI.EPA = ifelse(epa > (1*percentage), 1, 0)) %>% 
  mutate(RDI.DHA = ifelse(dha > (1*percentage), 1, 0)) %>% 
  summarise_each(funs(sum), contains("RDI")) %>% View



trait_data <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data-processed/n.long_lat3.csv")

trait_data2 <- trait_data %>% 
  filter(!grepl("^Mohanty", ref_info))

wide <- trait_data2 %>% 
  select(species_name, subgroup, seanuts_id2, nutrient, concentration, ref_info) %>% 
  distinct(species_name, nutrient, concentration, .keep_all = TRUE) %>% 
  spread(key = nutrient, value = concentration) 

wide2 <- wide %>% 
  group_by(species_name, subgroup) %>% 
  summarise(mean.CA = mean(ca_mg, na.rm = TRUE),
            mean.ZN = mean(zn_mg, na.rm = TRUE), 
            mean.FE = mean(fe_mg, na.rm = TRUE),
            mean.EPA = mean(epa, na.rm = TRUE),
            mean.DHA = mean(dha, na.rm = TRUE), 
            mean.protein = mean(protcnt_g, na.rm = TRUE),
            mean.fat = mean(fat_g, na.rm = TRUE)) 


rdis <- wide2 %>% 
  mutate(RDI.CA = ifelse(mean.CA > (1200*percentage), 1, 0)) %>% 
  mutate(RDI.FE = ifelse(mean.FE > (18*percentage), 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(mean.ZN > (11*percentage), 1, 0)) %>%
  mutate(RDI.EPA = ifelse(mean.EPA > (1*percentage), 1, 0)) %>% 
  mutate(RDI.DHA = ifelse(mean.DHA > (1*percentage), 1, 0)) %>%
  mutate(RDI.fat = ifelse(mean.fat > (70*percentage), 1, 0)) %>%
  mutate(RDI.protein = ifelse(mean.protein > (56*percentage), 1, 0)) %>% 
  ungroup() %>% 
  mutate(RDI.micro.tot = rowSums(.[10:14])) 


### proportion that reach RDI targets
rdis %>% 
  gather(key = nutrient, value = rdi, 10:16) %>% 
  filter(!is.na(rdi)) %>% 
  select(-contains("mean")) %>% 
  group_by(nutrient, rdi, subgroup) %>% 
  tally() %>% 
  group_by(nutrient, subgroup) %>% 
  mutate(total = cumsum(n)) %>% 
  filter(rdi == 1) %>% 
  mutate(proportion_that_reach_RDI = n*100/total) %>% 
  xtable(type = "latex", digits = 0)
 
rdis1 <- rdis %>% 
  gather(key = nutrient, value = rdi, 10:16) %>% 
  filter(!is.na(rdi)) %>% 
  mutate(nutrient = str_replace(nutrient, "RDI.ZN", "zinc")) %>% 
  mutate(nutrient = str_replace(nutrient, "RDI.FE", "iron")) %>% 
  mutate(nutrient = str_replace(nutrient, "RDI.CA", "calcium")) %>%
  mutate(nutrient = str_replace(nutrient, "RDI.fat", "fat")) %>% 
  mutate(nutrient = str_replace(nutrient, "RDI.DHA", "DHA")) %>% 
  mutate(nutrient = str_replace(nutrient, "RDI.EPA", "EPA")) %>%
  mutate(nutrient = str_replace(nutrient, "RDI.protein", "protein"))
  
  
  
  

rdis1$nutrient <- factor(rdis1$nutrient, levels = c("protein", "EPA", "zinc", "iron", "DHA", "fat", "calcium"))


rdis1 %>% 
  select(-contains("mean")) %>% 
  group_by(nutrient, rdi, subgroup) %>% 
  tally() %>% 
  group_by(nutrient, subgroup) %>% 
  mutate(total = cumsum(n)) %>% 
  filter(rdi == 1) %>% 
  mutate(proportion_that_reach_RDI = n*100/total) %>% 
  ggplot(aes(nutrient, proportion_that_reach_RDI, group = subgroup, color = nutrient, fill = nutrient)) + geom_bar(position = "dodge", stat="identity") +
  facet_wrap( ~ subgroup) +
  geom_hline(yintercept = 50, color = "grey") +
  coord_polar() + ylab("percentage of species that reach 10% DRI") + xlab("")
ggsave("figures/flower_diagram_percentage_reach_DRI.png")


rdis %>% 
  gather(key = nutrient, value = rdi, 10:16) %>% 
  filter(!is.na(rdi)) %>% 
  select(species_name, subgroup, RDI.micro.tot) %>%
  distinct(species_name, .keep_all = TRUE) %>% 
  filter(!is.na(RDI.micro.tot)) %>% View
