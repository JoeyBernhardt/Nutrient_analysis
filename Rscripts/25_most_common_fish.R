library(tidyverse)
library(readxl)
library(janitor)


fish <- read_csv("~/Desktop/FoodSupply_LivestockFish_E_All_Data.csv")

### this is the data from FAO on fish species with highest production
tonnes <- read_excel("~/Desktop/a1e.xlsx")

tonnes <- tonnes %>% 
  clean_names()

sub <- tonnes %>% 
  select(na_2) %>% 
  rownames_to_column() %>% 
  filter(rowname != c("1", "2", "3", "4")) %>% 
  filter(rowname != c("81", "82", "83", "84")) %>% 
  distinct(na_2) %>% 
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname)) %>% 
  filter(rowname < 77) %>% 
  rename(species_name = na_2)


trait_data <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data-processed/n.long_lat3.csv")

ntbl.RDI.all <- trait_data %>% 
  filter(!grepl("^Mohanty", ref_info)) %>% 
  spread(nutrient, concentration) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(mean.CA = mean(ca_mg, na.rm = TRUE),
            mean.ZN = mean(zn_mg, na.rm = TRUE), 
            mean.FE = mean(fe_mg, na.rm = TRUE),
            mean.EPA = mean(epa, na.rm = TRUE),
            mean.DHA = mean(dha, na.rm = TRUE)) %>% 
  mutate(RDI.CA = ifelse(mean.CA > 300, 1, 0)) %>% 
  mutate(RDI.FE = ifelse(mean.FE > 4.5, 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(mean.ZN > 2.75, 1, 0)) %>%
  mutate(RDI.EPA = ifelse(mean.EPA > 0.25, 1, 0)) %>% 
  mutate(RDI.DHA = ifelse(mean.DHA > 0.25, 1, 0)) %>% 
  ungroup() %>% 
  mutate(RDI.micro.tot = rowSums(.[8:12])) %>% 
  filter(!is.na(RDI.micro.tot)) 


most_common <- left_join(sub, ntbl.RDI.all)

most_common <- most_common %>% 
  filter(!is.na(RDI.micro.tot)) 

write_csv(most_common, "data-processed/most_common_species.csv")


most_common <- read_csv("data-processed/most_common_species.csv")


mc2 <- most_common %>% 
  summarise_each(funs(mean), contains("mean")) %>%
  gather(key = nutrient, value = concentration) %>% 
  rownames_to_column()


d <- c(1:15)

get_to_amount <- function(x) {
  df2 <- mc2 %>% 
    mutate(concentration_per_portion = concentration *x) %>% 
    mutate(amount = x)
}


output <- d %>% 
  map_df(get_to_amount)

mish_mash <- output %>%
  select(amount, nutrient, concentration_per_portion) %>% 
  spread(key = nutrient, value = concentration_per_portion) %>% 
  mutate(RDI.CA = ifelse(mean.CA > 300, 1, 0)) %>% 
  mutate(RDI.FE = ifelse(mean.FE > 4.5, 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(mean.ZN > 2.75, 1, 0)) %>%
  mutate(RDI.EPA = ifelse(mean.EPA > 0.25, 1, 0)) %>% 
  mutate(RDI.DHA = ifelse(mean.DHA > 0.25, 1, 0)) %>% 
  ungroup() %>% 
  mutate(RDI.micro.tot = rowSums(.[7:11])) %>% 
  filter(!is.na(RDI.micro.tot)) %>% 
  mutate(group = "most_common")


accumulation_data <- read_csv("data-processed/all_accumulation_curve_data.csv")


accumulation_data %>%
  filter(number_of_species < 15) %>% 
  rename(group = subgroup) %>% 
  filter(group %in% c("mollusc", "finfish", "crustacean", "all")) %>% 
  mutate(group = str_replace(group, "finfish", "finfish only, no invertebrates")) %>% 
  mutate(group = str_replace(group, "all", "combined, finfish and invertebrates")) %>% 
  mutate(group = str_replace(group, "crustacean", "only crusteaceans")) %>% 
  mutate(group = str_replace(group, "mollusc", "only molluscs")) %>% 
  filter(group != "only crusteaceans") %>% 
  ggplot(data = ., aes(x = number_of_species, y = number_of_targets, color = group)) +
  geom_line(aes(linetype = group), size = 1) +
  # scale_linetype_manual(values = c("dotted", "dashed", "solid", "dot-dash")) +
  theme_bw() +
  geom_ribbon(aes(ymin = number_of_targets - se, ymax = number_of_targets + se), alpha = 0.2, size = 0) +
  geom_line(aes(x = amount, y = RDI.micro.tot), data = mish_mash)+
  ylab("number of nutrient requirements fulfilled") +
  xlab("number of species") + theme(text = element_text(size=16)) + 
  scale_color_grey(start = 0.01, end = 0.5) +
  theme_bw() 



library(tidyverse)
library(vegan)

most_common <- read_csv("data-processed/most_common_species.csv")



mostcommon_spa <- most_common %>%
  dplyr::select(-RDI.micro.tot) %>%
  dplyr::select(-contains("mean")) %>% 
  dplyr::select(-species_name) %>%
  mutate(subgroup = "all") %>% 
  split( .$subgroup) %>% 
  map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.EPA", "RDI.DHA")) %>%
  map(.f = specaccum, method = "random")


accumulated_targets <- mostcommon_spa %>% 
  map(.f = `[`, "richness") %>% 
  unlist() %>% 
  as.data.frame()

accumulated_targets_sd <- mostcommon_spa %>% 
  map(.f = `[`, "sd") %>% 
  unlist() %>% 
  as.data.frame()

accumulated_targets$richness_level = rownames(accumulated_targets)
colnames(accumulated_targets) <- c("number_of_targets", "richness_level")

accumulated_targets_sd$sd = rownames(accumulated_targets_sd)
colnames(accumulated_targets_sd) <- c("sd", "number_of_targets")

accumulated_targets_sd <- accumulated_targets_sd %>% 
  separate(number_of_targets, into = c("subgroup", "number_of_species")) %>%
  mutate(number_of_species = str_replace(number_of_species, "sd", "")) %>%
  mutate(number_of_species = as.numeric(number_of_species))


accumulated_targets <- accumulated_targets %>% 
  separate(richness_level, into = c("subgroup", "number_of_species")) %>%
  mutate(number_of_species = str_replace(number_of_species, "richness", "")) %>%
  mutate(number_of_species = as.numeric(number_of_species))

accumulated_targets_all <- left_join(accumulated_targets, accumulated_targets_sd)
accumulated_targets_all <- accumulated_targets_all %>% 
  mutate(se = sd / sqrt(number_of_species)) 


most_common_accumulated_targets <- accumulated_targets_all

write_csv(most_common_accumulated_targets, "data-processed/most_common_accumulated_targets.csv")

