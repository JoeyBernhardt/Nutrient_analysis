## 10% accumulation curves
library(purrr)
library(vegan)

trait_data <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data-processed/n.long_lat3.csv")


data <- trait_data %>% 
  filter(subgroup == "crustacean") 


?specaccum

accumulate <- function(data, threshold) {
ntbl.RDI.all <- data %>% 
  filter(!grepl("^Mohanty", ref_info)) %>% 
  spread(nutrient, concentration) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(mean.CA = mean(ca_mg, na.rm = TRUE),
            mean.ZN = mean(zn_mg, na.rm = TRUE), 
            mean.FE = mean(fe_mg, na.rm = TRUE),
            mean.EPA = mean(epa, na.rm = TRUE),
            mean.DHA = mean(dha, na.rm = TRUE)) %>% 
  mutate(RDI.CA = ifelse(mean.CA > (1200*threshold), 1, 0)) %>% 
  mutate(RDI.FE = ifelse(mean.FE > (18*threshold), 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(mean.ZN > (11*threshold), 1, 0)) %>%
  mutate(RDI.EPA = ifelse(mean.EPA > (1*threshold), 1, 0)) %>% 
  mutate(RDI.DHA = ifelse(mean.DHA > (1*threshold), 1, 0)) %>% 
  ungroup() %>% 
  mutate(RDI.micro.tot = rowSums(.[8:12])) %>% 
  filter(!is.na(RDI.micro.tot)) 

all_spa <- ntbl.RDI.all %>%
  dplyr::select(-RDI.micro.tot) %>%
  dplyr::select(-contains("mean")) %>% 
  dplyr::select(-species_name) %>%
  mutate(subgroup = "all") %>% 
  split( .$subgroup) %>% 
  map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.EPA", "RDI.DHA")) %>%
  map(.f = specaccum, method = "random", permutations = 100000)


accumulated_targets <- all_spa %>% 
  map(.f = `[`, "richness") %>% 
  unlist() %>% 
  as.data.frame()

accumulated_targets_sd <- all_spa %>% 
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
return(accumulated_targets_all)

}


finfish_accumulation <- trait_data %>% 
  filter(subgroup == "finfish") %>% 
  accumulate(., threshold = 0.1) %>% 
  mutate(subgroup = "finfish") %>% 
  mutate(threshold = "10 percent")

invertebrate_accumulation <- trait_data %>% 
  filter(subgroup %in% c("crustacean", "mollusc")) %>% 
  accumulate(., threshold = 0.1) %>% 
  mutate(subgroup = "invertebrates") %>% 
  mutate(threshold = "10 percent")

all_accumulation_10 <- bind_rows(finfish_accumulation, invertebrate_accumulation)


finfish_accumulation_25 <- trait_data %>% 
  filter(subgroup == "finfish") %>% 
  accumulate(., threshold = 0.25) %>% 
  mutate(subgroup = "finfish") %>% 
  mutate(threshold = "25 percent")

invertebrate_accumulation_25 <- trait_data %>% 
  filter(subgroup %in% c("crustacean", "mollusc")) %>% 
  accumulate(., threshold = 0.25) %>% 
  mutate(subgroup = "invertebrates") %>% 
  mutate(threshold = "25 percent")

all_accumulation_25 <- bind_rows(finfish_accumulation_25, invertebrate_accumulation_25)



all_accumulation <- bind_rows(all_accumulation_10, all_accumulation_25)


write_csv(all_accumulation, "data-processed/27_all_accumulation.csv")

all_subset <- all_accumulation %>%
  filter(number_of_species < 11) %>% 
  filter(subgroup != "mollusc") 

threshold25 <- all_subset %>% 
  filter(threshold == "25 percent")
threshold10 <- all_subset %>% 
  filter(threshold == "10 percent")

  ggplot(data = threshold25, aes(x = number_of_species, y = number_of_targets, group = subgroup, color = threshold)) + geom_line(size =1) +
  geom_ribbon(aes(ymin = number_of_targets - se, ymax = number_of_targets + se), alpha = 0.2, size = 0, data = threshold25) +
  geom_line(aes(x = number_of_species, y = number_of_targets, group = subgroup, color = threshold), data = threshold10, linetype = "dashed", size = 1) +
  geom_ribbon(aes(ymin = number_of_targets - se, ymax = number_of_targets + se), alpha = 0.2, size = 0, data = threshold10) +
  ylab("number of nutrient requirements fulfilled (10% DRI)") +
  xlab("number of species") + theme(text = element_text(size=14)) + 
  # scale_color_grey(start = 0.01, end = 0.2) +
  theme_bw() +
  # theme(legend.position = c(0.6, 0.2)) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  theme(legend.title=element_blank())
  
  
  all_subset %>% 
    filter(threshold == "10 percent") %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets, group = subgroup, color = subgroup)) + geom_line(size =1.5) +
    geom_ribbon(aes(ymin = number_of_targets - se, ymax = number_of_targets + se, color = subgroup, group = subgroup), alpha = 0.2, size = 0) +
    ylab("number of nutrient requirements fulfilled (10% DRI)") +
    xlab("number of species") + theme(text = element_text(size=14)) + 
    scale_color_grey(start = 0.01, end = 0.7) +
    theme_bw() +
    # theme(legend.position = c(0.6, 0.2)) +
    scale_x_continuous(breaks = seq(1,10,1)) +
  theme(legend.title=element_blank())
