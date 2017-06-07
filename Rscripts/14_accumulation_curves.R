### nutrient accumulation curves


# packages ----------------------------------------------------------------

library(stringr)
library(purrr)
library(janitor)
library(broom)
library(forcats)
library(tidyverse)
library(vegan)




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


ntbl.RDI.all %>% 
  filter(subgroup == "crustacean") %>% View


all %>%
filter(number_of_species < 15) %>% 
rename(group = subgroup) %>% 
filter(group %in% c("crustacean", "finfish", "mollusc")) %>% 
ggplot(data = ., aes(x = number_of_species, y = number_of_targets, color = group)) + geom_line(size =1, linetype = "dotted") + theme_bw() +
geom_ribbon(aes(ymin = number_of_targets - se, ymax = number_of_targets + se), alpha = 0.2, size = 1) +
ylab("number of nutrient requirements fulfilled") +
xlab("number of species") + theme(text = element_text(size=14)) 




all %>% 
mutate(upperCI = number_of_targets + (1.96*se)) %>% 
mutate(lowerCI = number_of_targets - (1.96*se)) %>% 
filter(number_of_species < 15) %>% 
rename(group = subgroup) %>% 
filter(group %in% c("crustacean", "finfish", "mollusc")) %>% 
ggplot(data = ., aes(x = number_of_species, y = number_of_targets, color = group)) + geom_line(size =1, linetype = "dotted") + theme_bw() +
geom_ribbon(aes(ymin = lowerCI, ymax = upperCI), alpha = 0.2, size = 1) +
ylab("number of nutrient requirements fulfilled") +
xlab("number of species") + theme(text = element_text(size=14)) 


#### how many species do you need to sample before reaching 3 RDI targets?
### create spa curves for each of the subgroups individually

most_common <- read_csv("data-processed/most_common_species.csv")


subgroup_spa <- ntbl.RDI.all %>%
	dplyr::select(-RDI.micro.tot) %>%
	dplyr::select(-contains("mean")) %>% 
	dplyr::select(-species_name) %>%
	split( .$subgroup) %>% 
	map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.EPA", "RDI.DHA")) %>%
	map(.f = specaccum, method = "random")


all_spa <- ntbl.RDI.all %>%
	dplyr::select(-RDI.micro.tot) %>%
	dplyr::select(-contains("mean")) %>% 
	dplyr::select(-species_name) %>%
	mutate(subgroup = "all") %>% 
	split( .$subgroup) %>% 
	map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.EPA", "RDI.DHA")) %>%
	map(.f = specaccum, method = "random")

mostcommon_spa <- most_common %>%
  dplyr::select(-RDI.micro.tot) %>%
  dplyr::select(-contains("mean")) %>% 
  dplyr::select(-species_name) %>%
  mutate(subgroup = "all") %>% 
  split( .$subgroup) %>% 
  map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.EPA", "RDI.DHA")) %>%
  map(.f = specaccum, method = "random")


without_crustaceans <- ntbl.RDI.all %>%
	dplyr::select(-RDI.micro.tot) %>%
	dplyr::select(-contains("mean")) %>% 
	dplyr::select(-species_name) %>%
	filter(subgroup == "finfish") %>% 
	mutate(subgroup = "noinverts") %>% 
	split( .$subgroup) %>% 
	map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.EPA", "RDI.DHA")) %>%
	map(.f = specaccum, method = "random")


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

## add in the all accumulation curve

accumulated_targets_full <- all_spa %>% 
	map(.f = `[`, "richness") %>% 
	unlist() %>% 
	as.data.frame()

accumulated_targets_full_sd <- all_spa %>% 
	map(.f = `[`, "sd") %>% 
	unlist() %>% 
	as.data.frame()

accumulated_targets_full$richness_level = rownames(accumulated_targets_full)
colnames(accumulated_targets_full) <- c("number_of_targets", "richness_level")

accumulated_targets_full_sd$sd = rownames(accumulated_targets_full_sd)
colnames(accumulated_targets_full_sd) <- c("sd", "number_of_targets")

accumulated_targets_full_sd <- accumulated_targets_full_sd %>% 
	separate(number_of_targets, into = c("subgroup", "number_of_species")) %>%
	mutate(number_of_species = str_replace(number_of_species, "sd", "")) %>%
	mutate(number_of_species = as.numeric(number_of_species))


accumulated_targets_full <- accumulated_targets_full %>% 
	separate(richness_level, into = c("subgroup", "number_of_species")) %>%
	mutate(number_of_species = str_replace(number_of_species, "richness", "")) %>%
	mutate(number_of_species = as.numeric(number_of_species))

accumulated_targets_all_full <- left_join(accumulated_targets_full, accumulated_targets_full_sd)
accumulated_targets_all_full <- accumulated_targets_all_full %>% 
	mutate(se = sd / sqrt(number_of_species)) 


## add in the  without crustaceans accumulation curve

accumulated_targets_nc <- without_crustaceans %>% 
	map(.f = `[`, "richness") %>% 
	unlist() %>% 
	as.data.frame()

accumulated_targets_nc_sd <- without_crustaceans %>% 
	map(.f = `[`, "sd") %>% 
	unlist() %>% 
	as.data.frame()

accumulated_targets_nc$richness_level = rownames(accumulated_targets_nc)
colnames(accumulated_targets_nc) <- c("number_of_targets", "richness_level")

accumulated_targets_nc_sd$sd = rownames(accumulated_targets_nc_sd)
colnames(accumulated_targets_nc_sd) <- c("sd", "number_of_targets")

accumulated_targets_nc_sd <- accumulated_targets_nc_sd %>% 
	separate(number_of_targets, into = c("subgroup", "number_of_species")) %>%
	mutate(number_of_species = str_replace(number_of_species, "sd", "")) %>%
	mutate(number_of_species = as.numeric(number_of_species))


accumulated_targets_nc <- accumulated_targets_nc %>% 
	separate(richness_level, into = c("subgroup", "number_of_species")) %>%
	mutate(number_of_species = str_replace(number_of_species, "richness", "")) %>%
	mutate(number_of_species = as.numeric(number_of_species))

accumulated_targets_all_nc <- left_join(accumulated_targets_nc, accumulated_targets_nc_sd)
accumulated_targets_all_nc <- accumulated_targets_all_nc %>% 
	mutate(se = sd / sqrt(number_of_species)) 





## now bind rows with the subroup split accum
# all <- bind_rows(accumulated_targets_all, accumulated_targets_all_nc)
all <- bind_rows(accumulated_targets_all, accumulated_targets_all_full, accumulated_targets_all_nc)
write_csv(all, "data-processed/all_accumulation_curve_data.csv")


### make plots!

accumulation_data <- read_csv("data-processed/all_accumulation_curve_data.csv")

most_common_accumulation <- read_csv("data-processed/most_common_accumulated_targets.csv")

mca <- most_common_accumulation %>% 
  mutate(subgroup = "most common")

all <- bind_rows(accumulation_data, mca)


all  %>%
	filter(number_of_species < 11) %>% 
	rename(group = subgroup) %>% 
	filter(group %in% c("mollusc", "finfish", "crustacean", "all", "most common")) %>% 
	mutate(group = str_replace(group, "finfish", "finfish only, no invertebrates")) %>% 
	mutate(group = str_replace(group, "all", "combined, finfish and invertebrates")) %>% 
	mutate(group = str_replace(group, "crustacean", "only crusteaceans")) %>% 
	mutate(group = str_replace(group, "mollusc", "only molluscs")) %>% 
  filter(group != "combined, finfish and invertebrates") %>%
  filter(group != "most common") %>%
  # filter(group == "most common" | group == "combined, finfish and invertebrates") %>% 
	ggplot(data = ., aes(x = number_of_species, y = number_of_targets, color = group)) +
  geom_line(aes(linetype = group), size = 1) +
  theme_bw() +
  geom_ribbon(aes(ymin = number_of_targets - se, ymax = number_of_targets + se, fill = group), alpha = 0.2, size = 0) +
  ylab("number of nutrient requirements fulfilled") +
	xlab("number of species") + theme(text = element_text(size=16)) + 
  scale_x_continuous(breaks = seq(1,10,1)) +
	# scale_color_grey(start = 0.01, end = 0.5) +
	theme_bw() +
	theme(legend.position = c(0.6, 0.2)) +
	theme(legend.title=element_blank()) 
ggsave("/Users/Joey/Documents/Nutrient_Analysis/figures/nutrient_accum_grey_dashed.png", width = 5, height = 4)
ggsave("/Users/Joey/Documents/Nutrient_Analysis/figures/nutrient_accum_grey_dashed.pdf")
ggsave("/Users/Joey/Documents/Nutrient_Analysis/figures/nutrient_accum_color.pdf")


 accumulation_data %>%
  filter(number_of_species < 15) %>% 
  rename(group = subgroup) %>% 
  mutate(group = str_replace(group, "noinverts", "finfish only, no invertebrates")) %>% 
  ggplot(data = ., aes(x = number_of_species, y = number_of_targets, color = group)) + geom_line(size =1, aes(linetype = group)) + theme_bw() + geom_ribbon(aes(ymin = number_of_targets - se, ymax = number_of_targets + se), alpha = 0.2, size = 0.5) + ylab("number of nutrient requirements fulfilled") +
  xlab("number of species") + theme(text = element_text(size=14)) + 
  # scale_color_grey(start = 0.01, end = 0.5) +
  theme_bw() +
  theme(legend.position = c(0.6, 0.2)) +
  theme(legend.title=element_blank()) 
ggsave("/Users/Joey/Documents/Nutrient_Analysis/figures/nutrient_accum_grey_dashed.png", width = 5, height = 4)



