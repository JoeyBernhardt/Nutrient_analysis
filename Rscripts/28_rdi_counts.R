library(tidyverse)
library(purrr)
library(janitor)
library(xtable)
library(stringr)
library(viridis)


data <- read_csv("data-processed/mean_nuts.csv")

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



trait_data <- read_csv("data-processed/n.long_lat3.csv")
View(trait_data)
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
  geom_hline(yintercept = 100, color = "black") +
  theme(text=element_text(family="Helvetica", size=16)) +
  theme_bw() +
  theme(legend.title=element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ylab("number of different micronutrient \n DRI targets per 100g portion") +
  coord_polar() + ylab("percentage of species that reach 10% DRI") + xlab("") +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  theme(strip.background = element_rect(colour="white", fill="white"))
ggsave("figures/flower_diagram_percentage_reach_DRI.png", width = 12, height = 5)



rdis1$nutrient <- factor(rdis1$nutrient, levels = c("protein", "fat", "calcium", "zinc", "iron", "EPA", "DHA"))
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
  geom_hline(yintercept = 50, color = "grey") + theme_bw() + 
  ylab("Proportion of species that reach 10% of DRI")+ xlab("Nutrient")
ggsave("figures/DRI_barchart_figure2.png")

rdis1 %>% 
  filter(subgroup == "mollusc") %>% View

rdis %>% 
  gather(key = nutrient, value = rdi, 10:16) %>% 
  filter(!is.na(rdi)) %>% 
  select(species_name, subgroup, RDI.micro.tot) %>%
  distinct(species_name, .keep_all = TRUE) %>% 
  filter(!is.na(RDI.micro.tot)) %>% View


trait_data3 <- trait_data2 %>% 
  distinct(species_name, food_name_clean, ref_info, seanuts_id2, nutrient, concentration, .keep_all = TRUE) %>% 
  spread(key = nutrient, value = concentration) %>% 
  mutate(protein = protcnt_g) %>% 
  mutate(protein = ifelse(is.na(protein), protein_g, protein)) %>% 
  mutate(protein = ifelse(is.na(protein), prot_g, protein)) %>% 
  select(species_name, seanuts_id2, protein, fat_g, dha, epa, ca_mg, zn_mg, fe_mg, subgroup, ref_info) %>% 
  rename(fat = fat_g,
         calcium = ca_mg, 
         zinc = zn_mg,
         iron = fe_mg) %>% 
  gather(key = nutrient, value = concentration, 3:9) %>% 
  filter(!is.na(concentration))

#### 

percentages <- trait_data3 %>% 
  mutate(dri_per = NA) %>% 
  mutate(dri_per = ifelse(nutrient == "calcium", concentration/1200, dri_per)) %>% 
  mutate(dri_per = ifelse(nutrient == "iron", concentration/18, dri_per)) %>%
  mutate(dri_per = ifelse(nutrient == "zinc", concentration/11, dri_per)) %>% 
  mutate(dri_per = ifelse(nutrient == "epa", concentration/1, dri_per)) %>%
  mutate(dri_per = ifelse(nutrient == "dha", concentration/1, dri_per)) %>% 
  mutate(dri_per = ifelse(nutrient == "protein", concentration/50, dri_per)) %>% 
  mutate(dri_per = ifelse(nutrient == "fat", concentration/70, dri_per)) %>% 
  mutate(dri_per = dri_per*100) %>% 
  filter(dri_per < 2000) 

write_csv(percentages, "data-processed/percentages_refs.csv")

outliers <- trait_data3 %>% 
  mutate(dri_per = NA) %>% 
  mutate(dri_per = ifelse(nutrient == "calcium", concentration/1200, dri_per)) %>% 
  mutate(dri_per = ifelse(nutrient == "iron", concentration/18, dri_per)) %>%
  mutate(dri_per = ifelse(nutrient == "zinc", concentration/11, dri_per)) %>% 
  mutate(dri_per = ifelse(nutrient == "epa", concentration/1, dri_per)) %>%
  mutate(dri_per = ifelse(nutrient == "dha", concentration/1, dri_per)) %>% 
  mutate(dri_per = ifelse(nutrient == "protein", concentration/50, dri_per)) %>% 
  mutate(dri_per = ifelse(nutrient == "fat", concentration/70, dri_per)) %>% 
  mutate(dri_per = dri_per*100) %>% 
  filter(dri_per > 2000)

unique(trait_data3$nutrient)
percentages$nutrient <- factor(percentages$nutrient, levels = c("protein", "fat", "calcium", "zinc", "iron", "epa", "dha"))

write_csv(percentages, "data-processed/percentages.csv")

g <- ggplot(percentages, aes(dri_per, fill = subgroup)) + geom_histogram(binwidth = 0.07) 
g + facet_grid(nutrient ~ ., scales = "free_y") + theme_bw() + scale_x_log10(breaks = c(0,1,10,100)) + geom_vline(xintercept = 10) +
  xlab("percentage of DRI in 100g edible portion") + scale_color_grey() 
ggsave("figures/dri_histogram_figure2.png")
  