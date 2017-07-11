

### making plots with resampling data
library(tidyverse)
library(plotrix)
library(ggplot2)

# reps100 <- read_csv("data-processed/grams-required-15-spp-1000reps.csv")
reps100 <- read.csv("data-processed/grams-required-10-spp-1000reps.csv")
molluscs <- read_csv("data-processed/mollusc_sampling_all.csv")
mostcommon <- read_csv("data-processed/grams-required-10-spp-1000reps-10most-common.csv")
inuit_resampling <- read_csv("data-processed/grams-required-10-spp-100reps-inuit.csv")


summary <- inuit_resampling %>%
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_required, grams_for_25_percent)

inuit_10sp <- inuit_resampling %>%
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  filter(species_no < 11)

mollusc_processed <- molluscs %>% 
  rename(species_number = subsample_size) %>%
  group_by(species_number, sample_id) %>% 
  mutate(cal_total = (calcium/species_number)) %>% ## get the amount of calcium each species will contribute
  mutate(zinc_total = (zinc/species_number)) %>% 
  mutate(iron_total = (iron/species_number)) %>% 
  mutate(epa_total = (epa/species_number)) %>%
  mutate(dha_total = (dha/species_number)) %>%
  summarise_each(funs(sum), contains("total")) %>% ## sum up all of each of the nutrients
  mutate(cal_grams = (cal_total/(1200))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
  mutate(iron_grams = (iron_total/(18))) %>%
  mutate(zinc_grams = (zinc_total/(11))) %>% 
  mutate(epa_grams = (epa_total/(1))) %>%
  mutate(dha_grams = (dha_total/(1))) %>%
  rename(species_no = species_number) %>% 
  group_by(species_no, sample_id) %>% 
  select(-contains("total")) %>% 
  gather(key = nutrient, value = concentration, 3:7) %>% 
  group_by(species_no, sample_id) %>% 
  summarise(min_percentage = min(concentration)) %>% 
  mutate(grams_required = 100/min_percentage) 

mollusc_summary <- mollusc_processed %>%
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_required, grams_for_25_percent) %>% 
  filter(species_no < 11)


reps100b <- reps100 %>% 
  filter(species_no < 11) %>% 
  mutate(grams_for_25_percent = grams_required/10) 



mostcommon_summary <- mostcommon %>%
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_required, grams_for_25_percent)


trait_data_pro <- read_csv("data-processed/micronutrients-species-mean.csv")
species_number <- 1

one_species_grams <- trait_data_pro %>% 
  mutate(cal_total = (calcium/species_number)) %>% ## get the amount of calcium each species will contribute
  mutate(zinc_total = (zinc/species_number)) %>% 
  mutate(iron_total = (iron/species_number)) %>% 
  mutate(epa_total = (epa/species_number)) %>%
  mutate(dha_total = (dha/species_number)) %>%
  # summarise_each(funs(sum), contains("total")) %>% ## sum up all of each of the nutrients
  mutate(cal_grams = (cal_total/(1200*0.1))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
  mutate(iron_grams = (iron_total/(18*0.1))) %>%
  mutate(zinc_grams = (zinc_total/(11*0.1))) %>% 
  mutate(epa_grams = (epa_total/(1*0.1))) %>%
  mutate(dha_grams = (dha_total/(1*0.1))) %>% 
  select(-contains("total")) %>% 
  gather(key = nutrient, value = concentration, 8:12) %>% 
  group_by(subgroup, species_name) %>% 
  summarise(min_percentage = min(concentration)) %>% 
  mutate(grams_required = 100/min_percentage) %>% 
  mutate(species_no = "1") %>% 
  ungroup() %>% 
  mutate(species_no = as.numeric(species_no))



write_csv(one_species_grams, "data-processed/one_species_grams_required.csv")


one_species_gramsb <- one_species_grams %>% 
  filter(grams_required <= 100 | grams_required > 2000)


  

summary2 <- summary %>%
  mutate(group = "all species")

mostcommonb2 <- mostcommon_summary %>% 
  mutate(group = "10 most commonly consumed species")

mollusc_summary2 <- mollusc_summary %>% 
  mutate(group = "molluscs only")

all_summary <- bind_rows(summary2, mostcommonb2, mollusc_summary2)


ggplot(aes(x = species_no, y = grams_for_25_percent_median, group = group, color = group), data = all_summary) + geom_line() +
  # geom_ribbon(aes(ymin = grams_for_25_percent_median - grams_for_25_percent_std.error*1.96, ymax = grams_for_25_percent_median + grams_for_25_percent_std.error*1.96, group = group)) +
  theme_bw() + xlab("species richness") + ylab("grams required to reach 5 RDI targets (10% RDI)")



### now for the plot that highlights which species reach the targets with fewest grams

grams_most_common <- mostcommon %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  filter(species_no == 1) %>% 
  distinct(grams_for_25_percent, .keep_all = TRUE)


one_species_grams %>% 
  ggplot(aes(x = reorder(species_name, grams_required), y = grams_required)) + geom_point() +
  scale_y_log10() +
  coord_flip() + theme_bw()
  


violin_plot <- ggplot() +
  geom_violin(aes(x = species_no, y = grams_for_25_percent, group = species_no), data = reps100b, fill = "lightgrey") +
  geom_point(aes(x = species_no, y = grams_for_25_percent_median), data = summary, size = 4, shape = 1) +
  geom_point(aes(x = species_no, y = grams_required), data = one_species_gramsb, color = "black", size = 3, alpha = 0.5) +
  geom_point(aes(x = species_no, y = grams_for_25_percent), data = grams_most_common, color = "black", shape = 17, size = 3, alpha = 0.5) +
  # geom_point(aes(shape = c('most common species', 'median'))) +
  scale_shape_manual(name = '', values = c('most common species' = 17, 'median' = 1)) +
  geom_hline(yintercept = 100, linetype = "dotted") +
  geom_hline(yintercept = 200, linetype = "dashed") +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1:10)) +
  theme_bw() + xlab("species richness") + ylab("grams required to reach 5 RDI targets (10% RDI)") +
  background_grid(major = "none", minor = "none") 
ggsave("figures/violin_grams_req_with_moll_in_red.pdf")
ggsave("figures/violin_grams_req_with_moll_in_grey_plus_outliers.pdf")
ggsave("figures/violin_grams_req_common_species.pdf")
ggsave("figures/violin_grams_req_10common_species.pdf")

require(cowplot)
line_graph <- ggplot() +
  # geom_violin(aes(x = species_no, y = grams_for_25_percent, group = species_no), data = inuit_10sp) +
  geom_line(aes(x = species_no, y = grams_for_25_percent_median), data = summary, size = 2) +
  # geom_violin(aes(x = species_no, y = grams_for_25_percent, group = species_no), data = mostcommonc, color = "blue") +
  geom_line(aes(x = species_no, y = grams_for_25_percent_min), data = summary, size = 2, color = "grey") +
  # geom_point(aes(x = species_no, y = grams_for_25_percent_median), data = mostcommonb, size = 4, color = "blue") +
  # geom_point(aes(x = species_no, y = grams_for_25_percent_median), shape = 18, data = mollusc_summary, color = "black", size = 4) +
  # geom_point(aes(x = species_no, y = grams_required), data = one_species_gramsb, color = "black", size = 4, alpha = 0.5) +
  # geom_point(aes(x = species_no, y = grams_for_25_percent), data = grams_most_common, color = "blue", size = 4, alpha = 0.5) +
  geom_hline(yintercept = 100, linetype = "dotted") +
  geom_hline(yintercept = 200, linetype = "dashed") +
  scale_x_continuous(breaks = c(1:10)) +
  # scale_y_log10() +
  # scale_y_continuous(breaks = seq(0,1000, 100)) +
  scale_colour_grey() +
  theme_bw() + xlab("species richness") + ylab("grams required to reach 5 RDI targets (10% RDI)") +
  theme(legend.position = c(0.6, 0.85),
        legend.title = element_blank()) +
  background_grid(major = "none", minor = "none")
ggsave("figures/line_graph_grams_required.pdf")



library(cowplot)


all_accumulation <- read_csv("data-processed/27_all_accumulation.csv")

all_subset <- all_accumulation %>%
  filter(number_of_species < 11) %>% 
  filter(subgroup != "mollusc") 


acc_plot <- all_subset %>% 
  filter(threshold == "10 percent") %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets, group = subgroup, color = subgroup)) + geom_line(size =1.5, aes(linetype = subgroup)) +
  geom_ribbon(aes(ymin = number_of_targets - se, ymax = number_of_targets + se, color = subgroup, group = subgroup), alpha = 0.2, size = 0) +
  ylab("number of nutrient requirements fulfilled (10% DRI)") +
  xlab("species richness") + theme(text = element_text(size=14)) + 
  scale_color_grey(start = 0.01, end = 0.7) +
  theme_bw() +
  # theme(legend.position = c(0.6, 0.2)) +
  scale_x_continuous(breaks = c(1:10)) +
  theme(legend.position = c(0.6, 0.3),
legend.title = element_blank()) +
  background_grid(major = "none", minor = "none")



  
  grid <- plot_grid(violin_plot, line_graph, acc_plot, labels = c("A", "B", "C"), align = "v", ncol =1, nrow = 3)




?plot_grid



save_plot("figures/grams_required_accum.png", grid,
          ncol = 1,
          nrow = 3,
          base_aspect_ratio = 1.3, 
          base_height = 5)


