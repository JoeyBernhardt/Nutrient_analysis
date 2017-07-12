library(purrr)
library(dplyr)
library(tidyr)
library(readr)
library(plotrix)
library(ggplot2)
library(broom)
library(gridExtra)
library(grid)
library(tidyverse)
library(cowplot)


### Code for figure 3. 

all_output <- read_csv("data-processed/single_nutrient_accumulation_by_fractions.csv")

summary <- all_output %>%
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_required, grams_for_25_percent)


summary %>% 
  ggplot(aes(x = species_no, y = grams_for_25_percent_median)) + geom_point()

single_functions <- all_output %>% 
  ggplot(aes(x = species_no, y = grams_required)) + geom_point() +
  geom_smooth(method = "lm") + theme_bw() + facet_wrap( ~ nutrient, scales = "free") + ylab("grams of tissue required to reach 10% DRI") + xlab("species richness")
ggsave("figures/single_functions.png")



output_calcium_1000 %>% 
  ggplot(aes(x = species_no, y = grams_required)) + geom_point() +
  geom_smooth(method = "lm") + theme_bw() 


output_calcium_1000 %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  lm(grams_for_25_percent ~ species_no, data = .) %>% 
  summary


reps100 <- read_csv("data-processed/grams-required-10-spp-1000reps.csv")


reps100 %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  lm(grams_for_25_percent ~ species_no, data = .) %>% 
  summary
  
  ### get the power functions for each nutrient
  cal_sum <- all_output %>% 
  filter(nutrient == "calcium") %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>% 
  summarise_at(c("grams_for_25_percent"), funs(mean, min, max, median, std.error))



calcium_power <- nls(formula=(median ~ a * species_no^b), data=cal_sum, start = c(a=10000, b=-0.7))
a.cal <- coef(calcium_power)[1]
b.cal <- coef(calcium_power)[2]  

## iron
iron_sum <- all_output %>% 
  filter(nutrient == "iron") %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>% 
  summarise_at(c("grams_for_25_percent"), funs(mean, min, max, median, std.error))

iron_power <- nls(formula=(median ~ a * species_no^b), data=iron_sum, start = c(a=10000, b=-0.7))
a.iron <- coef(iron_power)[1]
b.iron <- coef(iron_power)[2]  
iron_power_function <- function(x) a.iron*x^b.iron


## zinc 

zinc_sum <- all_output %>% 
  filter(nutrient == "calcium") %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>% 
  summarise_at(c("grams_for_25_percent"), funs(mean, min, max, median, std.error))

zinc_power <- nls(formula=(median ~ a * species_no^b), data=zinc_sum, start = c(a=10000, b=-0.7))
a.zinc <- coef(zinc_power)[1]
b.zinc <- coef(zinc_power)[2]  
zinc_power_function <- function(x) a.zinc*x^b.zinc

## epa 

epa_sum <- all_output %>% 
  filter(nutrient == "epa") %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>% 
  summarise_at(c("grams_for_25_percent"), funs(mean, min, max, median, std.error))

epa_power <- nls(formula=(median ~ a * species_no^b), data=epa_sum, start = c(a=10000, b=-0.7))
a.epa <- coef(epa_power)[1]
b.epa <- coef(epa_power)[2]  
epa_power_function <- function(x) a.epa*x^b.epa

## dha


dha_sum <- all_output %>% 
  filter(nutrient == "calcium") %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>% 
  summarise_at(c("grams_for_25_percent"), funs(mean, min, max, median, std.error))

dha_power <- nls(formula=(median ~ a * species_no^b), data=dha_sum, start = c(a=10000, b=-0.7))
a.dha <- coef(dha_power)[1]
b.dha <- coef(dha_power)[2]  
dha_power_function <- function(x) a.dha*x^b.dha

## all 
all_sum <- reps100 %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>%
  summarise_at(c("grams_for_25_percent"), funs(mean, min, max, median, std.error))

all_power <- nls(formula=(median ~ a * species_no^b), data=all_sum, start = c(a=10000, b=-0.7))
a.all <- coef(all_power)[1]
b.all <- coef(all_power)[2]  
all_power_function <- function(x) a.all*x^b.all



cor(1/cal_sum$median, predict(calcium_power))

# original calcium plot
cal_sum %>% 
  mutate(service = 1/median) %>% 
  ggplot(aes(x = species_no, y = median)) + geom_point(size = 3) +
  # stat_function(fun = calcium_power_function, color = "grey") +
  theme_bw() + ylab("nutritional service 1/median grams required to reach 10% of DRI for calcium") + xlab("species richness") +
  scale_x_continuous(breaks = 1:10)



reps <- reps100 %>% 
  mutate(nutrient = "all 5 micronutrients") %>% 
  select(-run, -`X1`) 

all_output2 <- all_output %>% 
  select(-run) %>% 
  mutate(sample_id = as.integer(sample_id))

str(all_output)
str(reps)

all_output_with5 <- bind_rows(all_output2, reps)

all_summary <- all_output_with5 %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(nutrient, species_no) %>%
  summarise_at(.vars = "grams_for_25_percent", funs(mean, min, max, median, std.error)) 

params <- all_summary %>% 
  ungroup() %>% 
  mutate(nutrient = ifelse(nutrient == "all 5 micronutrients", "all", nutrient)) %>% 
  group_by(nutrient) %>% 
  do(tidy(nls(median ~ a * species_no^b, data =., start = c(a=10000, b=-0.7)), conf.int = TRUE)) %>% 
  filter(term == "b") %>% 
  ggplot(aes(x = reorder(nutrient, estimate), y = estimate, color = nutrient)) + geom_point(size = 3) +
  # facet_wrap( ~ term, scales = "free") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + theme_bw() +
  scale_y_reverse() + xlab("nutrient") + ylab("b estimate") +
  theme(legend.position = "none") + xlab("") +
  theme(text=element_text(family="Helvetica", size=12)) +
  theme(legend.title=element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="none")
ggsave("figures/power_function_params_by_nutrient.png")

all_summary %>% 
  group_by(nutrient) %>% 
  do(tidy(lm(median ~ species_no, data =.), conf.int = TRUE)) %>%  View


### try with cowplot

figure3 <- ggdraw() +
  draw_plot(bef, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(params, x = 0.4, y = 0.1, width = 0.35, height = .35) +
  draw_plot_label(c("A", "B"), x = c(0, 0.7), y = c(1, 0.43), size = 15)

save_plot("figures/figure3.pdf", figure3, base_height = 6, base_width = 9)
bef <- all_summary %>% 
  ggplot(aes(x = species_no, y = median, color = nutrient)) + geom_point(size = 3) +
  geom_line() + theme_bw() +
  scale_y_reverse() +
  theme(text=element_text(family="Helvetica", size=14)) +
  theme(legend.title=element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = c(1:10)) + xlab("species richness") + ylab("median grams required to reach 10% of DRI") +
  theme(legend.position = c(0.8, 0.2))
ggsave("figures/all_nutrients_efficiency_power_fits_rev_y.png")


### try with cowplot




accumulation <- read_csv("data-processed/DRI_accumulation_replacement_design.csv")


accum_plot <- accumulation %>% 
  filter(species_no < 11, threshold == 10) %>% 
  group_by(group, species_no, threshold) %>% 
  summarise_at(c("number_of_targets"), funs(mean, median, std.error)) %>% 
  ggplot(aes(x = species_no, y = mean, color = group)) + geom_line(size = 2) +
  theme_bw() + ylim(1, 5) + scale_x_continuous(breaks = c(0:10)) + 
  theme(text=element_text(family="Helvetica", size=14)) +
  theme(legend.title=element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ylab("number of different micronutrient DRI targets per 100g portion") +
  xlab("species richness") +
  theme(legend.position = c(0.7, 0.2)) +
  theme(legend.text = element_text(size = 14))

### now we need the plot with the most commont species and the violin plot
reps100 <- read_csv("data-processed/grams-required-10-spp-1000reps.csv")
molluscs <- read_csv("data-processed/mollusc_sampling_all.csv")
mostcommon <- read_csv("data-processed/grams-required-10-spp-1000reps-10most-common.csv")

reps100b <- reps100 %>% 
  filter(species_no < 11) %>% 
  mutate(grams_for_25_percent = grams_required/10) 

reps100_summary <- reps100b %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_required, grams_for_25_percent)


violin_plot <- ggplot() +
  geom_violin(aes(x = species_no, y = grams_for_25_percent, group = species_no), data = reps100b) +
  geom_point(aes(x = species_no, y = grams_for_25_percent_median), data = reps100_summary, size = 4) +
   geom_hline(yintercept = 100, linetype = "dotted") +
  geom_hline(yintercept = 200, linetype = "dashed") +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1:10)) +
  theme_bw() + xlab("species richness") + ylab("grams required to reach 5 RDI targets (10% RDI)") +
  background_grid(major = "none", minor = "none") +
  theme(text=element_text(family="Helvetica", size=14)) +
  theme(axis.text = element_text(size = 14)) 

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

summary2 <- reps100_summary %>%
  mutate(group = "all species")

mostcommon_summary <- mostcommon %>%
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_required, grams_for_25_percent)



mostcommonb2 <- mostcommon_summary %>% 
  mutate(group = "10 most commonly consumed species")

mollusc_summary2 <- mollusc_summary %>% 
  mutate(group = "molluscs only")

all_summary <- bind_rows(summary2, mostcommonb2, mollusc_summary2)


most_common_plot <- ggplot(aes(x = species_no, y = grams_for_25_percent_median, group = group, color = group), data = all_summary) + geom_line(size = 2) +
  # geom_ribbon(aes(ymin = grams_for_25_percent_median - grams_for_25_percent_std.error*1.96, ymax = grams_for_25_percent_median + grams_for_25_percent_std.error*1.96, group = group)) +
  theme_bw() + xlab("species richness") + ylab("grams required to reach 5 RDI targets (10% RDI)") +
  theme(text=element_text(family="Helvetica", size=14)) +
  theme(legend.title=element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = c(1:10)) +
  theme(legend.position = c(0.7, 0.8)) +
  theme(legend.text = element_text(size=14, family = "Helvetica")) +
  theme(axis.text = element_text(size = 16, family = "Helvetica")) 






figure3 <- ggdraw() +
  draw_plot(bef, x = 0, y = 0.5, width = 0.5, height = 0.5) +
  draw_plot(params, x = 0.14, y = 0.58, width = 0.2, height = .2) +
  draw_plot(accum_plot, x = 0.5, y = 0.5, width = 0.5, height = .5) + 
  draw_plot(violin_plot, x = 0, y = 0, width = 0.5, height = .5) + 
  draw_plot(most_common_plot, x = .5, y = 0, width = 0.5, height = .5) + 
draw_plot_label(c("A", "B", "C", "D"), x = c(0.02, .04, 0.5, 0.5), y = c(1, 0.5, 1, 0.43), size = 15)

save_plot("figures/figure3b.pdf", figure3, base_height = 10, base_width = 12)

