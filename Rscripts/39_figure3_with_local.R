### Figure 3 with local

library(tidyverse)
library(plotrix)
library(cowplot)


res <- read_csv("data-processed/nut_accumulation_trad_foods.csv")
res_global_all <- read_csv("data-processed/res_global_all.csv")
res_global <- read_csv("data-processed/res_global.csv")
res_all <- bind_rows(res, res_global)
reps1000 <- read_csv("data-processed/grams-required-10-spp-1000reps-new-global.csv")

all_min_rdi <- read_csv("data-processed/all_resampling_new_global_local.csv")



plot3B <- all_min_rdi %>% 
  # filter(dataset %in% species_numbers$culture | dataset %in% c("25", "29", "57")) %>% 
  filter(species_no < 11) %>% 
  filter(dataset != "global") %>% 
  filter(!dataset %in% c("25", "25", "29", "57")) %>% 
  ggplot(aes(x = species_no, y = median, group = dataset)) + geom_line(size = 0.5, alpha = 0.5) +
  # geom_ribbon(aes(ymin = mean - grams_required_10_std.error, ymax = mean + grams_required_10_std.error), fill = "grey", alpha = 0.5) +
  theme_classic() + ylab("Median grams required to reach 5 DRI targets") + xlab("Species richness") +
  geom_line(color = "cadetblue", size =1, data = filter(all, dataset == "global40", species_no < 11)) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  theme(text=element_text(family="Helvetica", size=14)) 
ggsave("figures/min_rdi_local_BEF.png", width = 3, height = 3)


plot3C <- res_all %>% 
  filter(culture %in% species_numbers$culture | culture == "global") %>% 
  filter(number_of_species < 11) %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets, group = culture)) +
  geom_ribbon(aes(ymin = number_of_targets - se, ymax = number_of_targets + se), alpha = 0.05, size = 0) +
  geom_line(size =.5, alpha = 0.5) +
  geom_line(color = "cadetblue", size =1, data = filter(res_all, culture == "global", number_of_species < 11)) +
  ylab("Number of nutrient requirements fulfilled (10% DRI)") +
  xlab("Species richness") + theme(text = element_text(size=14)) + 
  theme(legend.title=element_blank()) + theme_classic() +
  ylim(0,5) +
  scale_x_continuous(breaks = seq(1,10,1))
ggsave("figures/nutrient_accumulation_plots_na_cultures_overlay_bw.png", width = 3, height = 3)


line_graphs <- plot_grid(plot3B, plot3C, nrow = 2, ncol = 1)
save_plot("figures/fig3_line_graphsw.png", line_graphs,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 1, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 0.5
)


?plot_grid

res_all2 <- res_all %>% 
  rename(dataset = culture) %>% 
filter(!dataset %in% c("25", "25", "29", "57", "40", "20")) %>% 
  mutate(dataset = str_replace(dataset, "Inuit-Inupiaq", "II")) %>% 
  mutate(dataset = str_replace(dataset, "Central Salish", "CS")) %>% 
  mutate(dataset = str_replace(dataset, "Wampanoag", "WA")) %>% 
  mutate(dataset = str_replace(dataset, "Cree", "CR")) %>%
  mutate(dataset = str_replace(dataset, "Nootkan", "NO")) %>% 
  mutate(dataset = str_replace(dataset, "Bella Coola", "BC")) %>% 
  mutate(dataset = str_replace(dataset, "Tlingit", "TL")) %>%
  mutate(dataset = str_replace(dataset, "Haida", "HA")) %>%
  mutate(dataset = str_replace(dataset, "Tsimshian", "TS")) %>% 
  mutate(dataset = str_replace(dataset, "Montagnais-Naskapi", "MN")) %>%
  mutate(dataset = str_replace(dataset, "Yupik", "YU")) %>% 
  mutate(dataset = str_replace(dataset, "Abenaki", "AB")) %>%
  mutate(dataset = str_replace(dataset, "Micmac", "MI")) %>%
  mutate(dataset = str_replace(dataset, "Kwakiutl", "KW")) %>%
  mutate(dataset = str_replace(dataset, "global", "GL"))
cultures <- c("II", "CS", "WA", "CR", "NO", "BC", "TL", "HA",
              "TS", "MN", "YU", "AB", "MI", "KW", "GL")

mod <- res_all2 %>% 
  rename(culture = dataset) %>% 
  filter(culture %in% cultures | culture == "global") %>% 
  filter(number_of_species < 11) %>% 
  group_by(culture) %>% 
  do(tidy(nls(formula = (number_of_targets ~ a * number_of_species^b),data = .,  start = c(a=1, b=0.3)))) 


a_terms <- mod %>% 
  filter(term == "a") %>% 
  ungroup() %>% 
  mutate(culture = as.character(culture))

str(a_terms)

a_plot <- a_terms %>% 
  ggplot(aes(x = reorder(culture, estimate), y = estimate)) + geom_point(size = 2) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_point(size = 2, data = filter(a_terms, culture == "GL"), color = "cadet blue") +
  xlab("") + ylab("") + theme(axis.ticks.x = element_blank())
  


b_terms <- mod %>% 
  filter(term == "b")

b_plot <- b_terms %>% 
  ungroup() %>% 
  ggplot(aes(x = reorder(culture, estimate), y = estimate)) + geom_point(size = 2) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") + theme(axis.ticks.x = element_blank()) +
  geom_point(size = 2, data = filter(b_terms, culture == "GL"), color = "cadet blue")

BEF_params_plot <- plot_grid(a_plot, b_plot, nrow = 1, ncol = 2)
save_plot("figures/BEF-params.png", BEF_params_plot,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 1, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1
)



### now violin plot

reps100b <- reps1000 %>% 
  filter(species_no < 11) %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) 

reps100_summary <- reps100b %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_required, grams_for_25_percent)



violin_greyscale <- ggplot() +
  geom_violin(aes(x = species_no, y = grams_for_25_percent, group = species_no), data = reps100b, color = "grey", size = 1, fill = "grey") +
  geom_point(aes(x = species_no, y = grams_for_25_percent_median), data = reps100_summary, size = 2) +
  geom_hline(yintercept = 100, linetype = "dotted") +
  geom_hline(yintercept = 200, linetype = "dashed") +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1:10)) +
  xlab("Species richness") + ylab("grams required to reach 5 RDI targets (10% RDI)") +
  background_grid(major = "none", minor = "none") +
  theme(text=element_text(family="Helvetica", size=14)) 
ggsave("figures/violin_plot_fig3.pdf", width = 4.3, height = 3.5)
ggsave("figures/violin_plot_fig3.png", width = 4, height = 3.5)


