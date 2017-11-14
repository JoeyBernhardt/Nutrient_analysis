### Figure 3 with local

library(tidyverse)


res <- read_csv("data-processed/nut_accumulation_trad_foods.csv")
res_global_all <- read_csv("data-processed/res_global_all.csv")
res_global <- read_csv("data-processed/res_global.csv")
res_all <- bind_rows(res, res_global)
reps1000 <- read_csv("data-processed/grams-required-10-spp-1000reps-new-global.csv")




res_all %>% 
  filter(culture %in% species_numbers$culture | culture == "global") %>% 
  filter(number_of_species < 11) %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets, group = culture)) +
  geom_ribbon(aes(ymin = number_of_targets - se, ymax = number_of_targets + se), alpha = 0.05, size = 0) +
  geom_line(size =.5, alpha = 0.5) +
  geom_line(color = "cadetblue", size =1, data = filter(res_all, culture == "global", number_of_species < 11)) +
  ylab("Number of nutrient requirements fulfilled (10% DRI)") +
  xlab("Number of species") + theme(text = element_text(size=14)) + 
  theme(legend.title=element_blank()) + theme_classic() +
  ylim(0,5) +
  scale_x_continuous(breaks = seq(1,10,1))
ggsave("figures/nutrient_accumulation_plots_na_cultures_overlay_bw.png", width = 4, height = 4)
mod <- res_all %>% 
  filter(culture %in% species_numbers$culture | culture == "global") %>% 
  filter(number_of_species < 11) %>% 
  group_by(culture) %>% 
  do(tidy(nls(formula = (number_of_targets ~ a * number_of_species^b),data = .,  start = c(a=1, b=0.3)))) 


a_terms <- mod %>% 
  filter(term == "a")

a_plot <- a_terms %>% 
  ggplot(aes(x = reorder(culture, estimate), y = estimate)) + geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Culture") + ylab("Parameter estimate (a)") 


b_terms <- mod %>% 
  filter(term == "b")

b_plot <- b_terms %>% 
  ggplot(aes(x = reorder(culture, estimate), y = estimate)) + geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Culture") + ylab("Parameter estimate (b)") 

BEF_params_plot <- plot_grid(a_plot, b_plot, nrow = 2, ncol = 1)
save_plot("figures/BEF-params.png", BEF_params_plot,
          ncol = 1, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.2
)



### now violin plot

reps100b <- reps1000 %>% 
  filter(species_no < 11) %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) 

reps100_summary <- reps100b %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_required, grams_for_25_percent)



ggplot() +
  geom_violin(aes(x = species_no, y = grams_for_25_percent, group = species_no), data = reps100b, color = "grey", size = 1, fill = "grey") +
  geom_point(aes(x = species_no, y = grams_for_25_percent_median), data = reps100_summary, size = 2) +
  geom_hline(yintercept = 100, linetype = "dotted") +
  geom_hline(yintercept = 200, linetype = "dashed") +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1:10)) +
  theme_bw() + xlab("species richness") + ylab("grams required to reach 5 RDI targets (10% RDI)") +
  background_grid(major = "none", minor = "none") +
  theme(text=element_text(family="Helvetica", size=16)) 
ggsave("figures/violin_plot_fig3.pdf", width = 4.3, height = 3.5)
ggsave("figures/violin_plot_fig3.png", width = 4, height = 3.5)


