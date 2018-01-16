

## new attempt at making the mega plot for figure 3!
library(patchwork)

giant_plot <- bef + fig3c + a_plot1 + b_plot1 + violin_greyscale  + plot3B + a_plot + b_plot + plot_layout(ncol = 4)

ggsave(giant_plot, "figures/figure3_multi.png", device = "png", width = 5, height = 3)

class(giant_plot)

all_min_rdi <- read_csv("data-processed/all_resampling_new_global_local.csv")
plot3B <- all_min_rdi %>% 
  # filter(dataset %in% species_numbers$culture | dataset %in% c("25", "29", "57")) %>% 
  filter(species_no < 11) %>% 
  filter(dataset != "global") %>% 
  filter(!dataset %in% c("25", "25", "29", "57")) %>% 
  ggplot(aes(x = species_no, y = median, group = dataset)) + geom_line(size = 0.5, alpha = 0.5) +
  # geom_ribbon(aes(ymin = mean - grams_required_10_std.error, ymax = mean + grams_required_10_std.error), fill = "grey", alpha = 0.5) +
  theme_classic() + ylab("") + xlab("Species richness") +
  geom_line(color = "cadetblue", size =1, data = filter(all, dataset == "global40", species_no < 11)) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  theme(text=element_text(family="Helvetica", size=14)) 

res_all <- read_csv("data-processed/res_all.csv")
res_sel <- res_all %>% 
  filter(culture %in% species_numbers$culture | culture == "global") %>% 
  filter(number_of_species < 11)

fig3c <- res_sel2 %>% 
  ggplot() +
  geom_ribbon(aes(x = number_of_species, ymin = number_of_targets - se, ymax = number_of_targets + se, group = culture), alpha = 0.15, size = 0) +
  geom_line(aes(x = number_of_species, y = number_of_targets, group = culture)) +
  geom_ribbon(aes(x = number_of_species, ymin = low, ymax = high), alpha = 0.5, size = 0, fill = "cadetblue", data = global_mean) +
  geom_line(aes(x = number_of_species, y = mean_targets), data = global_mean, color = "cadetblue", size = 1.5) +
  ylab("") +
  xlab("Species richness") + theme(text = element_text(size=14)) + 
  theme(legend.title=element_blank()) + theme_classic() +
  ylim(0,5) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  theme(text=element_text(family="Helvetica", size=14))


### params plots
res_all <- read_csv("data-processed/res_all.csv")

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


### min RDI plots
all <- read_csv("data-processed/all_resampling_new_global_local.csv")
mod <- all %>% 
  filter(dataset != "global") %>% 
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
  mutate(dataset = str_replace(dataset, "global40", "GL")) %>% 
  dplyr::group_by(dataset) %>% 
  filter(species_no < 11) %>% 
  do(tidy(nls(formula = (median ~ a * species_no^b),data = .,  start = c(a=10000, b=-0.7))))



a_terms1 <- mod %>% 
  filter(term == "a")

a_plot1 <- a_terms1 %>% 
  ungroup() %>% 
  ggplot(aes(x = reorder(dataset, estimate), y = estimate)) + geom_point(size = 2) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  geom_point(size = 2, data = filter(a_terms1, dataset == "GL"), color = "cadet blue") +
  theme(
    axis.ticks.x = element_blank())



b_terms1 <- mod %>% 
  filter(term == "b")

b_plot1 <- b_terms1 %>% 
  ungroup() %>% 
  ggplot(aes(x = reorder(dataset, estimate), y = estimate)) + geom_point(size = 2) +
  geom_point(size = 2, data = filter(b_terms1, dataset == "GL"), color = "cadet blue") +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.1) +
  geom_point(size = 2, data = filter(b_terms1, dataset == "GL"), color = "cadet blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  theme(
    axis.ticks.x = element_blank())

all_summaries <- read_csv("data-processed/all_summaries_BEF.csv")

bef <- all_summaries %>% 
  ungroup() %>% 
  # mutate(nutrient = ifelse(nutrient == "all 5 micronutrients", "all", nutrient)) %>% 
  ggplot(aes(x = species_no, y = median, color = nutrient)) + 
  geom_point(size = 2) +
  geom_line(size = 1.5) + 
  # theme_bw() +
  # scale_y_reverse() +
  theme(text=element_text(family="Helvetica", size=14)) +
  theme(legend.title=element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = c(1:10)) + xlab("species richness") +
  # ylab("median grams required \n to reach 10% of DRI") +
  ylab("") + xlab("Species richness") +
  theme(legend.position = "none") + 
  # theme(legend.position = c(0.66, 0.7), legend.direction = "horizontal") +
  scale_color_viridis(discrete = TRUE)

### violin plot
reps1000 <- read_csv("data-processed/grams-required-10-spp-1000reps-new-global.csv")
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
