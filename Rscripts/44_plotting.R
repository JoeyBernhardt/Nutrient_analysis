

## new attempt at making the mega plot for figure 3!
library(patchwork)
library(tidyverse)
library(stringr)
library(broom)
library(viridis)
library(plotrix)
library(cowplot)



# Take 1 ------------------------------------------------------------------


giant_plot <- bef + violin_greyscale + fig3c + plot3B + a_plot +  a_plot1 + b_plot + b_plot1 + plot_layout(ncol = 2)
ggplot2::ggsave(plot = giant_plot, filename = "figures/figure3_multi", device = "pdf", width =8, height = 12)

### this is the new plot, without the parameter values
giant_plot2 <- bef + violin_greyscale + fig3c + plot3B  + plot_layout(ncol = 2)
ggplot2::ggsave(plot = giant_plot2, filename = "figures/figure3_multi2", device = "pdf", width =10, height = 8)

class(giant_plot)

all_min_rdi <- read_csv("data-processed/all_resampling_new_global_local.csv")
plot3B <- all_min_rdi %>% 
  filter(species_no < 11) %>% 
  filter(dataset != "global") %>% 
  filter(!dataset %in% c("25", "25", "29", "57", "20", "40")) %>%
  ggplot(aes(x = species_no, y = median, group = dataset)) + 
  geom_ribbon(aes(ymin = median - grams_required_10_std.error, ymax = median + grams_required_10_std.error), fill = "darkgrey", alpha = 0.5) +
  geom_line(size = 0.5) +
  ylab("") + xlab("Species richness") +
  geom_line(color = "cadetblue", size =1, data = filter(all_min_rdi, dataset == "global40", species_no < 11)) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  theme(text=element_text(family="Helvetica", size=14)) 

res_all <- read_csv("data-processed/res_all.csv")
global_mean <- read_csv("data-processed/global_mean_accumulation.csv")
species_numbers <- read_csv("data-processed/species_numbers.csv")
res_sel <- res_all %>% 
  filter(culture %in% species_numbers$culture | culture == "global") %>% 
  filter(number_of_species < 11)
run <- data.frame(run = rep(1:1014, 10)) %>% 
  arrange(run)

# res_sel2 <- bind_cols(res_sel, run) %>% 
  # filter(culture != "global")

res_sel2 <- res_sel %>% 
  filter(culture != "global")

fig3c <- res_sel2 %>% 
  ggplot() +
  geom_ribbon(aes(x = number_of_species, ymin = number_of_targets - se, ymax = number_of_targets + se, group = culture), alpha = 0.5, size = 0, fill = "darkgrey") +
  geom_line(aes(x = number_of_species, y = number_of_targets, group = culture)) +
  geom_ribbon(aes(x = number_of_species, ymin = low, ymax = high), alpha = 0.5, size = 0, fill = "cadetblue", data = global_mean) +
  geom_line(aes(x = number_of_species, y = mean_targets), data = global_mean, color = "cadetblue", size = 0.75) +
  ylab("") +
  xlab("Species richness") + theme(text = element_text(size=14)) + 
  theme(legend.title=element_blank()) + theme_classic() +
  ylim(0,5) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  theme(text=element_text(family="Helvetica", size=14))


### params plots
res_all <- read_csv("data-processed/res_all.csv")
mean_target <- read_csv("data-processed/global_40_species_resampled_accumulation_mean.csv")

mt <- mean_target %>% 
  mutate(culture = "global")
res_all %>% 
  filter(culture == "Inuit-Inupiaq") %>% View

res_allb <- res_all %>% 
  filter(culture != "global")

res_allc <- bind_rows(res_allb, mt)

res_all2 <- res_allc %>% 
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

table(res_all2$dataset)

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


a_plot <- a_terms %>% 
  ggplot(aes(x = reorder(culture, estimate), y = estimate)) + geom_point(size = 2) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_point(size = 2, data = filter(a_terms, culture == "GL"), color = "cadet blue") +
  xlab("") + ylab("") + theme(axis.ticks.x = element_blank())



b_terms <- mod %>% 
  filter(term == "b")

lm(estimate ~ culture, data = b_terms) %>% summary


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
  ggplot(aes(x = species_no, y = median, color = nutrient)) + 
  geom_line(size = 1) + 
  geom_point(size = 1.5) +
  # geom_point(size = 1.5, shape = 1, color = "black") +
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
  xlab("") + ylab("") +
  theme(axis.text = element_text(size=16))



# Take 2 ------------------------------------------------------------------

### this plot comes from the bottom of script 38b_efficiency_trad
p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) 
efficiency_local <- p + 
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, x = species_no), data = WA_preds, alpha = 0.3, fill = "grey") +
  geom_line(aes(x = species_no, y = mean), data = WA_preds, alpha = 0.7, color = "black") +
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, x = species_no), data = AB_preds, alpha = 0.3, fill = "grey") +
  geom_line(aes(x = species_no, y = mean), data = AB_preds, alpha = 0.7, color = "black") +
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, x = species_no), data = HA_preds, alpha = 0.3, fill = "grey") +
  geom_line(aes(x = species_no, y = mean), data = HA_preds, alpha = 0.7, color = "black") +
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, x = species_no), data = MI_preds, alpha = 0.3, fill = "grey") +
  geom_line(aes(x = species_no, y = mean), data = MI_preds, alpha = 0.7, color = "black") +
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, x = species_no), data = MN_preds, alpha = 0.3, fill = "grey") +
  geom_line(aes(x = species_no, y = mean), data = MN_preds, alpha = 0.7, color = "black") +
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, x = species_no), data = NO_preds, alpha = 0.3, fill = "grey") +
  geom_line(aes(x = species_no, y = mean), data = NO_preds, alpha = 0.7, color = "black") +
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, x = species_no), data = BC_preds, alpha = 0.3, fill = "grey") +
  geom_line(aes(x = species_no, y = mean), data = BC_preds, alpha = 0.7, color = "black") +
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, x = species_no), data = CR_preds, alpha = 0.3, fill = "grey") +
  geom_line(aes(x = species_no, y = mean), data = CR_preds, alpha = 0.7, color = "black") +
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, x = species_no), data = YU_preds, alpha = 0.3, fill = "grey") +
  geom_line(aes(x = species_no, y = mean), data = YU_preds, alpha = 0.7, color = "black") +
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, x = species_no), data = KW_preds, alpha = 0.3, fill = "grey") +
  geom_line(aes(x = species_no, y = mean), data = KW_preds, alpha = 0.7, color = "black") +
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, x = species_no), data = CS_preds, alpha = 0.3, fill = "grey") +
  geom_line(aes(x = species_no, y = mean), data = CS_preds, alpha = 0.7, color = "black") +
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, x = species_no), data = TL_preds, alpha = 0.3, fill = "grey") +
  geom_line(aes(x = species_no, y = mean), data = TL_preds, alpha = 0.7, color = "black") +
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, x = species_no), data = II_preds, alpha = 0.3, fill = "grey") +
  geom_line(aes(x = species_no, y = mean), data = II_preds, alpha = 0.7, color = "black") +
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, x = species_no), data = TS_preds, alpha = 0.3, fill = "grey") +
  geom_line(aes(x = species_no, y = mean), data = TS_preds, alpha = 0.7, color = "black") +
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, x = species_no), data = GL_preds, alpha = 0.7, fill = "cadetblue") +
  geom_line(aes(x = species_no, y = mean), data = GL_preds, alpha = 0.7, color = "cadetblue") +
  ylab("") + xlab("") +
  scale_x_continuous(breaks = seq(1,10,1))


### efficiency_local is from 38b_efficiency_trad, rdi_accum_plot is from 31b_replacement_resampling_local, 
### violin greyscale is from this script, above, single_nut_plot is from 33_multifunction_gamfeldt
## 


giant_plot2 <- single_nut_plot + violin_greyscale + efficiency_local + rdi_accum_plot  + plot_layout(ncol = 2)
ggplot2::ggsave(plot = giant_plot2, filename = "figures/figure3_multi2", device = "pdf", width =10, height = 8)


