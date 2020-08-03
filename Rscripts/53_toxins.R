

### Toxinz
library(janitor)
library(tidyverse)
library(readxl)
library(cowplot)

theme_set(theme_cowplot())

tox <- read_excel("data/Hall-trace-elements.xlsx") %>% 
  clean_names() %>%
  mutate(species = str_to_lower(species)) %>% 
  mutate(area = str_to_lower(area)) %>% 
  mutate(part = str_to_lower(part)) %>% 
  filter(part != "liver") %>% 
  # select(6:20) %>% 
  filter(cadmium != 2.61) %>% 
  filter(nickel != 1.810) %>% 
  mutate(mercury = ifelse(subgroup == "mollusc", mercury*0.30, mercury*0.95)) %>% 
  rename(methylmercury = mercury) 

# all_merc <- read_csv("data-processed/mercury-data-compiled.csv")
str(tox)

names_tox <- names(tox)

tox_sum <- tox %>% 
  group_by(species) %>% 
  summarise_at(c(names_tox[6:20]), mean) %>% 
  dplyr::select(species, 2:5) %>% 
  gather(key = nutrient, value = concentration, 2:5) %>% 
  mutate(concentration = ifelse(concentration == 0, 0.00001, concentration)) %>%
  spread(key= nutrient, value = concentration) %>% 
  dplyr::select(-species) %>% 
  mutate_all(.funs = log)
  

tox_sum4 <- tox %>% 
  group_by(subgroup, species) %>% 
  summarise_at(c(names_tox[6:20]), mean) %>% 
  gather(key = element, value = concentration, 3:6)

#### toxins to use: mercury, cadmium, lead, arsenic
all_merc <- read_csv("data-processed/mercury-data-compiled.csv") %>% 
  filter(dataset != "hall") %>% 
  dplyr::select(species, mean_hg, dataset) %>% 
  rename(concentration = mean_hg) %>% 
  mutate(concentration = concentration / 16*100) %>% 
  mutate(contaminant = "mercury")

tox2 <- tox %>% 
  group_by(species, subgroup) %>% 
  summarise_at(c(names_tox[6:20]), mean) %>% 
  mutate_at(c(names_tox[6:20]), times_hundred) %>% 
  dplyr::select(1:6) %>% 
  filter(arsenic < 50000) %>%
  filter(cadmium < 500) %>%
  mutate(arsenic = arsenic / 150*100) %>%
  mutate(cadmium = cadmium / 70*100) %>%
  mutate(mercury = mercury / 16*100) %>%
  mutate(lead = lead / 250*100) %>%
  gather(key = contaminant, value = concentration, 3:6) %>%
  mutate(dataset = "hall")

tox2 %>% 
  filter(contaminant == "mercury") %>% View

allcon <- bind_rows(tox2, all_merc) %>% 
  filter(!is.na(concentration)) %>% 
  group_by(species, contaminant, dataset) %>% 
  summarise(concentration = mean(concentration))

allcon %>% 
  filter(contaminant == "mercury") %>% View

plot1 <- allcon %>% 
  mutate(contaminant = ifelse(contaminant == "mercury", "Methylmercury", contaminant)) %>% 
  mutate(contaminant = ifelse(contaminant == "arsenic", "Arsenic", contaminant)) %>% 
  mutate(contaminant = ifelse(contaminant == "lead", "Lead", contaminant)) %>% 
  mutate(contaminant = ifelse(contaminant == "cadmium", "Cadmium", contaminant)) %>% 
  ggplot(aes(x = concentration)) + geom_histogram() +
  facet_wrap( ~ contaminant, scales = "free", nrow = 2, ncol = 2) +
  ylab("Count") + xlab("Percentage of PTDI in 100g portion") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 2)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
  theme(strip.background =element_rect(fill="transparent"))
ggsave("figures/contaminant-distribution.pdf", width = 4, height = 8)
ggsave("figures/contaminant-distribution.pdf", width = 6, height = 4)


tox_sum4 %>% 
  ggplot(aes(x = concentration, color = subgroup)) + geom_density() +
  facet_wrap( ~ element, scales = "free")
ggsave("figures/toxin-distribution-subgroup.pdf", width = 10, height = 6)

length(unique(tox_sum$species))
str(tox_sum)

library(corrplot)

M <- cor(tox_sum)
corrplot(M, method = "number")
corrplot.mixed(M)

m2 <- as.data.frame(M) %>% 
  gather() %>% 
  distinct()



m2 %>% 
  filter(value != 1) %>% 
  summarise(mean_corr = mean(value))

m2 %>% 
  filter(value != 1) %>% 
  ggplot(aes(x = value)) + geom_density(fill = "lightblue") +
  geom_vline(xintercept = 0) +
  ylab("Density") +
  xlab("Correlation coefficient") 
ggsave("figures/toxin-correlations.png", width = 8, height = 6)

library(GGally)
tox_cor <- ggcorr(tox_sum, method = c("everything", "pearson"), label = TRUE, label_size = 4, legend.size = 10) +
  ggtitle("A")
ggsave("figures/contaminants_sum.pdf", width = 6, height = 4)



ggpairs(tox_sum) 
ggsave("figures/tox-corr-plots-sum.pdf", width = 15, height = 15)



tox_sum %>% 
  gather(key = element, value = concentration) %>% 
  mutate(concentration = exp(concentration)) %>% 
  filter(element %in% c("lead", "mercury")) %>% 
  filter(concentration > 0) %>% 
  # mutate(concentration = ifelse(concentration == 0, 0.001, concentration)) %>% 
  ggplot(aes(x = concentration)) + geom_histogram() +
  facet_wrap( ~ element, scales = "free") 
ggsave("figures/elements-hist.pdf", width = 10, height = 8)
ggsave("figures/elements-hist-lead-mercury.png", width = 8, height = 4)



### looks at multidimensional trade-offs
library(vegan)


library(vegan)
# tox_sum <- tox %>% 
#   group_by(species) %>% 
#   summarise_at(c(names_tox[6:20]), mean) %>% 
#   select(2:16)
#   

tox_sum2 <- tox %>% 
  group_by(species, subgroup) %>% 
  summarise_at(c(names_tox[6:20]), mean) %>% 
  dplyr::select(subgroup, species, 3:6) %>% 
  gather(key = nutrient, value = concentration, 3:6) %>% 
  mutate(concentration = ifelse(concentration == 0, 0.001, concentration)) %>%
  mutate(concentration = log(concentration)) %>%
  spread(key= nutrient, value = concentration) 

tscale <- scale(tox_sum2[, 3:6], center = TRUE, scale = TRUE)

pca_size <- prcomp(tox_sum2[, 3:6], scale. = TRUE)

pca_size2 <- rda(tox_sum2[, 3:6], scale. = TRUE)

summary(pca_size)
summary(pca_size2)


pcas <- as.data.frame(scores(pca_size2, choices = 1:2)$sites)
pcas$subgroup <- tox_sum2$subgroup
pcas$mercury <- tox_sum2$mercury
pcas1 <- as.data.frame(scores(pca_size2, choices = 1:2)$species) %>% 
  mutate(trait = rownames(.))
tox_pca <- pcas %>% 
  ggplot(aes(x = PC1, y = PC2)) + geom_point(size = 3, alpha = 0.1) +
  geom_point(size = 3, color = "black", shape = 1) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  geom_text(data = pcas1, aes(x = PC1, y = PC2, label = trait), col = 'blue') +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2, text =  trait), data = pcas1, color = "blue",
               arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  ylab("PC2 (28.52% variance)") + xlab("PC1 (39.98% variance)") + scale_color_viridis_d() +
  ggtitle("B")
ggsave("figures/toxin-pca.png", width = 8, height = 6)
ggsave("figures/contaminant-pca.png", width = 10, height = 6)


con_plot <- tox_cor + tox_pca
ggsave(plot = con_plot, "figures/con_plots.pdf", width = 8, height = 4)
#### accumulate toxins

times_hundred <- function(x, na.rm = FALSE) (x*100)

tox_sum2 <- tox %>% 
  group_by(species) %>% 
  summarise_at(c(names_tox[6:20]), mean) %>% 
  mutate_at(c(names_tox[6:20]), times_hundred)

data <- tox_sum2
threshold <- 1
library(vegan)



accumulate <- function(data, threshold) {
  ntbl.RDI.all <- data %>% 
    mutate(ul_zinc = ifelse(zinc > (40000*threshold), 1, 0)) %>% 
    mutate(ul_cadmium = ifelse(cadmium > (28.57*threshold), 1, 0)) %>% 
    mutate(ul_copper = ifelse(copper > (10000*threshold), 1, 0)) %>% 
    mutate(ul_chromium = ifelse(chromium > (12000*threshold), 1, 0)) %>% 
    mutate(ul_mang = ifelse(mang > (11000*threshold), 1, 0)) %>% 
    mutate(ul_molyb = ifelse(molyb > (2000*threshold), 1, 0)) %>% 
    mutate(ul_nickel = ifelse(nickel > (1000*threshold), 1, 0)) %>% 
    mutate(ul_vanadium = ifelse(vanadium > (1800*threshold), 1, 0)) %>%
    mutate(ul_arsenic = ifelse(arsenic > (800*threshold), 1, 0)) %>%
    mutate(ul_mercury = ifelse(mercury > (18.29*threshold), 1, 0)) %>%
    mutate(ul_lead = ifelse(lead > (12.5*threshold), 1, 0)) %>%
    mutate(ul_selenium = ifelse(selenium > (450*threshold), 1, 0)) %>% 
    ungroup() %>% 
    mutate(RDI.micro.tot = rowSums(.[17:28])) %>% 
    filter(!is.na(RDI.micro.tot)) 
  
  uls <- names(ntbl.RDI.all)[17:28]
  
  all_spa <- ntbl.RDI.all %>% 
    dplyr::select(species, 17:29) %>% 
    mutate(subgroup = "all") %>% 
    split( .$subgroup) %>% 
    map(.f = `[`, c(names(ntbl.RDI.all)[17:28])) %>%
    map(.f = specaccum, method = "random", permutations = 1000)
  
  
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



# try the accumulation with only 4 toxins ---------------------------------
data <- tox_sum2 %>% View
  dplyr::select(species, cadmium, arsenic, mercury, lead)
threshold <- 0.5
accumulate <- function(data, threshold) {
  ntbl.RDI.all <- data %>% 
    mutate(ul_cadmium = ifelse(cadmium > (70*threshold), 1, 0)) %>% 
    mutate(ul_arsenic = ifelse(arsenic > (150*threshold), 1, 0)) %>%
    mutate(ul_mercury = ifelse(mercury > (16*threshold), 1, 0)) %>%
    mutate(ul_lead = ifelse(lead > (250*threshold), 1, 0)) %>%
    ungroup() %>% 
    mutate(RDI.micro.tot = rowSums(.[6:9])) %>% 
    filter(!is.na(RDI.micro.tot)) 
  
  uls <- names(ntbl.RDI.all)[6:9]
  
  all_spa <- ntbl.RDI.all %>% 
    dplyr::select(species, 6:9) %>% 
    mutate(subgroup = "all") %>% 
    split( .$subgroup) %>% 
    map(.f = `[`, c(names(ntbl.RDI.all)[6:9])) %>%
    map(.f = specaccum, method = "random", permutations = 1000)
  
  
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


accumulated_targets_all %>% 
  filter(number_of_species < 11) %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets)) + geom_line() +
  geom_ribbon(aes(x = number_of_species, ymin = number_of_targets - se, ymax = number_of_targets + se), alpha = 0.1)


accumulated_targets_all %>% 
  filter(number_of_species < 11) %>% 
  lm(log(number_of_targets) ~ log(number_of_species), data = .) %>% summary()

library(broom)

accumulated_targets_all %>% 
  filter(number_of_species < 11) %>% 
do(tidy(nls(formula = (number_of_targets ~ a * number_of_species^b),data = .,  start = c(a=1, b=0.1)))) 



rep_acc_function <- function(x) accumulate(sample_n(data, size = x, replace = FALSE), threshold = 0.5)


samples <- rep(40, 1000)


repeat_global <- samples %>% 
  map_df(rep_acc_function, .id = "run")


mean_target <- repeat_global %>% 
  group_by(number_of_species) %>% 
  summarise(mean_targets = mean(number_of_targets),
            low=quantile(number_of_targets, probs=0.025),
            high=quantile(number_of_targets, probs=0.975)) %>% 
  filter(number_of_species < 11) 

mean_target_10 <-  samples %>% 
  map_df(rep_acc_function, .id = "run") %>% 
  group_by(number_of_species) %>%  
  group_by(number_of_species) %>% 
  summarise(mean_targets = mean(number_of_targets),
            low=quantile(number_of_targets, probs=0.025),
            high=quantile(number_of_targets, probs=0.975)) %>% 
  filter(number_of_species < 11) %>% 
  mutate(threshold = "10")

mean_target_25 <- samples %>% 
  map_df(rep_acc_function, .id = "run") %>% 
  group_by(number_of_species) %>%  
  group_by(number_of_species) %>% 
  summarise(mean_targets = mean(number_of_targets),
            low=quantile(number_of_targets, probs=0.025),
            high=quantile(number_of_targets, probs=0.975)) %>% 
  filter(number_of_species < 11) %>% 
  mutate(threshold = "25")

mean_target_50 <- samples %>% 
  map_df(rep_acc_function, .id = "run") %>% 
  group_by(number_of_species) %>%  
  group_by(number_of_species) %>% 
  summarise(mean_targets = mean(number_of_targets),
            low=quantile(number_of_targets, probs=0.025),
            high=quantile(number_of_targets, probs=0.975)) %>% 
  filter(number_of_species < 11) %>% 
  mutate(threshold = "50")
mean_target_75 <- samples %>% 
  map_df(rep_acc_function, .id = "run") %>% 
  group_by(number_of_species) %>%  
  group_by(number_of_species) %>% 
  summarise(mean_targets = mean(number_of_targets),
            low=quantile(number_of_targets, probs=0.025),
            high=quantile(number_of_targets, probs=0.975)) %>% 
  filter(number_of_species < 11) %>% 
  mutate(threshold = "75")

mean_target_100 <- samples %>% 
  map_df(rep_acc_function, .id = "run") %>% 
  group_by(number_of_species) %>%  
  group_by(number_of_species) %>% 
  summarise(mean_targets = mean(number_of_targets),
            low=quantile(number_of_targets, probs=0.025),
            high=quantile(number_of_targets, probs=0.975)) %>% 
  filter(number_of_species < 11) %>% 
  mutate(threshold = "100")


write_csv(mean_target, "data-processed/global_40_species_resampled_accumulation_mean_toxins_50percent.csv")
write_csv(mean_target_10, "data-processed/global_40_species_resampled_accumulation_mean_toxins_10percent.csv")
write_csv(mean_target_20, "data-processed/global_40_species_resampled_accumulation_mean_toxins_20percent.csv")

mean_target_50 <- read_csv("data-processed/global_40_species_resampled_accumulation_mean_toxins_50percent.csv")
mean_target_20 <- read_csv("data-processed/global_40_species_resampled_accumulation_mean_toxins_20percent.csv")


# all_targets <- bind_rows(mean_target_10, mean_target_25, mean_target_50, mean_target_75, mean_target_100)
all_targets <- bind_rows(mean_target_50, mean_target_100)

mean_target %>% 
  ggplot(aes(x = number_of_species, y = mean_targets)) + geom_line() +
  geom_ribbon(aes(x = number_of_species, ymin = low, ymax = high), alpha = 0.5) +
  geom_line(aes(x = number_of_species, y = mean_targets), data = mean_target_10, color = "blue") +
  geom_line(aes(x = number_of_species, y = mean_targets), data = mean_target_20, color = "green") +
  geom_line(aes(x = number_of_species, y = mean_targets), data = mean_target_50, color = "pink") +
  geom_line(aes(x = number_of_species, y = mean_targets), data = mean_target_100, color = "red") +
  ylab("Number of tolerable limits exceeded per 100g portion") +
  xlab("Species richness") +
  scale_x_continuous(breaks = seq(1,10,1))+
  scale_y_continuous(breaks = seq(1,12,1), limits = c(0, 6)) 

all_targets %>% 
  ggplot(aes(x = number_of_species, y = mean_targets, color = factor(threshold), group = threshold)) + geom_line() +
  geom_ribbon(aes(x = number_of_species, ymin = low, ymax = high, fill = factor(threshold)), alpha = 0.5) +
  scale_color_viridis_d(name = "Threshold") + 
  scale_fill_viridis_d(name = "Threshold") +
  ylab("Number of tolerable limits exceeded per 100g portion") +
  xlab("Species richness")
ggsave("figures/toxin-accumulation.pdf", width = 8, height = 6)


b_terms <- all_targets %>% 
  filter(number_of_species < 11) %>% 
  group_by(threshold) %>% 
  do(tidy(nls(formula = (mean_targets ~ a * number_of_species^b),data = .,  start = c(a=1, b=0.1)), conf.int = TRUE)) %>% 
  filter(term == "b")

b_terms %>% 
  ungroup() %>% 
  mutate(threshold = as.numeric(threshold)) %>% 
  ggplot(aes(x = threshold, y = estimate, color = factor(threshold))) + geom_point() +
  geom_errorbar(aes(x = threshold, ymin = conf.low, ymax = conf.high, color = factor(threshold)), width = 0.1) +
  ylab("b estimate (biodiversity effect)") +
  xlab("% of upper tolerable limit in a portion") +
  scale_color_viridis_d(name = "Threshold")
ggsave("figures/toxin-accumulation-b-estimates.pdf", width = 4, height = 3)

library(broom)
mean_target_100 %>% 
filter(number_of_species < 11) %>% 
  do(tidy(nls(formula = (mean_targets ~ a * number_of_species^b),data = .,  start = c(a=1, b=0.1)), conf.int = TRUE)) %>% 
  filter(term == "b")

mean_target_100 %>% 
  filter(number_of_species < 11) %>% 
  do(tidy(lm(log(mean_targets) ~ log(number_of_species), data = .,)))

mean_target_100 %>% 
  filter(number_of_species < 11) %>% 
  do(tidy(nls(formula = (mean_targets ~ a * number_of_species^b),data = .,  start = c(a=1, b=0.1)), conf.int = TRUE)) %>% 
  filter(term == "b")

plot2 <- ggplot() +
  # geom_line(aes(x = number_of_species, y = mean_targets), data = mean_target_20, color = "green") +
  # geom_line(aes(x = number_of_species, y = mean_targets), data = mean_target_10, color = "orange") +
  geom_line(aes(x = number_of_species, y = mean_targets), data = mean_target_100, color = "black") +
  # geom_line(aes(x = number_of_species, y = mean_targets), data = mean_target_50, color = "blue") +
  geom_ribbon(aes(x = number_of_species, ymin = low, ymax = high), alpha = 0.2, data = mean_target_100) +
  # geom_ribbon(aes(x = number_of_species, ymin = low, ymax = high), alpha = 0.2, data = mean_target_50, fill = "blue") +
  xlab("Species richness") +
  ylab("Nc (# PDTI)") +
  scale_x_continuous(breaks = seq(1,10,1))+
  scale_y_continuous(breaks = seq(1,4,0.5))
ggsave("figures/tol-limits-bef-50-100-thres-4.png", width = 6, height = 4)
ggsave("figures/tol-limits-bef-100-thres-4.png", width = 6, height = 4)
ggsave("figures/tol-limits-bef-50-100-thres-4.pdf", width = 6, height = 4)
  

### contaminant plots

library(patchwork)

plot1 <- allcon %>% 
  mutate(contaminant = ifelse(contaminant == "mercury", "Methylmercury", contaminant)) %>% 
  mutate(contaminant = ifelse(contaminant == "arsenic", "Arsenic", contaminant)) %>% 
  mutate(contaminant = ifelse(contaminant == "lead", "Lead", contaminant)) %>% 
  mutate(contaminant = ifelse(contaminant == "cadmium", "Cadmium", contaminant)) %>% 
  ggplot(aes(x = concentration)) + geom_histogram() +
  facet_wrap( ~ contaminant, scales = "free", nrow = 2, ncol = 2) +
  ylab("Count") + xlab("Percentage of PTDI in 100g portion") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 2)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
  theme(strip.background =element_rect(fill="transparent"))
plot2 <- CS_preds %>% 
  ggplot(aes(x = species_no, y = mean)) + geom_line() +
  geom_ribbon(aes(x = species_no, ymin = q2.5, ymax = q97.5), alpha = 0.1) +
  xlab("Species richness") +
  ylab("Nc (# PDTI)") +
  scale_x_continuous(breaks = seq(1,10,1))+
  scale_y_continuous(breaks = seq(1,2,0.15))

library(patchwork)
conplot <- plot1 + plot2 +  plot_layout(widths = c(1.2, 1))
ggsave(plot =  conplot, file = "figures/conplot-updated.pdf", width = 8, height = 3.5)
  # geom_line(aes(x = number_of_species, y = mean_targets), data = mean_target_50, color = "pink") 
