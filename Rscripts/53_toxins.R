

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
  filter(nickel != 1.810) 

str(tox)

names_tox <- names(tox)

tox_sum <- tox %>% 
  group_by(species) %>% 
  summarise_at(c(names_tox[6:20]), mean) %>%
  select(species, 2:16) %>% 
  gather(key = nutrient, value = concentration, 2:16) %>% 
  mutate(concentration = ifelse(concentration == 0, 0.00001, concentration)) %>%
  spread(key= nutrient, value = concentration) %>% 
  select(-species) %>% 
  mutate_all(.funs = log)
  

tox_sum4 <- tox %>% 
  group_by(subgroup, species) %>% 
  summarise_at(c(names_tox[6:20]), mean) %>% 
  gather(key = element, value = concentration, 3:17)


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
ggcorr(tox_sum, method = c("everything", "pearson"), label = TRUE) 
ggsave("figures/trace-elements-correlation_sum.pdf", width = 8, height = 6)



ggpairs(tox_sum) 
ggsave("figures/tox-corr-plots-sum.pdf", width = 15, height = 15)



tox_sum %>% 
  gather(1:15, key = element, value = concentration) %>% 
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



tox_sum <- tox %>% 
  group_by(species) %>% 
  summarise_at(c(names_tox[6:20]), mean) %>% 
  select(2:16)
  
tscale <- scale(tox_sum, center = TRUE, scale = TRUE)

pca_size <- prcomp(tox_sum, scale. = TRUE)

pca_size2 <- rda(tox_sum, scale. = TRUE)

summary(pca_size)
summary(pca_size2)


pcas <- as.data.frame(scores(pca_size2, choices = 1:2)$sites)
pcas1 <- as.data.frame(scores(pca_size2, choices = 1:2)$species) %>% 
  mutate(trait = rownames(.))
pcas %>% 
  ggplot(aes(x = PC1, y = PC2)) + geom_point(size = 3, alpha = 0.5) +
  geom_point(size = 3, color = "black", shape = 1) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  geom_text(data = pcas1, aes(x = PC1, y = PC2, label = trait), col = 'cadetblue') +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2, text =  trait), data = pcas1, color = "cadetblue",
               arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  ylab("PC2 (13.22% variance)") + xlab("PC1 (26.31% variance)")
ggsave("figures/toxin-pca.png", width = 8, height = 6)


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



# try the accumulation with only 5 toxins ---------------------------------
data <- tox_sum2 %>% 
  select(species, cadmium, copper, chromium, arsenic, mercury, lead)
threshold <- 1
accumulate <- function(data, threshold) {
  ntbl.RDI.all <- data %>% 
    mutate(ul_cadmium = ifelse(cadmium > (28.57*threshold), 1, 0)) %>% 
    mutate(ul_chromium = ifelse(chromium > (12000*threshold), 1, 0)) %>% 
    mutate(ul_arsenic = ifelse(arsenic > (800*threshold), 1, 0)) %>%
    mutate(ul_mercury = ifelse(mercury > (18.29*threshold), 1, 0)) %>%
    mutate(ul_lead = ifelse(lead > (12.5*threshold), 1, 0)) %>%
    ungroup() %>% 
    mutate(RDI.micro.tot = rowSums(.[8:12])) %>% 
    filter(!is.na(RDI.micro.tot)) 
  
  uls <- names(ntbl.RDI.all)[8:12]
  
  all_spa <- ntbl.RDI.all %>% 
    dplyr::select(species, 8:12) %>% 
    mutate(subgroup = "all") %>% 
    split( .$subgroup) %>% 
    map(.f = `[`, c(names(ntbl.RDI.all)[8:12])) %>%
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



rep_acc_function <- function(x) accumulate(sample_n(tox_sum2, size = x, replace = FALSE), threshold = 1)


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

mean_target_10 <- mean_target_10 %>% 
  mutate(threshold = 10)
mean_target_20 <- mean_target_25 %>% 
  mutate(threshold = 20)
mean_target_50 <- mean_target_50 %>% 
  mutate(threshold = 50)
mean_target_70 <- mean_target_75 %>% 
  mutate(threshold = 70)
mean_target_100 <- mean_target_100 %>% 
  mutate(threshold = 100)

all_targets <- bind_rows(mean_target_10, mean_target_25, mean_target_50, mean_target_75, mean_target_100)

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

ggplot() +
  # geom_line(aes(x = number_of_species, y = mean_targets), data = mean_target_20, color = "green") +
  # geom_line(aes(x = number_of_species, y = mean_targets), data = mean_target_10, color = "orange") +
  geom_line(aes(x = number_of_species, y = mean_targets), data = mean_target_100, color = "black") +
  geom_line(aes(x = number_of_species, y = mean_targets), data = mean_target_50, color = "blue") +
  geom_ribbon(aes(x = number_of_species, ymin = low, ymax = high), alpha = 0.5, data = mean_target_100) +
  geom_ribbon(aes(x = number_of_species, ymin = low, ymax = high), alpha = 0.5, data = mean_target_50, fill = "blue") +
  xlab("Species richness") +
  ylab("Number of tolerable limits \nexceeded per 100g portion") +
  scale_x_continuous(breaks = seq(1,10,1))+
  scale_y_continuous(breaks = seq(1,12,1))
ggsave("figures/tol-limits-bef-50-100-thres.png", width = 6, height = 4)
ggsave("figures/tol-limits-bef-50-100-thres.pdf", width = 6, height = 4)
  
  # geom_line(aes(x = number_of_species, y = mean_targets), data = mean_target_50, color = "pink") 
