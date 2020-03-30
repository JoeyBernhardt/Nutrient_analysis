

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
  filter(part == "muscle") %>% 
  # select(6:20) %>% 
  filter(cadmium != 2.61) %>% 
  filter(nickel != 1.810) 

str(tox)

names_tox <- names(tox)

tox_sum <- tox %>% 
  group_by(species) %>% 
  summarise_at(c(names_tox[6:20]), mean) %>% 
  select(2:16)
  
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
  ggplot(aes(x = value)) + geom_histogram(bins = 35) +
  geom_vline(xintercept = 0) +
  ylab("Frequency") + xlab("Correlation coefficient") 
ggsave("figures/toxin-correlations.pdf", width = 8, height = 6)

library(GGally)
ggcorr(tox_sum, method = c("everything", "pearson"), label = TRUE) 
ggsave("figures/trace-elements-correlation_sum.pdf", width = 8, height = 6)


ggpairs(tox_sum) 
ggsave("figures/tox-corr-plots-sum.pdf", width = 15, height = 15)


tox_sum %>% 
  gather(1:15, key = element, value = concentration) %>% 
  mutate(concentration = ifelse(concentration == 0, 0.001, concentration)) %>% 
  ggplot(aes(x = concentration)) + geom_histogram() +
  facet_wrap( ~ element, scales = "free") 
ggsave("figures/elements-hist.pdf", width = 10, height = 8)



#### accumulate toxins

times_hundred <- function(x, na.rm = FALSE) (x*100)

tox_sum2 <- tox %>% 
  group_by(species) %>% 
  summarise_at(c(names_tox[6:20]), mean) %>% 
  mutate_at(c(names_tox[6:20]), times_hundred)

data <- tox_sum2
threshold <- 0.5
library(vegan)

?specaccum

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

accumulated_targets_all %>% 
  filter(number_of_species < 11) %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets)) + geom_point() +
  geom_errorbar(aes(x = number_of_species, ymin = number_of_targets - se, ymax = number_of_targets + se), width = 0.1)


accumulated_targets_all %>% 
  filter(number_of_species < 11) %>% 
  lm(log(number_of_targets) ~ log(number_of_species), data = .) %>% summary()

library(broom)

accumulated_targets_all %>% 
  filter(number_of_species < 11) %>% 
do(tidy(nls(formula = (number_of_targets ~ a * number_of_species^b),data = .,  start = c(a=1, b=0.1)))) 



rep_acc_function <- function(x) accumulate(sample_n(tox_sum2, size = x, replace = FALSE), threshold = 1)


samples <- rep(40, 100)


repeat_global <- samples %>% 
  map_df(rep_acc_function, .id = "run")


mean_target <- repeat_global %>% 
  group_by(number_of_species) %>% 
  summarise(mean_targets = mean(number_of_targets),
            low=quantile(number_of_targets, probs=0.025),
            high=quantile(number_of_targets, probs=0.975)) %>% 
  filter(number_of_species < 11) 

mean_target_10 <- repeat_global %>% 
  group_by(number_of_species) %>% 
  summarise(mean_targets = mean(number_of_targets),
            low=quantile(number_of_targets, probs=0.025),
            high=quantile(number_of_targets, probs=0.975)) %>% 
  filter(number_of_species < 11)
mean_target_20 <- repeat_global %>% 
  group_by(number_of_species) %>% 
  summarise(mean_targets = mean(number_of_targets),
            low=quantile(number_of_targets, probs=0.025),
            high=quantile(number_of_targets, probs=0.975)) %>% 
  filter(number_of_species < 11)

mean_target_50 <- repeat_global %>% 
  group_by(number_of_species) %>% 
  summarise(mean_targets = mean(number_of_targets),
            low=quantile(number_of_targets, probs=0.025),
            high=quantile(number_of_targets, probs=0.975)) %>% 
  filter(number_of_species < 11) 
mean_target_70 <- repeat_global %>% 
  group_by(number_of_species) %>% 
  summarise(mean_targets = mean(number_of_targets),
            low=quantile(number_of_targets, probs=0.025),
            high=quantile(number_of_targets, probs=0.975)) %>% 
  filter(number_of_species < 11)

mean_target_100 <- repeat_global %>% 
  group_by(number_of_species) %>% 
  summarise(mean_targets = mean(number_of_targets),
            low=quantile(number_of_targets, probs=0.025),
            high=quantile(number_of_targets, probs=0.975)) %>% 
  filter(number_of_species < 11)


write_csv(mean_target, "data-processed/global_40_species_resampled_accumulation_mean_toxins_50percent.csv")
write_csv(mean_target_10, "data-processed/global_40_species_resampled_accumulation_mean_toxins_10percent.csv")
write_csv(mean_target_20, "data-processed/global_40_species_resampled_accumulation_mean_toxins_20percent.csv")

mean_target_10 <- mean_target_10 %>% 
  mutate(threshold = 10)
mean_target_20 <- mean_target_20 %>% 
  mutate(threshold = 20)
mean_target_50 <- mean_target_50 %>% 
  mutate(threshold = 50)
mean_target_70 <- mean_target_70 %>% 
  mutate(threshold = 70)
mean_target_100 <- mean_target_100 %>% 
  mutate(threshold = 100)

all_targets <- bind_rows(mean_target_10, mean_target_20, mean_target_50, mean_target_70, mean_target_100)

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
  ggplot(aes(x = number_of_species, y = mean_targets, color = threshold, group = threshold)) + geom_line() +
  geom_ribbon(aes(x = number_of_species, ymin = low, ymax = high, fill = threshold), alpha = 0.5) +
  scale_color_viridis_c() + 
  scale_fill_viridis_c() +
  ylab("Number of tolerable limits exceeded per 100g portion") +
  xlab("Species richness")
ggsave("figures/toxin-accumulation.pdf", width = 8, height = 6)


b_terms <- all_targets %>% 
  filter(number_of_species < 11) %>% 
  group_by(threshold) %>% 
  do(tidy(nls(formula = (mean_targets ~ a * number_of_species^b),data = .,  start = c(a=1, b=0.1)), conf.int = TRUE)) %>% 
  filter(term == "b")

b_terms %>% 
  ggplot(aes(x = threshold, y = estimate, color = threshold)) + geom_point() +
  geom_errorbar(aes(x = threshold, ymin = conf.low, ymax = conf.high, color = threshold), width = 0.1) +
  ylab("b estimate (biodiversity effect)") +
  xlab("% of upper tolerable limit in a portion") +
  scale_color_viridis_c()
ggsave("figures/toxin-accumulation-b-estimates.pdf", width = 4, height = 3)

