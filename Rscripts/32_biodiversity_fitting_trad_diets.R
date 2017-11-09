### fit power function the grams required curves

library(tidyverse)
library(broom)
library(viridis)
yupik_resampling <- read_csv("data-processed/grams-required-10-spp-100reps-yupik.csv")
all_trad_resampling <- read_csv("data-processed/all_trad_reps.csv")
bang_resampling <- read_csv("data-processed/grams-required-10-spp-1000reps-bangladesh.csv")
bang_data <- read.csv("data-processed/bangladesh-micronutrients.csv")

bang_sum <- bang_resampling %>% 
  filter(!is.infinite(grams_required)) %>%
  mutate(grams_required = grams_required/10) %>% 
  group_by(species_no) %>% 
  summarise_each(funs(mean, median), grams_required) %>% 
  mutate(dataset = "bangladesh") %>% 
  rename(median = grams_required_median,
         mean = grams_required_mean) 
  



all_resamp <- all_trad_resampling %>% 
  filter(!is.infinite(grams_required)) %>% 
  group_by(culture, species_no) %>%
  mutate(grams_required = grams_required/10) %>% 
  summarise_each(funs(mean, median), grams_required) %>% 
  rename(median = grams_required_median,
         mean = grams_required_mean) %>% 
  rename(dataset = culture) %>% 
  filter(!dataset %in% c("Coosan", "Tillamook", "Siuslaw", "Quileute", "Eyak", "Chinook")) ## these only have 11 distinct species

resamp <- bind_rows(all_resamp, bang_sum)

yupik_sum <- yupik %>% 
  group_by(species_no) %>% 
  summarise_each(funs(mean, median), grams_required) %>% 
  rename(median = grams_required_median,
         mean = grams_required_mean) %>% 
  mutate(dataset = "yupik")


yupik_power <- nls(formula=(grams_required_median ~ a * species_no^b), data=yupik_sum, start = c(a=10000, b=-0.7))
a.yupik <- coef(yupik_power)[1]
b.yupik <- coef(yupik_power)[2]


all <- read_csv("data-processed/summary_resampling_global_bang_inuit.csv") %>% 
  bind_rows(all_resamp)


all %>% 
  dplyr::group_by(dataset) %>% 
  filter(species_no < 6) %>% 
  do(tidy(nls(formula = (median ~ a * species_no^b),data = .,  start = c(a=10000, b=-0.7)))) %>% 
  ggplot(aes(x = reorder(dataset, estimate), y = estimate)) + geom_point() + 
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error)) +
  facet_wrap( ~ term, scales = "free") + theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +xlab("local population")
ggsave("figures/trad_diets_fitted_power_coefs.png")


all %>% 
  filter(species_no < 6) %>% 
ggplot(aes(x = species_no, y = median, color = dataset)) + geom_line(size = 1) +
  theme_classic() + ylab("Median grams required to reach 5 DRI targets") + xlab("Species richness") +
  scale_x_continuous(breaks = 1:10) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap( ~ dataset)

ggsave("figures/all-bef-curves-trad-1000reps.png", width = 8, height = 6)



central_power <- nls(formula=(mean ~ a * species_no^b), data=subset(all, dataset == "Central Salish"), start = c(a=100, b=-0.9))
a.central <- coef(central_power)[1]
b.central <- coef(central_power)[2]
central_power_function <- function(x) a.central*x^b.central

summary(central_power)


View(subset(all, dataset == "bangladesh"))

  all %>% 
  filter(dataset == "Central Salish") %>% 
  ggplot(aes(x = species_no, y = mean, color = dataset)) + geom_line(size = 1) +
  stat_function(fun = central_power_function, color = "grey") 
  
  +
  # stat_function(fun = inuit_power_function, color = "grey") +
  # stat_function(fun = global_power_function, color = "grey") +
  theme_classic() + ylab("Median grams required to reach 5 DRI targets") + xlab("Species richness") +
  scale_x_continuous(breaks = 1:10) +
  scale_color_viridis(discrete = TRUE)
