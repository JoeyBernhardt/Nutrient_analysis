### goal of this script is to find the diversity effect for each level of DRI threshold

library(tidyverse)
library(purrr)
library(plotrix)

targets <- read_csv("data-processed/targets_richness.csv")


targets_split <- targets %>% 
filter(number_of_species < 11) %>% 
  split(.$threshold1)


nderiv <- function(fit, x, eps=1e-5){(predict(fit, x + eps) - predict(fit, x - eps))/(2 * eps)}

spline.slope.single <- function(x, y,  n=101, eps=1e-5, span=0.2){
  max(nderiv(loess(log(y) ~ x, degree=1, span=span), seq(min(df$x), max(df$x), length=n)), na.rm=TRUE)
}

spline.slope <- function(df, n=101, eps=1e-5, span=0.5){
  x <- df$number_of_species
  y <- df$number_of_targets
  species_number <- seq(min(x), max(x), length=n)
  diversity_effect <- nderiv(loess(y ~ x, degree=1, span=span), species_number)
  output <- top_n(data.frame(diversity_effect, species_number), n = 1, wt = diversity_effect)
  return(output)
}



results <- targets_split %>% 
  map_df(spline.slope, .id = "threshold")

  
  
results %>%
  mutate(threshold = as.numeric(threshold)) %>% 
  mutate(threshold = threshold*100) %>% 
  group_by(threshold) %>% 
  summarise_each(funs(mean, std.error), diversity_effect) %>% 
  ggplot(aes(x = threshold, y = mean)) + geom_point() + theme_bw() +
  geom_errorbar(aes(ymin = mean-std.error, ymax = mean + std.error)) +
  # geom_smooth() + 
  xlab("percentage of DRI threshold") + ylab("diversity effect (max slope of diversity-function curve)")

ggsave("figures/diversity_effect_DRI_30spp.pdf")

targets %>%
filter(number_of_species < 30) %>% 
  mutate(threshold1 = threshold1*100) %>% 
  group_by(threshold1) %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets, group = threshold1, color = threshold1)) + geom_line() +
  theme_bw() +
  # geom_hline(yintercept = 2, linetype = "dashed") +
  scale_color_gradient(name="Percent of DRI", low="blue", high="red") + xlab("species richness") + ylab("number of functions (distinct DRI targets reached)")
ggsave("figures/multifunction_curve_DRI_30spp.pdf")



targets %>%
  filter(number_of_species < 11) %>% 
  # mutate(threshold1 = threshold1*100) %>% 
  mutate(grams = 100/threshold1) %>% 
  filter(grams < 350) %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets, group = grams, color = grams)) + geom_line(size = 1) +
  theme_bw() +
  scale_color_gradient(name="grams required", low="blue", high="red") + xlab("species richness") + ylab("number of functions (distinct DRI targets reached)")



results %>%
  mutate(threshold = as.numeric(threshold)) %>% 
  mutate(grams = 100/threshold) %>% 
  filter(grams < 750) %>% 
  group_by(grams) %>% 
  summarise_each(funs(mean, std.error), diversity_effect) %>% 
  ggplot(aes(x = grams, y = mean)) + geom_point() + theme_bw() +
  geom_errorbar(aes(ymin = mean-std.error, ymax = mean + std.error)) +
  geom_smooth() +
  xlab("grams consumed") + ylab("diversity effect (max slope of diversity-function curve)")





targets %>% 
  filter(number_of_species < 30) %>% 
  filter(threshold1 == 1) %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets, group = threshold1, color = threshold1)) + geom_line() +
  theme_bw()
  
### now bring in null model

trait_data_pro <- read_csv("data-processed/micronutrients-species-mean.csv")


trait_data_pro %>% 
  filter(species_name %in% c("Fenneropenaeus indicus", "Puntius sophore", "Amblypharyngodon mola")) %>% View

number_of_grams <- 100



trait_data_pro %>% 
  filter(species_name == "Fenneropenaeus indicus") %>%
  select(calcium, iron, zinc, epa, dha) %>% 
  gather() %>% 
  mutate(per_200g = value*2) %>% View

  
trait_data_pro %>% 
  filter(species_name == "Hexaplex trunculus") %>% View
  

## ok what if I divide all the nutrients by their RDI and then add them up
trait_data_pro %>% 
  # filter(species_name == "Carcinus maenus") %>% 
mutate(calcium_rdi = calcium/(1200),
       zinc_rdi = zinc/(11),
       iron_rdi = iron/(18),
       epa_rdi = epa/(1),
       dha_rdi = dha/(1)) %>% 
  mutate(total_points = calcium_rdi + zinc_rdi + iron_rdi + epa_rdi + dha_rdi) %>% 
  # top_n(n = 1, wt = total_points) %>% 
  dplyr::select(species_name, subgroup, contains("rdi")) %>% 
  gather(key = nutrient, value = concentration, contains("rdi")) %>% 
  mutate(meets_target = ifelse(concentration >= 1, "1", "0")) %>% 
  group_by(species_name, subgroup) %>% 
  arrange(meets_target) %>% 
  mutate(total_targets = cumsum(meets_target)) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(total_targets_per_species = max(total_targets)) %>% View
  ggplot(aes(x = reorder(species_name, -total_targets_per_species), y = total_targets_per_species)) + geom_point() +
  theme_bw() + xlab("species") + ylab("number of DRI targets reached per 100g portion (10% DRI)") +
  # theme(axis.text.y= element_text(angle = 30, hjust = 1)) +
  coord_flip()
ggsave("figures/species_by_RDI_tagets_reached_25_percentRDI.pdf")
ggsave("figures/species_by_RDI_tagets_reached_10_percentRDI.pdf")


