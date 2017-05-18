### goal of this script is to find the diversity effect for each level of DRI threshold

library(tidyverse)
library(purrr)
library(plotrix)

targets <- read_csv("data-processed/targets_richness.csv")


targets_split <- targets %>% 
filter(number_of_species < 30) %>% 
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
  geom_errorbar(aes(ymin = mean-std.error, ymax = mean + std.error)) + geom_smooth() + 
  xlab("percentage of DRI threshold") + ylab("diversity effect (max slope of diversity-function curve)")

ggsave("figures/diversity_effect_DRI_30spp.pdf")

targets %>%
filter(number_of_species < 30) %>% 
  mutate(threshold1 = threshold1*100) %>% 
  group_by(threshold1) %>% 
  ggplot(aes(x = log(number_of_species), y = log(number_of_targets), group = threshold1, color = threshold1)) + geom_line() +
  theme_bw() +
  scale_color_gradient(name="Percent of DRI", low="blue", high="red") + xlab("log species richness") + ylab("log number of functions (distinct DRI targets reached)")
ggsave("figures/multifunction_curve_DRI_30spp.pdf")
