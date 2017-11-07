### fit power function the grams required curves

library(tidyverse)
library(broom)
yupik_resampling <- read_csv("data-processed/grams-required-10-spp-100reps-yupik.csv")


yupik <- yupik_resampling %>% 
  filter(!is.infinite(grams_required))


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
  bind_rows(yupik_sum)


all %>% 
  dplyr::group_by(dataset) %>% 
  do(tidy(nls(formula = (median ~ a * species_no^b),data = .,  start = c(a=10000, b=-0.7)))) %>% 
  ggplot(aes(x = dataset, y = estimate)) + geom_point() + 
  facet_wrap( ~ term, scales = "free") + theme_classic()


ggplot(aes(x = species_no, y = median, color = dataset), data = all) + geom_line(size = 1) +
  theme_classic() + ylab("Median grams required to reach 5 DRI targets") + xlab("Species richness") +
  scale_x_continuous(breaks = 1:10) +
  scale_color_viridis(discrete = TRUE)


