

library(tidyverse)
library(viridis)


nuts_trad <- read_csv("data-processed/trad-foods-cleaned.csv")


nuts_trad %>% 
  filter(reference != "88") %>% View
  gather(key = "nutrient", value = "concentration", 9:19) %>% 
  ggplot(aes(x = latin_name, y = concentration)) + geom_point() +
  facet_wrap( ~ nutrient, scales = "free") + theme_classic()
  