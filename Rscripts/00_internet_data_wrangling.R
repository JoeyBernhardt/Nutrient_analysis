library(tidyverse)


species_list <- read_csv("/Users/Joey/Documents/temporary/species_list_bogard.csv")
fatty_list <- read_csv("/Users/Joey/Documents/temporary/fatty_acids_bogard.csv")
vitamins_list <- read_csv("/Users/Joey/Documents/temporary/vitamins_bogard.csv")

all <- left_join(species_list, fatty_list)

vitamins <- left_join(species_list, vitamins_list)


write_csv(all, "/Users/Joey/Documents/temporary/all_fatty_acids_bogard.csv")
write_csv(vitamins, "/Users/Joey/Documents/temporary/all_vitamins_bogard.csv")
