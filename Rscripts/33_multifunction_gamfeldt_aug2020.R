
library(purrr)
library(dplyr)
library(tidyr)
library(readr)
library(plotrix)
library(ggplot2)
library(broom)
library(gridExtra)
library(grid)
library(cowplot)
library(nlstools)

mean_nuts <- read_csv("data-processed/mean_nuts.csv")
mean_seadiv_raw <- read_csv("data-processed/mean_seadiv.csv")
new_global <- read_csv("data-processed/new_global.csv") 
mn_sp <- mean_nuts$species_name
mean_nuts_new <- read_csv("data-processed/mean_nuts_aug2020_micro_macro.csv") %>%
  mutate(genus_species = ifelse(genus_species == "Dentex spp", "Dentex sp.", genus_species)) %>%
  mutate(genus_species = ifelse(genus_species == "Mya spp.", "Mya spp", genus_species)) %>% 
  mutate(genus_species = ifelse(genus_species == "Sardinella spp", "Sardinella sp.", genus_species)) %>%
  filter(genus_species %in% new_global$species_name) ### update aug 2020

write_csv(mean_nuts_new, "data-processed/mean_nuts_new.csv")

new_global <- new_global %>% 
  filter(species_name != "Ostreidae") %>% 
  filter(iron < 100)

mean(new_global$zinc)
mean(mean_nuts$zinc)

mean_seadiv2 <- mean_seadiv_raw %>% 
  filter(species_name %in% mn_sp) 

protein_data <- mean_seadiv2 %>% 
  select(species_name, protein) %>%
  filter(!is.na(protein)) 

mean_nuts_protein <- left_join(mean_nuts, protein_data, by = "species_name") %>% 
  filter(!is.na(protein))

mean_seadiv <- mean_seadiv_raw %>% 
  filter(!grepl("juvenile", species_name)) %>% 
  filter(species_name != "Ostreidae")

length(unique(mean_seadiv$species_name))

hist(mean_seadiv$iron)
hist(mean_nuts$iron)


mn2 <- mean_nuts
mn2$dataset <- "mean_nuts"

sd2 <- mean_seadiv
sd2$dataset <- "mean_seadiv"



all_nuts <- bind_rows(mn2, sd2)

all_nuts %>% 
  select(-protein) %>% 
  select(-fat) %>% 
  gather(key = nutrient, value = concentration, 3:7) %>% 
  group_by(nutrient) %>% 
  ggplot(aes(x = nutrient, y = log(concentration), group = dataset, color = dataset)) + geom_boxplot() +
  facet_wrap( ~ nutrient, scales = "free")


sample_size <- 10

dataset <- mean_nuts

nutrient_fishing_function <- function(sample_size) {
  ntbl_sub1 <- mean_nuts_new %>% 
    sample_n(size = sample_size, replace = FALSE)
  
  sample_list <- NULL
  for (i in 1:nrow(ntbl_sub1) ) {
    output <- combn(nrow(ntbl_sub1), i, FUN=function(x) ntbl_sub1[x,], simplify = FALSE)
    output <- bind_rows(output, .id = "sample_id")
    subsample_size <- rep(i, nrow(output))
    output <- cbind(output, subsample_size)
    sample_list <- rbind(sample_list,output)
  }
  
  sample_list <- split(sample_list, f = sample_list$subsample_size)
  
  new_data_sub1 <- sample_list %>% 
    map_df(`[`, .id = "replicate")
  
  resampling_15 <- new_data_sub1 %>% 
    dplyr::rename(species_number = subsample_size) %>%
    group_by(species_number, sample_id) %>% 
    mutate(cal_total = (calcium/species_number)) %>% ## get the amount of calcium each species will contribute
    mutate(zinc_total = (zinc/species_number)) %>% 
    mutate(iron_total = (iron/species_number)) %>% 
    mutate(epa_total = (epa/species_number)) %>% 
    # mutate(protein_total = (protein/species_number)) %>%
    # mutate(fat_total = (fat/species_number)) %>%
    mutate(dha_total = (dha/species_number)) %>%
    summarise_each(funs(sum), contains("total")) %>%  ## sum up all of each of the nutrients
    mutate(cal_grams = (cal_total/(1200))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
    mutate(iron_grams = (iron_total/(18))) %>%
    mutate(zinc_grams = (zinc_total/(11))) %>% 
    mutate(epa_grams = (epa_total/(1))) %>%
    mutate(dha_grams = (dha_total/(1))) %>%
    # mutate(fat_grams = (fat_total/(78))) %>%
    # mutate(protein_grams = (protein_total/(56))) %>%
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no, sample_id) %>% 
    select(-contains("total")) %>% 
    gather(key = nutrient, value = concentration, contains("grams")) %>% 
    group_by(species_no, sample_id) %>% 
    summarise(min_percentage = min(concentration)) %>%
    mutate(grams_required = 100/min_percentage)
}


samples_rep <- rep(10, 100)


output_all5m <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run") %>% 
  mutate(nutrient = "all_5_micronutrients")

# write_csv(output_all5, "data-processed/all_5_micronutrients_grams_required_mean_nuts.csv")
# write_csv(output_all5, "data-processed/all_5_micronutrients_grams_required.csv")


# calcium -----------------------------------------------------------------
dataset <- mean_nuts
nutrient_fishing_calcium <- function(sample_size) {
  ntbl_sub1 <- mean_nuts_new %>% 
    sample_n(size = sample_size, replace = FALSE)
  
  sample_list <- NULL
  for (i in 1:nrow(ntbl_sub1) ) {
    output <- combn(nrow(ntbl_sub1), i, FUN=function(x) ntbl_sub1[x,], simplify = FALSE)
    output <- bind_rows(output, .id = "sample_id")
    subsample_size <- rep(i, nrow(output))
    output <- cbind(output, subsample_size)
    sample_list <- rbind(sample_list,output)
  }
  
  sample_list <- split(sample_list, f = sample_list$subsample_size)
  
  new_data_sub1 <- sample_list %>% 
    map_df(`[`, .id = "replicate")
  
  resampling_15 <- new_data_sub1 %>% 
    dplyr::rename(species_number = subsample_size) %>%
    group_by(species_number, sample_id) %>% 
    mutate(cal_total = (calcium/species_number)) %>% ## get the amount of calcium each species will contribute
       summarise_each(funs(sum), contains("total")) %>% ## sum up all of each of the nutrients
    mutate(cal_grams = (cal_total/(1200))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no, sample_id) %>% 
    select(-contains("total")) %>% 
    gather(key = nutrient, value = concentration, contains("grams")) %>% 
    group_by(species_no, sample_id) %>% 
    summarise(min_percentage = min(concentration)) %>% 
    mutate(grams_required = 100/min_percentage)
}
output_calciumm <- samples_rep %>% 
  map_df(nutrient_fishing_calcium, .id = "run") %>% 
  mutate(nutrient = "calcium")

# write_csv(output_calcium, "data-processed/calcium_grams_required_mean_nuts.csv")
# write_csv(output_calcium, "data-processed/calcium_grams_required.csv")
# iron --------------------------------------------------------------------
dataset <- mean_nuts
nutrient_fishing_iron <- function(sample_size) {
  ntbl_sub1 <- mean_nuts_new %>% 
    sample_n(size = sample_size, replace = FALSE)
  
  sample_list <- NULL
  for (i in 1:nrow(ntbl_sub1) ) {
    output <- combn(nrow(ntbl_sub1), i, FUN=function(x) ntbl_sub1[x,], simplify = FALSE)
    output <- bind_rows(output, .id = "sample_id")
    subsample_size <- rep(i, nrow(output))
    output <- cbind(output, subsample_size)
    sample_list <- rbind(sample_list,output)
  }
  
  sample_list <- split(sample_list, f = sample_list$subsample_size)
  
  new_data_sub1 <- sample_list %>% 
    map_df(`[`, .id = "replicate")
  
  resampling_15 <- new_data_sub1 %>% 
    dplyr::rename(species_number = subsample_size) %>%
    group_by(species_number, sample_id) %>% 
    mutate(iron_total = (iron/species_number)) %>% 
    summarise_each(funs(sum), contains("total")) %>% ## sum up all of each of the nutrients
    mutate(iron_grams = (iron_total/(18))) %>%
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no, sample_id) %>% 
    select(-contains("total")) %>% 
    gather(key = nutrient, value = concentration, contains("grams")) %>%
    group_by(species_no, sample_id) %>% 
    summarise(min_percentage = min(concentration)) %>% 
    mutate(grams_required = 100/min_percentage)
}


output_ironm <- samples_rep %>% 
  map_df(nutrient_fishing_iron, .id = "run") %>% 
  mutate(nutrient = "iron")

# write_csv(output_iron_nuts, "data-processed/iron_grams_required_mean_nuts.csv")
# write_csv(output_iron, "data-processed/iron_grams_required.csv")

# zinc --------------------------------------------------------------------
dataset <- mean_nuts
nutrient_fishing_zinc <- function(sample_size) {
  ntbl_sub1 <- mean_nuts_new %>% 
    sample_n(size = sample_size, replace = FALSE)
  
  sample_list <- NULL
  for (i in 1:nrow(ntbl_sub1) ) {
    output <- combn(nrow(ntbl_sub1), i, FUN=function(x) ntbl_sub1[x,], simplify = FALSE)
    output <- bind_rows(output, .id = "sample_id")
    subsample_size <- rep(i, nrow(output))
    output <- cbind(output, subsample_size)
    sample_list <- rbind(sample_list,output)
  }
  
  sample_list <- split(sample_list, f = sample_list$subsample_size)
  
  new_data_sub1 <- sample_list %>% 
    map_df(`[`, .id = "replicate")
  
  resampling_15 <- new_data_sub1 %>% 
    dplyr::rename(species_number = subsample_size) %>%
    group_by(species_number, sample_id) %>% 
    mutate(zinc_total = (zinc/species_number)) %>% 
    summarise_each(funs(sum), contains("total")) %>% ## sum up all of each of the nutrients
    mutate(zinc_grams = (zinc_total/(11))) %>% 
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no, sample_id) %>% 
    select(-contains("total")) %>% 
    gather(key = nutrient, value = concentration, contains("grams")) %>% 
    group_by(species_no, sample_id) %>% 
    summarise(min_percentage = min(concentration)) %>% 
    mutate(grams_required = 100/min_percentage)
}


output_zincm <- samples_rep %>% 
  map_df(nutrient_fishing_zinc, .id = "run") %>% 
  mutate(nutrient = "zinc")
# 
# write_csv(output_zinc, "data-processed/zinc_grams_required_mean_nuts.csv")
# write_csv(output_zinc, "data-processed/zinc_grams_required.csv")

# epa ---------------------------------------------------------------------
dataset <- mean_nuts
nutrient_fishing_epa <- function(sample_size) {
  ntbl_sub1 <- mean_nuts_new %>% 
    sample_n(size = sample_size, replace = FALSE)
  
  sample_list <- NULL
  for (i in 1:nrow(ntbl_sub1) ) {
    output <- combn(nrow(ntbl_sub1), i, FUN=function(x) ntbl_sub1[x,], simplify = FALSE)
    output <- bind_rows(output, .id = "sample_id")
    subsample_size <- rep(i, nrow(output))
    output <- cbind(output, subsample_size)
    sample_list <- rbind(sample_list,output)
  }
  
  sample_list <- split(sample_list, f = sample_list$subsample_size)
  
  new_data_sub1 <- sample_list %>% 
    map_df(`[`, .id = "replicate")
  
  resampling_15 <- new_data_sub1 %>% 
    dplyr::rename(species_number = subsample_size) %>%
    group_by(species_number, sample_id) %>% 
    mutate(epa_total = (epa/species_number)) %>%
    summarise_each(funs(sum), contains("total")) %>% ## sum up all of each of the nutrients
    mutate(epa_grams = (epa_total/(1))) %>%
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no, sample_id) %>% 
    select(-contains("total")) %>% 
    gather(key = nutrient, value = concentration, contains("grams")) %>% 
    group_by(species_no, sample_id) %>% 
    summarise(min_percentage = min(concentration)) %>% 
    mutate(grams_required = 100/min_percentage)
}

output_epam <- samples_rep %>% 
  map_df(nutrient_fishing_epa, .id = "run") %>% 
  mutate(nutrient = "epa")

# write_csv(output_epa, "data-processed/epa_grams_required_mean_nuts.csv")
# write_csv(output_epa, "data-processed/epa_grams_required.csv")
# dha ---------------------------------------------------------------------
dataset <- mean_nuts
nutrient_fishing_dha <- function(sample_size) {
  ntbl_sub1 <- mean_nuts_new %>% 
    sample_n(size = sample_size, replace = FALSE)
  
  sample_list <- NULL
  for (i in 1:nrow(ntbl_sub1) ) {
    output <- combn(nrow(ntbl_sub1), i, FUN=function(x) ntbl_sub1[x,], simplify = FALSE)
    output <- bind_rows(output, .id = "sample_id")
    subsample_size <- rep(i, nrow(output))
    output <- cbind(output, subsample_size)
    sample_list <- rbind(sample_list,output)
  }
  
  sample_list <- split(sample_list, f = sample_list$subsample_size)
  
  new_data_sub1 <- sample_list %>% 
    map_df(`[`, .id = "replicate")
  
  resampling_15 <- new_data_sub1 %>% 
    dplyr::rename(species_number = subsample_size) %>%
    group_by(species_number, sample_id) %>% 
    mutate(dha_total = (dha/species_number)) %>%
    summarise_each(funs(sum), contains("total")) %>% ## sum up all of each of the nutrients
    mutate(dha_grams = (dha_total/(1))) %>%
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no, sample_id) %>% 
    select(-contains("total")) %>% 
    gather(key = nutrient, value = concentration, contains("grams")) %>% 
    group_by(species_no, sample_id) %>% 
    summarise(min_percentage = min(concentration)) %>% 
    mutate(grams_required = 100/min_percentage)
}



output_dham <- samples_rep %>% 
  map_df(nutrient_fishing_dha, .id = "run") %>% 
  mutate(nutrient = "dha")

# write_csv(output_dha, "data-processed/dha_grams_required_mean_nuts.csv")
# write_csv(output_dha, "data-processed/dha_grams_required.csv")
# protein -----------------------------------------------------------------

dataset <- mean_nuts_protein
nutrient_fishing_protein <- function(sample_size) {
  ntbl_sub1 <- mean_nuts_new %>% 
    sample_n(size = sample_size, replace = FALSE)
  
  sample_list <- NULL
  for (i in 1:nrow(ntbl_sub1) ) {
    output <- combn(nrow(ntbl_sub1), i, FUN=function(x) ntbl_sub1[x,], simplify = FALSE)
    output <- bind_rows(output, .id = "sample_id")
    subsample_size <- rep(i, nrow(output))
    output <- cbind(output, subsample_size)
    sample_list <- rbind(sample_list,output)
  }
  
  sample_list <- split(sample_list, f = sample_list$subsample_size)
  
  new_data_sub1 <- sample_list %>% 
    map_df(`[`, .id = "replicate")
  
  resampling_15 <- new_data_sub1 %>% 
    dplyr::rename(species_number = subsample_size) %>%
    group_by(species_number, sample_id) %>% 
    mutate(protein_total = (protein/species_number)) %>%
    summarise_each(funs(sum), contains("total")) %>% ## sum up all of each of the nutrients
    mutate(protein_grams = (protein_total/(56))) %>%
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no, sample_id) %>% 
    select(-contains("total")) %>% 
    gather(key = nutrient, value = concentration, 3) %>% 
    group_by(species_no, sample_id) %>% 
    summarise(min_percentage = min(concentration)) %>% 
    mutate(grams_required = 100/min_percentage)
}
output_proteinm <- samples_rep %>% 
  map_df(nutrient_fishing_protein, .id = "run") %>% 
  mutate(nutrient = "protein")


# write_csv(output_protein, "data-processed/protein_grams_required.csv")
## quick diversion to plot this!


# fat ---------------------------------------------------------------------

nutrient_fishing_fat <- function(sample_size) {
  ntbl_sub1 <- dataset %>% 
    sample_n(size = sample_size, replace = FALSE)
  
  sample_list <- NULL
  for (i in 1:nrow(ntbl_sub1) ) {
    output <- combn(nrow(ntbl_sub1), i, FUN=function(x) ntbl_sub1[x,], simplify = FALSE)
    output <- bind_rows(output, .id = "sample_id")
    subsample_size <- rep(i, nrow(output))
    output <- cbind(output, subsample_size)
    sample_list <- rbind(sample_list,output)
  }
  
  sample_list <- split(sample_list, f = sample_list$subsample_size)
  
  new_data_sub1 <- sample_list %>% 
    map_df(`[`, .id = "replicate")
  
  resampling_15 <- new_data_sub1 %>% 
    dplyr::rename(species_number = subsample_size) %>%
    group_by(species_number, sample_id) %>% 
    # mutate(cal_total = (calcium/species_number)) %>% ## get the amount of calcium each species will contribute
    # mutate(zinc_total = (zinc/species_number)) %>% 
    # mutate(iron_total = (iron/species_number)) %>% 
    # mutate(epa_total = (epa/species_number)) %>% 
    # mutate(protein_total = (protein/species_number)) %>%
    mutate(fat_total = (fat/species_number)) %>%
    # mutate(dha_total = (dha/species_number)) %>%
    summarise_each(funs(sum), contains("total")) %>%  ## sum up all of each of the nutrients
    # mutate(cal_grams = (cal_total/(1200))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
    # mutate(iron_grams = (iron_total/(18))) %>%
    # mutate(zinc_grams = (zinc_total/(11))) %>% 
    # mutate(epa_grams = (epa_total/(1))) %>%
    # mutate(dha_grams = (dha_total/(1))) %>%
    mutate(fat_grams = (fat_total/(78))) %>%
    # mutate(protein_grams = (protein_total/(56))) %>%
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no, sample_id) %>% 
    select(-contains("total")) %>% 
    gather(key = nutrient, value = concentration, contains("grams")) %>% 
    group_by(species_no, sample_id) %>% 
    summarise(min_percentage = min(concentration)) %>%
    mutate(grams_required = 100/min_percentage)
}

output_fatm <- samples_rep %>% 
  map_df(nutrient_fishing_fat, .id = "run") %>% 
  mutate(nutrient = "fat")

# Get BEF params ----------------------------------------------------------

# all_grams_seadiv <- bind_rows(output_all5s, output_calciums, output_dhas, output_epas, output_irons, output_zincs, output_proteins) %>% 
#   filter(grams_required < 50000)

all_grams_mean_nuts <- bind_rows(output_all5m, output_calciumm, output_dham,
                                 output_epam, output_ironm, output_zincm, output_proteinm)  
# all_grams_global <- bind_rows(output_all5m, output_calciumm, output_dham, output_epam, output_ironm, output_zincm) %>% 
#   filter(grams_required < 50000)


# write_csv(all_grams_mean_nuts, "data-processed/single_nutrient_grams_required.csv")
# write_csv(all_grams_mean_nuts, "~/Documents/Nutrient_Analysis_large_files/single_nutrient_grams_required_aug2020.csv")
# read in single nut grams required ---------------------------------------


all_grams_mean_nuts <- read_csv("~/Documents/Nutrient_Analysis_large_files/single_nutrient_grams_required.csv")
all_grams_mean_nuts %>% 
  group_by(species_no, nutrient) %>% 
  summarise_each(funs(mean, median), grams_required) %>% 
  ggplot(aes(x = species_no, y = median, color = nutrient, group = nutrient))  + geom_point(size = 2) + geom_line()


all_grams_median_nuts <- all_grams_mean_nuts %>% 
  mutate(grams_required = grams_required/10) %>% 
  group_by(nutrient, species_no) %>% 
  summarise_each(funs(mean, median), grams_required) %>% 
  rename(grams_required_median = median)


# all_grams_median_nuts <- all_grams_global %>%
#   group_by(nutrient, species_no) %>%
#   summarise_each(funs(mean, median), grams_required)

prediction_function <- function(df) {
  pf <-function(x){
    res<-(df$a[[1]]*x^df$b[[1]])
    res
  }
  
  pred <- function(x) {
    y <- pf(x)
  }
  
  x <- seq(1, 10, by = 0.1)
  
  preds <- sapply(x, pred)
  preds <- data.frame(x, preds) %>% 
    rename(species_no = x, 
           grams_required = preds)
}




# fit power functions -----------------------------------------------------
library(nlstools)

cal_mod <- nls(formula = (grams_required_median ~ a * species_no^b),data = filter(all_grams_median_nuts, nutrient == "calcium"),  start = c(a=10000, b=-0.2))
cal_boot <- nlsBoot(cal_mod)
cal_boot$bootCI
cal_boot$estiboot
cal_boot_df <- as_data_frame(cal_boot$coefboot) 



all_mod <- nls(formula = (grams_required_median ~ a * species_no^b),data = filter(all_grams_median_nuts, nutrient == "all_5_micronutrients"),  start = c(a=10000, b=-0.2))
all_boot <- nlsBoot(all_mod)
all_boot$bootCI
all_boot$estiboot
all_boot_df <- as_data_frame(all_boot$coefboot) 


zinc_mod <- nls(formula = (grams_required_median ~ a * species_no^b), data = filter(all_grams_median_nuts, nutrient == "zinc"),  start = c(a=10000, b=-0.5))
zinc_boot <- nlsBoot(zinc_mod)
zinc_boot$bootCI
zinc_boot$estiboot
zinc_boot_df <- as_data_frame(zinc_boot$coefboot) 

iron_mod <- nls(formula = (grams_required_median ~ a * species_no^b), data = filter(all_grams_median_nuts, nutrient == "iron"),  start = c(a=10000, b=-0.5))
iron_boot <- nlsBoot(iron_mod)
iron_boot$bootCI
iron_boot$estiboot
iron_boot_df <- as_data_frame(iron_boot$coefboot) 

epa_mod <- nls(formula = (grams_required_median ~ a * species_no^b),data = filter(all_grams_median_nuts, nutrient == "epa"),  start = c(a=10000, b=-0.5))
epa_boot <- nlsBoot(epa_mod)
epa_boot$bootCI
epa_boot$estiboot
epa_boot_df <- as_data_frame(epa_boot$coefboot) 

dha_mod <- nls(formula = (grams_required_median ~ a * species_no^b),data = filter(all_grams_median_nuts, nutrient == "dha"),  start = c(a=10000, b=-0.5))
dha_boot <- nlsBoot(dha_mod)
dha_boot$bootCI
dha_boot$estiboot
dha_boot_df <- as_data_frame(dha_boot$coefboot) 

protein_mod <- nls(formula = (grams_required_median ~ a * species_no^b),data = filter(all_grams_median_nuts, nutrient == "protein"),  start = c(a=300, b=-0.5))
protein_boot <- nlsBoot(protein_mod)
protein_boot$bootCI
protein_boot$estiboot
protein_boot_df <- as_data_frame(protein_boot$coefboot) 

library(janitor)

dha_b <- as_data_frame(dha_boot$bootCI) 
dha_b$nutrient <- "dha"

epa_b <- as_data_frame(epa_boot$bootCI) 
epa_b$nutrient <- "epa"

iron_b <- as_data_frame(iron_boot$bootCI) 
iron_b$nutrient <- "iron"

zinc_b <- as_data_frame(zinc_boot$bootCI) 
zinc_b$nutrient <- "zinc"

cal_b <- as_data_frame(cal_boot$bootCI) 
cal_b$nutrient <- "calcium"

all_b <- as_data_frame(all_boot$bootCI) 
all_b$nutrient <- "all"

protein_b <- as_data_frame(protein_boot$bootCI) 
protein_b$nutrient <- "protein"

### these are the parameter estimates for the b terms
library(janitor)
library(viridis)
all_params <- bind_rows(dha_b, epa_b, cal_b, iron_b, zinc_b, all_b, protein_b) %>% 
  clean_names() %>% 
  rename(lower = x2_5_percent,
         upper = x97_5_percent) %>% 
  filter(median < 1) %>% 
  mutate(nutrient = ifelse(nutrient == "all", "5 Micronutrients", nutrient)) %>% 
  mutate(nutrient = ifelse(nutrient == "calcium", "Calcium", nutrient)) %>% 
  mutate(nutrient = ifelse(nutrient == "iron", "Iron", nutrient)) %>%
  mutate(nutrient = ifelse(nutrient == "zinc", "Zinc", nutrient)) %>%
  mutate(nutrient = ifelse(nutrient == "epa", "EPA", nutrient)) %>%
  mutate(nutrient = ifelse(nutrient == "dha", "DHA", nutrient)) %>% 
  mutate(nutrient = ifelse(nutrient == "protein", "Protein", nutrient))

# write_csv(all_params, "data-processed/all_single_efficiency_nutrient_params.csv")
# all_params <- read_csv("data-processed/all_single_efficiency_nutrient_params.csv")

all_params$nutrient <- factor(all_params$nutrient, levels = c("5 Micronutrients", "Calcium", "Iron", "Zinc", "EPA", "DHA", "Protein"))
# all_params$nutrient <- factor(all_params$nutrient, levels = c("Protein", "DHA", "EPA", "Zinc", "Iron", "Calcium", "5 Micronutrients"))

ggplot(aes(x = reorder(nutrient, -median), y = median, color = nutrient), data = all_params) + geom_point(size = 4) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  scale_color_viridis(discrete = TRUE) + coord_flip() + ylab("") +
  theme(legend.position="none")
  
# ggsave("figures/BEF_params.pdf", width = 4, height = 3)


cal_split <- cal_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate)

cal_preds <- cal_split %>% 
  map_df(prediction_function, .id = "replicate")

iron_split <- iron_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate)
iron_preds <- iron_split %>% 
  map_df(prediction_function, .id = "replicate")

zinc_split <- zinc_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate)
zinc_preds <- zinc_split %>% 
  map_df(prediction_function, .id = "replicate")

epa_split <- epa_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate)
epa_preds <- epa_split %>% 
  map_df(prediction_function, .id = "replicate")

dha_split <- dha_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate)
dha_preds <- dha_split %>% 
  map_df(prediction_function, .id = "replicate")

all_split <- all_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate)
all_preds <- all_split %>% 
  map_df(prediction_function, .id = "replicate")

protein_split <- protein_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate)
protein_preds <- protein_split %>% 
  map_df(prediction_function, .id = "replicate")

limits_protein <- protein_preds %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required)) %>% 
  mutate(nutrient = "protein")

limits_cal <- cal_preds %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required))%>% 
  mutate(nutrient = "calcium")

limits_iron <- iron_preds %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required))%>% 
  mutate(nutrient = "iron")

limits_epa <- epa_preds %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required))%>% 
  mutate(nutrient = "epa")

limits_dha <- dha_preds %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required))%>% 
  mutate(nutrient = "dha")


limits_zinc <- zinc_preds %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required))%>% 
  mutate(nutrient = "zinc")

limits_all <- all_preds %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required))%>% 
  mutate(nutrient = "five micronutrients")


all_preds <- bind_rows(limits_all, limits_cal, limits_dha, limits_epa, limits_iron, limits_protein, limits_zinc)

# write_csv(all_preds, "data-processed/single_nutrient_prediction_limits.csv")


# all_preds <- read_csv("data-processed/single_nutrient_prediction_limits.csv")

library(viridis)

all_grams <- all_grams_median_nuts %>% 
  ungroup() %>% 
  mutate(nutrient = ifelse(nutrient == "all_5_micronutrients", "five micronutrients", nutrient))

p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) 

all_preds2 <- all_preds %>% 
  mutate(nutrient = ifelse(nutrient == "five micronutrients", "5 Micronutrients", nutrient)) %>% 
  mutate(nutrient = ifelse(nutrient == "calcium", "Calcium", nutrient)) %>% 
  mutate(nutrient = ifelse(nutrient == "iron", "Iron", nutrient)) %>%
  mutate(nutrient = ifelse(nutrient == "zinc", "Zinc", nutrient)) %>%
  mutate(nutrient = ifelse(nutrient == "epa", "EPA", nutrient)) %>%
  mutate(nutrient = ifelse(nutrient == "dha", "DHA", nutrient)) %>% 
  mutate(nutrient = ifelse(nutrient == "protein", "Protein", nutrient))
all_preds2$nutrient <- factor(all_preds2$nutrient, levels = c("5 Micronutrients", "Calcium", "Iron", "Zinc", "EPA", "DHA", "Protein"))

all_grams2 <- all_grams %>% 
  mutate(nutrient = ifelse(nutrient == "five micronutrients", "5 Micronutrients", nutrient)) %>% 
  mutate(nutrient = ifelse(nutrient == "calcium", "Calcium", nutrient)) %>% 
  mutate(nutrient = ifelse(nutrient == "iron", "Iron", nutrient)) %>%
  mutate(nutrient = ifelse(nutrient == "zinc", "Zinc", nutrient)) %>%
  mutate(nutrient = ifelse(nutrient == "epa", "EPA", nutrient)) %>%
  mutate(nutrient = ifelse(nutrient == "dha", "DHA", nutrient)) %>% 
  mutate(nutrient = ifelse(nutrient == "protein", "Protein", nutrient))
all_grams2$nutrient <- factor(all_grams2$nutrient, levels = c("5 Micronutrients", "Calcium", "Iron", "Zinc", "EPA", "DHA", "Protein"))

# single_nut_plot <- p + 
#   geom_ribbon(aes(ymin = q2.5, ymax = q97.5, x = species_no, fill = nutrient), data = all_preds2, alpha = 0.5) +
#   geom_line(aes(y = mean, x = species_no, color= nutrient), data = all_preds2, alpha = 1, size = 1.5) +
#   geom_point(data = all_grams2, aes(x = species_no, y = grams_required_median, color = nutrient), size = 2) + 
#   scale_color_viridis(discrete = TRUE, option = "viridis") +  scale_fill_viridis(discrete = TRUE, option = "viridis") +
#   scale_x_continuous(breaks = seq(1,10,1)) + xlab("") + ylab("") +
#   theme(legend.position="none") +
#   theme(axis.text = element_text(size=16))

# p + 
#   geom_ribbon(aes(ymin = 1/q2.5, ymax = 1/q97.5, x = species_no, fill = nutrient), data = all_preds2, alpha = 0.5) +
#   geom_line(aes(y = 1/mean, x = species_no, color= nutrient), data = all_preds2, alpha = 1, size = 1.5) +
#   geom_point(data = all_grams2, aes(x = species_no, y = 1/median, color = nutrient), size = 2) + 
#   scale_color_viridis(discrete = TRUE, option = "viridis") +  scale_fill_viridis(discrete = TRUE, option = "viridis") +
#   scale_x_continuous(breaks = seq(1,10,1)) + xlab("") + ylab("") +
#   theme(legend.position="none") +
#   theme(axis.text = element_text(size=16)) +
#   # ylim(0, 0.04) +
#   # scale_y_log10() +
#   # scale_x_log10() +
#   ylab("Fraction of RDA target per gram") +
#   xlab("Species richness")
# ggsave("figures/Ne-per-gram-single-nutrient.png", width = 8, height = 6)

# b_estimates_inverse <- all_grams2 %>% 
#   group_by(nutrient) %>% 
#   do(tidy(lm(log(1/median) ~ log(species_no), data = .))) 
# 
# library(broom)
# b_estimates <- all_grams2 %>% 
#   group_by(nutrient) %>% 
#   do(tidy(lm(log(grams_required_median) ~ log(species_no), data = .), conf.int = TRUE)) 


 p + 
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, x = species_no, fill = nutrient), data = all_preds2, alpha = 0.5) +
  geom_line(aes(y = mean, x = species_no, color= nutrient), data = all_preds2, alpha = 1, size = 1.5) +
  geom_point(data = all_grams2, aes(x = species_no, y = grams_required_median, color = nutrient), size = 2, alpha = 0.5) + 
  scale_color_viridis(discrete = TRUE, option = "viridis") +  scale_fill_viridis(discrete = TRUE, option = "viridis") +
  scale_x_continuous(breaks = seq(1,10,1)) + xlab("") + ylab("") +
  theme(legend.position="right") +
  theme(axis.text = element_text(size=16)) 
 ggsave("figures/pmin-plot.pdf", width = 7, height = 4)

 
 # other stuff -------------------------------------------------------------
 
 
 all_grams_mean_nuts %>% 
   # filter(nutrient == "iron") %>% 
   group_by(run, species_no, nutrient) %>% 
   summarise_each(funs(mean, median), grams_required) %>% 
   group_by(nutrient, run) %>% 
   do(tidy(lm(log(grams_required_median) ~ log(species_no), data = .))) %>%
   filter(term != "(Intercept)") %>%
   group_by(nutrient) %>%
   summarise(mean_slope = mean(estimate)) %>% View
 
 mods_mean <- all_grams %>% 
   group_by(nutrient, run, species_no) %>% 
   summarise_each(funs(mean, median), grams_required) %>% 
   group_by(nutrient, run) %>%
   do(tidy(nls(formula = (grams_required_mean ~ a * species_no^b),data = .,  start = c(a=10000, b=-0.5)))) 
 
 
 mod_sum_seadiv <- mods_seadiv %>% 
   filter(term == "b") %>% 
   group_by(nutrient) %>% 
   summarise_each(funs(mean, std.error), estimate)
 
 
 
 
 
 all_grams_median_nuts %>% 
   mutate(efficiency = 1/grams_required_median) %>% 
   ggplot(aes(x = species_no, y = grams_required_median, color = nutrient)) + geom_point() + geom_line()
 
 
 all_grams_median_nuts %>% 
   ggplot(aes(x = log(species_no), y = log(grams_required_median), color = nutrient)) + geom_point() +
   geom_smooth(method = "lm")
 
 all_grams_median_seadiv <- all_grams_seadiv %>% 
   group_by(nutrient, species_no) %>% 
   summarise_each(funs(mean, median), grams_required) 
 
 all_grams_median_nuts %>% 
   mutate(efficiency = 1/grams_required_median) %>% 
   group_by(nutrient) %>% 
   do(tidy(lm(log(grams_required_median) ~ log(species_no), data = .), conf.int = TRUE)) %>% 
   filter(term != "(Intercept)") %>% View
 group_by(nutrient) %>% 
   summarise(mean_slope = mean(estimate)) %>% View
 
 all_iron <- all_grams_seadiv %>% 
   filter(nutrient == "iron") 
 
 all_grams_median_nuts %>% 
   filter(nutrient == "protein") %>% 
   group_by(run) %>% 
   do(tidy(nls(formula = (grams_required ~ a * species_no^b),data = .,  start = c(a=10000, b=-0.5)))) %>% 
   filter(term == "b") %>% 
   ungroup() %>% 
   summarise(mean_slope = mean(estimate)) %>% View
 
 
 
# quick diversion to plot (remove later) ----------------------------------


output_calcium %>% 
  ungroup() %>% 
  ggplot(aes(x = log(species_no), y = log(grams_required), group = run, color = run)) + geom_smooth(method = "lm")

output_calcium_1000 %>% 
  group_by(run) %>% 
  do(tidy(lm(log(grams_required) ~ log(species_no), data = .), conf.int = TRUE)) %>% 
  filter(term != "(Intercept)") %>% 
  ungroup() %>% 
  summarise_each(funs(mean, sd), estimate) %>% 
  ggplot(aes(x = estimate_mean, y = estimate_mean)) + geom_point() +
  geom_errorbar(aes(ymin = estimate_mean - estimate_sd, ymax = estimate_mean + estimate_sd)) + ylim(-1, 0)
  
output_calcium %>% 
  group_by(run) %>% 
  do(tidy(nls(formula = (grams_required ~ a * species_no^b),data = .,  start = c(a=1000, b=-0.5)))) %>% 
  filter(term == "b") %>% 
  ungroup() %>% 
  summarise_each(funs(mean, sd), estimate) %>% 
  ggplot(aes(x = estimate_mean, y = estimate_mean)) + geom_point() +
  geom_errorbar(aes(ymin = estimate_mean - estimate_sd, ymax = estimate_mean + estimate_sd)) + ylim(-1, 0)

 estimates_1000 <- output_calcium_1000 %>% 
  group_by(run) %>% 
  do(tidy(nls(formula = (grams_required ~ a * species_no^b),data = .,  start = c(a=1000, b=-0.5)))) 

estimates_200 <- output_calcium_200 %>% 
  group_by(run) %>% 
  do(tidy(nls(formula = (grams_required ~ a * species_no^b),data = .,  start = c(a=1000, b=-0.5)))) 

estimates_100 <- output_calcium_100 %>% 
  group_by(run) %>% 
  do(tidy(nls(formula = (grams_required ~ a * species_no^b),data = .,  start = c(a=1000, b=-0.5)))) 

estimates_100b <- output_calcium_100b %>% 
  group_by(run) %>% 
  do(tidy(nls(formula = (grams_required ~ a * species_no^b),data = .,  start = c(a=1000, b=-0.5)))) 
estimates_100c <- output_calcium_100c %>% 
  group_by(run) %>% 
  do(tidy(nls(formula = (grams_required ~ a * species_no^b),data = .,  start = c(a=1000, b=-0.5)))) 

estimates_10 <- output_calcium_10 %>% 
  group_by(run) %>% 
  do(tidy(nls(formula = (grams_required ~ a * species_no^b),data = .,  start = c(a=1000, b=-0.5)))) 

estimates_10$replication <- "10_reps"
estimates_100$replication <- "100_reps"
estimates_100b$replication <- "100_repsb"
estimates_100c$replication <- "100_repsc"
estimates_200$replication <- "200_reps"
estimates_1000$replication <- "1000_reps"

all_est <- bind_rows(estimates_10, estimates_100, estimates_100b, estimates_100c, estimates_200, estimates_1000)

all_est %>% 
  filter(term == "b") %>% 
  group_by(replication) %>% 
  summarise_each(funs(mean, sd), estimate) %>% 
  ggplot(aes(x = replication, y = estimate_mean)) + geom_point() +
  geom_errorbar(aes(ymin = estimate_mean - estimate_sd, ymax = estimate_mean + estimate_sd)) + ylim(-1, 0)

  

mod1000 <- output_calcium_1000 %>% 
  group_by(run) %>% 
  do(tidy(nls(formula = (grams_required ~ a * species_no^b),data = .,  start = c(a=1000, b=-0.5)))) 

mod1000_linear <- output_calcium_1000 %>% 
  group_by(run) %>% 
  do(tidy(lm(log(grams_required) ~ log(species_no), data = .), conf.int = TRUE))
 
  
o287 <- mod1000 %>% 
  filter(run == 287)

l287 <- mod1000_linear %>% 
  filter(run == 287)

data287 <- output_calcium_1000 %>% 
  filter(run == 287)

pf <- function(x) 7542.6337779*x^-0.3375611

data287 %>% 
  ggplot(aes(x = species_no, y = grams_required)) + geom_jitter(width = 0.4) +
  stat_function(fun = pf, color = "blue")

lf <- function(x) -0.1113618*x + 8.5263642

data287 %>% 
  ggplot(aes(x = log(species_no), y = log(grams_required))) + geom_point() +
 stat_function(fun = lf, color = "blue", size =1.5)

mod1000 %>% 
  filter(term == "b") %>%
  ungroup() %>% 
  summarise_each(funs(mean, sd), estimate) %>% 
  ggplot(aes(x = estimate_mean, y = estimate_mean)) + geom_point() +
  geom_errorbar(aes(ymin = estimate_mean - estimate_sd, ymax = estimate_mean + estimate_sd)) + ylim(-1, 0)

output_calcium_1000 <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run") %>% 
  mutate(nutrient = "calcium")

output_calcium <- output_calcium %>% 
  mutate(nutrient = "calcium")


# back to other nutrients -------------------------------------------------


all_output <- bind_rows(output_calcium, output_iron, output_zinc, output_dha, output_epa, output_protein, output_all5)
write_csv(all_output, "data-processed/single_nutrient_accumulation_by_fractions.csv")

all_output <- read_csv("data-processed/single_nutrient_accumulation_by_fractions.csv")
str(all_output)

output_protein2  <- output_protein %>% 
  mutate(sample_id = as.integer(sample_id))
all_output2 <- bind_rows(all_output, output_protein2)

write_csv(all_output2,"data-processed/single_nutrient_accumulation_by_fractions2.csv" )

summary <- output_iron %>%
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_required, grams_for_25_percent)


summary %>% 
  ggplot(aes(x = species_no, y = grams_for_25_percent_median)) + geom_point()

single_functions <- all_output %>% 
  ggplot(aes(x = species_no, y = grams_required)) + geom_point() +
  geom_smooth(method = "lm") + theme_bw() + facet_wrap( ~ nutrient, scales = "free") + ylab("grams of tissue required to reach 10% DRI") + xlab("species richness")
ggsave("figures/single_functions.png")



output_calcium_1000 %>% 
  ggplot(aes(x = species_no, y = grams_required)) + geom_point() +
  geom_smooth(method = "lm") + theme_bw() 


output_calcium_1000 %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  lm(grams_for_25_percent ~ species_no, data = .) %>% 
  summary


reps100 <- read_csv("data-processed/grams-required-10-spp-1000reps.csv")


reps100 %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  lm(grams_for_25_percent ~ species_no, data = .) %>% 
  summary

reps100 %>% 
ggplot(aes(x = species_no, y = grams_required)) + geom_point() +

### get the power functions for each nutrient
cal_sum <- output_calcium %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_for_25_percent)
    
calcium_power <- nls(formula=(median ~ a * species_no^b), data=cal_sum, start = c(a=10000, b=-0.7))
a.cal <- coef(calcium_power)[1]
b.cal <- coef(calcium_power)[2]  

## iron
iron_sum <- output_iron %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_for_25_percent)

iron_power <- nls(formula=(median ~ a * species_no^b), data=iron_sum, start = c(a=10000, b=-0.7))
a.iron <- coef(iron_power)[1]
b.iron <- coef(iron_power)[2]  
iron_power_function <- function(x) a.iron*x^b.iron


## zinc 

zinc_sum <- output_zinc %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_for_25_percent)

zinc_power <- nls(formula=(median ~ a * species_no^b), data=zinc_sum, start = c(a=10000, b=-0.7))
a.zinc <- coef(zinc_power)[1]
b.zinc <- coef(zinc_power)[2]  
zinc_power_function <- function(x) a.zinc*x^b.zinc

## epa 

epa_sum <- output_epa %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_for_25_percent)

epa_power <- nls(formula=(median ~ a * species_no^b), data=epa_sum, start = c(a=10000, b=-0.7))
a.epa <- coef(epa_power)[1]
b.epa <- coef(epa_power)[2]  
epa_power_function <- function(x) a.epa*x^b.epa

## dha


dha_sum <- output_dha %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_for_25_percent)

dha_power <- nls(formula=(median ~ a * species_no^b), data=dha_sum, start = c(a=10000, b=-0.7))
a.dha <- coef(dha_power)[1]
b.dha <- coef(dha_power)[2]  
dha_power_function <- function(x) a.dha*x^b.dha

## all 
all_sum <- reps100 %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_for_25_percent)

all_power <- nls(formula=(median ~ a * species_no^b), data=all_sum, start = c(a=10000, b=-0.7))
a.all <- coef(all_power)[1]
b.all <- coef(all_power)[2]  
all_power_function <- function(x) a.all*x^b.all



cor(1/cal_sum$median, predict(calcium_power))


calcium_power_function <- function(x) a.cal*x^b.cal


cal_sum %>% 
  mutate(service = 1/median) %>% 
ggplot(aes(x = species_no, y = service)) + geom_point(size = 3) +
  stat_function(fun = calcium_power_function, color = "grey") +
  theme_bw() + ylab("nutritional service 1/median grams required to reach 10% of DRI for calcium") + xlab("species richness") +
  scale_x_continuous(breaks = 1:10)

## original calcium plot
cal_sum %>% 
  mutate(service = 1/median) %>% 
  ggplot(aes(x = species_no, y = median)) + geom_point(size = 3) +
  # stat_function(fun = calcium_power_function, color = "grey") +
  theme_bw() + ylab("nutritional service 1/median grams required to reach 10% of DRI for calcium") + xlab("species richness") +
  scale_x_continuous(breaks = 1:10)



reps <- reps100 %>% 
  mutate(nutrient = "all 5 micronutrients") %>% 
  select(-run, -`[EMPTY]`) 

all_output2 <- all_output %>% 
  select(-run) %>% 
  mutate(sample_id = as.integer(sample_id))

str(all_output)
str(reps)

all_output_with5 <- bind_rows(all_output2, reps)

all_summary <- all_output_with5 %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(nutrient, species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_for_25_percent) 

params <- all_summary %>% 
  ungroup() %>% 
  mutate(nutrient = ifelse(nutrient == "all 5 micronutrients", "all", nutrient)) %>% 
  group_by(nutrient) %>% 
  do(tidy(nls(median ~ a * species_no^b, data =., start = c(a=10000, b=-0.7)), conf.int = TRUE)) %>% 
  filter(term == "b") %>% 
  ggplot(aes(x = reorder(nutrient, estimate), y = estimate, color = nutrient)) + geom_point() +
  # facet_wrap( ~ term, scales = "free") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + theme_bw() +
  scale_y_reverse() + xlab("nutrient") + ylab("b estimate") +
  theme(legend.position = "none") + xlab("") +
  theme(text=element_text(family="Helvetica", size=12)) +
  theme(legend.title=element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="none")
ggsave("figures/power_function_params_by_nutrient.png")
 
all_summary %>% 
  group_by(nutrient) %>% 
  do(tidy(lm(median ~ species_no, data =.), conf.int = TRUE)) %>%  View


bef <- all_summary %>% 
  ggplot(aes(x = species_no, y = median, color = nutrient)) + geom_point() +
  geom_line() + theme_bw() +
  scale_y_reverse() +
  theme(text=element_text(family="Helvetica", size=12)) +
  theme(legend.title=element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = c(1:10)) + xlab("species richness") + ylab("median grams required to reach 10% of DRI")
ggsave("figures/all_nutrients_efficiency_power_fits_rev_y.png")


grid.newpage()
vpb_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
vpa_ <- viewport(width = 0.4, height = 0.4, x = 0.52, y = 0.3)  # the inset in upper right
print(bef, vp = vpb_)
print(params, vp = vpa_)

ggsave("figures/figure3a_accum.pdf")
