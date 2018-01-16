

library(purrr)
library(tidyverse)
library(cowplot)
library(stringr)
library(broom)

mean_nuts <- read_csv("data-processed/new_global.csv")

# define resampling function ----------------------------------------------

nutrient_fishing_function <- function(sample_size, dataset) {
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
    mutate(cal_total = (calcium/species_number)) %>% ## get the amount of calcium each species will contribute
    mutate(zinc_total = (zinc/species_number)) %>% 
    mutate(iron_total = (iron/species_number)) %>% 
    mutate(epa_total = (epa/species_number)) %>%
    mutate(dha_total = (dha/species_number)) %>%
    summarise_each(funs(sum), contains("total")) %>% ## sum up all of each of the nutrients
    mutate(cal_grams = (cal_total/(1200))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
    mutate(iron_grams = (iron_total/(18))) %>%
    mutate(zinc_grams = (zinc_total/(11))) %>% 
    mutate(epa_grams = (epa_total/(1))) %>%
    mutate(dha_grams = (dha_total/(1))) %>%
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no, sample_id) %>% 
    select(-contains("total")) %>% 
    gather(key = nutrient, value = concentration, 3:7) %>% 
    group_by(species_no, sample_id) %>% 
    summarise(min_percentage = min(concentration)) %>% 
    mutate(grams_required = 100/min_percentage) 
}



# define dataset to pull from ---------------------------------------------

mean(species_numbers$n_species)

# sample away! ------------------------------------------------------------

samples_rep <- rep(10, 100)

dataset29 <- mean_nuts %>% 
  sample_n(size = 29, replace = FALSE)
output29 <- samples_rep %>% 
  map_df(nutrient_fishing_function, dataset = dataset29, .id = "run") %>% 
  mutate(pool_size = 29)

dataset25 <- mean_nuts %>% 
  sample_n(size = 25, replace = FALSE)
output25 <- samples_rep %>% 
  map_df(nutrient_fishing_function, dataset = dataset25, .id = "run") %>% 
  mutate(pool_size = 25)


dataset20 <- mean_nuts %>% 
  sample_n(size = 20, replace = FALSE)
output20 <- samples_rep %>% 
  map_df(nutrient_fishing_function, dataset = dataset20, .id = "run") %>% 
  mutate(pool_size = 20)


dataset57 <- mean_nuts %>% 
  sample_n(size = 57, replace = FALSE)
output57 <- samples_rep %>% 
  map_df(nutrient_fishing_function, dataset = dataset57, .id = "run") %>% 
  mutate(pool_size = 57)

varying_sizes <- bind_rows(output29, output25, output57, output20)
write_csv(varying_sizes, "data-processed/resampling_global_new_varying_sizes.csv")



 var <- varying_sizes %>% 
  filter(!is.infinite(grams_required)) %>% 
  group_by(species_no, pool_size) %>%
  mutate(grams_required = grams_required/10) %>% 
  summarise_each(funs(mean, median), grams_required) %>% 
  rename(median = grams_required_median,
         mean = grams_required_mean) 
 
 
 var %>% 
   dplyr::group_by(pool_size) %>% 
   filter(species_no < 11) %>% 
   do(tidy(nls(formula = (median ~ a * species_no^b),data = .,  start = c(a=10000, b=-0.7)))) %>% 
   ggplot(aes(x = reorder(pool_size, estimate), y = estimate)) + geom_point() + 
   geom_errorbar(aes(ymin = estimate - std.error*1.96, ymax = estimate + std.error*1.96)) +
   facet_wrap( ~ term, scales = "free") + theme_classic() +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))
 
 
 var %>% 
   filter(species_no < 11) %>% 
   ggplot(aes(x = species_no, y = median, color = pool_size)) + geom_line(size = 1) +
   theme_classic() + ylab("Median grams required to reach 5 DRI targets") + xlab("Species richness") +
   scale_x_continuous(breaks = 1:10) +
   scale_color_viridis(discrete = FALSE) +
   facet_wrap( ~ pool_size)
 
 
 ### ok it looks like the random sample that we take from the global dataset really matters. Let's repeat the resampling,
 ### then average
 
 samples_rep_1000 <- rep(10, 1000)
 
 output40 <- samples_rep_1000 %>% 
   map_df(nutrient_fishing_function, dataset = sample_n(mean_nuts, size = 40, replace = FALSE), .id = "run") %>% 
   mutate(pool_size = 40)
 
 
 
 
 
 ### compare to the trad diets
 
 all_trad_resampling <- read_csv("data-processed/all_trad_reps.csv")
 output_abenaki <- read_csv("data-processed/output_abenaki_resampling_100.csv")
 all_trad2 <- bind_rows(all_trad_resampling, output_abenaki)
 reps1000 <- read_csv("data-processed/grams-required-10-spp-1000reps-new-global.csv") %>% 
   mutate(dataset = "global")
 reps100_40sp<- read_csv("data-processed/grams-required-10-spp-100reps-new-global-40sp.csv") %>% 
   mutate(dataset = "global40")
 
trad <- all_trad_resampling%>% 
   filter(!is.infinite(grams_required)) %>% 
   group_by(species_no, culture) %>%
   mutate(grams_required_10 = grams_required/10) %>% 
   summarise_each(funs(mean, median, std.error), grams_required_10) %>% 
   rename(median = grams_required_10_median,
          mean = grams_required_10_mean) %>% 
  rename(dataset = culture)



reps100b <- reps1000 %>% 
  filter(species_no < 11) %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_required_10 = grams_required/10) 

reps100_summary <- reps100b %>% 
  group_by(species_no, dataset) %>%
  summarise_each(funs(mean, median, std.error), grams_required_10) %>%
  rename(median = grams_required_10_median,
         mean = grams_required_10_mean) 


reps40 <-  reps100_40sp %>% 
  filter(species_no < 11) %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_required_10 = grams_required/10) 

reps40_summary <- reps40 %>% 
  group_by(species_no, dataset) %>%
  summarise_each(funs(mean, median, std.error), grams_required_10) %>%
  rename(median = grams_required_10_median,
         mean = grams_required_10_mean) 

var <- varying_sizes %>% 
  filter(!is.infinite(grams_required)) %>% 
  group_by(species_no, pool_size) %>%
  mutate(grams_required = grams_required/10) %>% 
  summarise_each(funs(mean, median), grams_required) %>% 
  rename(median = grams_required_median,
         mean = grams_required_mean) %>% 
  mutate(dataset = as.character(pool_size))
  

var40 <- output40 %>% 
  filter(!is.infinite(grams_required)) %>% 
  group_by(species_no, pool_size) %>%
  mutate(grams_required = grams_required/10) %>% 
  summarise_each(funs(mean, median), grams_required) %>% 
  rename(median = grams_required_median,
         mean = grams_required_mean) %>% 
  mutate(dataset = as.character(pool_size))


all <- bind_rows(trad, reps100_summary, reps40_summary, var, var40)

write_csv(all, "data-processed/all_resampling_new_global_local.csv")

all <- read_csv("data-processed/all_resampling_new_global_local.csv")
mod <- all %>% 
  filter(dataset != "global") %>% 
  filter(!dataset %in% c("25", "25", "29", "57", "40", "20")) %>% 
  mutate(dataset = str_replace(dataset, "Inuit-Inupiaq", "II")) %>% 
  mutate(dataset = str_replace(dataset, "Central Salish", "CS")) %>% 
  mutate(dataset = str_replace(dataset, "Wampanoag", "WA")) %>% 
  mutate(dataset = str_replace(dataset, "Cree", "CR")) %>%
  mutate(dataset = str_replace(dataset, "Nootkan", "NO")) %>% 
  mutate(dataset = str_replace(dataset, "Bella Coola", "BC")) %>% 
  mutate(dataset = str_replace(dataset, "Tlingit", "TL")) %>%
  mutate(dataset = str_replace(dataset, "Haida", "HA")) %>%
  mutate(dataset = str_replace(dataset, "Tsimshian", "TS")) %>% 
  mutate(dataset = str_replace(dataset, "Montagnais-Naskapi", "MN")) %>%
  mutate(dataset = str_replace(dataset, "Yupik", "YU")) %>% 
  mutate(dataset = str_replace(dataset, "Abenaki", "AB")) %>%
  mutate(dataset = str_replace(dataset, "Micmac", "MI")) %>%
  mutate(dataset = str_replace(dataset, "Kwakiutl", "KW")) %>%
  mutate(dataset = str_replace(dataset, "global40", "GL")) %>% 
  dplyr::group_by(dataset) %>% 
  filter(species_no < 11) %>% 
  do(tidy(nls(formula = (median ~ a * species_no^b),data = .,  start = c(a=10000, b=-0.7))))



a_terms1 <- mod %>% 
  filter(term == "a")

a_plot1 <- a_terms1 %>% 
  ungroup() %>% 
  ggplot(aes(x = reorder(dataset, estimate), y = estimate)) + geom_point(size = 2) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  geom_point(size = 2, data = filter(a_terms1, dataset == "GL"), color = "cadet blue") +
  theme(
    axis.ticks.x = element_blank())



b_terms1 <- mod %>% 
  filter(term == "b")

b_plot1 <- b_terms1 %>% 
  ungroup() %>% 
  ggplot(aes(x = reorder(dataset, estimate), y = estimate)) + geom_point(size = 2) +
  geom_point(size = 2, data = filter(b_terms1, dataset == "GL"), color = "cadet blue") +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.1) +
  geom_point(size = 2, data = filter(b_terms1, dataset == "GL"), color = "cadet blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("") +
  theme(
    axis.ticks.x = element_blank())

  BEF_params_plot_min <- plot_grid(a_plot1, b_plot1, nrow = 1, ncol = 2)
save_plot("figures/BEF-params-min-dri.png", BEF_params_plot_min,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 1, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1
)


all %>% 
  # filter(dataset %in% species_numbers$culture | dataset %in% c("25", "29", "57")) %>% 
  filter(species_no < 11) %>% 
  filter(dataset != "global") %>% 
  filter(!dataset %in% c("25", "25", "29", "57")) %>% 
  ggplot(aes(x = species_no, y = median, group = dataset)) + geom_line(size = 0.5, alpha = 0.5) +
  # geom_ribbon(aes(ymin = mean - grams_required_10_std.error, ymax = mean + grams_required_10_std.error), fill = "grey", alpha = 0.5) +
  theme_classic() + ylab("Median grams required to reach 5 DRI targets") + xlab("Species richness") +
  geom_line(color = "cadetblue", size =1, data = filter(all, dataset == "global40", species_no < 11)) +
  scale_x_continuous(breaks = seq(1,10,1))
ggsave("figures/min_rdi_local_BEF.png", width = 3, height = 3)




var2 <- var %>% 
  rename(dataset = pool_size) %>% 
  mutate(dataset = as.character(dataset))

alltrad <- bind_rows(trad, var2) 

## pull out only the communities that have more than 25
species_numbers <- nuts_mean %>% 
  group_by(culture) %>% 
  summarise(n_species = n_distinct(latin_name)) %>% 
  filter(n_species >= 25) 

trad %>% 
  filter(dataset %in% species_numbers$culture | dataset %in% c("25", "29", "57")) %>% 
  dplyr::group_by(dataset) %>% 
  filter(species_no < 11) %>% 
  do(tidy(nls(formula = (median ~ a * species_no^b),data = .,  start = c(a=10000, b=-0.7)))) %>% 
  ggplot(aes(x = reorder(dataset, estimate), y = estimate)) + geom_point() + 
  geom_errorbar(aes(ymin = estimate - std.error*1.96, ymax = estimate + std.error*1.96)) +
  facet_wrap( ~ term, scales = "free") + theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

trad %>% 
  filter(dataset %in% species_numbers$culture | dataset %in% c("25", "29", "57")) %>% 
  filter(species_no < 11) %>% 
  ggplot(aes(x = species_no, y = median, color = dataset)) + geom_line(size = 1) +
  theme_classic() + ylab("Median grams required to reach 5 DRI targets") + xlab("Species richness") +
  scale_x_continuous(breaks = 1:10) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap( ~ dataset)


micmac <- cnuts %>% 
  filter(culture == "Micmac") %>%
  group_by(latin_name) %>% 
  summarise(calcium = mean(ca_mg, na.rm = TRUE),
            zinc = mean(zn_mg, na.rm = TRUE), 
            iron = mean(fe_mg, na.rm = TRUE),
            epa = mean(epa, na.rm = TRUE),
            dha = mean(dha, na.rm = TRUE)) %>% 
  filter(!is.na(calcium), !is.na(iron), !is.na(zinc), !is.na(epa), !is.na(dha)) %>% 
  mutate(dataset = "micmac")
  
dataset57 <- dataset57 %>% 
  mutate(dataset = "global_subset")

all3 <- bind_rows(micmac, dataset57)

all3 %>% 
  # filter(epa < 2) %>% 
  # filter(iron < 100) %>% 
  # filter(zinc < 25) %>% 
  gather(key = "nutrient", value = "concentration", 2:6) %>% 
  ggplot(aes(x = nutrient, y = concentration, fill = dataset)) + geom_violin() +
  facet_wrap( ~ nutrient, scales = "free")

## now another example

abenaki <- cnuts %>% 
  filter(culture == "Abenaki") %>%
  group_by(latin_name) %>% 
  summarise(calcium = mean(ca_mg, na.rm = TRUE),
            zinc = mean(zn_mg, na.rm = TRUE), 
            iron = mean(fe_mg, na.rm = TRUE),
            epa = mean(epa, na.rm = TRUE),
            dha = mean(dha, na.rm = TRUE)) %>% 
  filter(!is.na(calcium), !is.na(iron), !is.na(zinc), !is.na(epa), !is.na(dha)) %>% 
  mutate(dataset = "abenaki")

dataset29 <- dataset29 %>% 
  mutate(dataset = "global_subset")

all4 <- bind_rows(abenaki, dataset29)

all4 %>% 
  filter(epa < 2) %>% 
  gather(key = "nutrient", value = "concentration", 2:6) %>% 
  ggplot(aes(x = nutrient, y = concentration, fill = dataset)) + geom_violin() +
  facet_wrap( ~ nutrient, scales = "free") 
