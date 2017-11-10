

library(tidyverse)
library(viridis)
library(readxl)
library(stringr)
library(purrr)
library(vegan)
library(cowplot)


nuts_trad <- read_csv("data-processed/trad-foods-cleaned.csv")
culture_foods <- read_excel("~/Documents/traditional-foods/culture-foods.xlsx")

nuts_trad %>% 
  distinct(latin_name) %>% 
  tally

mean_trad <- nuts_trad %>% 
  group_by(latin_name) %>% 
summarise(calcium = mean(ca_mg, na.rm = TRUE),
          zinc = mean(zn_mg, na.rm = TRUE), 
          iron = mean(fe_mg, na.rm = TRUE),
          epa = mean(epa, na.rm = TRUE),
          dha = mean(dha, na.rm = TRUE)) %>% 
  filter(!is.na(calcium), !is.na(iron), !is.na(zinc), !is.na(epa), !is.na(dha)) 


nuts_trad %>% 
  filter(reference != "88") %>% 
  filter(part != "eggs") %>% 
  gather(key = "nutrient", value = "concentration", 9:19) %>% 
  filter(!is.na(concentration)) %>% 
  filter(nutrient == "ca_mg") %>% 
  mutate(latin_name = factor(latin_name)) %>% 
  ggplot(aes(x = reorder(latin_name, concentration), y = concentration)) + geom_point() +
  facet_wrap( ~ nutrient, scales = "free") + theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

cnuts <- left_join(culture_foods, nuts_trad)

write_csv(cnuts, "data-processed/cnuts-trad-foods-culture.csv")
cnuts <- read_csv("data-processed/cnuts-trad-foods-culture.csv")


nuts_mean <- cnuts %>% 
  group_by(culture, latin_name) %>% 
  summarise(calcium = mean(ca_mg, na.rm = TRUE),
            zinc = mean(zn_mg, na.rm = TRUE), 
            iron = mean(fe_mg, na.rm = TRUE),
            epa = mean(epa, na.rm = TRUE),
            dha = mean(dha, na.rm = TRUE)) %>% 
  filter(!is.na(calcium), !is.na(iron), !is.na(zinc), !is.na(epa), !is.na(dha))

write_csv(nuts_mean, "data-processed/trad-foods-mean.csv")

### how many species per culture?

species_numbers <- nuts_mean %>% 
  group_by(culture) %>% 
  summarise(n_species = n_distinct(latin_name)) %>% 
  filter(n_species >= 25) 

median(species_numbers$n_species)
hist(species_numbers$n_species)

nuts_mean %>% 
  filter(culture == "Haida") %>% View


### ok write out what we've got!

write_csv(nuts_mean, "data-processed/trad-foods-means.csv")

trad_nuts_mean <- read_csv("data-processed/trad-foods-means.csv")
mean_nuts <- read.csv("data-processed/mean_nuts.csv")



# compare species lists
spp <- trad_nuts_mean %>% 
  distinct(latin_name)

spp2 <- mean_trad %>% 
  distinct(latin_name)

spp3 <- mean_nuts %>% 
  select(species_name) %>% 
  rename(latin_name = species_name)
  
intersect(spp$latin_name, spp3$latin_name)

new_trad_species <- setdiff(spp$latin_name, spp3$latin_name) ### these are the species in the new trad foods, that are not in the global dataset

newtrad <- mean_trad %>% 
  filter(latin_name %in% new_trad_species) %>% 
  rename(species_name = latin_name)

write_csv(newtrad, "data-processed/newtrad.csv")


setdiff(spp3$latin_name, spp$latin_name)

lj <- inner_join(mean_nuts, mean_trad, by = c("species_name" = "latin_name"))
lj %>% 
  # select(starts_with("calcium"), starts_with("iron"), starts_with("epa")) %>% 
  ggplot(aes(x = calcium.x, y = calcium.y)) + geom_point() +
  # ylim(0,10) +
  geom_abline(slope = 1, intercept = 0)


# accumulation analysis ---------------------------------------------------


threshold = 0.1
data <- cnuts_split[[1]]

accumulate <- function(data, threshold) {
  ntbl.RDI.all <- data %>% 
    mutate(RDI.CA = ifelse(calcium > (1200*threshold), 1, 0)) %>% 
    mutate(RDI.FE = ifelse(iron > (18*threshold), 1, 0)) %>% 
    mutate(RDI.ZN = ifelse(zinc > (11*threshold), 1, 0)) %>%
    mutate(RDI.EPA = ifelse(epa > (1*threshold), 1, 0)) %>%
    mutate(RDI.DHA = ifelse(dha > (1*threshold), 1, 0)) %>%
    ungroup() %>% 
    mutate(RDI.micro.tot = rowSums(.[7:11])) %>% 
    filter(!is.na(RDI.micro.tot)) 
  
  all_spa <- ntbl.RDI.all %>%
    dplyr::select(latin_name, 7:12) %>% 
    mutate(subgroup = "all") %>% 
    split( .$subgroup) %>% 
    map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.EPA", "RDI.DHA")) %>%
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


cnuts_split <- nuts_mean %>% 
  split(.$culture)

cnuts_split[[1]]
res <- cnuts_split %>% 
  map_df(accumulate, threshold = 0.1, .id = "culture")

write_csv(res, "data-processed/nut_accumulation_trad_foods.csv")
res <- read_csv("data-processed/nut_accumulation_trad_foods.csv")

## now for global dataset
mean_nuts <- read.csv("data-processed/mean_nuts.csv")

accumulate <- function(data, threshold) {
  ntbl.RDI.all <- data %>% 
    mutate(RDI.CA = ifelse(calcium > (1200*threshold), 1, 0)) %>% 
    mutate(RDI.FE = ifelse(iron > (18*threshold), 1, 0)) %>% 
    mutate(RDI.ZN = ifelse(zinc > (11*threshold), 1, 0)) %>%
    mutate(RDI.EPA = ifelse(epa > (1*threshold), 1, 0)) %>%
    mutate(RDI.DHA = ifelse(dha > (1*threshold), 1, 0)) %>%
    ungroup() %>% 
    mutate(RDI.micro.tot = rowSums(.[7:11])) %>% 
    filter(!is.na(RDI.micro.tot)) 
  
  all_spa <- ntbl.RDI.all %>%
    dplyr::select(species_name, 7:12) %>% 
    mutate(subgroup = "all") %>% 
    split( .$subgroup) %>% 
    map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.EPA", "RDI.DHA")) %>%
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

res_global <- accumulate(sample_n(mean_nuts, size = 40, replace = FALSE), threshold = 0.1) %>% 
  mutate(culture = "global")


res_all <- bind_rows(res, res_global)

res_all %>% 
  filter(culture %in% species_numbers$culture | culture == "global") %>% 
  filter(number_of_species < 11) %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets, group = culture, color = culture)) + geom_line(size =1) +
  # geom_ribbon(aes(ymin = number_of_targets - se, ymax = number_of_targets + se, fill = culture), alpha = 0.2, size = 0) +
  ylab("Number of nutrient requirements fulfilled (10% DRI)") +
  xlab("Number of species") + theme(text = element_text(size=14)) + 
  # scale_color_grey(start = 0.01, end = 0.7) +
  # theme_bw() +
  # theme(legend.position = c(0.6, 0.2)) +
  # scale_x_continuous(breaks = seq(1,10,1)) +
  theme(legend.title=element_blank()) + theme_classic() +
  ylim(0,5) +
  # facet_wrap( ~ culture) +
  scale_color_viridis(discrete = TRUE) + scale_fill_viridis(discrete = TRUE)

ggsave("figures/nutrient_accumulation_plots_na_cultures_overlay.png", width = 8, height = 6)

## same figure, no color
res_all %>% 
  mutate(culture = ifelse(culture == "global", "Global", culture)) %>% 
  filter(culture %in% species_numbers$culture | culture == "Global") %>% 
  filter(number_of_species < 11) %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets)) + geom_line(size =1) +
  geom_ribbon(aes(ymin = number_of_targets - se, ymax = number_of_targets + se), alpha = 0.2, size = 0) +
  ylab("Number of nutrient requirements fulfilled (10% DRI)") +
  xlab("Number of species") + theme(text = element_text(size=14)) + 
  # scale_color_grey(start = 0.01, end = 0.7) +
  # theme_bw() +
  # theme(legend.position = c(0.6, 0.2)) +
  # scale_x_continuous(breaks = seq(1,10,1)) +
  theme(legend.title=element_blank()) + theme_classic() +
  ylim(0,5) +
  facet_wrap( ~ culture) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
ggsave("figures/nutrient_accumulation_plots_nut_accum_bw.png", width = 8, height = 6)





## ok now try to fit a power function to each of these accumulation curves


# parameter estimate plots ------------------------------------------------

mod <- res_all %>% 
  filter(culture %in% species_numbers$culture | culture == "global") %>% 
  filter(number_of_species < 11) %>% 
  group_by(culture) %>% 
  do(tidy(nls(formula = (number_of_targets ~ a * number_of_species^b),data = .,  start = c(a=1, b=0.3)))) 


a_terms <- mod %>% 
  filter(term == "a")

a_plot <- a_terms %>% 
  ggplot(aes(x = reorder(culture, estimate), y = estimate)) + geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Culture") + ylab("Parameter estimate (a)") 


b_terms <- mod %>% 
  filter(term == "b")

b_plot <- b_terms %>% 
  ggplot(aes(x = reorder(culture, estimate), y = estimate)) + geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Culture") + ylab("Parameter estimate (b)") 

plot_grid(a_plot, b_plot, nrow = 1, ncol = 2)


a_est <- mod$estimate[mod$culture == "Haida" & mod$term == "a"]
b_est <-mod$estimate[mod$culture == "Haida" & mod$term == "b"]

yupik_power_function <- function(x) a_est*x^b_est


res_all %>% 
  filter(number_of_species < 11) %>%
  filter(culture == "Haida") %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets)) + geom_line() +
  ylim(0, 5) +
  stat_function(fun = yupik_power_function, color = "green")
