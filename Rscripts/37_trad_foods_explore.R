

library(tidyverse)
library(viridis)
library(readxl)
library(stringr)
library(purrr)
library(vegan)
library(cowplot)
library(broom)


nuts_trad <- read_csv("data-processed/trad-foods-cleaned.csv")
culture_foods <- read_excel("~/Documents/traditional-foods/culture-foods.xlsx")

View(nuts_trad)

# data prep section -------------------------------------------------------

nuts_trad %>% 
  distinct(reference) %>% View


nuts_trad %>% 
  distinct(latin_name, common_name, reference) %>% View
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
  filter(nutrient == "epa") %>% 
  mutate(latin_name = factor(latin_name)) %>% View
  ggplot(aes(x = reorder(latin_name, concentration), y = concentration)) + geom_point() +
  facet_wrap( ~ nutrient, scales = "free") + theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

cnuts <- left_join(culture_foods, nuts_trad)

write_csv(cnuts, "data-processed/cnuts-trad-foods-culture.csv")
cnuts <- read_csv("data-processed/cnuts-trad-foods-culture.csv")

View(cnuts)
cnuts_species <- cnuts %>% 
  distinct(common_name, latin_name)
write_csv(cnuts_species, "data-processed/cnuts_species.csv")

percentages <- read_csv("data-processed/percentages.csv")

### 12, 13 and 63, 88 are from USDA
# cine_keep_ref3 <- c(1, 2, 3, 4, 5, 6, 8, 11, 12, 13, 17, 19, 20, 24, 25, 27, 28, 34, 37, 38, 39, 40, 41, 43,
#                     44, 49, 52,53, 54,
#                     63, 65, 66, 88, 90, 91)

cine_keep_ref_trad <- c(1, 2, 3, 4, 5, 6, 8, 11, 17, 19, 20, 24, 25, 27,23,
                   28, 34, 37, 38, 39, 40, 41, 43, 44, 49, 50, 52,53, 54, 58,
            65, 66, 90, 91)

###ok now bring in the updated researched source list

updated_ref <- read_csv("data-processed/CINE-nutrients-fish-references-annotated.csv")

keep_refs <- updated_ref %>% 
  filter(grepl("yes", peer_reviewed))

# cine_discard_refs3 <- setdiff(cine_refs$reference, cine_keep_ref_trad)

cnuts %>% 
  filter(grepl(" ", latin_name)) %>% 
  filter(reference == 9) %>% View

cnuts2 <-  cnuts %>% 
  # filter(grepl(" ", latin_name)) %>% 
  filter(reference %in% keep_refs$ref_number) 
cnuts_fill_in <- cnuts %>% 
# filter(grepl(" ", latin_name)) %>% 
filter(reference %in% keep_refs$ref_number) %>% 
  group_by(common_name, latin_name) %>% 
  summarise(calcium = mean(ca_mg, na.rm = TRUE),
            zinc = mean(zn_mg, na.rm = TRUE), 
            iron = mean(fe_mg, na.rm = TRUE),
            epa = mean(epa, na.rm = TRUE),
            dha = mean(dha, na.rm = TRUE)) 
write_csv(cnuts_fill_in, "data-processed/cnuts_fill_in.csv")



cnuts %>% 
  filter(reference %in% c(12, 13, 63, 88)) %>% 
  dplyr::select(common_name, latin_name, part) %>% 
  distinct() %>% View

unique(cnuts2$reference)
c2 <- cnuts2 %>% 
  dplyr::select(latin_name, ca_mg, zn_mg, fe_mg, epa, dha) %>% 
  rename(calcium = ca_mg,
         zinc = zn_mg,
         iron = fe_mg,
         species_name = latin_name) %>% 
  gather(key = nutrient, value = concentration, 2:6)



c3 <- c2 %>% 
  left_join(., percentages, by = c("nutrient", "species_name")) %>% 
  distinct() %>% 
  gather(key = source, value = concentration_all, concentration.x, concentration.y) %>% 
  group_by(species_name, nutrient) %>% 
  summarise(mean_concentration = mean(concentration_all, na.rm = TRUE)) %>% 
  filter(!is.na(mean_concentration)) %>% 
  spread(key = nutrient, value = mean_concentration) %>% 
  summarise(calcium = mean(calcium, na.rm = TRUE),
            zinc = mean(zinc, na.rm = TRUE), 
            iron = mean(iron, na.rm = TRUE),
            epa = mean(epa, na.rm = TRUE),
            dha = mean(dha, na.rm = TRUE)) %>% 
  filter(!is.na(calcium), !is.na(iron), !is.na(zinc), !is.na(epa), !is.na(dha)) 

c4 <- cnuts %>% 
  dplyr::select(culture, latin_name, common_name) %>% 
  rename(species_name = latin_name) %>% 
  distinct()

c5 <- left_join(c3, c4) %>% 
  rename(latin_name = species_name)

c5 %>% 
  filter(!grepl(" ", latin_name)) %>% View

write_csv(c5, "data-processed/trad-foods-mean-aug2020b.csv")
write_csv(c5, "data-processed/trad-foods-mean-aug2020c.csv")

unique(c5$latin_name)
nuts_mean <- cnuts %>%  
  # filter(!reference %in% cine_discard_refs3) %>%
  group_by(culture, latin_name, level_1) %>% 
  summarise(calcium = mean(ca_mg, na.rm = TRUE),
            zinc = mean(zn_mg, na.rm = TRUE), 
            iron = mean(fe_mg, na.rm = TRUE),
            epa = mean(epa, na.rm = TRUE),
            dha = mean(dha, na.rm = TRUE)) %>% 
  filter(!is.na(calcium), !is.na(iron), !is.na(zinc), !is.na(epa), !is.na(dha)) %>% 
  rename(subgroup = level_1)

write_csv(nuts_mean, "data-processed/trad-foods-mean.csv")
write_csv(nuts_mean, "data-processed/trad-foods-mean-aug2020.csv")


nuts_mean <- cnuts %>%  
  filter(reference %in% keep_refs$ref_number) %>% 
  group_by(culture, latin_name, level_1, common_name) %>% 
  summarise(calcium = mean(ca_mg, na.rm = TRUE),
            zinc = mean(zn_mg, na.rm = TRUE), 
            iron = mean(fe_mg, na.rm = TRUE),
            epa = mean(epa, na.rm = TRUE),
            dha = mean(dha, na.rm = TRUE)) %>% 
  filter(!is.na(calcium), !is.na(iron), !is.na(zinc), !is.na(epa), !is.na(dha)) %>% 
  rename(subgroup = level_1)

write_csv(nuts_mean, "data-processed/trad-foods-mean-aug2020d.csv")

### how many species per culture?

nuts_mean <- read_csv("data-processed/trad-foods-mean.csv")
trad_nuts_mean <- read_csv("data-processed/trad-foods-mean.csv")
mean_nuts_global <- read_csv("data-processed/mean_nuts.csv")


## ok let's make a new global dataset that also includes the new trad foods

trad_means <- trad_nuts_mean %>% 
  distinct(latin_name, .keep_all = TRUE) %>%
  dplyr::select(-culture) %>% 
  rename(species_name = latin_name)

## ok this will be the new global dataset!
new_global <- bind_rows(mean_nuts_global, trad_means)

write_csv(new_global, "data-processed/new_global.csv")
new_global <- read_csv("data-processed/new_global.csv")

species_numbers <- nuts_mean %>% 
  group_by(culture) %>% 
  summarise(n_species = n_distinct(latin_name)) %>% 
  filter(n_species >= 25) 

write_csv(species_numbers, "data-processed/species_numbers.csv")
species_numbers <- read_csv("data-processed/species_numbers.csv")

# comparing species lists -------------------------------------------------

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




lj <- inner_join(mean_nuts, mean_trad, by = c("species_name" = "latin_name"))
lj %>% 
  # select(starts_with("calcium"), starts_with("iron"), starts_with("epa")) %>% 
  ggplot(aes(x = dha.x, y = dha.y)) + geom_point() +
  # ylim(0,10) +
  geom_abline(slope = 1, intercept = 0)


# accumulation analysis ---------------------------------------------------

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
mean_nuts <- read_csv("data-processed/mean_nuts.csv")

accumulate_global <- function(data, threshold) {
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
## but now use new global

res_global <- accumulate_global(sample_n(new_global, size = 40, replace = FALSE), threshold = 0.1) %>% 
  mutate(culture = "global")

write_csv(res_global, "data-processed/res_global.csv")
res_global <- read_csv("data-processed/res_global.csv")

res_global_all <- accumulate_global(new_global, threshold = 0.1) %>% 
  mutate(culture = "global")

write_csv(res_global_all, "data-processed/res_global_all.csv")

res_global_all <- read_csv("data-processed/res_global_all.csv")

res_all <- bind_rows(res, res_global)

res_all %>% 
  filter(culture %in% species_numbers$culture | culture == "global") %>% 
  filter(number_of_species < 11) %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets, group = culture)) +
  geom_ribbon(aes(ymin = number_of_targets - se, ymax = number_of_targets + se), alpha = 0.05, size = 0) +
  geom_line(size =.5, alpha = 0.5) +
  geom_line(color = "cadetblue", size =1, data = filter(res_all, culture == "global", number_of_species < 11)) +
  ylab("Number of nutrient requirements fulfilled (10% DRI)") +
  xlab("Number of species") + theme(text = element_text(size=14)) + 
  # scale_color_grey(start = 0.01, end = 0.7) +
  # theme_bw() +
  # theme(legend.position = c(0.6, 0.2)) +
  # scale_x_continuous(breaks = seq(1,10,1)) +
  theme(legend.title=element_blank()) + theme_classic() +
  ylim(0,5) +
  scale_x_continuous(breaks = seq(1,10,1))
  #facet_wrap( ~ culture) +
  # scale_color_viridis(discrete = TRUE, option = "magma") + scale_fill_viridis(discrete = TRUE, option = "magma")

ggsave("figures/nutrient_accumulation_plots_na_cultures_overlay.png", width = 8, height = 6)
ggsave("figures/nutrient_accumulation_plots_na_cultures_overlay_bw.png", width = 4, height = 4)

## same figure, no color
res_all %>% 
  mutate(culture = ifelse(culture == "global", "Global", culture)) %>% 
  filter(culture %in% species_numbers$culture | culture == "Global") %>% 
  filter(number_of_species < 11) %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets)) + geom_line(size =.5) +
  geom_ribbon(aes(ymin = number_of_targets - se, ymax = number_of_targets + se), alpha = 0.2, size = 0) +
  ylab("Number of nutrient requirements fulfilled (10% DRI)") +
  xlab("Number of species") + theme(text = element_text(size=12)) + 
  # scale_color_grey(start = 0.01, end = 0.7) +
  # theme_bw() +
  # theme(legend.position = c(0.6, 0.2)) +
  # scale_x_continuous(breaks = seq(1,10,1)) +
  theme(legend.title=element_blank()) + theme_classic() +
  ylim(0,5) +
  scale_x_continuous(breaks = seq(1,10,2)) +
  facet_wrap( ~ culture) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
ggsave("figures/nutrient_accumulation_plots_nut_accum_bw.png", width = 4, height = 4)





## ok now try to fit a power function to each of these accumulation curves


# parameter estimate plots ------------------------------------------------
power_function <- function(x) 2.5*x^0.23
power_function4 <- function(x) 2.3*x^0.23

p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
p + stat_function(fun = power_function, color = "black", size = 1) +
  stat_function(fun = power_function4, color = "red", size = 1) + xlim(1,10)


xes <- seq(1:10)
intercept_2.5 <- sapply(xes, power_function)
intercept_2.3 <- sapply(xes, power_function4)

df <- data.frame(xes, intercept_2.5, intercept_2.3) %>% 
  gather(key = "intercept", value = "y", contains("inter"))

df %>% 
  ggplot(aes( x = log(xes), y = log(y), color = intercept)) + geom_line() 

library(broom)

mod <- res_all %>% 
  filter(culture %in% species_numbers$culture | culture == "global") %>% 
  filter(number_of_species < 11) %>% 
  group_by(culture) %>% 
  do(tidy(nls(formula = (number_of_targets ~ a * number_of_species^b),data = .,  start = c(a=1, b=0.3)))) 

bef_terms <- mod %>% 
  select(culture, term, estimate) %>% 
  spread(key = term, value = estimate) 

cor(bef_terms$a, bef_terms$b)

linear_slopes <- res_all %>% 
  filter(culture %in% species_numbers$culture | culture == "global") %>% 
  filter(number_of_species < 11) %>% 
  group_by(culture) %>% 
  do(tidy(lm(log(number_of_targets) ~ log(number_of_species),data = .))) %>% 
  filter(term != "(Intercept)")


res_all %>% 
  filter(culture %in% species_numbers$culture | culture == "global") %>% 
  filter(number_of_species < 11) %>%
  ggplot(aes(x = log(number_of_species), y = log(number_of_targets))) + geom_point()

a_terms <- mod %>% 
  filter(term == "a")

a_plot <- a_terms %>% 
  ggplot(aes(x = reorder(culture, estimate), y = estimate)) + geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Culture") + ylab("Parameter estimate (a)") 


b_terms <- mod %>% 
  filter(term == "b")

write_csv(b_terms, "data-processed/b_terms_bef.csv")

b_plot <- b_terms %>% 
  ggplot(aes(x = reorder(culture, estimate), y = estimate)) + geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Culture") + ylab("Parameter estimate (b)") 

BEF_params_plot <- plot_grid(a_plot, b_plot, nrow = 2, ncol = 1)
save_plot("figures/BEF-params.png", BEF_params_plot,
          ncol = 1, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.2
)


a_est <- mod$estimate[mod$culture == "Haida" & mod$term == "a"]
b_est <-mod$estimate[mod$culture == "Haida" & mod$term == "b"]

yupik_power_function <- function(x) a_est*x^b_est


res_all %>% 
  filter(number_of_species < 11) %>%
  filter(culture == "Haida") %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets)) + geom_line() +
  ylim(0, 5) +
  stat_function(fun = yupik_power_function, color = "green")


# Functional diversity analysis -------------------------------------------

## let's look at functional diversity
library(FD)

new_global <- read_csv("data-processed/new_global.csv")
trad_mean <- read_csv("data-processed/trad-foods-mean.csv")
mean_nuts <- read_csv("data-processed/mean-nuts.csv")


sample_40_global <- function(sample_size) {
  sample_n(mean_nuts, size = sample_size, replace = FALSE)
}

reps <- rep(40, 1000)

mean_nuts_rep <- reps %>% 
  map_df(sample_40_global, .id = "replicate") %>% 
  select(-species_name) %>%
  select(- subgroup) %>% 
  ungroup() %>% 
  filter(replicate != 1) %>% 
  split(.$replicate)

mean_nuts_rep[[1]]

cnuts_split <- trad_mean %>% 
  select(-latin_name) %>%
  select(- subgroup) %>% 
  ungroup() %>% 
  split(.$culture)
cnuts_split[[1]]

fds <- cnuts_split %>% 
  map(dbFD)

str(cnuts_split)
str(mean_nuts_rep)

fds_global <- mean_nuts_rep %>% 
  map(dbFD)

fEve_global <- fds_global %>% 
  map("FEve") %>% 
  unlist() %>% 
  as.data.frame() 

fEve_global$replicate <- rownames(fEve_global) 
names(fEve_global) <- c("FEve", "replicate")
fEve_global <- fEve_global %>% 
  mutate(culture = str_replace(replicate, ".Community1", "")) %>% 
  select(-replicate) %>% 
  rename(replicate = culture)


write_csv(fEve_global, "data-processed/functional_evenness_global.csv")



# Calculate expected functional evenness ----------------------------------
mean_nuts <- read_csv("data-processed/mean-nuts.csv")
repeat_feve <- function(sample_size){
  global <- sample_n(distinct(mean_nuts, latin_name, .keep_all = TRUE), size = sample_size, replace = FALSE) %>% 
    mutate(culture = "global") %>% 
    select(-species_name) %>%
    select(- subgroup) %>% 
    ungroup()
  res <- dbFD(global)$FEve[[1]]
results <- cbind(res, sample_size) 
results <- as.data.frame(results)
return(results)
}


sample_sizes <- rep(species_numbers$n_species, 1000)

feves <- sample_sizes %>% 
  map_df(repeat_feve, .id = "replicate")

write_csv(feves, "data-processed/expected_functional_evenness.csv")

####  Expected vs. Observed Functional Evenness
feves_summ <- feves %>% 
  group_by(sample_size) %>% 
  summarise_each(funs(mean, std.error), res)

species_numbers <- read_csv("data-processed/species_numbers.csv")

FEve_cultures <- left_join(fEve, species_numbers) %>% 
  filter(!is.na(n_species)) 

obs_exp_feve <- left_join(FEve_cultures, feves_summ, by = c("n_species" = "sample_size"))

obs_exp_feve_global <- fEve_global %>% 
  summarise_each(funs(mean, std.error), FEve) %>% 
  rename(res_mean = FEve_mean) %>% 
  mutate(n_species = 40) %>% 
  mutate(culture = "Global (40 spcies)") %>% 
  mutate(FEve = res_mean)

obs_exp_feve2 <- bind_rows(obs_exp_feve, obs_exp_feve_global)

write_csv(obs_exp_feve2, "data-processed/obs_exp_feve2.csv")

obs_exp_feve2 <- read_csv("data-processed/obs_exp_feve2.csv")

library(LaCroixColoR)
colors <- lacroix_palette("PeachPear", type = "continuous", n = 15) 
colorsp <- c(unique(lacroix_palette(type = "paired")), "yellow", "cadetblue", "purple") 

obs_exp_feve_plot <- obs_exp_feve2 %>% 
ggplot(aes(x = res_mean, y = FEve, color = culture)) +
  geom_abline(slope = 1, intercept = 0) +geom_point(size = 4) +
  geom_point(size = 4, shape = 1, color = "black") +
  ylim(0.68, 0.82) + xlim(0.68, 0.82) +
  xlab("Expected FEve") + ylab("Observed FEve") + scale_color_manual(values = colorsp, name = "Region") +
  ggtitle("B") +
  theme(plot.title = element_text(hjust = 0))
ggsave("figures/obs_exp_functional_evenness.pdf", width = 7, height = 5)




fEve <- fds %>% 
  map("FEve") %>% 
  unlist() %>% 
  as.data.frame() 

fEve$culture <- rownames(fEve) 
names(fEve) <- c("FEve", "culture")
fEve <- fEve %>% 
  mutate(culture = str_replace(culture, ".Community1", ""))


fDiv <- fds %>% 
  map("FDiv") %>% 
  unlist() %>% 
  as.data.frame() 

fDiv$culture <- rownames(fDiv) 
names(fDiv) <- c("FDiv", "culture")
fDiv <- fDiv %>% 
  mutate(culture = str_replace(culture, ".Community1", ""))

FDs <- left_join(fDiv, fEve, by = "culture") %>% 
  select(culture, FDiv, FEve)

fdiv_expected <- read_csv("data-processed/fdiv_expected.csv") %>% 
  mutate(dataset = "expected") %>% 
  rename(value = fdiv_expected) %>% 
  mutate(metric = "FDiv")

feve_expected <- read_csv("data-processed/FEve_expected.csv") %>% 
  mutate(dataset = "expected") %>% 
  rename(value = FEve_expected) %>% 
  mutate(metric = "FEve")

fd_long <- FDs %>% 
  gather(key = "metric", value = "value", 2:3) %>% 
  rename(dataset = culture)


all_fd <- bind_rows(fdiv_expected, fd_long, feve_expected)
species_numbers <- read_csv("data-processed/species_numbers.csv")

all_fd %>% 
  filter(dataset %in% species_numbers$culture | dataset == "expected") %>% 
  filter(metric == "FEve") %>% 
  ggplot(aes(x = value), fill = "grey") + geom_histogram(bins = 40) +
  geom_vline(xintercept = filter(all_fd, dataset == "Abenaki", metric == "FEve")[[1]]) +
  geom_vline(xintercept = filter(all_fd, dataset == "Bella Coola", metric == "FEve")[[1]]) +
  geom_vline(xintercept = filter(all_fd, dataset == "Haida", metric == "FEve")[[1]]) +
  geom_vline(xintercept = filter(all_fd, dataset == "Tlingit", metric == "FEve")[[1]]) +
  geom_vline(xintercept = filter(all_fd, dataset == "Micmac", metric == "FEve")[[1]]) +
  geom_vline(xintercept = quantile(feve_expected$value, probs = c(0.025)), color = "red") +
  geom_vline(xintercept = quantile(feve_expected$value, probs = c(0.975)), color = "red") +
  geom_vline(xintercept = mean(feve_expected$value), color = "red")

### compare to global dataset
mean_nuts <- read_csv("data-processed/mean_nuts.csv")  

mean_nuts2 <- sample_n(mean_nuts, size = 40, replace = FALSE)
ntbl.matrix.mic <- data.matrix(mean_nuts2[, 3:7])
rownames(ntbl.matrix.mic) <- mean_nuts2$species_name
FD_global <- data.frame(FDiv = dbFD(ntbl.matrix.mic)$FEve) %>% 
  mutate(culture = "global")

all_fd <- bind_rows(fDiv, FD_global)

all_fd %>% 
  # rename(culture = dataset) %>% 
  filter(culture %in% species_numbers$culture | culture == "global") %>% 
  ggplot(aes(x = culture, y = FDiv)) + geom_histogram(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Nutritional functional evenness (FEve)") + xlab("Culture")


### FEve result
FDs %>% 
  filter(culture %in% species_numbers$culture) %>%
  summarise_each(funs(mean, std.error), FEve) %>% View

### FD and FEve data
write_csv(FDs, "data-processed/functional_diversity_results.csv")

(unames <- map_chr(fds, c(1,1)))

com_names <- fds %>% 
  map_chr(1) %>% 
  names()

(udf <- fds %>%
    set_names(com_names) %>% 
    enframe("community", "diversity_info"))

udf %>% 
  mutate(repo_info = diversity_info %>%
           map(. %>% map_df(`[`, c("FDiv", "FDis"))))

