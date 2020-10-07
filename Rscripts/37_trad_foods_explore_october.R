

library(tidyverse)
library(viridis)
library(readxl)
library(stringr)
library(purrr)
library(vegan)
library(cowplot)
library(broom)
library(plotrix)
theme_set(theme_cowplot())


nuts_trad <- read_csv("data-processed/trad-foods-cleaned.csv")
culture_foods <- read_excel("~/Documents/traditional-foods/culture-foods.xlsx")
species_numbers <- read_csv("data-processed/species_numbers_october.csv")

View(nuts_trad)

# data prep section -------------------------------------------------------

updated_ref <- read_csv("data-processed/CINE-nutrients-fish-references-annotated.csv")









# Functional diversity analysis -------------------------------------------

## let's look at functional diversity
library(FD)


#### read in data

trad_mean <- read_csv("data-processed/traditional_foods_nutrients_cultures_for_analysis.csv") %>% 
  rename(latin_name = genus_species)
mean_nuts <- read_csv("data-processed/mean_nuts_oct-4-2020.csv") %>% 
  rename(latin_name = taxize_name)


sample_40_global <- function(sample_size) {
  sample_n(mean_nuts, size = sample_size, replace = FALSE)
}

reps <- rep(40, 1000)

mean_nuts_rep <- reps %>% 
  map_df(sample_40_global, .id = "replicate") %>% 
  dplyr::select(-latin_name) %>%
  dplyr::select(- subgroup) %>% 
  ungroup() %>% 
  filter(replicate != 1) %>% 
  split(.$replicate)


cnuts_split <- trad_mean %>% 
  dplyr::select(-latin_name) %>%
  ungroup() %>% 
  split(.$culture)
cnuts_split[[1]]


#### estimate functional diversity metrics for the 1000 subsamples of 40 species from the global pool
fds <- cnuts_split %>% 
  map(dbFD)


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


write_csv(fEve_global, "data-processed/functional_evenness_global_october.csv")

fEve_global <- read_csv("data-processed/functional_evenness_global_october.csv") #### expected nutritional functional evenness at the global level

# Calculate expected functional evenness ----------------------------------
mean_nuts <- read_csv("data-processed/mean_nuts_oct-4-2020.csv") %>% 
  rename(latin_name = taxize_name)
View(mean_nuts)


#### estimate expected functional evenness, if the species came from the global species pool

repeat_feve <- function(sample_size){
  global <- sample_n(distinct(mean_nuts, latin_name, .keep_all = TRUE), size = sample_size, replace = FALSE) %>% 
    # mutate(culture = "global") %>% 
    dplyr::select(-latin_name) %>%
    dplyr::select(- subgroup) %>% 
    ungroup()
  res <- dbFD(global)$FEve[[1]]
results <- cbind(res, sample_size) 
results <- as.data.frame(results)
return(results)
}


sample_sizes <- rep(species_numbers$n_species, 1000)

feves <- sample_sizes %>% 
  map_df(repeat_feve, .id = "replicate")

write_csv(feves, "data-processed/expected_functional_evenness_october.csv")

feves <- read_csv("data-processed/expected_functional_evenness_october.csv")

####  Expected vs. Observed Functional Evenness
feves_summ <- feves %>% 
  group_by(sample_size) %>% 
  summarise_each(funs(mean), res) %>% 
  rename(expected_feve = res)


# FEve_cultures <- left_join(fEve, species_numbers) %>% 
#   filter(!is.na(n_species)) 

obs_exp_feve <- left_join(FEve_cultures, feves_summ, by = c("n_species" = "sample_size"))

fEve_global

obs_exp_feve_global <- fEve_global %>% 
  summarise_each(funs(mean, std.error), FEve) %>% 
  rename(res_mean = mean) %>% 
  mutate(n_species = 40) %>% 
  mutate(culture = "Global (40 spcies)") %>% 
  mutate(FEve = res_mean)


fEve <- fds %>% 
  map("FEve") %>% 
  unlist() %>% 
  as.data.frame() 

fEve$culture <- rownames(fEve) 
names(fEve) <- c("FEve", "culture")
fEve <- fEve %>% 
  mutate(culture = str_replace(culture, ".Community1", ""))

write_csv(fEve, "data-processed/local-functional-evenness-october.csv")

fEve <- read_csv("data-processed/local-functional-evenness-october.csv")

feve2 <- fEve %>% 
  left_join(., species_numbers)



obs_exp_local <- feve2 %>% 
  left_join(., feves_summ, by = c("n_species" = "sample_size")) %>% 
  bind_rows(., obs_exp_feve_global) %>% 
  mutate(expected_feve = ifelse(grepl("Global", culture), FEve, expected_feve))

write_csv(obs_exp_local, "data-processed/observed-expected-functional-evenness-october2020.csv")

mean(obs_exp_local$n_species)

obs_exp_local %>% 
  ggplot(aes(x = expected_feve, y = FEve, color = culture)) + geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0) + xlim(0.5, 0.9) + ylim(0.5, 0.9) +
  ylab("Observed FEve") + xlab("Expected FEve")
ggsave("figures/obs-expected-functional-evenness-oct2020.pdf", width = 6, height = 4)
  

# feve results ------------------------------------------------------------


#### ok so compare the expected to the observed Feves
#### feves results!
fEve %>% View
  left_join(., feves_summ) 
obs_exp_feves_local <- feves_summ  %>%
  left_join(., species_numbers, by = c("sample_size" = "n_species")) %>% 
  left_join(., fEve, by = "culture") %>% 
  rename(exp_feve = mean)

global_feve <- read_csv("data-processed/functional_evenness_global_october.csv") %>% 
  summarise_each(funs(mean), FEve) %>% 
  mutate(culture = "global")

all_feves <- bind_rows(obs_exp_feves_local, global_feve) %>% 
  mutate(exp_feve = ifelse(culture == "global", FEve, exp_feve))

write_csv(all_feves, "data-processed/all-functional-evenness-estimate.csv")

all_feves %>% 
  ggplot(aes(x = exp_feve, y = FEve, color = culture)) + geom_point() +
  geom_abline(slope = 1, intercept = 0) + xlim(0.5, 1) + ylim(0.5, 1)


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



