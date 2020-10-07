

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
species_numbers <- read_csv("data-processed/species_numbers_october.csv")

View(nuts_trad)

# data prep section -------------------------------------------------------

updated_ref <- read_csv("data-processed/CINE-nutrients-fish-references-annotated.csv")








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

mean_nuts_rep[[1]]

cnuts_split <- trad_mean %>% 
  dplyr::select(-latin_name) %>%
  ungroup() %>% 
  split(.$culture)
cnuts_split[[1]]

fds <- cnuts_split %>% 
  map(dbFD)

View(fds)

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


write_csv(fEve_global, "data-processed/functional_evenness_global_october.csv")



# Calculate expected functional evenness ----------------------------------
mean_nuts <- read_csv("data-processed/mean_nuts_oct-4-2020.csv") %>% 
  rename(latin_name = taxize_name)
View(mean_nuts)
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

####  Expected vs. Observed Functional Evenness
feves_summ <- feves %>% 
  group_by(sample_size) %>% 
  summarise_each(funs(mean, std.error), res)

species_numbers <- read_csv("data-processed/species_numbers_october.csv")

FEve_cultures <- left_join(fEve, species_numbers) %>% 
  filter(!is.na(n_species)) 

obs_exp_feve <- left_join(FEve_cultures, feves_summ, by = c("n_species" = "sample_size"))

fEve_global

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

write_csv(fEve, "data-processed/local-functional-evenness-october.csv")



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



