library(tidyverse)
library(cowplot)
library(viridis)
library(forcats)
library(plotrix)
theme_set(theme_cowplot())

# trad_nuts_mean <- read_csv("data-processed/trad-foods-mean.csv") %>% 
#   filter(!is.na(latin_name))
trad_nuts_mean <- read_csv("data-processed/traditional_foods_nutrients_cultures_for_analysis.csv") %>% 
  rename(latin_name = genus_species)

unique(trad_nuts_mean$culture)

trad_nuts_mean %>% 
  group_by(culture) %>% 
  tally()

species_numbers_cultures <- trad_nuts_mean %>% 
  group_by(culture) %>% 
  tally() 

trad_nuts_mean2 <- read_csv("data-processed/mean_nuts_oct-4-2020.csv") %>% 
  rename(latin_name = taxize_name)

# trad_nuts_mean2 <- read_csv("data-processed/mean_nuts.csv") %>% 
#   rename(latin_name = species_name) %>% 
#   filter(!is.na(latin_name))

# trad_nuts_mean <- read_csv("data-processed/mean_nuts.csv") %>% 
  # rename(latin_name = species_name) %>% 
  # filter(!is.na(latin_name))


repeat_fd <- function(sample_size){

global <- sample_n(distinct(trad_nuts_mean2, latin_name, .keep_all = TRUE), size = sample_size, replace = FALSE) %>% 
  mutate(culture = "global")

pres_abs <- global %>% 
  spread(key = culture, value = latin_name) %>%
 rename(species_name = global) %>% 
  mutate(global = 1) %>% 
  # mutate_all(.funs= str_replace_na, "0") %>% 
  arrange(species_name)  %>% 
  dplyr::select(global)


species_names <- global %>% 
  rename(species_name = latin_name) %>% 
  dplyr::select(species_name) %>% 
  arrange(species_name)



pres_abs <- as.data.frame(pres_abs)
rownames(pres_abs) <- species_names$species_name

landfile <- pres_abs

traitfile <- global %>% 
  distinct(latin_name, .keep_all = TRUE) %>% 
  dplyr::select(-culture, -subgroup) %>% 
  filter(latin_name %in% species_names$species_name) %>% 
  arrange(latin_name) %>% 
  dplyr::select(-latin_name) %>% 
  as.data.frame()

rownames(traitfile) <- species_names$species_name
FDs <- Calculate.FD(landfile = landfile, traitfile = traitfile, scale = F, writefiles = F)
fd3 <- data.frame(FD = FDs, region = names(FDs), sample_size = sample_size)
return(fd3)
}


sample_sizes <- rep(species_numbers$n_species, 1000)
sample_sizes <- rep(23, 1000)


exp_df2 <- sample_sizes %>% 
  map_df(repeat_fd, .id = "sample")
exp_df3_MN <- sample_sizes %>% 
  map_df(repeat_fd, .id = "sample")

# exp_df_mean_nuts <- exp_df

exp_df4 <- bind_rows(exp_df2, exp_df3_MN)

write_csv(exp_df4, "data-processed/expected_FD_regional_october-2020.csv")

exp_df <- read_csv("data-processed/expected_FD_regional_october-2020.csv")
expected_fds <- exp_df %>% 
  group_by(sample_size) %>% 
  summarise(exp_df = mean(FD))

View(expected_fds)

regional_fds_expected <- species_numbers_cultures %>% 
  left_join(expected_fds, by = c("n" = "sample_size")) 

observed_local_fd <- read_csv("data-processed/regional_functional_diversity_october2020.csv")
View(observed_local_fd)

global_fd <- read_csv("data-processed/global_FD_repeat_october.csv") %>% 
  mutate(region = "global_resampled") %>% 
  mutate(n = 40) %>% 
  group_by(n, region) %>% 
  summarise(FD = mean(FD)) %>% 
  mutate(exp_df = FD)

all_fds <- left_join(observed_local_fd, regional_fds_expected, by = c("region" = "culture")) %>% 
  filter(region != "global") %>% 
  bind_rows(., global_fd)


expected_fds2 <- exp_df2 %>% 
  group_by(sample_size) %>% 
  summarise(exp_df = mean(FD))

expected_fds_mean_nuts <- exp_df_mean_nuts %>% 
  group_by(sample_size) %>% 
  summarise(exp_df_nuts = mean(FD))

left_join(expected_fds_mean_nuts, expected_fds) %>% View
left_join(expected_fds_mean_nuts, expected_fds2) %>% View

observed_fd <- read_csv("data-processed/expected_FD_regional_october-2020.csv")




read_csv("data-processed/global_FD_repeat.csv") %>% 
  mutate(region = "global_resampled") %>% 
  mutate(n_species = 40) %>% 
  group_by(n_species, region) %>% 
  summarise_each(funs(mean, std.error), FD) %>% View ### this is to get the average global FD

all_observed_fd <- bind_rows(observed_fd, global_fd)

all_fds <- left_join(all_observed_fd, expected_fds, by = c("n_species" = "sample_size")) %>% 
  filter(region != "global")

write_csv(all_fds, "data-processed/all_fds_october.csv")


all_fds <- read_csv("data-processed/all_fds.csv")
all_feves <- read_csv("data-processed/observed-expected-functional-evenness-october20202.csv")
all_fds %>% 
  filter(region != "global_resampled") %>% 
  summarise_each(funs(mean, std.error), FD) %>% View

obs_exp_FD <- all_fds %>% 
  rename(Region = region) %>% 
  # mutate(FD = ifelse(Region == "global_resampled", exp_df, FD)) %>% 
  mutate(Region = ifelse(Region == "global_resampled", "Global", Region)) %>% 
  ggplot(aes(x = exp_df, y = FD, color = Region)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(size = 4) +
  geom_point(size = 4, shape = 1, color = "black") +
ylim(2, 5.1) + xlim(2, 5.1) +
  xlab("Expected NFD") + ylab("Observed NFD") + 
  # scale_color_manual(values = colorsp) +
  # scale_color_discrete(name = "Region") +
  ggtitle("A") +
  theme(plot.title = element_text(hjust = 0))
ggsave("figures/expected_vs_observed_FD_october.pdf", width = 7, height = 5)

obs_exp_FEve <- all_feves %>% 
  rename(Region = culture) %>%
  # mutate(FD = ifelse(Region == "global_resampled", exp_df, FD)) %>% 
  mutate(Region = ifelse(Region == "global", "Global", Region)) %>% 
  ggplot(aes(x = expected_feve, y = FEve, color = Region)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(size = 4) +
  geom_point(size = 4, shape = 1, color = "black") +
  xlim(0.5, 0.9) + ylim(0.5, 0.9) +
  xlab("Expected NFeve") + ylab("Observed NFeve") + 
  # scale_color_manual(values = colorsp) +
  # scale_color_discrete(name = "Region") +
  ggtitle("A") +
  theme(plot.title = element_text(hjust = 0))


library(patchwork)

obs_exp_plot <- obs_exp_FD + obs_exp_FEve + plot_layout(ncol = 2)
ggplot2::ggsave(plot = obs_exp_plot, filename = "figures/obs_exp_FD_no_order-october.pdf", device = "pdf", width =11, height = 4)



all_fds %>% 
  ggplot(aes(x = reorder(region, FD), y = FD)) + geom_histogram(stat = "identity") +
  geom_point(aes(x = region, y = exp_df))

all_fds %>% 
  ggplot(aes(x = n_species, y = FD)) + geom_point() +
  theme_classic() + geom_smooth(method = "lm") +
  xlab("Species richness") + ylab("FD")


b_terms <- read_csv("data-processed/b_terms_bef.csv")

all_terms <- left_join(b_terms, species_numbers)

all_fds_b <- left_join(all_fds, all_terms)

library(viridis)
all_fds_b %>% 
  mutate(redundancy = exp_df - FD) %>% 
  ggplot(aes(x = redundancy, y = estimate, color = region)) + geom_point(size = 4) +
  geom_smooth(method = "lm", color = "black") +
  geom_point(size = 4) +
  xlab("Functional redundancy") + ylab("Biodiversity effect (b)") + scale_color_viridis(discrete = TRUE)
ggsave("figures/biodiversity_vs_FD.png")

library(lmodel2)

all_fds_b2 <- all_fds_b %>% 
  mutate(redundancy = exp_df - FD) 

all_fds_b2 %>% 
  lm(estimate ~ redundancy, data = .) %>% 
  summary()


lmodel2(estimate ~ redundancy, data = all_fds_b2, range.y = "interval", range.x = "interval")
