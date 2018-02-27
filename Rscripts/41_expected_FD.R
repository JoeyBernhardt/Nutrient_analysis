library(tidyverse)
library(cowplot)
library(viridis)
library(forcats)
library(plotrix)

trad_nuts_mean <- read_csv("data-processed/trad-foods-mean.csv") %>% 
  filter(!is.na(latin_name))

trad_nuts_mean2 <- read_csv("data-processed/mean_nuts.csv") %>% 
  rename(latin_name = species_name) %>% 
  filter(!is.na(latin_name))

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
  select(global)


species_names <- global %>% 
  rename(species_name = latin_name) %>% 
  select(species_name) %>% 
  arrange(species_name)



pres_abs <- as.data.frame(pres_abs)
rownames(pres_abs) <- species_names$species_name

landfile <- pres_abs

traitfile <- global %>% 
  distinct(latin_name, .keep_all = TRUE) %>% 
  select(-culture, -subgroup) %>% 
  filter(latin_name %in% species_names$species_name) %>% 
  arrange(latin_name) %>% 
  select(-latin_name) %>% 
  as.data.frame()

rownames(traitfile) <- species_names$species_name
FDs <- Calculate.FD(landfile = landfile, traitfile = traitfile, scale = F, writefiles = F)
fd3 <- data.frame(FD = FDs, region = names(FDs), sample_size = sample_size)
return(fd3)
}


sample_sizes <- rep(species_numbers$n_species, 1000)


exp_df2 <- sample_sizes %>% 
  map_df(repeat_fd, .id = "sample")

exp_df_mean_nuts <- exp_df

write_csv(exp_df, "data-processed/expected_FD_regional.csv")

exp_df <- read_csv("data-processed/expected_FD_regional.csv")
expected_fds <- exp_df %>% 
  group_by(sample_size) %>% 
  summarise(exp_df = mean(FD))

expected_fds2 <- exp_df2 %>% 
  group_by(sample_size) %>% 
  summarise(exp_df = mean(FD))

expected_fds_mean_nuts <- exp_df_mean_nuts %>% 
  group_by(sample_size) %>% 
  summarise(exp_df_nuts = mean(FD))

left_join(expected_fds_mean_nuts, expected_fds) %>% View
left_join(expected_fds_mean_nuts, expected_fds2) %>% View

observed_fd <- read_csv("data-processed/regional_functional_diversity.csv")


global_fd <- read_csv("data-processed/global_FD_repeat.csv") %>% 
  mutate(region = "global_resampled") %>% 
  mutate(n_species = 40) %>% 
  group_by(n_species, region) %>% 
  summarise(FD = mean(FD))

read_csv("data-processed/global_FD_repeat.csv") %>% 
  mutate(region = "global_resampled") %>% 
  mutate(n_species = 40) %>% 
  group_by(n_species, region) %>% 
  summarise_each(funs(mean, std.error), FD) %>% View ### this is to get the average global FD

all_observed_fd <- bind_rows(observed_fd, global_fd)

all_fds <- left_join(all_observed_fd, expected_fds, by = c("n_species" = "sample_size")) %>% 
  filter(region != "global")

all_fds %>% 
  filter(region != "global_resampled") %>% 
  summarise_each(funs(mean, std.error), FD) %>% View

all_fds %>% 
  rename(Region = region) %>% 
  mutate(Region = ifelse(Region == "global_resampled", "Global (40 species)", Region)) %>% 
  ggplot(aes(x = exp_df, y = FD, color = fct_reorder2(Region, exp_df, FD))) + geom_point(size = 4) +
  geom_point(size = 4, shape = 1, color = "black") +
  geom_abline(slope = 1, intercept = 0) +
  theme_classic() +
ylim(1.5, 4) + xlim(1.5, 4) +
  xlab("Expected FD") + ylab("Observed FD") + scale_color_viridis(discrete = TRUE, name = "Region")
ggsave("figures/expected_vs_observed_FD.pdf", width = 7, height = 5)

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
