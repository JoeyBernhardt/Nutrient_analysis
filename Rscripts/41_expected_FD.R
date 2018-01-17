library(tidyverse)

trad_nuts_mean <- read_csv("data-processed/trad-foods-mean.csv") %>% 
  filter(!is.na(latin_name))


repeat_fd <- function(sample_size){

global <- sample_n(distinct(trad_nuts_mean, latin_name, .keep_all = TRUE), size = sample_size, replace = FALSE) %>% 
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


exp_df <- sample_sizes %>% 
  map_df(repeat_fd, .id = "sample")

write_csv(exp_df, "data-processed/expected_FD_regional.csv")

exp_df <- read_csv("data-processed/expected_FD_regional.csv")
expected_fds <- exp_df %>% 
  group_by(sample_size) %>% 
  summarise(exp_df = mean(FD))


observed_fd <- read_csv("data-processed/regional_functional_diversity.csv")


all_fds <- left_join(observed_fd, expected_fds, by = c("n_species" = "sample_size"))


all_fds %>% 
  ggplot(aes(x = exp_df, y = FD, color = region)) + geom_point(size = 4) +
  geom_abline(slope = 1, intercept = 0) +
  theme_classic() +ylim(2.5, 4) + xlim(2.5, 4) +
  xlab("Expected FD") + ylab("Observed FD")

all_fds %>% 
  ggplot(aes(x = n_species, y = FD)) + geom_point() +
  theme_classic() + geom_smooth(method = "lm") +
  xlab("Species richness") + ylab("FD")


b_terms <- read_csv("data-processed/b_terms_bef.csv")

all_terms <- left_join(b_terms, species_numbers)

all_fds_b <- left_join(all_fds, all_terms)

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
