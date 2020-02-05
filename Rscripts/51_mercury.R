

library(tidyverse)
library(janitor)
library(readxl)
library(cowplot)
theme_set(theme_cowplot())


anf_raw <- read_excel("data/AnFooD1.1.xlsx", sheet = "09 Fish & Shellfish")
ufish_raw <- read_excel("data/uFiSH1.0.xlsx", sheet = "10 Reftbl_RefDatasets") %>% 
  clean_names() %>%
  select(contains("hg")) %>% View

anf2 <- anf_raw %>% 
  clean_names() %>% 
  select(contains("hg")) %>% View



# Read in the Karimi data -------------------------------------------------

karimi <- read_excel("data/resourceMap_knb_295_2/data/rkarimi.7.1-Seafood_Hg_Database.data.xls", sheet = "Seafood Hg Database") %>% 
  clean_names() %>% 
  rename(ug_hg = mean_hg_concentration_ppm_wet_weight) %>% 
  mutate(ug_hg = ug_hg*100)

names(karimi)


karimi_mean <- karimi %>% 
  group_by(taxon_common_name) %>% 
  summarise(mean_ug = mean(ug_hg)) 

karimi_sum <- karimi_mean %>% 
  summarise_each(funs(mean, median), mean_ug)

karimi_mean %>% 
   ggplot(aes(x = mean_ug)) + geom_histogram() +
  geom_vline(xintercept = 7) + scale_x_log10() + geom_vline(xintercept = karimi_sum$mean[1], color = "purple") +
  geom_vline(xintercept = karimi_sum$median[1], color = "cadetblue") +
  xlab("Mercury concentration (ug/100g)")
ggsave("figures/mercury-histogram-karimi.png", width = 6, height = 4)

# nutrient fishing function -----------------------------------------------

sample_size <- 10

nutrient_fishing_function <- function(sample_size) {
  ntbl_sub1 <- karimi_mean %>% 
    sample_n(size = 40, replace = FALSE) %>%
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
    mutate(hg_total = (mean_ug/species_number)) %>% ## get the amount of calcium each species will contribute
    summarise_each(funs(sum), contains("total")) %>%  ## sum up all of each of the nutrients
    mutate(hg_grams = (hg_total/(1))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no, sample_id) %>% 
    select(-contains("total")) %>% 
    gather(key = nutrient, value = concentration, contains("grams"))
  # %>% 
  #   group_by(species_no, sample_id) %>% 
  #   summarise(max_percentage = max(concentration)) %>%
  #   mutate(grams_required = 100/max_percentage)
}


samples_rep <- rep(10, 100)
samples_rep[1]

global_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run") 

global_10$dataset <- "GL"


write_csv(global_10, "data-processed/global_10_40spp_mercury_efficiency.csv")


global_10_2 <- global_10 %>% 
  group_by(dataset, species_no) %>% 
  summarise(grams_median = median(concentration))

global_10_2 %>% 
  ggplot(aes(x = species_no, y = grams_median)) + geom_point() +
  geom_hline(yintercept = 7) + ylim(0, 30)

global_10 %>% 
  ggplot(aes(x = species_no, y = concentration, group = species_no)) + geom_violin() + 
  geom_point(aes(x = species_no, y = grams_median), data = global_10_2) +
  geom_hline(yintercept = 7)

library(nlstools)
library(broom)

lm(log(grams_median) ~ log(species_no), data = global_10_2) %>%
  tidy(., conf.int = TRUE)

global_10_2 %>% 
  ggplot(aes(x = log(species_no), y = log(grams_median))) + geom_point() +
  geom_smooth(method = "lm")


GL_mod <- nls(formula = (grams_median ~ a * species_no^b), data = global_10_2,  start = c(a=1, b=0.5))
GL_boot <- nlsBoot(GL_mod)
GL_boot$bootCI
GL_boot_df <- as_data_frame(GL_boot$coefboot) 
GL_b <- as_data_frame(GL_boot$bootCI) 
GL_b$culture <- "GL"


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

#15
GL_preds <- GL_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required)) %>% 
  mutate(dataset = "GL")

GL_preds %>% 
  ggplot(aes(x = species_no, y = mean)) + geom_line()

global_10_2 %>% 
  ggplot(aes(x = species_no, y = grams_median)) + geom_point() +
  geom_line(aes(x = species_no, y = mean), data = GL_preds) + 
  geom_hline(yintercept = 7) + ylim(0, 30) +
  # geom_hline(yintercept =  karimi_sum$median[1]) + 
  geom_line(aes(x = species_no, y = q2.5), data = GL_preds, color = "grey") +
  geom_line(aes(x = species_no, y = q97.5), data = GL_preds, color = "grey") +
  ylab("Mercury concentration (ug/100g)") + xlab("Species richness")
ggsave("figures/mercury-accumulation.png", width = 6, height= 4)


