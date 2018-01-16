library(ggplot2)
library(patchwork)
library(tidyverse)
library(purrr)
library(here)
library(stringr)
library(cowplot)
library(broom)
library(vegan)



new_global <- read_csv(here("data-processed", "new_global.csv"))




accumulate_global <- function(data, threshold) {
  ntbl.RDI.all <- data %>% 
    mutate(RDI.CA = ifelse(calcium > (1200*threshold), 1, 0)) %>% ## here we create a matrix of 0s and 1s, corresponding to whether the sample reaches DRI or not
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



rep_acc_function <- function(x) accumulate_global(sample_n(new_global, size = x, replace = FALSE), threshold = 0.1)


samples <- rep(40, 10000)


repeat_global <- samples %>% 
  map_df(rep_acc_function, .id = "run")

write_csv(repeat_global, "data-processed/global_accumulation_null.csv")
repeat_global <- read_csv("data-processed/global_accumulation_null.csv")


mean_target <- repeat_global %>% 
  group_by(number_of_species) %>% 
  summarise_each(funs(mean), number_of_targets) %>% 
  filter(number_of_species < 11) 


sub <- repeat_global %>%
  filter(number_of_species < 11) %>%
  mutate(culture = "global")
  

mean_target %>% 
 ggplot(aes(x = number_of_species, y = number_of_targets), color = "red") + geom_line() +
  geom_line(aes(x = number_of_species, y = number_of_targets, group = run), data = sub, alpha = 0.1) +
  geom_line(aes(x = number_of_species, y = number_of_targets), data = mean_target, color = "cadetblue", size = 1) +
    ylim(0, 5)
  

res <- read_csv(here("data-processed", "nut_accumulation_trad_foods.csv"))

res_all <- bind_rows(sub, res)

write_csv(res_all, "data-processed/res_all.csv")

species_numbers <- nuts_mean %>% ## get a list of communities for which we have at least 25 species
  group_by(culture) %>% 
  summarise(n_species = n_distinct(latin_name)) %>% 
  filter(n_species >= 25) 

global_mean  <- sub %>% 
  group_by(number_of_species) %>% 
  summarise(mean_targets = mean(number_of_targets),
            low=quantile(number_of_targets, probs=0.025),
            high=quantile(number_of_targets, probs=0.975))


res_sel <- res_all %>% 
  filter(culture %in% species_numbers$culture | culture == "global") %>% 
  filter(number_of_species < 11)

seq(1:10)
run <- data.frame(run = rep(1:1014, 10)) %>% 
  arrange(run)

res_sel2 <- bind_cols(res_sel, run) %>% 
  filter(culture != "global")

table(res_sel2$culture)


p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) 
fig3c <- res_sel2 %>% 
  ggplot() +
  geom_ribbon(aes(x = number_of_species, ymin = number_of_targets - se, ymax = number_of_targets + se, group = culture), alpha = 0.15, size = 0) +
  geom_line(aes(x = number_of_species, y = number_of_targets, group = culture)) +
  geom_ribbon(aes(x = number_of_species, ymin = low, ymax = high), alpha = 0.5, size = 0, fill = "cadetblue", data = global_mean) +
  geom_line(aes(x = number_of_species, y = mean_targets), data = global_mean, color = "cadetblue", size = 1.5) +
  ylab("Number of nutrient requirements fulfilled (10% DRI)") +
  xlab("Species richness") + theme(text = element_text(size=14)) + 
  theme(legend.title=element_blank()) + theme_classic() +
  ylim(0,5) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  theme(text=element_text(family="Helvetica", size=14)) 




