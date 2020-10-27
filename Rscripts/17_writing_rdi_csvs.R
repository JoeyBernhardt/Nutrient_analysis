
library(stringr)
library(purrr)
library(tidyverse)
library(broom)
library(vegan)
library(cowplot)
theme_set(theme_cowplot())

trait_data <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data-processed/n.long_lat3.csv")



function1<- function(percentage, trait_data){
  ntbl.RDI <- trait_data %>% 
  spread(nutrient, concentration) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(mean.CA = mean(ca_mg, na.rm = TRUE),
            mean.ZN = mean(zn_mg, na.rm = TRUE), 
            mean.FE = mean(fe_mg, na.rm = TRUE),
            mean.EPA = mean(epa, na.rm = TRUE),
            mean.DHA = mean(dha, na.rm = TRUE)) %>% 
  mutate(RDI.CA = ifelse(mean.CA > (1200*percentage), 1, 0)) %>% 
  mutate(RDI.FE = ifelse(mean.FE > (18*percentage), 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(mean.ZN > (11*percentage), 1, 0)) %>%
  mutate(RDI.EPA = ifelse(mean.EPA > (1*percentage), 1, 0)) %>% 
  mutate(RDI.DHA = ifelse(mean.DHA > (1*percentage), 1, 0)) %>% 
  ungroup() %>% 
  mutate(RDI.micro.tot = rowSums(.[8:12])) %>% 
  filter(!is.na(RDI.micro.tot)) 
  
  #save file
  write_csv(ntbl.RDI, paste0("./data-processed-rdi/rdi_", percentage, ".csv") )
  
}


function2 <- function(rdi_data, datafilename){
  
all_spa <- rdi_data %>%
  dplyr::select(-RDI.micro.tot) %>%
  dplyr::select(-contains("mean")) %>% 
  dplyr::select(-species_name) %>%
  mutate(subgroup = "all") %>% 
  split( .$subgroup) %>% 
  map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.EPA", "RDI.DHA")) %>%
  map(.f = specaccum, method = "random")

accumulated_targets_full <- all_spa %>% 
  map(.f = `[`, "richness") %>% 
  unlist() %>% 
  as.data.frame()

accumulated_targets_full$richness_level = rownames(accumulated_targets_full)
colnames(accumulated_targets_full) <- c("number_of_targets", "richness_level")

accumulated_targets_full <- accumulated_targets_full %>% 
  separate(richness_level, into = c("subgroup", "number_of_species")) %>%
  mutate(number_of_species = str_replace(number_of_species, "richness", "")) %>%
  mutate(number_of_species = as.numeric(number_of_species)) %>% 
  mutate(threshold = i) %>% 
  dplyr::select(-subgroup)

write_csv(accumulated_targets_full, paste0("./data-processed-targets2/rdi_targets_", datafilename) )

}

##### Run these to get all the files
for(i in seq(0,1, 0.01)){
  
  function1(i, trait_data)
  print(i)
}

rdi_files <- list.files("./data-processed-rdi")
rdi_files

for (i in 1:length(rdi_files)){
  setwd("./data-processed-rdi/")
  data_round2 <- read_csv(rdi_files[i])
  setwd("../")
  function2(data_round2, rdi_files[i])
  
}


target_files <- list.files("./data-processed-targets2", full.names = TRUE)


names(target_files) <- target_files %>% 
  gsub(pattern = ".csv$", replacement = "")


#### Step 3: read in all the files!

all_targets <- map_df(target_files, read_csv, .id = "file_name")


targets <- all_targets %>% 
  mutate(file_name = str_replace(file_name, "./data-processed-targets2/rdi_targets_rdi_", "")) %>%
  mutate(file_name = as.numeric(file_name)) %>% 
  rename(threshold1 = file_name)

write_csv(targets, "data-processed/targets_richness.csv")

targets <- read_csv("data-processed/targets_richness.csv")

targets %>%
  filter(number_of_species < 30) %>% 
  mutate(threshold1 = threshold1*100) %>% 
  group_by(threshold1) %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets, group = threshold1, color = threshold1)) + geom_line() +
  theme_bw() +
  scale_color_gradient(name="Percent of DRI", low="blue", high="red") + xlab("species richness") + ylab("number of functions (distinct DRI targets reached)")
ggsave("figures/nutrient_accumulation_by_threshold.pdf")

targets %>% 
  filter(number_of_species < 11) %>% 
  group_by(threshold1) %>% 
  do(tidy(lm(log(number_of_targets) ~ log(number_of_species), data =.), conf.int = TRUE)) %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = threshold1, y = estimate)) + geom_point() + geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + theme_bw() +
  xlab("threshold (DRI percentage)") + ylab("effect of adding one species to diet (linear coefficient)")
ggsave("figures/effect_of_adding_one_species_by_threshold.pdf")



targets %>%
  filter(number_of_species < 11) %>% 
  mutate(threshold1 = threshold1*100) %>% 
  # filter(threshold1 == 25) %>% 
  ggplot(aes(x = log(number_of_species), y = log(number_of_targets), group = threshold1, color = threshold1)) + geom_line() +
  theme_bw() +
  scale_color_gradient(name="Percent of DRI", low="blue", high="red") + xlab("species richness") + ylab("number of functions (distinct DRI targets reached)")


### ok now need to do the thing where I compare the accumulation curves to the best species at each richness level


#### read in the data
mean_nuts_new <- read_csv("data-processed/mean_nuts_oct-4-2020_micro_macro.csv") %>% 
  filter(grepl(" ", taxize_name))

mean_nuts_new <- read_csv("data-processed/mean_nuts_oct-4-2020.csv") %>% 
  filter(grepl(" ", taxize_name))


#### define the fishing function for all five micronutrients
nutrient_fishing_function <- function(sample_size) {
  ntbl_sub1 <- mean_nuts_new %>% 
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
    summarise_each(funs(sum), contains("total")) %>%  ## sum up all of each of the nutrients
    mutate(cal_grams = (cal_total/(1200))) %>% ## divide that total by the RDA, and into 100 to find out the number of grams required to reach target
    mutate(iron_grams = (iron_total/(18))) %>%
    mutate(zinc_grams = (zinc_total/(11))) %>% 
    mutate(epa_grams = (epa_total/(1))) %>%
    mutate(dha_grams = (dha_total/(1))) %>%
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no, sample_id) %>% 
    dplyr::select(-contains("total")) %>% 
    gather(key = nutrient, value = concentration, contains("grams")) %>% 
    group_by(species_no, sample_id) %>% 
    summarise(min_percentage = min(concentration)) %>% ### this finds, for each combination of species, the nutrient with the minimum concentration
    mutate(grams_required = 100/min_percentage) ### this quantifies the number of grams required to meet 100% of the RDA for nutrient which has the lowest concentration
}


samples_rep <- rep(10, 1000)


output_all5m <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run") %>% 
  mutate(nutrient = "all_5_micronutrients")

### ok this is for 10, now let's write a function to get every level between 0 and 100
library(nlstools)
all_grams_median_nuts <- output_all5m %>% 
  mutate(grams_required = grams_required/10) %>% 
  group_by(nutrient, species_no) %>% 
  summarise_each(funs(mean, median), grams_required) %>% 
  rename(grams_required_median = median)

pmin_function <- function(threshold) {

all_grams_median_nuts <- output_all5m %>% 
  mutate(grams_required = grams_required*threshold) %>% 
  group_by(species_no) %>% 
  summarise_each(funs(mean, median), grams_required) %>% 
  rename(grams_required_median = median) %>% 
  mutate(rda_threshold = threshold)

}

thresholds <- seq(.01, 1, by = 0.01)
thresholds2 <- 100/thresholds

all_pmins <- thresholds %>% 
  map_df(pmin_function)

all_pmins %>% 
  ungroup() %>%
  ggplot(aes(x = species_no, y = grams_required_median, color = rda_threshold, group = rda_threshold)) + geom_line() +
  scale_color_viridis_c()


View(all_grams_median_nuts)

### estimate b_pmin

bpmin_function <- function(threshold) {
all_mod <- nls(formula = (grams_required_median ~ a * species_no^b),data = filter(all_pmins, rda_threshold == 0.1),  start = c(a=500, b=-0.6))
all_boot <- nlsBoot(all_mod)
all_boot_df <- as_data_frame(all_boot$bootCI) %>% 
  mutate(rda_threshold = threshold)
return(all_boot_df)
}

bpmins <- thresholds %>% 
  map_df(bpmin_function)

results <- data.frame()
for (i in seq(.01, 1, by = 0.01)) {
all_mod <- nls(formula = (grams_required_median ~ a * species_no^b),data = filter(all_pmins, rda_threshold == i),  start = c(a=500, b=-0.6))
all_boot <- nlsBoot(all_mod)
all_boot_df <- as_data_frame(all_boot$bootCI) %>% 
  mutate(rda_threshold = i)
results <- bind_rows(results, all_boot_df)

}
View(results)

all_pmins %>% 
  ungroup() %>%
  ggplot(aes(x = species_no, y = grams_required_median, color = rda_threshold, group = rda_threshold)) + geom_line() +
  scale_color_viridis_c()


results %>% 
  filter(Median < 1) %>% 
  mutate(rda_threshold = rda_threshold*100) %>% 
  ggplot(aes(x = rda_threshold, y = Median, color = rda_threshold)) + geom_point() +
  ylim(-1, 0) + scale_color_gradient(name="RDA threshold", low="blue", high="red")



### ok let's estimate NT

targets <- read_csv("data-processed/targets_richness.csv") %>% 
  filter(number_of_species < 10) %>% 
  filter(complete.cases(.))
  
results_nt_thres <- data.frame()
for (i in unique(targets$threshold1)) {
  all_mod <- nls(formula = (number_of_targets ~ a * number_of_species^b),data = filter(targets, threshold1 == i),  start = c(a=1, b=0.5))
  all_boot <- nlsBoot(all_mod)
  all_boot_df <- as_data_frame(all_boot$bootCI) %>% 
    mutate(rda_threshold = i)
  results_nt_thres <- bind_rows(results_nt_thres, all_boot_df)
  
}

results_nt_thres %>% 
  filter(Median < 1) %>% 
  ggplot(aes(x = rda_threshold, y = Median)) + geom_point() +
  geom_smooth() +
  geom_vline(xintercept = 0.10)


results_nt <- targets_split %>% 
  map_df(spline.slope, .id = "threshold")



diversity_nt_plot <- results_nt %>%
  mutate(threshold = as.numeric(threshold)) %>% 
  mutate(threshold = threshold*100) %>%
  group_by(threshold) %>% 
  summarise_each(funs(mean, std.error), diversity_effect) %>% 
  ggplot(aes(x = threshold, y = mean)) + geom_point() + theme_bw() +
  # geom_ribbon(aes(ymin = diversity_effect_mean-diversity_effect_std.error, ymax = diversity_effect_mean + diversity_effect_std.error)) +
  # geom_smooth() + 
  xlab("Threshold (percent of RDA)") + ylab("NT") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text = element_text(size=16, family = "Helvetica")) 



targets %>%
  filter(number_of_species < 30) %>% 
  mutate(threshold1 = threshold1*100) %>% 
  # filter(threshold1 %in% c(25, 50, 75, 100)) %>% 
  group_by(threshold1) %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets, group = threshold1, color = threshold1)) + geom_line() +
  theme_bw() +
  # geom_hline(yintercept = 2, linetype = "dashed") +
  scale_color_gradient(name="Threshold \n(percent of RDA)", low="blue", high="red") + xlab("Species richness") + ylab("Number of functions (distinct DRI targets reached)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text = element_text(size=16, family = "Helvetica")) +
  theme(legend.position = c(0.7, 0.2)) +
  xlim(0, 10)


targets %>%
  filter(number_of_species < 30) %>% 
  mutate(threshold1 = threshold1*100) %>% 
  group_by(threshold1) %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets, group = threshold1, color = threshold1)) + geom_line() +
  theme_bw() +
  scale_color_gradient(name="Percent of DRI", low="blue", high="red") + xlab("species richness") + ylab("number of functions (distinct DRI targets reached)")
