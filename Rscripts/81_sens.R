
mean_nuts <- read_csv("data-processed/mean_nuts_oct-4-2020.csv") %>% 
  filter(grepl(" ", taxize_name))
mean_nuts$culture <- "global"

mean_nuts %>% 
  gather(key = nutrient, value = concentration, 3:7) %>%
  summarise(mean_conc = mean(concentration)) %>% View


threshold <- 0.5
sample_size <- 10
nutrient_fishing_function_global <- function(threshold) {
  ntbl_sub1 <- mean_nuts %>% 
    # sample_n(size = 40, replace = FALSE) %>% ### this first step samples 40 from the global pool, but removing this in the revision, since it's not necessary
    dplyr:: sample_n(size = 10, replace = FALSE) ## this is going to take out 10 species from the species at random
  
  
  ### this step takes the 10 species and the creates all possible subsets at each level of diversity, from 1-10
  sample_list <- NULL
  for (i in 1:nrow(ntbl_sub1) ) {
    output <- combn(nrow(ntbl_sub1), i, FUN=function(x) ntbl_sub1[x,], simplify = FALSE)
    output <- bind_rows(output, .id = "sample_id")
    subsample_size <- rep(i, nrow(output))
    output <- cbind(output, subsample_size)
    sample_list <- rbind(sample_list,output)
  }
  
  ## this step splits up all those combinations
  sample_list <- split(sample_list, f = sample_list$subsample_size)
  
  
  new_data_sub1 <- sample_list %>% 
    map_df(`[`, .id = "replicate")
  
  ### this step now calculates the number of RDA targets met, assuming each species contributes to each diet with a replacement design
  resampling_15 <- new_data_sub1 %>% 
    dplyr::rename(species_number = subsample_size) %>% ### species_number is the diversity level
    group_by(species_number, sample_id) %>% 
    mutate(cal_total = (calcium/species_number)) %>% ## get the amount of calcium each species will contribute
    mutate(zinc_total = (zinc/species_number)) %>% 
    mutate(iron_total = (iron/species_number)) %>% 
    mutate(epa_total = (epa/species_number)) %>%
    mutate(dha_total = (dha/species_number)) %>% 
    summarise_each(funs(sum), contains("total")) %>%   ## sum up all of each of the nutrients, to get total amount of nutrient per sample
    mutate(cal_grams = (cal_total/(1200*threshold))) %>% ## divide that total by the RDA
    mutate(iron_grams = (iron_total/(18*threshold))) %>%
    mutate(zinc_grams = (zinc_total/(11*threshold))) %>% 
    mutate(epa_grams = (epa_total/(1*threshold))) %>%
    mutate(dha_grams = (dha_total/(1*threshold))) %>%
    mutate(rdi_calcium = ifelse(cal_grams >= 1, 1, 0)) %>% ## if the amount of calcium exceeds 1 (i.e. reaches threshold), give it a value of 1, else 0
    mutate(rdi_iron = ifelse(iron_grams >= 1, 1, 0)) %>% 
    mutate(rdi_zinc = ifelse(zinc_grams >= 1, 1, 0)) %>% 
    mutate(rdi_epa = ifelse(epa_grams >= 1, 1, 0)) %>%
    mutate(rdi_dha = ifelse(dha_grams >= 1, 1, 0)) %>% 
    ungroup() %>% 
    mutate(rdi_micro_tot = rowSums(.[13:17])) %>%  ## add up all the targets reached in one sample
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no, sample_id) %>% 
    dplyr::select(-contains("total")) %>% 
    mutate(threshold_level = threshold)
}

#### Now define the number of species to be sampled and the number of replicate samplings (here we are sampling 10 species, and re-doing this 1000 times)
samples_rep <- rep(10, 10)

threshold <- 0.9 ## this is the RDA threshold we are using, here 10%

#### apply the fishing function to the global dataset
j <- 0.1
x <- rep(seq(.01, 1, by = 0.01), 1000)
results2 <- data.frame()
  for(j in 1:length(x)){
      global <- nutrient_fishing_function_global(threshold = x[j]) %>% 
      group_by(species_no) %>% 
      summarise(mean_nt = mean(rdi_micro_tot))

    
    # GL_mod <- nls(formula = (mean_nt ~ a * species_no^b), data = global,  start = c(a=2, b=0.5))
    b_parm <- try(coef(nls(formula = (mean_nt ~ a * species_no^b), data = global,  start = c(a=2, b=0.5))), TRUE)
    if(isTRUE(class(b_parm)=="try-error")) { next } else { hold <- data.frame(a = b_parm[1], b = b_parm[2], threshold = x[j])
    results2 <- bind_rows(results2, hold) } }
    
  
results2 %>% 
  group_by(threshold) %>% 
  # filter(b>0) %>%
  summarise(mean_b = mean(b),
            max_b = max(b),
            median_b = median(b)) %>% 
  filter(threshold < 0.5) %>%
  ggplot(aes(x = threshold, y = median_b)) + geom_point() +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0.1) + xlim(0, .5) +
  geom_hline(yintercept = 0.2)

write_csv(results2, "data-processed/nt-sensitivity-thresholds.csv")
results2 <- read_csv("data-processed/nt-sensitivity-thresholds.csv")

prediction_function <- function(df) {
  pf <-function(x){
    res<-(df$a[[1]]*x^df$b[[1]])
    res
  }
  
  pred <- function(x) {
    y <- pf(x)
  }
  
  x <- seq(1, 10, by = 0.5)
  
  preds <- sapply(x, pred)
  preds <- data.frame(x, preds) %>% 
    rename(species_no = x, 
           DRI_targets = preds)
}


CS_preds <- results2 %>% 
  filter(threshold < 0.5) %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") 

results3 <- results2 %>% 
  filter(threshold < 0.5) %>% 
  mutate(replicate = rownames(.))

cs2 <- CS_preds %>% 
  left_join(., results3)

cs2 %>% 
  # filter(b < 0) %>% 
  ggplot(aes(x = species_no, y = DRI_targets, group = replicate, color = threshold)) + geom_line(alpha = 0.1) +
  scale_color_viridis_c()


global_0.9 <- samples_rep %>% 
  map_df(nutrient_fishing_function_global, .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

GL_mod <- nls(formula = (mean ~ a * species_no^b), data = global_0.9,  start = c(a=2, b=0.5))
GL_boot <- nlsBoot(GL_mod)
GL_boot$bootCI




# now for Pmin ------------------------------------------------------------
all_pmins <- read_csv("data-processed/pmin-sensitivity-threshold.csv")

all_pmins %>% 
  ungroup() %>%
  ggplot(aes(x = species_no, y = grams_required_median, color = rda_threshold, group = rda_threshold)) + geom_line() +
  scale_color_viridis_c()


results_pmin <- data.frame()
for (i in seq(.01, 1, by = 0.01)) {
  all_mod <- nls(formula = (grams_required_median ~ a * species_no^b),data = filter(all_pmins, rda_threshold == i),  start = c(a=500, b=-0.6))
  all_boot <- nlsBoot(all_mod)
  all_boot_df <- as_data_frame(all_boot$bootCI) %>% 
    mutate(rda_threshold = i)
  results_pmin <- bind_rows(results_pmin, all_boot_df)
  
}


pmin_prediction_function <- function(df) {
  pf <-function(x){
    res<-(df$a[[1]]*x^df$b[[1]])
    res
  }
  
  pred <- function(x) {
    y <- pf(x)
  }
  
  x <- seq(1, 10, by = 0.5)
  
  preds <- sapply(x, pred)
  preds <- data.frame(x, preds) %>% 
    rename(species_no = x, 
           DRI_targets = preds)
}


all_pmins_calcium <- read_csv("data-processed/pmin-calcium-sensitivity-threshold.csv")
results_pmin_calcium <- read_csv("data-processed/pmin-calcium-b-params.csv")

calcium_pmin_preds <- results_pmin_calcium %>%
  dplyr::select(rda_threshold, Median) %>% 
  mutate(parameter = ifelse(Median < 1, "b", "a")) %>% 
  spread(key = parameter, value = Median) %>% 
  split(.$rda_threshold) %>% 
  map_df(pmin_prediction_function, .id = "threshold") 


pmin_preds <- results_pmin %>%
 dplyr::select(rda_threshold, Median) %>% 
  mutate(parameter = ifelse(Median < 1, "b", "a")) %>% 
  spread(key = parameter, value = Median) %>% 
  split(.$rda_threshold) %>% 
  map_df(pmin_prediction_function, .id = "threshold") 


cal_pmin_plot <- calcium_pmin_preds %>% 
  mutate(threshold = as.numeric(threshold)) %>% 
  filter(threshold < 0.5) %>% 
  mutate(threshold = threshold*100) %>% 
  ggplot(aes(x = species_no, y = DRI_targets, color = threshold, group = threshold)) + geom_line() +
  scale_color_gradient(name="RDA threshold (%)", low="blue", high="red") +
  xlab("Species richness") + labs(y=expression(paste(italic(Pmin[calcium])))) + 
  scale_x_continuous(breaks = c(1:10)) + ggtitle("A")

cal_bpmin_plot <- results_pmin_calcium %>% 
  filter(Median < 1) %>% 
  mutate(rda_threshold = rda_threshold*100) %>% 
  filter(rda_threshold< 50) %>% 
  ggplot(aes(x = rda_threshold, y = Median, color = rda_threshold)) + geom_point() +
  ylim(-1, 0) + 
  scale_color_gradient(name="RDA threshold (%)", low="blue", high="red") +
  labs(y=expression(paste(italic(b[Pmin[calcium]])))) + xlab("RDA threshold (%)") + ggtitle("B")


pmin_plot <- pmin_preds %>% 
  mutate(threshold = as.numeric(threshold)) %>% 
  filter(threshold < 0.5) %>% 
  mutate(threshold = threshold*100) %>% 
  ggplot(aes(x = species_no, y = DRI_targets, color = threshold, group = threshold)) + geom_line() +
  scale_color_gradient(name="RDA threshold (%)", low="blue", high="red") +
  xlab("Species richness") + labs(y=expression(paste(italic(Pmin)))) + 
  scale_x_continuous(breaks = c(1:10)) + ggtitle("C")

bpmin_plot <- results_pmin %>% 
  filter(Median < 1) %>% 
  mutate(rda_threshold = rda_threshold*100) %>% 
  filter(rda_threshold< 50) %>% 
  ggplot(aes(x = rda_threshold, y = Median, color = rda_threshold)) + geom_point() +
  ylim(-1, 0) + scale_color_gradient(name="RDA threshold (%)", low="blue", high="red") +
  labs(y=expression(paste(italic(b[Pmin])))) + xlab("RDA threshold (%)") + ggtitle("D")

nt_plot <- cs2 %>% 
  mutate(threshold = threshold*100) %>% 
  group_by(threshold, species_no) %>%
  top_n(n = 5, replicate) %>% 
  # filter(b < 0) %>% 
  ggplot(aes(x = species_no, y = DRI_targets, group = replicate, color = threshold)) + geom_line(alpha = 1) +
  scale_color_gradient(name="RDA threshold (%)", low="blue", high="red") +
  labs(y=expression(paste(italic(NT))))+ xlab("Species richness") +
  scale_x_continuous(breaks = c(1:10)) + ggtitle("E")

bnt_plot <- results2 %>% 
  group_by(threshold) %>% 
  summarise(mean_b = mean(b),
            max_b = max(b),
            median_b = median(b)) %>% 
  mutate(threshold = threshold*100) %>% 
  filter(threshold < 50) %>% 
  ggplot(aes(x = threshold, y = median_b, color = threshold)) + geom_point() +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 10) +
  scale_color_gradient(name="RDA threshold (%)", low="blue", high="red") +
  labs(y=expression(paste(italic(b[NT])))) +xlab("RDA threshold (%)") + ggtitle("F")


library(patchwork)
sens_plot <- (cal_pmin_plot + pmin_plot + nt_plot) / (cal_bpmin_plot + bpmin_plot + bnt_plot)
ggplot2::ggsave(plot = sens_plot, filename = "figures/sensitivity_plot_single", device = "pdf", width =15, height = 7)

