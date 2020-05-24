

#### BEF slope calculations



dats <- data.frame(trait_log_norm = rlnorm(n = 100, meanlog = log(100), sdlog = log(9)), trait_norm = rnorm(n = 100, mean = 100, sd = 15))

sd(dats$trait_log_norm)
mean(dats$trait_log_norm)
sd(dats$trait_norm)
sd(dats$trait_log_norm)


dats %>% 
  gather(key = dist_type, value = trait) %>% 
  ggplot(aes(x = trait, fill = dist_type)) + geom_histogram(bins = 40) +
  facet_wrap( ~ dist_type, nrow = 2, ncol = 1, scales = 'free') 


nutrient_fishing_function_log_norm <- function(sample_size) {
  ntbl_sub1 <- dats %>% 
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
    mutate(hg_total = (trait_log_norm/species_number)) %>% ## get the amount of calcium each species will contribute
    summarise_each(funs(sum), contains("total")) %>%  ## sum up all of each of the nutrients
    mutate(hg_grams = (hg_total/(1))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no, sample_id) %>% 
    select(-contains("total")) %>% 
    gather(key = nutrient, value = concentration, contains("grams"))
}


samples_rep <- rep(10, 500)
samples_rep[1]


log_norm_sampling <- samples_rep %>% 
  map_df(nutrient_fishing_function_log_norm, .id = "run") 

log_norm_sampling$dataset <- "log_norm"
log_norm_res <- log_norm_sampling %>% 
  group_by(dataset, species_no) %>% 
  summarise(grams_median = median(concentration))

lm(log(grams_median) ~ log(species_no), data = log_norm_res) %>% summary()

log_norm_mod <- nls(formula = (grams_median ~ a * species_no^b), data = log_norm_res,  start = c(a=1, b=0.5))
log_norm_boot <- nlsBoot(log_norm_mod)
log_norm_boot$bootCI
log_norm_boot_df <- as_data_frame(log_norm_boot$coefboot) 
log_norm_b <- as_data_frame(log_norm_boot$bootCI) 
log_norm_b$culture <- "log_norm"

log_norm_preds <- log_norm_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required)) %>% 
  mutate(dataset = "GL")

log_norm_res %>% 
  ggplot(aes(x = species_no, y = grams_median)) + geom_point() +
  geom_line(aes(x = species_no, y = mean), data = log_norm_preds) + 
  # geom_hline(yintercept = 7) + 
  # ylim(0, 30) +
  # geom_hline(yintercept =  karimi_sum$median[1]) + 
  geom_line(aes(x = species_no, y = q2.5), data = log_norm_preds, color = "grey") +
  geom_line(aes(x = species_no, y = q97.5), data = log_norm_preds, color = "grey") +
  ylab("Nutrient concentration (ug/100g)") + xlab("Species richness") +
  ggtitle("b = 0.068")


#### norm

nutrient_fishing_function_norm <- function(sample_size) {
  ntbl_sub1 <- dats %>% 
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
    mutate(hg_total = (trait_norm/species_number)) %>% ## get the amount of calcium each species will contribute
    summarise_each(funs(sum), contains("total")) %>%  ## sum up all of each of the nutrients
    mutate(hg_grams = (hg_total/(1))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no, sample_id) %>% 
    select(-contains("total")) %>% 
    gather(key = nutrient, value = concentration, contains("grams"))
}


## at sd = 4, this was negative

norm_sampling <- samples_rep %>% 
  map_df(nutrient_fishing_function_norm, .id = "run") 

norm_sampling$dataset <- "norm"
norm_res <- norm_sampling %>% 
  group_by(dataset, species_no) %>% 
  summarise(grams_median = median(concentration))

lm(log(grams_median) ~ log(species_no), data = norm_res) %>% summary()

norm_mod <- nls(formula = (grams_median ~ a * species_no^b), data = norm_res,  start = c(a=100, b=0.5))
norm_boot <- nlsBoot(norm_mod)
norm_boot$bootCI
norm_boot_df <- as_data_frame(norm_boot$coefboot) 
norm_b <- as_data_frame(norm_boot$bootCI) 
norm_b$culture <- "norm"

norm_preds <- norm_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required)) %>% 
  mutate(dataset = "norm")

 norm_res %>% 
  ggplot(aes(x = species_no, y = grams_median)) + geom_point() +
  geom_line(aes(x = species_no, y = mean), data = norm_preds) + 
  geom_line(aes(x = species_no, y = q2.5), data = norm_preds, color = "grey") +
  geom_line(aes(x = species_no, y = q97.5), data = norm_preds, color = "grey") +
  ylab("Nutrient concentration (ug/100g)") + xlab("Species richness") 


 
 ### all together
norm_res %>% 
  ggplot(aes(x = species_no, y = grams_median)) + geom_point() +
  geom_line(aes(x = species_no, y = mean), data = norm_preds) + 
  geom_point(aes(x = species_no, y = grams_median), data = log_norm_res, color = "pink") +
  geom_line(aes(x = species_no, y = mean), data = log_norm_preds, color = "pink") + 
  geom_line(aes(x = species_no, y = q2.5), data = log_norm_preds, color = "pink") +
  geom_line(aes(x = species_no, y = q97.5), data = log_norm_preds, color = "pink") +
  geom_line(aes(x = species_no, y = q2.5), data = norm_preds, color = "grey") +
  geom_line(aes(x = species_no, y = q97.5), data = norm_preds, color = "grey") +
  ylab("Nutrient concentration (ug/100g)") + xlab("Species richness") 



