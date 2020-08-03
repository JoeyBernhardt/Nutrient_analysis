
all_mean_nuts_traits_all <- read_csv("data-processed/all_mean_nuts_traits_all.csv")
library(FD)

fdis_function <- function(df){
  dbFD(as.matrix(df[[1]][, 2:5]))$FDis[[1]][[1]]
}

threshold <- 0.1
sample_size <- 10
results_mac <- data.frame()
for (i in 1:1000) {
  ntbl_sub1 <- all_mean_nuts_traits_all %>% 
    sample_n(size = 40, replace = FALSE) %>%
    dplyr:: sample_n(size = sample_size, replace = FALSE)
  
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
    map_df(`[`, .id = "replicate") %>% 
    mutate(unique_sample_id = paste(sample_id, subsample_size, sep = "_"))
  
  splitnt <- new_data_sub1 %>% 
    filter(subsample_size > 9) %>% 
    dplyr::select(unique_sample_id, DemersPelag, bulk_trophic_level, Length, feeding_mode) %>% 
    split(.$unique_sample_id) 
  
  res <- vector()
  for (i in 1:length(splitnt)){
    res[i] <-  fdis_function(splitnt[i])
  }
  
resampling_15 <- new_data_sub1 %>% 
    filter(subsample_size > 9) %>% 
    dplyr::rename(species_number = subsample_size) %>%
    group_by(species_number, sample_id) %>% 
    mutate(cal_total = (calcium/species_number)) %>% ## get the amount of calcium each species will contribute
    mutate(zinc_total = (zinc/species_number)) %>% 
    mutate(iron_total = (iron/species_number)) %>% 
    mutate(epa_total = (epa/species_number)) %>%
    mutate(dha_total = (dha/species_number)) %>% 
    summarise_each(funs(sum), contains("total")) %>%   ## sum up all of each of the nutrients, to get total amount of nutrient per sample
    mutate(cal_grams = (cal_total/(1200*threshold))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
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
  
  hold <-  data.frame(fdis = res, sample = names(splitnt), nt = resampling_15$rdi_micro_tot, s = resampling_15$species_no) 
  results_mac <- bind_rows(results_mac, hold)
}

write_csv(results_mac, "data-processed/fdis-nt-908-10-species.csv")
results_mac %>% 
  ungroup() %>% 
  # mutate(fdis_round = round(fdis, digits = 4)) %>% 
  # group_by(fdis_round) %>% 
  # summarise_each(funs(mean), nt) %>% 
  ggplot(aes(x = fdis, y = nt)) + 
  geom_point() +
  geom_smooth(method = "lm", color = "black")

cor(results_mac$fdis, results_mac$nt)
library(MASS)

rm2 <- results_mac %>% 
  mutate(nt = as.factor(nt))
m <- polr(nt ~ fdis, data= rm2)
summary(m)
(ctable <- coef(summary(m)))

## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ci <- confint(m))
## combined table
(ctable <- cbind(ctable, "p value" = p))
exp(coef(m))
exp(cbind(OR = coef(m), ci))

newdat <- data.frame(
  fdis = seq(from = 0, to = 1, length.out = 100))
newdat <- cbind(newdat, predict(m, newdat, type = "probs")) %>% 
  gather(key = targets, value = probability, 2:6)

library(cowplot)
theme_set(theme_cowplot())
newdat %>% 
  ggplot(aes(x = fdis, y = probability, color = targets)) + geom_line(size = 1.5) +
  xlab("Ecological functional diversity") + ylab("Probability") +
  scale_color_viridis_d(option = "inferno", begin = 0.1, end = 0.8, name = "NT") +
  theme(legend.position = c(0.07, 0.95), legend.direction = "horizontal") +
  xlim(0.22, 1)


# ok now pmin -------------------------------------------------------------


fdis_function2 <- function(df){
  df2 <- as.matrix(df[, 8:11])
  rownames(df2) <- df$species_name
  res <- dbFD(df2)$FDis[[1]][[1]]
  return(res)
}   
fdis <- function(df){
  # cn1 <- data.matrix(df[, c("DemersPelag", "Length", "EnvTemp", "feeding_level")])
  cn1 <- data.matrix(df[, c("DemersPelag", "Length", "bulk_trophic_level", "feeding_mode")])
  rownames(cn1) <- df$species1
  results <- data.frame(fdis = dbFD(cn1)$FDis[[1]][[1]])
}


all_mean2 <- all_mean_nuts_traits_all %>% 
  distinct(species_name, .keep_all = TRUE) %>% 
  ungroup()
length(unique(all_mean2$species_name))

j <- 1
sample_size <- 10
results3 <- data.frame()
for (j in 1:1000) {
  ntbl_sub1 <- all_mean2 %>% 
    sample_n(size = sample_size, replace = FALSE)
  
  
  
  resampling_15 <- ntbl_sub1 %>% 
    mutate(species_number = sample_size) %>% 
    # mutate(unique_sample = paste(species_number, sample_id, sep = "_")) %>% 
    group_by(species_number) %>% 
    mutate(cal_total = (calcium/species_number)) %>% ## get the amount of calcium each species will contribute
    mutate(zinc_total = (zinc/species_number)) %>% 
    mutate(iron_total = (iron/species_number)) %>% 
    mutate(epa_total = (epa/species_number)) %>% 
    mutate(dha_total = (dha/species_number)) %>%
    summarise_each(funs(sum), contains("total")) %>%  ## sum up all of each of the nutrients
    mutate(cal_grams = (cal_total/(1200/10))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
    mutate(iron_grams = (iron_total/(18/10))) %>%
    mutate(zinc_grams = (zinc_total/(11/10))) %>% 
    mutate(epa_grams = (epa_total/(1/10))) %>%
    mutate(dha_grams = (dha_total/(1/10))) %>%
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no) %>% 
    dplyr::select(-contains("total")) %>% 
    gather(key = nutrient, value = concentration, contains("grams")) %>% 
    group_by(species_no) %>% 
    summarise(min_percentage = min(concentration)) %>%
    mutate(grams_required = 100/min_percentage) %>% 
    mutate(run = j)
 
  
  fdis <- fdis_function2(ntbl_sub1)
hold <- data.frame(fdis = fdis, pmin = resampling_15$grams_required, run = resampling_15$run)
  results3 <- bind_rows(results3, hold)
  
}

results3 %>%
  # mutate(fdis_round = round(fdis, digits = 4)) %>% 
  # group_by(fdis_round) %>% 
  # summarise_each(funs(median, mean), pmin) %>% 
  ggplot(aes(x = fdis, y = pmin)) + geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm") +
  geom_abline(slope = -3035.5316, intercept = 1918.90)
  

library(lmodel2)
m1 <- lmodel2(pmin ~ fdis, data = results3)



# Plot the pmin stuff -----------------------------------------------------

# smatr bootstrap example

# load packages
library(smatr)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

# load data
data(leaflife)

leaflife <- results3 %>% 
  rename(log_longev = pmin) %>% 
  rename(log_lma = fdis)
# # make columns log scale
# leaflife <- mutate(leaflife, log_longev = log10(log_longev),
#                    log_lma = log10(log_lma))

# fit sma
mod <- sma(log_longev ~ log_lma, data=leaflife, method="SMA")
summary(mod)
# plot model
plot(mod)
mod$r2

# create new data set of log_lma at a high resolution (200 points from min to max)
preds <- data.frame(expand.grid(log_lma = seq(min(leaflife$log_lma, na.rm = T), max(leaflife$log_lma, na.rm = T), length.out = 200), stringsAsFactors = FALSE))

# bootstrap data and get predictions
preds <- leaflife %>%
  # create new bootstrapped data sets
  modelr::bootstrap(n = 1000, id = 'boot_num') %>%
  # fit sma to every bootstrap
  group_by(boot_num) %>%
  mutate(., fit = map(strap, ~ sma(log_longev ~ log_lma, data=data.frame(.), method="SMA"))) %>%
  ungroup() %>%
  # extract intercept and slope from each fit
  mutate(., intercept = map_dbl(fit, ~coef(.x)[1]),
         slope = map_dbl(fit, ~coef(.x)[2])) %>%
  dplyr::select(., -fit) %>%
  # get fitted values for each bootstrapped model
  # uses the preds dataframe we made earlier
  group_by(boot_num) %>%
  do(data.frame(fitted = .$intercept + .$slope*preds$log_lma, 
                log_lma = preds$log_lma)) %>%
  ungroup() %>%
  # calculate the 2.5% and 97.5% quantiles at each log_lma value
  group_by(., log_lma) %>%
  dplyr::summarise(., conf_low = quantile(fitted, 0.025),
                   conf_high = quantile(fitted, 0.975)) %>%
  ungroup() %>%
  # add fitted value of actual unbootstrapped model
  mutate(., log_longev = coef(mod)[1] + coef(mod)[2]*log_lma)

# plot with ggplot
ggplot(leaflife, aes(log_lma, log_longev)) +
  geom_point(alpha = 0.3) +
  geom_line(data = preds) +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high), alpha = 0.1, preds) +
  labs(y=expression(paste("Minimun portion size, ", italic(P[min])))) +
  xlab("Ecological functional diversity")
ggsave("figures/pmin-efd.pdf", width = 6, height = 4)


library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())


summary(m1)

library(GGally)
m <- cor(results3$fdis, results3$pmin)
ggcorr(results3, method = c("everything", "pearson"), label = TRUE)
summary(results3)
cor.test(results3$fdis, results3$pmin, method=c("pearson"))

results3 %>%
  filter(fdis > 0.45) %>% 
  lm(pmin ~ fdis, data = .) %>% summary()
