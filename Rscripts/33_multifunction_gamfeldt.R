
library(purrr)
library(dplyr)
library(tidyr)
library(readr)
library(plotrix)
library(ggplot2)
library(broom)
library(gridExtra)
library(grid)

mean_nuts <- read_csv("data-processed/mean_nuts.csv")
mean_seadiv <- read_csv("data-processed/mean_seadiv.csv")


sample_size <- 10

nutrient_fishing_function <- function(sample_size) {
  ntbl_sub1 <- mean_nuts %>% 
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
    # mutate(zinc_total = (zinc/species_number)) %>% 
    # mutate(iron_total = (iron/species_number)) %>% 
    # mutate(epa_total = (epa/species_number)) %>%
    # mutate(protein_total = (protein/species_number)) %>%
    # mutate(dha_total = (dha/species_number)) %>%
    summarise_each(funs(sum), contains("total")) %>% ## sum up all of each of the nutrients
    mutate(cal_grams = (cal_total/(1200))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
    # mutate(iron_grams = (iron_total/(18))) %>%
    # mutate(zinc_grams = (zinc_total/(11))) %>% 
    # mutate(epa_grams = (epa_total/(1))) %>%
    # mutate(dha_grams = (dha_total/(1))) %>%
    # mutate(protein_grams = (protein_total/(56))) %>%
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no, sample_id) %>% 
    select(-contains("total")) %>% 
    gather(key = nutrient, value = concentration, 3) %>% 
    group_by(species_no, sample_id) %>% 
    summarise(min_percentage = min(concentration)) %>% 
    mutate(grams_required = 100/min_percentage)
}


samples_rep <- rep(10, 100)

output_calcium_100c <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run") %>% 
  mutate(nutrient = "calcium")

## quick diversion to plot this!

output_calcium %>% 
  ungroup() %>% 
  ggplot(aes(x = log(species_no), y = log(grams_required), group = run, color = run)) + geom_smooth(method = "lm")

output_calcium_1000 %>% 
  group_by(run) %>% 
  do(tidy(lm(log(grams_required) ~ log(species_no), data = .), conf.int = TRUE)) %>% 
  filter(term != "(Intercept)") %>% 
  ungroup() %>% 
  summarise_each(funs(mean, sd), estimate) %>% 
  ggplot(aes(x = estimate_mean, y = estimate_mean)) + geom_point() +
  geom_errorbar(aes(ymin = estimate_mean - estimate_sd, ymax = estimate_mean + estimate_sd)) + ylim(-1, 0)
  
output_calcium %>% 
  group_by(run) %>% 
  do(tidy(nls(formula = (grams_required ~ a * species_no^b),data = .,  start = c(a=1000, b=-0.5)))) %>% 
  filter(term == "b") %>% 
  ungroup() %>% 
  summarise_each(funs(mean, sd), estimate) %>% 
  ggplot(aes(x = estimate_mean, y = estimate_mean)) + geom_point() +
  geom_errorbar(aes(ymin = estimate_mean - estimate_sd, ymax = estimate_mean + estimate_sd)) + ylim(-1, 0)

 estimates_1000 <- output_calcium_1000 %>% 
  group_by(run) %>% 
  do(tidy(nls(formula = (grams_required ~ a * species_no^b),data = .,  start = c(a=1000, b=-0.5)))) 

estimates_200 <- output_calcium_200 %>% 
  group_by(run) %>% 
  do(tidy(nls(formula = (grams_required ~ a * species_no^b),data = .,  start = c(a=1000, b=-0.5)))) 

estimates_100 <- output_calcium_100 %>% 
  group_by(run) %>% 
  do(tidy(nls(formula = (grams_required ~ a * species_no^b),data = .,  start = c(a=1000, b=-0.5)))) 

estimates_100b <- output_calcium_100b %>% 
  group_by(run) %>% 
  do(tidy(nls(formula = (grams_required ~ a * species_no^b),data = .,  start = c(a=1000, b=-0.5)))) 
estimates_100c <- output_calcium_100c %>% 
  group_by(run) %>% 
  do(tidy(nls(formula = (grams_required ~ a * species_no^b),data = .,  start = c(a=1000, b=-0.5)))) 

estimates_10 <- output_calcium_10 %>% 
  group_by(run) %>% 
  do(tidy(nls(formula = (grams_required ~ a * species_no^b),data = .,  start = c(a=1000, b=-0.5)))) 

estimates_10$replication <- "10_reps"
estimates_100$replication <- "100_reps"
estimates_100b$replication <- "100_repsb"
estimates_100c$replication <- "100_repsc"
estimates_200$replication <- "200_reps"
estimates_1000$replication <- "1000_reps"

all_est <- bind_rows(estimates_10, estimates_100, estimates_100b, estimates_100c, estimates_200, estimates_1000)

all_est %>% 
  filter(term == "b") %>% 
  group_by(replication) %>% 
  summarise_each(funs(mean, sd), estimate) %>% 
  ggplot(aes(x = replication, y = estimate_mean)) + geom_point() +
  geom_errorbar(aes(ymin = estimate_mean - estimate_sd, ymax = estimate_mean + estimate_sd)) + ylim(-1, 0)

  

mod1000 <- output_calcium_1000 %>% 
  group_by(run) %>% 
  do(tidy(nls(formula = (grams_required ~ a * species_no^b),data = .,  start = c(a=1000, b=-0.5)))) 

mod1000_linear <- output_calcium_1000 %>% 
  group_by(run) %>% 
  do(tidy(lm(log(grams_required) ~ log(species_no), data = .), conf.int = TRUE))
 
  
o287 <- mod1000 %>% 
  filter(run == 287)

l287 <- mod1000_linear %>% 
  filter(run == 287)

data287 <- output_calcium_1000 %>% 
  filter(run == 287)

pf <- function(x) 7542.6337779*x^-0.3375611

data287 %>% 
  ggplot(aes(x = species_no, y = grams_required)) + geom_jitter(width = 0.4) +
  stat_function(fun = pf, color = "blue")

lf <- function(x) -0.1113618*x + 8.5263642

data287 %>% 
  ggplot(aes(x = log(species_no), y = log(grams_required))) + geom_point() +
 stat_function(fun = lf, color = "blue", size =1.5)

mod1000 %>% 
  filter(term == "b") %>%
  ungroup() %>% 
  summarise_each(funs(mean, sd), estimate) %>% 
  ggplot(aes(x = estimate_mean, y = estimate_mean)) + geom_point() +
  geom_errorbar(aes(ymin = estimate_mean - estimate_sd, ymax = estimate_mean + estimate_sd)) + ylim(-1, 0)

output_calcium_1000 <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run") %>% 
  mutate(nutrient = "calcium")

output_calcium <- output_calcium %>% 
  mutate(nutrient = "calcium")

output_zinc <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run") %>% 
  mutate(nutrient = "zinc")

output_iron <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run") %>% 
  mutate(nutrient = "iron")

output_epa <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run") %>% 
  mutate(nutrient = "epa")

output_dha <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run") %>% 
  mutate(nutrient = "dha")

output_protein <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run") %>% 
  mutate(nutrient = "protein") %>% 
  mutate(run = as.integer(run))

all_output <- bind_rows(output_calcium, output_iron, output_zinc, output_dha, output_epa)
write_csv(all_output, "data-processed/single_nutrient_accumulation_by_fractions.csv")

all_output <- read_csv("data-processed/single_nutrient_accumulation_by_fractions.csv")
str(all_output)

output_protein2  <- output_protein %>% 
  mutate(sample_id = as.integer(sample_id))
all_output2 <- bind_rows(all_output, output_protein2)

write_csv(all_output2,"data-processed/single_nutrient_accumulation_by_fractions2.csv" )

summary <- output_iron %>%
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_required, grams_for_25_percent)


summary %>% 
  ggplot(aes(x = species_no, y = grams_for_25_percent_median)) + geom_point()

single_functions <- all_output %>% 
  ggplot(aes(x = species_no, y = grams_required)) + geom_point() +
  geom_smooth(method = "lm") + theme_bw() + facet_wrap( ~ nutrient, scales = "free") + ylab("grams of tissue required to reach 10% DRI") + xlab("species richness")
ggsave("figures/single_functions.png")



output_calcium_1000 %>% 
  ggplot(aes(x = species_no, y = grams_required)) + geom_point() +
  geom_smooth(method = "lm") + theme_bw() 


output_calcium_1000 %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  lm(grams_for_25_percent ~ species_no, data = .) %>% 
  summary


reps100 <- read_csv("data-processed/grams-required-10-spp-1000reps.csv")


reps100 %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  lm(grams_for_25_percent ~ species_no, data = .) %>% 
  summary

reps100 %>% 
ggplot(aes(x = species_no, y = grams_required)) + geom_point() +

### get the power functions for each nutrient
cal_sum <- output_calcium %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_for_25_percent)
    
calcium_power <- nls(formula=(median ~ a * species_no^b), data=cal_sum, start = c(a=10000, b=-0.7))
a.cal <- coef(calcium_power)[1]
b.cal <- coef(calcium_power)[2]  

## iron
iron_sum <- output_iron %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_for_25_percent)

iron_power <- nls(formula=(median ~ a * species_no^b), data=iron_sum, start = c(a=10000, b=-0.7))
a.iron <- coef(iron_power)[1]
b.iron <- coef(iron_power)[2]  
iron_power_function <- function(x) a.iron*x^b.iron


## zinc 

zinc_sum <- output_zinc %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_for_25_percent)

zinc_power <- nls(formula=(median ~ a * species_no^b), data=zinc_sum, start = c(a=10000, b=-0.7))
a.zinc <- coef(zinc_power)[1]
b.zinc <- coef(zinc_power)[2]  
zinc_power_function <- function(x) a.zinc*x^b.zinc

## epa 

epa_sum <- output_epa %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_for_25_percent)

epa_power <- nls(formula=(median ~ a * species_no^b), data=epa_sum, start = c(a=10000, b=-0.7))
a.epa <- coef(epa_power)[1]
b.epa <- coef(epa_power)[2]  
epa_power_function <- function(x) a.epa*x^b.epa

## dha


dha_sum <- output_dha %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_for_25_percent)

dha_power <- nls(formula=(median ~ a * species_no^b), data=dha_sum, start = c(a=10000, b=-0.7))
a.dha <- coef(dha_power)[1]
b.dha <- coef(dha_power)[2]  
dha_power_function <- function(x) a.dha*x^b.dha

## all 
all_sum <- reps100 %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_for_25_percent)

all_power <- nls(formula=(median ~ a * species_no^b), data=all_sum, start = c(a=10000, b=-0.7))
a.all <- coef(all_power)[1]
b.all <- coef(all_power)[2]  
all_power_function <- function(x) a.all*x^b.all



cor(1/cal_sum$median, predict(calcium_power))


calcium_power_function <- function(x) a.cal*x^b.cal


cal_sum %>% 
  mutate(service = 1/median) %>% 
ggplot(aes(x = species_no, y = service)) + geom_point(size = 3) +
  stat_function(fun = calcium_power_function, color = "grey") +
  theme_bw() + ylab("nutritional service 1/median grams required to reach 10% of DRI for calcium") + xlab("species richness") +
  scale_x_continuous(breaks = 1:10)

## original calcium plot
cal_sum %>% 
  mutate(service = 1/median) %>% 
  ggplot(aes(x = species_no, y = median)) + geom_point(size = 3) +
  # stat_function(fun = calcium_power_function, color = "grey") +
  theme_bw() + ylab("nutritional service 1/median grams required to reach 10% of DRI for calcium") + xlab("species richness") +
  scale_x_continuous(breaks = 1:10)



reps <- reps100 %>% 
  mutate(nutrient = "all 5 micronutrients") %>% 
  select(-run, -`[EMPTY]`) 

all_output2 <- all_output %>% 
  select(-run) %>% 
  mutate(sample_id = as.integer(sample_id))

str(all_output)
str(reps)

all_output_with5 <- bind_rows(all_output2, reps)

all_summary <- all_output_with5 %>% 
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(nutrient, species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_for_25_percent) 

params <- all_summary %>% 
  ungroup() %>% 
  mutate(nutrient = ifelse(nutrient == "all 5 micronutrients", "all", nutrient)) %>% 
  group_by(nutrient) %>% 
  do(tidy(nls(median ~ a * species_no^b, data =., start = c(a=10000, b=-0.7)), conf.int = TRUE)) %>% 
  filter(term == "b") %>% 
  ggplot(aes(x = reorder(nutrient, estimate), y = estimate, color = nutrient)) + geom_point() +
  # facet_wrap( ~ term, scales = "free") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + theme_bw() +
  scale_y_reverse() + xlab("nutrient") + ylab("b estimate") +
  theme(legend.position = "none") + xlab("") +
  theme(text=element_text(family="Helvetica", size=12)) +
  theme(legend.title=element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="none")
ggsave("figures/power_function_params_by_nutrient.png")
 
all_summary %>% 
  group_by(nutrient) %>% 
  do(tidy(lm(median ~ species_no, data =.), conf.int = TRUE)) %>%  View


bef <- all_summary %>% 
  ggplot(aes(x = species_no, y = median, color = nutrient)) + geom_point() +
  geom_line() + theme_bw() +
  scale_y_reverse() +
  theme(text=element_text(family="Helvetica", size=12)) +
  theme(legend.title=element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = c(1:10)) + xlab("species richness") + ylab("median grams required to reach 10% of DRI")
ggsave("figures/all_nutrients_efficiency_power_fits_rev_y.png")


grid.newpage()
vpb_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
vpa_ <- viewport(width = 0.4, height = 0.4, x = 0.52, y = 0.3)  # the inset in upper right
print(bef, vp = vpb_)
print(params, vp = vpa_)

ggsave("figures/figure3a_accum.pdf")
