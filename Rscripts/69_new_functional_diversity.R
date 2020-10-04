

trad_nuts_mean_raw <- read_csv("data-processed/trad-foods-mean.csv")
species_numbers <- read_csv("data-processed/species_numbers.csv")
mean_nuts <- read_csv("data-processed/mean_nuts.csv") %>% 
  filter(!grepl("spp", species_name)) %>% 
  filter(!grepl("sp.", species_name)) %>% 
  mutate(species_name = ifelse(species_name == "Tenualosa ilisha (juvenile)","Tenualosa ilisha", species_name)) %>% 
  mutate(species_name = ifelse(species_name == "Pangasianodon hypophthalmus (juvenile)","Pangasianodon hypophthalmus", species_name))



trad2 <- trad_nuts_mean_raw %>% 
  select(-culture) %>% 
  distinct() %>% 
  rename(species_name = latin_name) %>% 
  filter(subgroup == "Fish")

mean_nuts %>% View

all <- bind_rows(trad2, mean_nuts) %>% 
  # select(-subgroup) %>% 
  distinct() %>% 
  filter(!grepl("spp.", species_name)) 

inverts <- all %>% 
  filter(!subgroup %in% c("Fish", "finfish"))
finfish <- all %>% 
  filter(subgroup %in% c("Fish", "finfish"))

library(rfishbase)

data <- inverts %>% 
  rename(species1 = species_name)
data <- finfish %>% 
  rename(species1 = species_name)
data <- mean_nuts %>% 
  rename(species1 = species_name)
data<- mean_nuts %>% 
  filter(subgroup == "finfish") %>% 
  rename(species1 = species_name)

library(rfishbase)
more_traits2 <- species(data$species1, server = "fishbase") ## contains BodyShapeI, DemersPelag, AnaCat, DepthRangeDeep
mat2 <- maturity(data$species1, server = "fishbase") ### contains age at maturity
stocks12 <- stocks(data$species1, server = "fishbase") ###contains EnvTemp
ecology2 <- ecology(data$species1, server = "fishbase")


mt3 <- more_traits2 %>% 
  dplyr::select(Species, BodyShapeI, DemersPelag, DepthRangeDeep, Length, Fresh, Brack, Saltwater) %>% 
  group_by(Species, BodyShapeI, DemersPelag, Fresh, Brack, Saltwater) %>% 
  mutate(DepthRangeDeep = as.numeric(DepthRangeDeep)) %>% 
  mutate(Length = as.numeric(Length)) %>% 
  summarise_each(funs(mean), DepthRangeDeep, Length)

ec3 <- ecology2 %>% 
  dplyr::select(Species, Herbivory2, FoodTroph, FeedingType) %>% 
  group_by(Species, Herbivory2, FeedingType) %>% 
  mutate(FoodTroph = as.numeric(FoodTroph)) %>% 
  summarise(FoodTroph = mean(FoodTroph)) 

mat3 <- mat2 %>%
  filter(!is.na(AgeMatMin)) %>% 
  dplyr::select(Species, AgeMatMin) %>% 
  mutate(AgeMatMin = as.numeric(AgeMatMin)) %>% 
  group_by(Species) %>% 
  summarise(AgeMatMin = mean(AgeMatMin))

stocks3 <- stocks12 %>% 
  dplyr::select(Species, EnvTemp) %>% 
  dplyr::distinct(Species, EnvTemp) %>% 
  filter(!is.na(EnvTemp)) %>% 
  distinct(Species, .keep_all = TRUE)

all_traits2 <- mat3 %>% 
  full_join(., ec3)

all_traits3 <- all_traits2 %>% 
  full_join(., stocks3)

all_traits4 <- all_traits3 %>% 
  full_join(., mt3) %>% 
  full_join(., data, by = c("Species"= "species1"))


mean_nuts_inverts <- all_traits4 %>% 
  rename(bulk_trophic_level = FoodTroph) %>% 
  rename(feeding_mode = FeedingType) %>% 
  dplyr::select(Species, DemersPelag, Length, bulk_trophic_level, feeding_mode) %>% 
  filter(complete.cases(.))
mean_nuts_traits <- all_traits4 %>% 
  rename(bulk_trophic_level = FoodTroph) %>% 
  rename(feeding_mode = FeedingType) %>% 
  dplyr::select(Species, DemersPelag, Length, bulk_trophic_level, feeding_mode) %>% 
  filter(complete.cases(.))

all_mean_nuts_traits <- bind_rows(mean_nuts_inverts, mean_nuts_traits)

invert_traits <- all_traits4 %>% 
  rename(bulk_trophic_level = FoodTroph) %>% 
  rename(feeding_mode = FeedingType) %>% 
  select(Species, DemersPelag, Length, bulk_trophic_level, feeding_mode) %>% 
  filter(complete.cases(.))

finfish_traits <- all_traits4 %>% 
  rename(bulk_trophic_level = FoodTroph) %>% 
  rename(feeding_mode = FeedingType) %>% 
  select(Species, DemersPelag, Length, bulk_trophic_level, feeding_mode) %>% 
  filter(complete.cases(.))

allt <- bind_rows(invert_traits, finfish_traits)
allnt <- all %>% 
  left_join(allt, by = c("species_name" = "Species")) %>% 
  filter(complete.cases(.)) %>% 
  distinct(.) 

write_csv(allnt, "data-processed/all_complete_nuts_traits.csv")
allnt <- read_csv("data-processed/all_complete_nuts_traits.csv")
allnt$bulk_trophic_level <- scale(allnt$bulk_trophic_level)
allnt$Length <- scale(allnt$Length) 

lm(log(zinc) ~ log(Length) + DemersPelag + feeding_mode + bulk_trophic_level + subgroup, data = allnt) %>% summary()

mt <- allnt %>% 
  filter(species_name %in% c(mean_nuts$species_name))


fdis_function <- function(df){
  dbFD(as.matrix(df[[1]][, 2:5]))$FDis[[1]][[1]]
}

all_mean_nuts_traits_all <- left_join(mean_nuts, all_mean_nuts_traits, by = c("species_name" = "Species")) %>% 
  filter(complete.cases(.))

write_csv(all_mean_nuts_traits_all, "data-processed/all_mean_nuts_traits_all.csv")


nutrient_fishing_function <- function(sample_size) {
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
  filter(subsample_size > 2) %>% 
  dplyr::select(unique_sample_id, DemersPelag, bulk_trophic_level, Length, feeding_mode) %>% 
  split(.$unique_sample_id) 
 
  res <- vector()
  for (i in 1:length(splitnt)){
    res[i] <-  fdis_function(splitnt[i])
  }
  
 

  
  resampling_15 <- new_data_sub1 %>% 
    filter(subsample_size > 2) %>% 
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
  
  output <-  data.frame(fdis = res, sample = names(splitnt), nt = resampling_15$rdi_micro_tot, s = resampling_15$species_no) 
  return(output)
}


samples_rep <- rep(10, 100)

threshold <- 0.1

global_10_mean <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

global_10_mean2 <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run")



names(results_mac)


g2 <- bind_rows(global_10_mean, results_mac, results_mac) %>% 
  mutate(nt = as.factor(nt))

g2 %>% 
  ungroup() %>%
  mutate(fdis_round = round(fdis, digits = 4)) %>% 
  group_by(fdis_round) %>% 
  summarise_each(funs(mean), nt) %>% 
  ggplot(aes(x = fdis_round, y = nt)) + 
  geom_point() +
  geom_smooth(method = "lm", color = "black")

library(tidyverse)
g2 <- read_csv("data-processed/fdis-nt-sims2.csv") %>% 
  bind_rows(results_mac)

g3 <- g2 %>% 
  ungroup() %>%
  filter(s == 10) %>% 
  mutate(fdis_round = round(fdis, digits = 2)) %>% 
  group_by(fdis_round) %>% 
  summarise_each(funs(mean), nt) %>% 
  ungroup() %>% 
  filter(complete.cases(.))

class(g3)
str(g3)
lm(nt ~ fdis, data = g3) %>% summary()
cor(g2$fdis, g2$nt)
g3 %>% 
  ggplot(aes(x = fdis, y = nt)) + geom_point() +
  geom_smooth(method = "lm")

library(MASS)
m <- polr(factor(nt) ~ fdis, data= g2)
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
  fdis = seq(from = 0, to = 2, length.out = 100))

min(g2$fdis)
write_csv(g2, "data-processed/fdis-nt-sims.csv")
write_csv(g2, "data-processed/fdis-nt-sims2.csv")

newdat <- cbind(newdat, predict(m, newdat, type = "probs")) %>% 
  gather(key = targets, value = probability, 2:7)

newdat %>% 
  ggplot(aes(x = fdis, y = probability, color = targets)) + geom_line(size = 1.5) +
  xlab("Ecological functional diversity") + ylab("Probability") +
  scale_color_viridis_d(option = "inferno", begin = 0.1, end = 0.8, name = "NT") +
  theme(legend.position = c(0.07, 0.95), legend.direction = "horizontal") +
  xlim(1.5, 1.9)






global_10$dataset <- "GL"

global_10 %>% 
  group_by(dataset, species_no) %>% 
  summarise(mean_rdi_count = mean(mean)) %>% 
  ggplot(aes(x = species_no, y = mean_rdi_count, color = dataset)) + geom_line(size = 2)


summ <- global_10 %>% 
  group_by(dataset, species_no) %>% 
  summarise(mean_rdi_count = mean(mean))

lm(log(mean) ~ log(species_no), data = global_10_mean) %>% summary()



