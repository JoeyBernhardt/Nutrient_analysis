

#### Functional diversity analysis of fishes

library(tidyverse)
library(FD)
library(cowplot)
theme_set(theme_cowplot())

# all_traits <- read_csv("data-processed/more_traits-finfish.csv")
# cine_traits <- read_csv("data-processed/cine-traits.csv")
all_traits <- read_csv("data-processed/all-traits-nuts.csv")


### ok let's calculate functional diversity at the different levels of richness



sample_40_global <- function(sample_size) {
  sample_n(all_traits, size = sample_size, replace = FALSE)
}

reps <- rep(40, 1000)

mean_nuts_rep <- reps %>% 
  map_df(sample_40_global, .id = "replicate") %>% 
  select(-species_name) %>%
  select(- subgroup) %>% 
  ungroup() %>% 
  filter(replicate != 1) %>% 
  split(.$replicate)

fds <- mean_nuts_rep %>% 
  map(dbFD)


sam <- mean_nuts_rep[[1]]
names(all_traits)

traits <- all_traits %>% 
  select(species_name, log_length, bulk_trophic_level, feeding_level, DemersPelag, BodyShapeI,
         feeding_mode, EnvTemp, DepthRangeDeep, AgeMatMin) %>% 
  mutate(Length = exp(log_length)) %>% 
  select(-log_length) %>% 
  distinct(species_name, Length, bulk_trophic_level, feeding_level, DemersPelag, BodyShapeI,
                  feeding_mode, EnvTemp, DepthRangeDeep, AgeMatMin) %>% 
  group_by(species_name, bulk_trophic_level, feeding_level, DemersPelag, BodyShapeI,
           feeding_mode, EnvTemp) %>% 
  summarise_each(funs(mean), Length, AgeMatMin, DepthRangeDeep) %>% 
  ungroup() %>% 
  filter(complete.cases(.)) %>% 
  distinct(species_name, .keep_all = TRUE) %>% 
  mutate(feeding_level = as.factor(feeding_level)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(BodyShapeI = as.factor(BodyShapeI)) %>% 
  mutate(EnvTemp = as.factor(EnvTemp)) 

str(traits)

ntbl.matrix.mic <- data.matrix(traits[, 2:10])
rownames(ntbl.matrix.mic) <- traits$species_name
?dbFD

length(unique(rownames(ntbl.matrix.mic)))

rand.fd <- dbFD(ntbl.matrix.mic)

results <- dbFD(traits)

results$FDiv

randsp.data <- ntbl.matrix.mic[sample(1:length(row.names(ntbl.matrix.mic)), 20, replace = FALSE),]

### Step 5. Run the functional diversity function on the random subset
rand.fd <- dbFD(randsp.data)
rand.fd$FDiv


### ok now need to figure out how to do something like this:

sample_size <- 10
nutrient_fishing_function <- function(sample_size) {
  ntbl_sub1 <- traits %>%
    select(-species_name) %>% 
    sample_n(size = 40, replace = FALSE) %>%
    sample_n(size = sample_size, replace = FALSE)
  
  sample_list <- NULL
  for (i in 3:nrow(ntbl_sub1) ) {
    output <- combn(nrow(ntbl_sub1), i, FUN=function(x) ntbl_sub1[x,], simplify = FALSE)
    output <- bind_rows(output, .id = "sample_id")
    subsample_size <- rep(i, nrow(output))
    output <- cbind(output, subsample_size)
    sample_list <- rbind(sample_list,output)
  }
  
  sample_list <- split(sample_list, f = sample_list$subsample_size)
  sample_list[[1]]

  
  randsp.data <- sample_list %>% 
    map_df(`[`, .id = "replicate")
  
  randsp.data <- sample_list %>% 
    data.matrix() %>% 
    map(dbFD)
  
  results <- data.frame(fdiv = dbFD(randsp.data)$FDiv, frich = dbFD(randsp.data)$FRic,
                        fdispersion = dbFD(randsp.data)$FDis)
  
  
}


samples_rep <- rep(10, 12)
samples_rep[1]

global_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run") 

global_10$dataset <- "GL"


str(ntbl.matrix.mic)
j <- 3

i <- 1
results <- data.frame()
for (i in 1:100) {
  ntbl.matrix.mic2 <- ntbl.matrix.mic[sample(1:length(row.names(ntbl.matrix.mic)), 40, replace = FALSE),]
    for(j in 3:40){
  randsp.data<- ntbl.matrix.mic2[sample(1:length(row.names(ntbl.matrix.mic2)), j, replace = FALSE),]
  hold <- data.frame(fdiv = dbFD(randsp.data)$FDiv, frich = dbFD(randsp.data)$FRic,
                     fdispersion = dbFD(randsp.data)$FDis, richness = j, 
                     replicate = i, speciespool = paste(rownames(ntbl.matrix.mic2), sep = "_", collapse = ""),
                     species = paste(rownames(randsp.data), sep = "_", collapse = "_"), 
                     qual = dbFD(randsp.data)$qual.FRic)
  results <- bind_rows(results, hold)
}}

length(unique(results$speciespool))

results$speciespool[1]

res1 <- results %>% 
  filter(speciespool == results$speciespool[1]) 

results %>%
  # filter(richness > 10) %>% 
  ggplot(aes(x = richness, y = fdispersion)) + geom_point() + geom_smooth() +
  # xlim(0, 10) +
  ylab("Functional dispersion") + xlab("Species richness")
ggsave("figures/global-functional-dispersion.png", width = 8, height = 6)

simuls <- simul.dbFD(s = c(2, 3, 5, 10, 15, 20, 25, 30, 35, 40), t = 9, r = 100, p = 126)

res <- read_csv("data-processed/nut_accumulation_trad_foods.csv")
res_global_all <- read_csv("data-processed/res_global_all.csv")
res_global <- read_csv("data-processed/res_global.csv")
res_all <- bind_rows(res, res_global)
species_numbers <- read_csv("data-processed/species_numbers.csv")

unique(res_all$culture)
unique(culture_foods$culture)

cine_traits %>% View
culture_foods <- read_excel("~/Documents/traditional-foods/culture-foods.xlsx")

cine_traits2 <- left_join(cine_traits, culture_foods, by = c("pageid" = "page_id"))


### ok now let's see if the cultures that have higher functional dispersion at species richness = 10 also have higher nutritional diversity

### ok so now apply the functional dispersion metric to each of the cultures

cine_traits2 %>% View

names(cine_traits2)

cine3 <- cine_traits2 %>% 
  filter(culture %in% species_numbers$culture) %>% 
  select(culture, latin_name, Length, FoodTroph, Herbivory2, DemersPelag, BodyShapeI,
         FeedingType, EnvTemp, DepthRangeDeep, AgeMatMin) %>% 
  distinct(culture, latin_name, Length, FoodTroph, Herbivory2, DemersPelag, BodyShapeI,
         FeedingType, EnvTemp, DepthRangeDeep, AgeMatMin) %>% 
  group_by(culture, latin_name, FoodTroph, Herbivory2, DemersPelag, BodyShapeI,
           FeedingType, EnvTemp) %>% 
  summarise_each(funs(mean), Length, AgeMatMin, DepthRangeDeep) %>% 
  ungroup() %>% 
  filter(complete.cases(.)) %>% 
  group_by(culture) %>% 
  distinct(latin_name, .keep_all = TRUE) %>%
  rename(feeding_level = Herbivory2) %>% 
  rename(feeding_mode = FeedingType) %>% 
  rename(species_name = latin_name) %>% 
  ungroup() %>% 
  mutate(feeding_level = as.factor(feeding_level)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(BodyShapeI = as.factor(BodyShapeI)) %>% 
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(culture = as.factor(culture)) %>% 
  select(culture, species_name, DemersPelag, BodyShapeI, Length, EnvTemp)

str(cine3)
cnuts_split <- cine3 %>% 
  # select(-species_name) %>%
  ungroup() %>% 
  split(.$culture)
str(cnuts_split)

View(cnuts_split[[1]])

fds <- cnuts_split %>% 
  map(dbFD)

length(cnuts_split)
length(unique(cine3$culture))


randsp.data<- ntbl.matrix.mic2[sample(1:length(row.names(ntbl.matrix.mic2)), j, replace = FALSE),]


results <- data.frame()
for (j in 1:100){
for (i in 1:length(cnuts_split)) {
cnuts_split[[i]] <- cnuts_split[[i]][sample(1:length(row.names(cnuts_split[[i]])), 10, replace = FALSE),]
cn1 <- data.matrix(cnuts_split[[i]][, 3:6])
rownames(cn1) <- cnuts_split[[i]]$species_name
hold <- data.frame(fdis = dbFD(cn1)$FDis, culture = cnuts_split[[i]][1, 1], replicate = j)
results <- bind_rows(results, hold)

}}


results_fdis <- results %>% 
  mutate(dataset = culture) %>% 
  mutate(dataset = str_replace(dataset, "Inuit-Inupiaq", "II")) %>% 
  mutate(dataset = str_replace(dataset, "Central Salish", "CS")) %>% 
  mutate(dataset = str_replace(dataset, "Wampanoag", "WA")) %>% 
  mutate(dataset = str_replace(dataset, "Cree", "CR")) %>%
  mutate(dataset = str_replace(dataset, "Nootkan", "NO")) %>% 
  mutate(dataset = str_replace(dataset, "Bella Coola", "BC")) %>% 
  mutate(dataset = str_replace(dataset, "Tlingit", "TL")) %>%
  mutate(dataset = str_replace(dataset, "Haida", "HA")) %>%
  mutate(dataset = str_replace(dataset, "Tsimshian", "TS")) %>% 
  mutate(dataset = str_replace(dataset, "Montagnais-Naskapi", "MN")) %>%
  mutate(dataset = str_replace(dataset, "Yupik", "YU")) %>% 
  mutate(dataset = str_replace(dataset, "Abenaki", "AB")) %>%
  mutate(dataset = str_replace(dataset, "Micmac", "MI")) %>%
  mutate(dataset = str_replace(dataset, "Kwakiutl", "KW")) %>% 
  group_by(dataset) %>% 
  summarise_each(funs(mean, std.error), fdis) %>% 
  rename(mean_fdis = mean) %>% 
  rename(std_error_fdis = std.error)

all_accumulation_lims <- read_csv("data-processed/all_accumulation_lims.csv") %>% 
  rename(nd = mean)

accums <- left_join(all_accumulation_lims, results_fdis) %>% 
  filter(species_no == 10)

accums %>% 
  ggplot(aes(x = mean_fdis, y = nd, color = dataset)) + geom_point() + 
  geom_smooth(method = "lm", color = "black") + 
  ylab("Nutritional diversity") + xlab("Ecological functional diversity (dispersion)")
ggsave("figures/functional-dispersion-nutritional-diversity.pdf", width = 8, height = 6)

library(lmodel2)
lmodel2(nd ~ mean_fdis, data = accums)


all_traits_raw <- read_csv("data-processed/more_traits-finfish.csv")

names(all_traits_raw)
all_nuts <- all_traits_raw %>% 
  mutate(concentration = exp(log_concentration)) %>% 
  select(species1, nutrient, concentration) %>% 
  group_by(species1, nutrient) %>% 
  summarise(concentration = mean(concentration)) %>% 
  spread(key = nutrient, value = concentration) %>% 
  select(species1, ca_mg, zn_mg, fe_mg, epa, dha) %>% 
  ungroup() %>% 
  filter(complete.cases(.))

all_traits <- all_traits_raw %>% 
  select(species1, log_length, bulk_trophic_level, feeding_mode, feeding_level, AgeMatMin, EnvTemp, BodyShapeI, DemersPelag, DepthRangeDeep) %>% 
  distinct(species1, log_length, bulk_trophic_level, feeding_mode, feeding_level, AgeMatMin, EnvTemp, BodyShapeI, DemersPelag, DepthRangeDeep) %>% 
  mutate(Length = exp(log_length))

all_traits2 <- left_join(all_nuts, all_traits)



# come back here ----------------------------------------------------------

## deal with cine3
cine3 <- read_csv("data-processed/cine-traits-new-species2.csv") %>% 
  mutate(species1 = ifelse(is.na(species1), latin_name, species1)) %>% 
  mutate(concentration = ifelse(is.na(concentration), exp(log_concentration), concentration)) %>%
  mutate(Length = ifelse(is.na(Length), exp(log_length), Length)) 

unique(cine3$nutrient) ### ok realizing why some of the species are missing is that AgeMatMin is missing

cine3_traits <- cine3 %>% 
  select(species1, 13:21) %>% 
  group_by(species1, BodyShapeI, DemersPelag,
           feeding_mode, feeding_level, EnvTemp) %>%
  summarise_each(funs(mean), AgeMatMin, DepthRangeDeep, bulk_trophic_level, Length)

cine3_nuts <- cine3 %>% 
  select(species1, nutrient, concentration) %>% 
  filter(nutrient %in% c("ca_mg", "zn_mg", "fe_mg", "epa", "dha")) %>% 
  group_by(species1, nutrient) %>% 
  summarise_each(funs(mean), concentration) %>% 
  filter(!is.na(concentration)) %>% 
  spread(key = nutrient, value = concentration)

all_cine_updated <- left_join(cine3_traits, cine3_nuts)

cine_traits <- read_csv("data-processed/cine-traits.csv") %>% 
  # mutate(latin_name1 = species1) %>% 
  rename(bulk_trophic_level = FoodTroph) %>%
  rename(feeding_level = Herbivory2) %>%
  rename(feeding_mode = FeedingType) %>%
  distinct(latin_name, latin_name, Length, bulk_trophic_level, feeding_mode, feeding_level, AgeMatMin, EnvTemp, BodyShapeI, DemersPelag, DepthRangeDeep, nutrient) 
nuts_trad <- read_csv("data-processed/trad-foods-cleaned.csv") %>% 
  select(latin_name, epa, dha) %>% 
  filter(!is.na(epa)) %>% 
  group_by(latin_name) %>% 
  summarise_each(funs(mean), epa, dha) 

cine_nuts <- cine_traits %>% 
  # rename(bulk_trophic_level = FoodTroph) %>%
  # rename(feeding_level = Herbivory2) %>%
  # rename(feeding_mode = FeedingType) %>%
  ungroup() %>% 
  select(latin_name, nutrient, concentration) %>% 
  group_by(latin_name, nutrient) %>% 
  summarise(concentration = mean(concentration)) %>% 
  spread(key = nutrient, value = concentration)

all_cine <- left_join(cine_nuts, nuts_trad) %>% 
  left_join(., cine_traits) %>% 
  rename(species1 = latin_name)

all_cine <- left_join(cine_nuts, nuts_trad) %>% 
  left_join(., cine_traits) %>% 
  rename(species1 = latin_name)

# all_traits_nuts <- bind_rows(all_cine, all_traits2) %>% 
#   select(species1, ca_mg, zn_mg, fe_mg, epa, dha, Length, bulk_trophic_level, feeding_mode, feeding_level, AgeMatMin, EnvTemp, BodyShapeI, DemersPelag, DepthRangeDeep) %>%
#   ungroup() %>% 
#   filter(complete.cases(.))

all_traits_nuts <- bind_rows(all_cine, all_traits2) %>% 
  select(species1, ca_mg, zn_mg, fe_mg, epa, dha, Length, bulk_trophic_level, feeding_mode, feeding_level, AgeMatMin, EnvTemp, BodyShapeI, DemersPelag, DepthRangeDeep) %>%
  ungroup() %>% View
filter(complete.cases(.))

all_traits_nuts <- bind_rows(all_cine_updated, all_traits2) %>% 
  select(species1, ca_mg, zn_mg, fe_mg, epa, dha, Length, bulk_trophic_level, feeding_mode, feeding_level, AgeMatMin, EnvTemp, BodyShapeI, DemersPelag, DepthRangeDeep) %>%
  ungroup() %>% View
  filter(complete.cases(.))

write_csv(all_traits_nuts, "data-processed/all-traits-nuts-updated.csv")
write_csv(all_traits_nuts, "data-processed/all-traits-nuts.csv")
all_traits_nuts <- read_csv("data-processed/all-traits-nuts.csv")
all_traits_nuts_incomplete <- bind_rows(all_cine, all_traits2)

threshold <- 0.1

names(ntbl.RDI.all)
ntbl.RDI.all <- all_traits_nuts %>% 
  mutate(RDI.CA = ifelse(ca_mg > (1200*threshold), 1, 0)) %>% ## here we create a matrix of 0s and 1s, corresponding to whether the sample reaches DRI or not
  mutate(RDI.FE = ifelse(fe_mg > (18*threshold), 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(zn_mg > (11*threshold), 1, 0)) %>%
  mutate(RDI.EPA = ifelse(epa > (1*threshold), 1, 0)) %>%
  mutate(RDI.DHA = ifelse(dha > (1*threshold), 1, 0)) %>%
  ungroup() %>% 
  mutate(RDI.micro.tot = rowSums(.[16:20])) %>% 
  filter(!is.na(RDI.micro.tot)) %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(feeding_level = as.factor(feeding_level)) %>% 
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(BodyShapeI = as.factor(BodyShapeI)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>% 
  group_by(species1, RDI.DHA, RDI.EPA, RDI.CA, RDI.ZN, RDI.FE, BodyShapeI, DemersPelag,
           feeding_mode, feeding_level, EnvTemp) %>%
  summarise_each(funs(mean), AgeMatMin, DepthRangeDeep, bulk_trophic_level, Length) %>% 
  ungroup()
 

all_spa <- ntbl.RDI.all %>% 
  dplyr::select(species1, 16:20) %>% 
  mutate(subgroup = "all") %>% 
  split( .$subgroup) %>% 
  map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.EPA", "RDI.DHA")) %>%
  map(.f = specaccum, method = "collector")

str(all_spa)
all_spa$all$perm

### ok new method. First sample 10 randomly, then do specaccum collector method, then caculate fdis

ntbl_sub2 <- ntbl.RDI.all %>% 
  distinct(species1, .keep_all = TRUE)
results <- data.frame()
for (i in 1:5000){
ntbl_sub <- ntbl_sub2
ntbl_sub <- ntbl_sub[sample(1:length(row.names(ntbl_sub)), 10, replace = FALSE),]

all_spa <- ntbl_sub %>% 
  dplyr::select(species1, 2:6) %>% 
  mutate(subgroup = "all") %>% 
  split( .$subgroup) %>% 
  map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.EPA", "RDI.DHA")) %>%
  map(.f = specaccum, method = "collector")

accumulated_targets <- all_spa %>% 
  map(.f = `[`, "richness") %>% 
  unlist() %>% 
  as.data.frame()

accumulated_targets$richness_level = rownames(accumulated_targets)
colnames(accumulated_targets) <- c("number_of_targets", "richness_level")

accumulated_targets <- accumulated_targets %>% 
  separate(richness_level, into = c("subgroup", "number_of_species")) %>%
  mutate(number_of_species = str_replace(number_of_species, "richness", "")) %>%
  mutate(number_of_species = as.numeric(number_of_species))


cn1 <- data.matrix(ntbl_sub[, c(7, 8, 11, 15)])
rownames(cn1) <- ntbl_sub$species1
hold <- data.frame(fdis = dbFD(cn1)$FDis[[1]][[1]], targets = accumulated_targets[10, 1], replicate = i)
results <- bind_rows(results, hold)
}

results %>% 
  ggplot(aes(x = fdis, y = targets)) + geom_point(alpha =0.05) +
  geom_smooth(color = "black", method = "lm") + ylab("Nutritional diversity") +
  xlab("Ecological functional diversity (dispersion)")
ggsave("figures/nd-fdisp-4traits.pdf", width = 8, height = 6)

lm(targets ~ fdis, data = results) %>% summary()
write_csv(results, "data-processed/fdis-nd-4traits.csv")
write_csv(results, "data-processed/fdis-nd-4traits2.csv")

#### do this same approach for the efficiency measure

all_traits_nuts2 <- all_traits_nuts %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(feeding_level = as.factor(feeding_level)) %>% 
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(BodyShapeI = as.factor(BodyShapeI)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>% 
  group_by(species1, dha, epa, ca_mg, zn_mg, fe_mg, BodyShapeI, DemersPelag,
           feeding_mode, feeding_level, EnvTemp) %>%
  summarise_each(funs(mean), AgeMatMin, DepthRangeDeep, bulk_trophic_level, Length) %>% 
  ungroup() %>% 
  distinct(species1, .keep_all = TRUE)


# this is where we caculate the fdis and the Ne for all the combos --------



nuts_traits <- read_csv("data-processed/all-traits-nuts.csv") %>% 
  rename(species1 = Species) %>% 
  rename(feeding_level = Herbivory2) %>% 
  rename(bulk_trophic_level = FoodTroph) %>% 
  rename(feeding_mode = FeedingType) %>% 
  select(species1, Length, EnvTemp, DemersPelag, feeding_level) %>% 
  # filter(complete.cases(.)) %>% 
  group_by(species1, DemersPelag,
           feeding_level, EnvTemp) %>%
  summarise_each(funs(mean), Length) %>% 
  ungroup() %>% 
  filter(complete.cases(.))


nuts_nuts <- read_csv("data-processed/all-traits-nuts.csv") %>% 
  filter(!is.na(concentration)) %>% 
  group_by(Species, nutrient) %>% 
  summarise_each(funs(mean), concentration) %>% 
  filter(nutrient %in% c("ca_mg", "zn_mg", "fe_mg", "epa", "dha")) %>% 
  ungroup() %>% 
  spread(key = nutrient, value = concentration) %>% 
  filter(complete.cases(.)) %>% 
  rename(species1 = Species) 

all_nuts_traits <- full_join(nuts_traits, nuts_nuts) %>% 
  filter(complete.cases(.)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>% 
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(feeding_level = as.factor(feeding_level))


fdis <- function(df){
  cn1 <- data.matrix(df[, c("DemersPelag", "Length", "EnvTemp", "feeding_level")])
  rownames(cn1) <- df$species1
  results <- data.frame(fdis = dbFD(cn1)$FDis[[1]][[1]])
}

i <- 1
results3 <- data.frame()
for (j in 1:1) {
  ntbl_sub1 <- all_nuts_traits %>% 
    sample_n(size = 20, replace = FALSE)
  
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
    mutate(unique_sample = paste(species_number, sample_id, sep = "_")) %>% 
    group_by(unique_sample, species_number, sample_id) %>% 
    mutate(cal_total = (ca_mg/species_number)) %>% ## get the amount of calcium each species will contribute
    mutate(zinc_total = (zn_mg/species_number)) %>% 
    mutate(iron_total = (fe_mg/species_number)) %>% 
    mutate(epa_total = (epa/species_number)) %>% 
    mutate(dha_total = (dha/species_number)) %>%
    summarise_each(funs(sum), contains("total")) %>%  ## sum up all of each of the nutrients
    mutate(cal_grams = (cal_total/(1200/10))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
    mutate(iron_grams = (iron_total/(18/10))) %>%
    mutate(zinc_grams = (zinc_total/(11/10))) %>% 
    mutate(epa_grams = (epa_total/(1/10))) %>%
    mutate(dha_grams = (dha_total/(1/10))) %>%
    dplyr::rename(species_no = species_number) %>% 
    group_by(unique_sample, species_no, sample_id) %>% 
    select(-contains("total")) %>% 
    gather(key = nutrient, value = concentration, contains("grams")) %>% 
    group_by(unique_sample, species_no, sample_id) %>% 
    summarise(min_percentage = min(concentration)) %>%
    mutate(grams_required = 100/min_percentage) %>% 
    mutate(run = j)
  
  sample3 <- new_data_sub1 %>% 
    filter(subsample_size > 2) %>% 
    mutate(unique_sample = paste(subsample_size, sample_id, sep = "_")) %>% 
    split(.$unique_sample)
  
  ress <- sample3 %>% 
    map_df(fdis, .id = "unique_sample")
  
  hold <- left_join(resampling_15, ress) %>% 
    ungroup() 
  results3 <- bind_rows(results3, hold)
  
}

write_csv(results2, "data-processed/fdis-ne-species-3-10.csv")

species310 <- read_csv("data-processed/fdis-ne-species-3-10.csv")
species310 %>% 
  ungroup() %>% 
  filter(!is.na(fdis)) %>% 
  group_by(species_no, run) %>%
  summarise_each(funs(mean), grams_required, fdis) %>% 
  # filter(species_no == 10) %>% 
  ggplot(aes(x = fdis, y = grams_required, color = factor(species_no))) + geom_point() + geom_smooth(color = "black", method = "lm") +
  scale_color_viridis_d(name = "Species richness") + ylab("Grams required to reach 5 micronutrient targets") +
  xlab("Ecological functional diversity (dispersion)")
  ggsave("figures/fdis-ne-310-colours.pdf", width = 8, height = 6)

species310 %>% 
  lm(grams_required ~ fdis, data = .) %>% summary()

### ok let's turn the fdis thing into a function


df <- samples_split[[4]] %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>% 
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(feeding_level = as.factor(feeding_level))

fdis <- function(df){
  cn1 <- data.matrix(df[, c("DemersPelag", "Length", "EnvTemp", "feeding_level")])
  rownames(cn1) <- df$species1
  results <- data.frame(fdis = dbFD(cn1)$FDis[[1]][[1]])
  # results2 <- bind_rows(results2, hold)
}

ress <- samples_split %>% 
  map_df(fdis, .id = "unique_sample")


sample_size <- 3
results3 <- data.frame()
for (i in 1:1000) {
  ntbl_sub1 <- all_nuts_traits %>% 
    sample_n(size = sample_size, replace = FALSE)
  
  # sample_list <- NULL
  # for (i in 1:nrow(ntbl_sub1) ) {
  #   output <- combn(nrow(ntbl_sub1), i, FUN=function(x) ntbl_sub1[x,], simplify = FALSE)
  #   output <- bind_rows(output, .id = "sample_id")
  #   subsample_size <- rep(i, nrow(output))
  #   output <- cbind(output, subsample_size)
  #   sample_list <- rbind(sample_list,output)
  # }
  # 
  # sample_list <- split(sample_list, f = sample_list$subsample_size)
  # 
  # new_data_sub1 <- sample_list %>% 
  #   map_df(`[`, .id = "replicate")
  
  resampling_15 <- ntbl_sub1 %>% 
    # dplyr::rename(species_number = subsample_size) %>%
    # group_by(species_number, sample_id) %>% 
    mutate(species_number = sample_size) %>% 
    mutate(cal_total = (ca_mg/species_number)) %>% ## get the amount of calcium each species will contribute
    mutate(zinc_total = (zn_mg/species_number)) %>% 
    mutate(iron_total = (fe_mg/species_number)) %>% 
    mutate(epa_total = (epa/species_number)) %>% 
    mutate(dha_total = (dha/species_number)) %>%
    summarise_each(funs(sum), contains("total")) %>%  ## sum up all of each of the nutrients
    mutate(cal_grams = (cal_total/(1200/10))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
    mutate(iron_grams = (iron_total/(18/10))) %>%
    mutate(zinc_grams = (zinc_total/(11/10))) %>% 
    mutate(epa_grams = (epa_total/(1/10))) %>%
    mutate(dha_grams = (dha_total/(1/10))) %>%
    # dplyr::rename(species_no = species_number) %>% 
    # group_by(species_no, sample_id) %>% 
    select(-contains("total")) %>% 
    gather(key = nutrient, value = concentration, contains("grams")) %>% 
    # group_by(species_no, sample_id) %>% 
    summarise(min_percentage = min(concentration)) %>%
    mutate(grams_required = 100/min_percentage) 
  
  cn1 <- data.matrix(ntbl_sub1[, c("DemersPelag", "EnvTemp", "Length", "feeding_level")])
  rownames(cn1) <- ntbl_sub1$species1
  hold <- data.frame(fdis = dbFD(cn1)$FDis[[1]][[1]], replicate = i, grams_required = resampling_15$grams_required[1], species_number = sample_size)
  results3 <- bind_rows(results3, hold)
}

results20 <- results2
results20$species_number <- 20
results2$species_number <- 10

results_3 <- bind_rows(results20, results2, results40, results3)

write_csv(results_3, "data-processed/fdis-ne-10-20-40.csv")

results_3 <- read_csv("data-processed/fdis-ne-10-20-40.csv")
species310 <- read_csv("data-processed/fdis-ne-species-3-10.csv") %>% 
  rename(species_number = species_no)

all_fdis <- bind_rows(species310, results_3)

all_fdis %>% 
  filter(!is.na(fdis)) %>% 
  ggplot(aes(x = fdis, y = grams_required, color = factor(species_number))) + geom_point(alpha = 0.5) +
  geom_smooth( color = "black") + ylab("Nutritional efficiency") + xlab("Ecological functional diversity (dispersion)")
ggsave("figures/ne-fdis-multi-levels.pdf", width = 8, height = 6)


results_3 %>% 
  ggplot(aes(x = fdis, fill = factor(species_number), group = factor(species_number))) + geom_histogram(alpha = 0.5)

lm(grams_required ~ fdis, data = results_3) %>% summary()

write_csv(results, "data-processed/NE-fdis.csv")
write_csv(results2, "data-processed/NE-fdis-5000.csv")
