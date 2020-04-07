

#### Functional diversity analysis of fishes

library(tidyverse)
library(FD)
library(cowplot)
theme_set(theme_cowplot())

all_traits <- read_csv("data-processed/more_traits-finfish.csv")
cine_traits <- read_csv("data-processed/cine-traits.csv")



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
  mutate(culture = as.factor(culture))

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
cn1 <- data.matrix(cnuts_split[[i]][, 3:11])
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



cine_traits <- read_csv("data-processed/cine-traits.csv")
nuts_trad <- read_csv("data-processed/trad-foods-cleaned.csv") %>% 
  select(latin_name, epa, dha) %>% 
  filter(!is.na(epa)) %>% 
  group_by(latin_name) %>% 
  summarise_each(funs(mean), epa, dha) 

cine_nuts <- cine_traits %>% 
  rename(bulk_trophic_level = FoodTroph) %>% 
  rename(feeding_level = Herbivory2) %>% 
  rename(feeding_mode = FeedingType) %>% 
  ungroup() %>% 
  select(latin_name, nutrient, concentration) %>% 
  group_by(latin_name, nutrient) %>% 
  summarise(concentration = mean(concentration)) %>% 
  spread(key = nutrient, value = concentration)

all_cine <- left_join(cine_nuts, nuts_trad)

ntbl.RDI.all <- data %>% 
  mutate(RDI.CA = ifelse(calcium > (1200*threshold), 1, 0)) %>% ## here we create a matrix of 0s and 1s, corresponding to whether the sample reaches DRI or not
  mutate(RDI.FE = ifelse(iron > (18*threshold), 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(zinc > (11*threshold), 1, 0)) %>%
  mutate(RDI.EPA = ifelse(epa > (1*threshold), 1, 0)) %>%
  mutate(RDI.DHA = ifelse(dha > (1*threshold), 1, 0)) %>%
  ungroup() %>% 
  mutate(RDI.micro.tot = rowSums(.[7:11])) %>% 
  filter(!is.na(RDI.micro.tot)) 
  