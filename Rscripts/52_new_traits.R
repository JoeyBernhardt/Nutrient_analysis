

### new traits analysis
library(rfishbase)
library(tidyverse)


data <- read_csv("data-processed/nutrients-traits-for-pgls.csv") %>% 
  mutate(species_name = str_replace(species_name, "(juvenile)", "")) %>% 
  mutate(species_name = str_replace(species_name, "()", ""))

CINE_merge <- read_csv("data-processed/CINE-body-parts.csv")

more_traits <- species(data$species1) ## contains BodyShapeI, DemersPelag, AnaCat, DepthRangeDeep
fec <- fecundity(data$species1) 
ecosys <- ecosystem(data$species1) 
mat <- maturity(data$species1) ### contains age at maturity
stocks1 <- stocks(data$species1) ###contains EnvTemp
pop <- popchar(data$species1) 
pop2 <- popqb(data$species1) ### contains K
poplw2 <- poplw(data$species1) ### c
popllf2 <- poplf(data$species1) ### c
popll <- popll(data$species1) ### c


rep1 <- reproduction(data$species1) 

mt2 <- more_traits %>% 
  select(BodyShapeI, DemersPelag, AnaCat, DepthRangeDeep, everything()) %>% 
  mutate(DepthRangeDeep = as.numeric(DepthRangeDeep))

str(mt2)

mt2 %>% 
  ggplot(aes(x = AnaCat)) + geom_histogram(stat = "count")


write_csv(more_traits, "data-processed/fb-traits-species.csv")
write_csv(mat, "data-processed/fb-traits-maturity.csv")
write_csv(pop2, "data-processed/fb-traits-popqb.csv")
write_csv(stocks1, "data-processed/fb-traits-stocks.csv")


### read in new traits

more_traits <- read_csv("data-processed/fb-traits-species.csv")
mat <- read_csv("data-processed/fb-traits-maturity.csv")
pop2 <- read_csv("data-processed/fb-traits-popqb.csv")
stocks1 <- read_csv("data-processed/fb-traits-stocks.csv")


traits <- read_csv("data-processed/nutrients-traits-for-pgls.csv") %>% 
  mutate(species_name = str_replace(species_name, "(juvenile)", "")) %>% 
  mutate(species_name = str_replace(species_name, "()", ""))

View(traits)
unique(traits$species1)


View(more_traits)
unique(more_traits$Species)

intersect(traits$species1, more_traits$Species)

more_traits2 <- more_traits %>% 
  dplyr::select(BodyShapeI, DemersPelag, AnaCat, DepthRangeDeep, everything()) %>% 
  mutate(DepthRangeDeep = as.numeric(DepthRangeDeep)) %>% 
  filter(!is.na(BodyShapeI), !is.na(DemersPelag), !is.na(DepthRangeDeep)) %>% 
  select(Species, BodyShapeI, DemersPelag, DepthRangeDeep) %>% 
  distinct(Species, .keep_all = TRUE)

mat2 <- mat %>% 
  select(Species, AgeMatMin) %>% 
  filter(!is.na(Species), !is.na(AgeMatMin)) %>% 
  distinct(Species, .keep_all = TRUE)

pop3 <- pop2 %>% 
  select(Species, K, Salinity, FoodType) %>% 
  filter(!is.na(Species), !is.na(K)) %>% 
  distinct(Species, .keep_all = TRUE)

stocks2 <- stocks1 %>% 
  select(Species, EnvTemp) %>% 
  filter(!is.na(EnvTemp)) %>% 
  distinct(Species, .keep_all = TRUE)


traits_all <- left_join(mat2, stocks2) 
 
traits2 <- traits_all %>% 
  left_join(., pop3)
  
traits4 <- traits_all %>% 
  left_join(., more_traits2) 



all_traits <- left_join(traits, traits4, by = c("species1" = "Species"))

write_csv(all_traits, "data-processed/more_traits-finfish.csv")


# CINE traits -------------------------------------------------------------

data2 <- read_csv("data-processed/CINE-body-parts.csv") %>% 
  mutate(species1 = latin_name)

more_traits2 <- species(data2$species1) ## contains BodyShapeI, DemersPelag, AnaCat, DepthRangeDeep
fec2 <- fecundity(data2$species1) 
ecosys2 <- ecosystem(data2$species1) 
mat2 <- maturity(data2$species1) ### contains age at maturity
stocks12 <- stocks(data2$species1) ###contains EnvTemp
ecology2 <- ecology(data2$species1)


mt3 <- more_traits2 %>% 
  dplyr::select(Species, BodyShapeI, DemersPelag, DepthRangeDeep, Length) %>% 
  group_by(Species, BodyShapeI, DemersPelag) %>% 
  mutate(DepthRangeDeep = as.numeric(DepthRangeDeep)) %>% 
  mutate(Length = as.numeric(Length)) %>% 
  summarise_each(funs(mean), DepthRangeDeep, Length)### 3000 rows

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
  dplyr::distinct(Species, EnvTemp)

all_traits2 <- mat3 %>% 
  left_join(., ec3)

all_traits3 <- all_traits2 %>% 
  left_join(., stocks3)

all_traits4 <- all_traits3 %>% 
  left_join(., mt3)


# Multi trait regressions -------------------------------------------------

CINE_merge <- read_csv("data-processed/CINE-body-parts.csv")
cine_traits <- CINE_merge %>% 
  left_join(., all_traits4, by = c("latin_name" = "Species"))

write_csv(cine_traits, "data-processed/cine-traits.csv")

unique(cine_traits$nutrient)
unique(cine2$part)

cine2 <- cine_traits %>% 
  mutate(log_concentration = log(concentration)) %>% 
  mutate(log_length = log(Length)) %>% 
  rename(bulk_trophic_level = FoodTroph) %>% 
  rename(feeding_level = Herbivory2) %>% 
  rename(feeding_mode = FeedingType) %>% 
  ungroup() %>%
  filter(complete.cases(.)) %>% 
  filter(nutrient == "ca_mg") %>% 
  filter(log_concentration > 0) %>% 
  group_by(latin_name, feeding_mode, feeding_level, BodyShapeI, part, DemersPelag, EnvTemp) %>% 
  summarise_each(funs(mean), AgeMatMin, log_concentration, log_length, bulk_trophic_level, log_length, DepthRangeDeep) 

cine2 %>% 
  ggplot(aes(x = part, y = log_concentration)) + geom_boxplot() + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cine2 %>% 
  ggplot(aes(x = log_concentration)) + geom_histogram()


full_mod <- lm(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + feeding_level +
                 DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + EnvTemp + part, data = cine2, na.action=na.exclude)

summary(full_mod)
anova_zinc <- anova(full_mod)  
confint(full_mod)

library(stargazer)
stargazer(full_mod, title = "", type = "html", out="tables/calcium-models-expanded2.htm")
library(report)
full_mod %>% 
  report() %>% 
  table_short()

log_length  + feeding_mode + feeding_level +
  DemersPelag + DepthRangeDeep + part + AgeMatMin + BodyShapeI + EnvTemp


table <- lm(log_concentration ~ bulk_trophic_level + log_length + feeding_mode + feeding_level +
     DemersPelag + DepthRangeDeep + part + AgeMatMin + BodyShapeI, data = cine2) %>% 
  report() %>% 
  table_long() 


library(visreg)


