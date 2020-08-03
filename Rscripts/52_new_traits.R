

### new traits analysis
library(rfishbase)
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())


data_sea <- read_csv("data-processed/nutrients-traits-for-pgls.csv") %>% View
  mutate(concentration = exp(log_concentration)) %>% 
  dplyr::select(species1, nutrient, concentration, reference, abs_lat, seanuts_id2, subgroup) %>% 
  mutate(source = "seanuts") %>% 
  mutate(part = "unknown")
data_cine <- read_csv("data-processed/trad-foods-cleaned-2020.csv") %>% 
  gather(key = nutrient, value = concentration, 9:19) %>% 
  dplyr::select(latin_name_cleaned, part, page_id, reference, level_1, level_2, nutrient, concentration) %>% 
  rename(species1 = latin_name_cleaned) %>% 
  mutate(source = "cine") %>% 
  mutate(reference = as.character(reference))

data <- bind_rows(data_sea, data_cine)



more_traits2 <- species(data$species1) ## contains BodyShapeI, DemersPelag, AnaCat, DepthRangeDeep
mat2 <- maturity(data$species1) ### contains age at maturity
stocks12 <- stocks(data$species1) ###contains EnvTemp
ecology2 <- ecology(data$species1)


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


write_csv(all_traits4, "data-processed/all-traits-nuts.csv") ### april 13 2020 -- this is the one!!


### ok let's try to make fresh brack marine into one column

all_traits5 <- all_traits4 %>% 
  mutate(Fresh = ifelse(Fresh == "-1", "fresh", NA)) %>% 
  mutate(Brack = ifelse(Brack == "-1", "brack", NA)) %>% 
  mutate(Saltwater = ifelse(Saltwater == "-1", "marine", NA)) %>% 
  mutate(realm = paste(Fresh, Brack, Saltwater, sep = "")) %>% 
  mutate(realm = str_replace_all(realm, "NA", "")) %>% 
  filter(realm != "") %>% 
  dplyr::select(-Fresh) %>% 
  dplyr::select(-Brack) %>% 
  dplyr::select(-Saltwater)

all_traits6 <- all_traits5 %>% 
  mutate(EnvTemp = ordered(EnvTemp, levels = c("temperate", "boreal", "polar", "deep-water", "subtropical", "tropical")))
  

# update seanuts may 10 to include realm ----------------------------------


write_csv(all_traits6, "data-processed/all-traits-nuts2.csv") ### updated May 10 to include freshwater, brack, salt

all_traits6 %>% View



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
  dplyr::select(BodyShapeI, DemersPelag, AnaCat, DepthRangeDeep, Fresh, Brack, Saltwater) %>% 
  mutate(DepthRangeDeep = as.numeric(DepthRangeDeep)) %>% 
  filter(!is.na(BodyShapeI), !is.na(DemersPelag), !is.na(DepthRangeDeep)) %>% 
  dplyr::select(Species, BodyShapeI, DemersPelag, DepthRangeDeep) %>% 
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

write_csv(all_traits, "data-processed/more_traits-finfish.csv") ### ok this is the first pass at trait data

all_traits <- read_csv("data-processed/more_traits-finfish.csv")


# CINE traits -------------------------------------------------------------
library(rfishbase)

nuts_trad <- read_csv("data-processed/trad-foods-cleaned-2020.csv") 
  data2 <- nuts_trad %>% 
    rename(species1 = latin_name_cleaned)


more_traits2 <- species(data2$species1) ## contains BodyShapeI, DemersPelag, AnaCat, DepthRangeDeep
# fec2 <- fecundity(data2$species1) 
# ecosys2 <- ecosystem(data2$species1) 
mat2 <- maturity(data2$species1) ### contains age at maturity
stocks12 <- stocks(data2$species1) ###contains EnvTemp
ecology2 <- ecology(data2$species1)


mt3 <- more_traits2 %>% 
  dplyr::select(Species, BodyShapeI, DemersPelag, DepthRangeDeep, Length, Fresh, Brack, Saltwater) %>% 
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
  full_join(., ec3)

all_traits3 <- all_traits2 %>% 
  full_join(., stocks3)

all_traits4 <- all_traits3 %>% 
  full_join(., mt3) %>% 
  filter(!is.na(EnvTemp)) %>% 
  mutate(EnvTemp = ordered(EnvTemp, levels = c("temperate", "boreal", "polar", "deep-water", "subtropical", "tropical"))) %>% 
  group_by(Species) %>% 
  top_n(n = 1, wt = EnvTemp) %>% 
  ungroup()



all_traits4b <- full_join(nuts_trad, all_traits4, by = c("latin_name_cleaned"= "Species"))


write_csv(all_traits4, "data-processed/epa-dha-traits.csv")
write_csv(all_traits4b, "data-processed/epa-dha-traits.csv") ## update May 24 2020

# Multi trait regressions -------------------------------------------------

identical(old_new_cine_species$latin_name, CINE_merge$latin_name)

str(old_new_cine_species)
CINE_merge <- read_csv("data-processed/CINE-body-parts.csv")

cine2 <- left_join(cine_traits, old_new_cine_species, by = "latin_name")
cine_traits <- CINE_merge %>% 
  mutate(latin_name = as.factor(latin_name)) %>% 
  arrange(latin_name)

old_new_cine_species <- old_new_cine_species %>% 
  arrange(latin_name)

new_cine <- bind_cols(old_new_cine_species, cine_traits) %>% 
  left_join(., all_traits4, by = c("species1" = "Species"))

write_csv(cine_traits, "data-processed/cine-traits.csv")
write_csv(new_cine, "data-processed/cine-traits-new-species.csv")
cine_traits_old <- read_csv("data-processed/cine-traits.csv") 

cine_traits_new <- read_csv("data-processed/cine-traits-new-species.csv") %>% 
  mutate(log_concentration = log(concentration)) %>% 
  mutate(log_length = log(Length)) %>% 
  rename(bulk_trophic_level = FoodTroph) %>% 
  rename(feeding_level = Herbivory2) %>% 
  rename(feeding_mode = FeedingType)  ### ok this is with the fixed species names
epa_dha <- read_csv("data-processed/epa-dha-traits.csv") %>% 
  gather(key = nutrient, value = concentration, epa, dha) %>% 
  mutate(log_concentration = log(concentration)) %>% 
  mutate(log_length = log(Length)) %>% 
  rename(bulk_trophic_level = FoodTroph) %>% 
  rename(feeding_level = Herbivory2) %>% 
  rename(feeding_mode = FeedingType) %>% 
  group_by(latin_name, feeding_mode, feeding_level, BodyShapeI, part, DemersPelag, EnvTemp, nutrient, reference) %>% 
  summarise_each(funs(mean), AgeMatMin, log_concentration,
                 log_length, bulk_trophic_level, log_length, DepthRangeDeep)

cine_traits_new2 <- bind_rows(cine_traits_new, epa_dha)
write_csv(cine_traits_new2, "data-processed/cine-traits-new-species2.csv") ### this is most updated cine trait data, update May 24 2020

# get realm data ----------------------------------------------------------
# update May 2020, grab the realm data
cine_traits_species <- read_csv("data-processed/cine-traits-new-species2.csv") %>% 
  dplyr::select(species1) %>% 
  distinct() 

realms <- species(cine_traits_species$species1) %>% 
  mutate(Fresh = ifelse(Fresh == "-1", "fresh", NA)) %>% 
  mutate(Brack = ifelse(Brack == "-1", "brack", NA)) %>% 
  mutate(Saltwater = ifelse(Saltwater == "-1", "marine", NA)) %>% 
  mutate(realm = paste(Fresh, Brack, Saltwater, sep = "")) %>% 
  mutate(realm = str_replace_all(realm, "NA", "")) %>% 
  filter(realm != "") %>% 
  dplyr::select(-Fresh) %>% 
  dplyr::select(-Brack) %>% 
  dplyr::select(-Saltwater) %>% 
  dplyr::select(Species, realm) 

cine_traits_new3 <- read_csv("data-processed/cine-traits-new-species2.csv") %>% 
  left_join(., realms, by = c("species1" = "Species")) %>% 
  filter(is.na(reference)) 

write_csv(cine_traits_new3, "data-processed/cine-traits-new-species3.csv")

cine_traits_new2 <- read_csv("data-processed/all-traits-nuts.csv")
unique(cine_traits_new1$nutrient)


cine_traits_new2 %>% View
cine_traits_new %>% View

cine2 <- cine_traits_new2 %>% 
  filter(part != "not specified") %>% 
  filter(part != "unknown") %>% 
  filter(!is.na(concentration)) %>% 
  filter(nutrient == "ca_mg") %>% 
  mutate(log_concentration = log(concentration)) %>% 
  mutate(log_length = log(Length)) %>%
  ungroup() %>% 
  group_by(latin_name, feeding_mode, feeding_level, BodyShapeI, DemersPelag, part, EnvTemp) %>% 
  summarise_each(funs(mean), AgeMatMin, log_concentration, log_length, Length, bulk_trophic_level, log_length, DepthRangeDeep) %>% 
  ungroup() %>% 
  filter(complete.cases(.))


# Compare models ----------------------------------------------------------


mod1 <- lm(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + feeding_level +
                 DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + part, data = cine2, na.action=na.exclude)
mod2 <- lm(log_concentration ~ log_length  + feeding_mode + feeding_level + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + EnvTemp + part, data = cine2, na.action=na.exclude)
mod3 <- lm(log_concentration ~ log_length  + feeding_level + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + EnvTemp + part, data = cine2, na.action=na.exclude)
mod4 <- lm(log_concentration ~ log_length  + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + EnvTemp + part, data = cine2, na.action=na.exclude)
mod5 <- lm(log_concentration ~ log_length + DepthRangeDeep + AgeMatMin + BodyShapeI + EnvTemp + part, data = cine2, na.action=na.exclude)
mod6 <- lm(log_concentration ~ log_length + AgeMatMin + BodyShapeI + EnvTemp + part, data = cine2, na.action=na.exclude)
mod7 <- lm(log_concentration ~ log_length + BodyShapeI + EnvTemp + part, data = cine2, na.action=na.exclude)
mod8 <- lm(log_concentration ~ log_length + EnvTemp + part, data = cine2, na.action=na.exclude)
mod9 <- lm(log_concentration ~ log_length + part, data = cine2, na.action=na.exclude)

model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9) %>% View
stargazer(mod1, title = "", type = "html",single.row=TRUE,  out="tables/cine-fe-models-expanded-all-tissues6.htm", ci=TRUE, ci.level=0.95, digits = 2)

summary(mod1)
library(MuMIn)
library(patchwork)
visreg(mod1, "log_length", by = c("part"), gg = TRUE, size = 4)  
visreg(mod1, "DemersPelag", by = c("BodyShapeI"), gg = TRUE, size = 4)  
plot1 <- visreg(mod1, "DemersPelag", gg = TRUE, size = 4) +
  ylab("log(calcium) mg/100g ") + xlab("Habitat association") +
  theme(axis.text.x = element_text(angle = 90))
plot2 <- visreg(mod1, "log_length", gg = TRUE, size = 4) +
  ylab("log(calcium) mg/100g ") + xlab("Log length (cm)")  +
  theme(axis.text.x = element_text(angle = 90))
plot3 <- visreg(mod1, "AgeMatMin", gg = TRUE, size = 4) +
  ylab("log(calcium) mg/100g ") + xlab("Age at Maturity (years)") +
  theme(axis.text.x = element_text(angle = 90))
plot4 <- visreg(mod1, "DepthRangeDeep", gg = TRUE, size = 4) +
  ylab("log(calcium) mg/100g ") + xlab("Depth (m)") +
  theme(axis.text.x = element_text(angle = 90))
plot5 <- visreg(mod1, "bulk_trophic_level", gg = TRUE, size = 4) +
  ylab("log(calcium) mg/100g ") + xlab("Trophic position") +
  theme(axis.text.x = element_text(angle = 90))
plot6 <- visreg(mod1, "part", gg = TRUE, size = 4) +
  ylab("log(calcium) mg/100g ") + xlab("Body part") +
  theme(axis.text.x = element_text(angle = 90))
plot7 <- visreg(mod1, "BodyShapeI", gg = TRUE, size = 4) +
  ylab("log(calcium) mg/100g ") + xlab("Body shape") +
  theme(axis.text.x = element_text(angle = 90))
plot8 <- visreg(mod1, "feeding_mode", gg = TRUE, size = 4) +
  ylab("log(calcium) mg/100g ") + xlab("Feeding mode") +
  theme(axis.text.x = element_text(angle = 90))
plot9 <- visreg(mod1, "feeding_level", gg = TRUE, size = 4) +
  ylab("log(calcium) mg/100g ") + xlab("Feeding level") +
  theme(axis.text.x = element_text(angle = 90))

library(patchwork)
plot_all <- plot1 + plot2 + plot3 + plot4 + plot5 + plot7 + plot8 +
  plot_annotation(tag_levels = 'A') + plot_layout(ncol = 4)
ggsave("figures/calcium-partial-regressions-muscle.pdf", plot = plot_all, width = 14, height = 10)
ggsave("figures/calcium-partial-regressions.png", plot = plot_all, width = 14, height = 10)


anova(mod1)
zn_mod <- visreg(full_mod, "BodyShapeI", plot = "FALSE")
zn_mod[1]
fits_zn <- data.frame(zn_mod$fit)

str(zn_mod)

library(stargazer)


cine2 %>% 
  group_by(latin_name, part) %>% 
  tally() %>% View
  


calcium <- cine_traits_new %>% 
  filter(part != "not specified") %>% 
  mutate(log_concentration = log(concentration)) %>% 
  mutate(log_length = log(Length)) %>% 
  rename(bulk_trophic_level = FoodTroph) %>% 
  rename(feeding_level = Herbivory2) %>% 
  rename(feeding_mode = FeedingType) %>% 
  ungroup() %>%
  filter(complete.cases(.)) %>% 
  filter(nutrient == "epa") %>% 
  # filter(part == "whole") %>% 
  # filter(Length < 100) %>% 
  group_by(latin_name, feeding_mode, feeding_level, BodyShapeI, part, DemersPelag, EnvTemp) %>% 
  summarise_each(funs(mean), AgeMatMin, log_concentration,
                 log_length, bulk_trophic_level, log_length, DepthRangeDeep) 




epa <- epa_dha %>% 
  filter(nutrient == "fat_g")

full_mod <- lm(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + feeding_level +
                 DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + EnvTemp + part, data = epa, na.action=na.exclude)

full_mod <- lm(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + feeding_level +
                 DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + EnvTemp + part, data = cine2, na.action=na.exclude)

summary(full_mod)
anova(full_mod)
visreg(full_mod)


library(stargazer)
stargazer(full_mod, title = "", type = "html", out="tables/cine-epa-models-expanded-new2.htm",ci=TRUE, ci.level=0.95, digits = 2, single.row = TRUE)
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

visreg(full_mod)

#### new question about how small fish compare to each other

all_traits_raw <- read_csv("data-processed/more_traits-finfish.csv") %>% 
  mutate(Length = exp(log_length)) %>% 
  mutate(concentration = exp(log_concentration))

cine_traits <- read_csv("data-processed/cine-traits-new-species2.csv") %>% 
  mutate(species1 = latin_name) %>% 
  mutate(reference = as.character(reference))

str(all_traits_raw)
str(cine_traits)

all_nuts_all <- bind_rows(cine_traits, all_traits_raw)
all_nuts_all %>% 
  filter(nutrient == "epa") %>% View

write_csv(all_nuts_all, "data-processed/traits-nuts-data-2020.csv") ### i think this is all the data (orig data plus CINE)

names(all_nuts_all)
names(all_traits_raw)
names(cine_traits)

calcium_traits <- all_nuts_all %>%
  filter(is.na(part)) %>% 
  select(Length, AgeMatMin, DepthRangeDeep, part, bulk_trophic_level, EnvTemp, feeding_mode,
         nutrient, BodyShapeI, concentration, feeding_level, species1, DemersPelag) %>% 
  # filter(is.na(Length)) %>% 
  filter(nutrient == "ca_mg") %>% 
  filter(part != "not specified") %>% 
  mutate(log_length = log(Length)) %>% 
  filter(complete.cases(.)) 

length(unique(calcium_traits$species1))  
  
missing_traits <- all_nuts_all %>% 
  select(Length, AgeMatMin, DepthRangeDeep, part, bulk_trophic_level, EnvTemp, feeding_mode,
         nutrient, BodyShapeI, concentration, feeding_level, species1, DemersPelag, common_name) %>% 
  filter(is.na(Length)) 

unique(missing_traits$species1) ### ok so we have about 60 missing species. Come back to fix this.
  
  

# getting more trait data that was missing --------------------------------

data <- missing_traits %>% 
  mutate(old_species1 = species1) %>% 
  mutate(species1 = ifelse(common_name == "Ninespine Stickleback", "Pungitius pungitius", species1)) %>%
  mutate(species1 = ifelse(common_name == "Menhaden", "Brevoortia tyrannus", species1)) %>%
  mutate(species1 = ifelse(common_name == "Bluefin Tuna", "Thunnus thynnus", species1)) %>%
  mutate(species1 = ifelse(species1 == "Acipenser fulvenscens","Acipenser fulvescens", species1)) %>% 
  mutate(species1 = ifelse(species1 == "Acipenser oxyrhyncus", "Acipenser oxyrinchus", species1)) %>% 
  mutate(species1 = ifelse(species1 == "Clupea pallasii", "Clupea pallasii pallasii", species1)) %>% 
  mutate(species1 = ifelse(species1 == "Morone saxatillis", "Morone saxatilis", species1)) %>% 
  mutate(species1 = ifelse(species1 == "Oncorhynchus mykiss irideus", "Oncorhynchus mykiss", species1)) %>% 
  mutate(species1 = ifelse(species1 == "Theragra chalcogramma", "Gadus chalcogrammus", species1)) %>% 
  mutate(species1 = ifelse(species1 == "Centropristes striata", "Centropristis striata", species1)) %>% 
  mutate(species1 = ifelse(species1 == "Coregonus autumnalis and Coregonus sardinella", "Coregonus autumnalis", species1)) %>% 
  mutate(species1 = ifelse(species1 == "Centropristes striata", "Centropristis striata", species1)) 


more_traits <- species(data$species1) ## contains BodyShapeI, DemersPelag, AnaCat, DepthRangeDeep
fec <- fecundity(data$species1) 
ecosys <- ecosystem(data$species1) 
mat <- maturity(data$species1) ### contains age at maturity
stocks1 <- stocks(data$species1) ###contains EnvTemp
ecology2 <- ecology(data$species1) 



mt3 <- more_traits %>% 
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

mat3 <- mat %>%
  filter(!is.na(AgeMatMin)) %>% 
  dplyr::select(Species, AgeMatMin) %>% 
  mutate(AgeMatMin = as.numeric(AgeMatMin)) %>% 
  group_by(Species) %>% 
  summarise(AgeMatMin = mean(AgeMatMin))

stocks3 <- stocks1 %>% 
  dplyr::select(Species, EnvTemp) %>% 
  dplyr::distinct(Species, EnvTemp)

all_traits2 <- mat3 %>% 
  full_join(., ec3)

all_traits3 <- all_traits2 %>% 
  full_join(., stocks3)

all_traits4 <- all_traits3 %>% 
  full_join(., mt3)

old_names <- data %>% 
  select(old_species1, species1)

all_new_missing_traits <- all_traits4 %>% 
  rename(bulk_trophic_level = FoodTroph) %>% 
  rename(feeding_level = Herbivory2) %>% 
  rename(feeding_mode = FeedingType) %>% 
  filter(!is.na(Length)) %>% 
  left_join(., old_names, by = c("Species" = "species1")) 

### ok get the CINE species from here.


#### ok now merge back with all_nuts_all

bulk_trophic_level + log_length  + feeding_mode + feeding_level +
  DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + EnvTemp

View(all_nuts_all)
all_nuts_all2 <- all_nuts_all %>% 
  filter(!is.na(Length)) %>% 
  left_join(., all_new_missing_traits, by = c("species1" = "old_species1")) %>% 
  mutate(Length = ifelse(is.na(Length.x), Length.y, Length.x)) %>% 
  mutate(bulk_trophic_level = ifelse(is.na(bulk_trophic_level.x), bulk_trophic_level.y, bulk_trophic_level.x)) %>% 
  mutate(feeding_mode = ifelse(is.na(feeding_mode.x), feeding_mode.y, feeding_mode.x)) %>%
  mutate(feeding_level = ifelse(is.na(feeding_level.x), feeding_level.y, feeding_level.x)) %>%
  mutate(DemersPelag = ifelse(is.na(DemersPelag.x), DemersPelag.y, DemersPelag.x)) %>%
  mutate(DepthRangeDeep = ifelse(is.na(DepthRangeDeep.x), DepthRangeDeep.y, DepthRangeDeep.x)) %>%
  mutate(AgeMatMin = ifelse(is.na(AgeMatMin.x), AgeMatMin.y, AgeMatMin.x)) %>%
  mutate(BodyShapeI = ifelse(is.na(BodyShapeI.x), BodyShapeI.y, BodyShapeI.x)) %>%
  mutate(EnvTemp = ifelse(is.na(EnvTemp.x), EnvTemp.y, EnvTemp.x)) %>% 
  select(Length, AgeMatMin, DepthRangeDeep, part, bulk_trophic_level, EnvTemp, feeding_mode,
         nutrient, BodyShapeI, concentration, feeding_level, species1, DemersPelag, common_name, part)


# new traits data ---------------------------------------------------------
write_csv(all_nuts_all2, "data-processed/new-traits-april10-2020.csv")

#### models without part data
ca_data <- read_csv("data-processed/new-traits-april10-2020.csv") %>% 
  filter(nutrient == "ca_mg") %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(feeding_level = as.factor(feeding_level)) %>% 
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>% 
  mutate(BodyShapeI = as.factor(BodyShapeI)) ### don't use this I don't think 
all_traits_new <- read_csv("data-processed/all-traits-nuts.csv") ### this the update from April 14th
ca_data <- all_traits_new %>% 
  filter(nutrient == "ca_mg") %>% 
  rename(species1 = Species) %>% 
  rename(bulk_trophic_level = FoodTroph) %>% 
  rename(feeding_level = Herbivory2) %>% 
  rename(feeding_mode = FeedingType) %>% 
  mutate(log_concentration = log(concentration)) %>% 
  mutate(log_length = log(Length)) %>% 
  filter(part != "unknown") %>% 
  group_by(species1, feeding_mode, feeding_level, BodyShapeI, part, DemersPelag, EnvTemp) %>% 
  summarise_each(funs(mean), AgeMatMin, concentration,
                 Length, bulk_trophic_level, log_length, DepthRangeDeep) 
 

unique(ca_data$part)

full_mod_wopart <- lm(log(concentration) ~ bulk_trophic_level + log(Length)  + feeding_mode + feeding_level +
                   DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + EnvTemp, data = ca_data, na.action=na.exclude)

full_mod <- lm(log(concentration) ~ bulk_trophic_level + log(Length)  + feeding_mode + feeding_level +
                        DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + EnvTemp + part, data = ca_data, na.action=na.exclude)


visreg(full_mod)
summary(full_mod_wopart) 
anova(full_mod_wopart)

library(stargazer)
stargazer(full_mod_wopart, title = "", type = "html", out="~/Desktop/calcium-models-expanded-woparts.htm")

stargazer(full_mod, title = "", type = "html", out="~/Desktop/calcium-models-expanded-withparts.htm")


ca <- cine_traits %>% 
  filter(nutrient == "ca_mg")

length(unique(ca$concentration))


ca_data_small <- cine_traits %>% 
  filter(part == "whole") %>% 
  filter(nutrient == "ca_mg") %>% 
  mutate(log_length = log(Length)) %>% 
  mutate(log_concentration = log(concentration)) %>% 
  filter(Length < 100) 

ca_data_big <- cine_traits %>% 
  filter(part == "whole") %>% 
  filter(nutrient == "ca_mg") %>% 
  mutate(log_length = log(Length)) %>% 
  mutate(log_concentration = log(concentration)) %>% 
  filter(Length > 100) 

ca_mod_small <- ca_data_small %>% 
  lm(log_concentration ~ log_length  +  bulk_trophic_level + feeding_mode + feeding_level +
                   DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + EnvTemp, data = .)

  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(feeding_level = as.factor(feeding_level)) %>%
  lm(log_concentration ~ log_length  +  bulk_trophic_level + feeding_mode +
       DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + EnvTemp, data = .)

summary(ca_mod_small)
summary(ca_mod_big)
anova(ca_mod_big)
aov(mod_ca)


ca_data_all <- cine_traits %>% 
  filter(part == "whole") %>% 
  filter(nutrient == "ca_mg") %>% 
  mutate(log_length = log(Length)) %>% 
  mutate(log_concentration = log(concentration))


ca_mod_all <- ca_data_all %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(feeding_level = as.factor(feeding_level)) %>%
  lm(log_concentration ~ log_length  +  bulk_trophic_level + feeding_mode +
       DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + EnvTemp, data = .)
 
library(visreg)
 
visreg(ca_mod_all)

