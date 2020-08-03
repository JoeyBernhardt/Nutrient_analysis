

### bring in maclean data

mac <- read_excel("data/mac-Nutrient_data.xlsx") %>% 
  clean_names() %>% 
  dplyr::select("species_from_search", "zn_value", "ca_value", "fe_value", "form_clean", "search") %>% 
  filter(search %in% c("WOS", "FAO")) %>% 
  rename(species = species_from_search) %>% 
  mutate(Species = str_replace(species, "_", " "))

unique(traits$level)

traits <- read_csv("data/mac-Traits_data.csv") %>% 
  filter(level %in% c("species", "Species", "sub species")) %>% 
  clean_names() %>% 
  rename(species = species_traits_all)


mac_all <- left_join(mac, traits)

ca <- mac_all %>% 
  filter(form_clean == "muscle") %>% 
  gather(key = nutrient, value = concentration, ca_value, zn_value, fe_value) %>%  
  group_by(species, env_temp, demers_pelag, body_shape_i, nutrient) %>% 
  summarise_each(funs(mean), l_max, depth_range_deep, trophic_level, concentration)

ca <- mac %>% 
  filter(form_clean == "muscle") %>% 
  gather(key = nutrient, value = concentration, ca_value, zn_value, fe_value)

ca %>% 
  filter(!is.na(concentration)) %>% 
  distinct(species) %>% View

names(ca)

mod <- lm(log(zn_value) ~ log(l_max) + depth_range_deep + trophic_level + env_temp + demers_pelag + body_shape_i, data = ca)
summary(mod)



# get fish base traits ----------------------------------------------------
library(rfishbase)
data <- mac %>% 
  dplyr::select(species) %>% 
  rename(species1 = species) %>% 
  mutate(species1 = str_replace(species1, "_", " "))


more_traits2 <- species(data$species1) ## contains BodyShapeI, DemersPelag, AnaCat, DepthRangeDeep
mat2 <- maturity(data$species1) ### contains age at maturity
stocks12 <- stocks(data$species1) ###contains EnvTemp
ecology2 <- ecology(data$species1)
data.frame(names = names(ecology2)) %>% 
  filter(grepl("Troph", names)) %>% View

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
  mutate(EnvTemp = ordered(EnvTemp, levels = c("temperate", "boreal", "polar", "deep-water", "subtropical", "tropical"))) %>% 
  left_join(., mac)

fao <- all_traits6 %>% 
  filter(search == "FAO")

flong <- fao %>%
  rename(ca_mg = ca_value) %>% 
  rename(fe_mg = fe_value) %>% 
  rename(zn_mg = zn_value) %>% 
  gather(key = nutrient, value = concentration, ca_mg, fe_mg, zn_mg)



write_csv(flong, "data-processed/mtraits_nuts.csv")
