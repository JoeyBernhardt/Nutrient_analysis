### Reksten data added May 24 2020

library(readxl)

reksten <- read_excel("data/reksten-2020.xlsx") %>% 
  mutate(part = ifelse(part == "fillet", "muscle", part)) %>% 
  mutate(reksten_id = paste0("reksten",rownames(.))) 

View(reksten)
reksten_species <- unique(reksten$species_name)



data2 <- reksten %>% 
  distinct(species_name) %>% 
  rename(species1 = species_name)


more_traits2 <- species(data2$species1) ## contains BodyShapeI, DemersPelag, AnaCat, DepthRangeDeep
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

all_traits4b <- full_join(reksten, all_traits4, by = c("species_name"= "Species"))

realms <- species(data2$species1) %>%  
  mutate(Fresh = ifelse(Fresh == "-1", "fresh", NA)) %>% 
  mutate(Brack = ifelse(Brack == "-1", "brack", NA)) %>% 
  mutate(Saltwater = ifelse(Saltwater == "-1", "marine", NA)) %>% 
  mutate(realm = paste(Fresh, Brack, Saltwater, sep = "")) %>% 
  mutate(realm = str_replace_all(realm, "NA", "")) %>% 
  filter(realm != "") %>% 
  dplyr::select(-Fresh) %>% 
  dplyr::select(-Brack) %>% 
  dplyr::select(-Saltwater) %>% 
  dplyr::select(Species, realm) %>% 
  distinct(Species, realm)

all_traits5 <- left_join(all_traits4b, realms, by = c("species_name" = "Species")) %>% 
  mutate(source = "reksten") 

write_csv(all_traits5, "data-processed/reksten-traits.csv")
