



species(server="sealifebase")

specieslist <- read_excel("data-processed/species_names_resolved_edited.xlsx")
data <- specieslist %>% 
  rename(species1 = matched_name2)

more_traits2 <- species(data$species1, server="sealifebase") ## contains BodyShapeI, DemersPelag, AnaCat, DepthRangeDeep
mat2 <- maturity(data$species1, server="sealifebase") ### contains age at maturity
stocks12 <- stocks(data$species1, server="sealifebase") ###contains EnvTemp
ecology2 <- ecology(data$species1, server="sealifebase")


mt3 <- more_traits2 %>% 
  dplyr::select(Species, DemersPelag, DepthRangeDeep, Length, Fresh, Brack, Saltwater) %>% 
  group_by(Species, DemersPelag, Fresh, Brack, Saltwater) %>% 
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
  mutate(Fresh = ifelse(Fresh == "1", "fresh", NA)) %>% 
  mutate(Brack = ifelse(Brack == "1", "brack", NA)) %>% 
  mutate(Saltwater = ifelse(Saltwater == "1", "marine", NA)) %>% 
  mutate(realm = paste(Fresh, Brack, Saltwater, sep = "")) %>%
  mutate(realm = str_replace_all(realm, "NA", "")) %>% 
  filter(realm != "") %>% 
  dplyr::select(-Fresh) %>% 
  dplyr::select(-Brack) %>% 
  dplyr::select(-Saltwater)

all_traits6_inverts <- all_traits5 %>% 
  mutate(EnvTemp = ordered(EnvTemp, levels = c("temperate", "boreal", "polar", "deep-water", "subtropical", "tropical")))

write_csv(all_traits6_inverts, "data-processed/fishbase_traits_inverts.csv")
