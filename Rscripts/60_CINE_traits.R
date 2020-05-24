#### Starting fresh with CINE traits


nuts_trad <- read_csv("data-processed/trad-foods-cleaned-2020.csv") ### this is the CINE dataset with cleaned latin names

# length(unique(nuts_trad$latin_name))
data2 <- nuts_trad %>% 
  rename(species1 = latin_name_cleaned)


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



all_traits4b <- full_join(nuts_trad, all_traits4, by = c("latin_name_cleaned"= "Species"))


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

all_traits5 <- left_join(all_traits4b, realms, by = c("latin_name_cleaned" = "Species")) %>% 
  mutate(source = "cine") 

length(unique(all_traits5$latin_name_cleaned))

write_csv(all_traits5, "data-processed/CINE-data-all.csv") ### CINE data as of May 24 2020

cine_traits_new_may <- read_csv("data-processed/CINE-data-all.csv") %>% 
  mutate(reference = as.character(reference)) %>% 
  gather(key = nutrient, value = concentration, 9:19)

### OK now bring in the latest seanuts database

seanuts_parts <- read_csv("data-processed/seanuts_parts3.csv") %>% 
  dplyr::select(seanuts_id2, part) %>% 
  rename(part_corr = part)

seanuts_traits2 <- read_csv("data-processed/all-traits-nuts2.csv") %>% 
  # filter(!is.na(seanuts_id2)) %>% 
  # dplyr::select(-part) %>% 
  filter(source != "cine")


traits <- seanuts_traits2 %>% 
  left_join(., seanuts_parts, by = "seanuts_id2") %>% 
  mutate(part = ifelse(is.na(part_corr), part, part_corr)) %>% 
  mutate(part = ifelse(part == "muscle-skinless", "muscle", part))  ### this is the latest version as of May 24 2020


all_nuts <- bind_rows(traits, cine_traits_new_may)

traits2 <- all_nuts %>% 
  mutate(part = ordered(part, levels = c("muscle", "muscle + skin", "muscle + small bones", "muscle + bones", "muscle + head", "muscle, bone + inside","whole",
                                         "head, eyes, cheeks + soft bones", "tongues + cheeks", "skin", "liver", "offal", "eggs", "oil", NA))) %>% 
  mutate(nutrient = ifelse(nutrient == "protein", "protein_g", nutrient))


write_csv(traits2, "data-processed/all-seanuts-may-24-2020.csv")
