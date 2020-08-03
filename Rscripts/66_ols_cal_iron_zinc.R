cine_traits_new <- read_csv("data-processed/CINE-data-all.csv") %>% 
  mutate(log_concentration = log(concentration)) %>% 
  mutate(log_length = log(Length)) %>% 
  rename(bulk_trophic_level = FoodTroph) %>% 
  rename(feeding_level = Herbivory2) %>% 
  rename(feeding_mode = FeedingType)  ### ok this is with the fixed species names


traits <- read_csv("data-processed/CINE-data-all.csv") %>% 
  mutate(reference = as.character(reference)) %>% 
  gather(key = nutrient, value = concentration, 9:19) %>% 
  mutate(part = str_replace(part, "fillet", "muscle")) %>% 
  mutate(part = str_replace(part, "meat", "muscle")) %>%
  mutate(part = str_replace(part, "flesh + skin", "muscle + skin")) %>%
  mutate(part = str_replace(part, "meat + skin", "muscle + skin")) %>%
  mutate(part = str_replace(part, "flesh", "muscle")) %>%
  mutate(part = str_replace(part, "middle cut", "middle")) %>%
  mutate(part = str_replace(part, "roe", "eggs")) %>% 
  mutate(part = str_replace(part, "grease", "oil")) %>% 
  mutate(part = str_replace(part, "tail cut", "muscle")) %>% 
  mutate(part = str_replace(part, "middle", "muscle")) %>%
  mutate(part = str_replace(part, "tail end", "muscle")) %>% 
  mutate(part = str_replace(part, "head end", "muscle")) %>% 
  mutate(part = str_replace(part, "muscle, dark meat", "muscle")) %>% 
  mutate(part = str_replace(part, "muscle, light meat", "muscle")) %>% 
  mutate(part = str_replace(part, "light meat", "muscle")) %>% 
  mutate(part = str_replace(part, "white meat", "muscle")) %>% 
  mutate(part = str_replace(part, "white muscle", "muscle")) %>% 
  mutate(part = str_replace(part, "muscle, dark muscle", "muscle")) %>% 
  mutate(part = str_replace(part, "muscle, light muscle", "muscle")) %>% 
  mutate(part = str_replace(part, "dark muscle", "muscle")) %>% 
  mutate(part = str_replace(part, "light muscle", "muscle")) %>% 
  mutate(part = str_replace(part, "tail", "muscle")) %>% 
  mutate(part = ifelse(part == "muscle + small bones", "muscle + bones", part)) %>% 
  mutate(part = ifelse(part == "muscle, bone + inside", "muscle + bones", part)) %>% 
  mutate(part = str_replace(part, "muscle, cheeks", "muscle"))

unique(traits$part)
unique(traits$nutrient)


traits2 <- traits %>% 
  mutate(part = ordered(part, levels = c("muscle",
                                         "muscle + skin",
                                         "muscle + small bones",
                                         "muscle + bones",
                                         "muscle + head",
                                         "muscle, bone + inside",
                                         "whole",
                                         "whole, no skin",
                                         "head, eyes, cheeks + soft bones",
                                         "tongues + cheeks",
                                         "skin",
                                         "liver",
                                         "offal",
                                         "esophagus",
                                         "eggs",
                                         "oil",
                                         "not specified",
                                         "unknown",
                                         NA))) %>% 
  mutate(nutrient = ifelse(nutrient == "protein", "protein_g", nutrient))

#### OLS models for calcium iron and zinc

traits3 <- traits %>% 
  rename(species1 = latin_name_cleaned) %>% 
  rename(bulk_trophic_level = FoodTroph) %>% 
  rename(feeding_level = Herbivory2) %>% 
  rename(feeding_mode = FeedingType) %>% 
  mutate(log_concentration = log(concentration))

unique(traits3$part)
  
cal2 <- traits3 %>% 
  filter(nutrient == "ca_mg") %>% 
  filter(!grepl("spp", species1)) %>% 
  filter(!species1 %in% c("Pleuronectinae", "Petromyzontinae")) %>% 
  filter(part != "unknown") %>%
  filter(part != "unspecified") %>% 
  filter(part %in% c("muscle", "muscle + skin")) %>% 
  filter(complete.cases(.)) %>% 
  mutate(log_length = log(Length)) %>% 
  group_by(species1, feeding_mode, EnvTemp, DemersPelag, BodyShapeI, part) %>%
  summarise_each(funs(mean), log_concentration, Length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) %>% 
  ungroup() %>% 
  filter(complete.cases(.))


cal2$log_length <- scale(cal2$log_length, center = FALSE)
cal2$bulk_trophic_level <- scale(cal2$bulk_trophic_level,  center = FALSE)
cal2$DepthRangeDeep <- scale(cal2$DepthRangeDeep,  center = FALSE)
cal2$AgeMatMin <- scale(cal2$AgeMatMin)

mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag +
               DepthRangeDeep + AgeMatMin + BodyShapeI, data = calg2, method = "ML")
mod1a <- lm(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag +
               DepthRangeDeep + AgeMatMin + BodyShapeI + realm, data = cal2)
summary(mod1a)

lambda <- round(0, digits = 2)

visreg(mod1a)
rsq_mod1a <- round(rsquared(mod1a)['R.squared'][[1]], digits = 2)

confints_cal <- data.frame(confint(mod1a), estimate = coef(mod1a)) %>% 
  mutate(term = rownames(.)) %>% 
  rename(lower = X2.5..) %>% 
  rename(upper = X97.5..)

stargazer(mod1a, title = "", type = "html", out="tables/calcium-models-expanded-muscle-only-pgls-lambda-muscleskin-cine.htm", 
          add.lines = list(c("R2", rsq_mod1a), c("Lamba", lambda)), ci=TRUE, ci.level=0.95, digits = 2)



# iron --------------------------------------------------------------------

cal2 <- traits2 %>% 
  filter(nutrient == "fe_mg") %>% 
  filter(!grepl("spp", species1)) %>%
  # filter(!species1 %in% c("Pleuronectinae", "Petromyzontinae", "Ensis directus", "Osmerus mordax")) %>% 
  filter(part != "unknown") %>%
  filter(part != "unspecified") %>% 
  filter(part %in% c("muscle", "muscle + skin")) %>% 
  group_by(species1, feeding_mode, EnvTemp, DemersPelag, BodyShapeI, part, realm) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) 


cal2$log_length <- scale(cal2$log_length)
cal2$bulk_trophic_level <- scale(cal2$bulk_trophic_level)
cal2$DepthRangeDeep <- scale(cal2$DepthRangeDeep)
cal2$AgeMatMin <- scale(cal2$AgeMatMin)


mod1a <- lm(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag +
              DepthRangeDeep + AgeMatMin + BodyShapeI + realm, data = cal2)
summary(mod1a)

confints_iron <- data.frame(confint(mod1a), estimate = coef(mod1a)) %>% 
  mutate(term = rownames(.)) %>% 
  rename(lower = X2.5..) %>% 
  rename(upper = X97.5..)


# zinc --------------------------------------------------------------------

cal2 <- traits2 %>% 
  filter(nutrient == "zn_mg") %>% 
  filter(!grepl("spp", species1)) %>% 
  # filter(!species1 %in% c("Pleuronectinae", "Petromyzontinae", "Ensis directus", "Osmerus mordax")) %>% 
  filter(part != "unknown") %>%
  filter(part != "unspecified") %>% 
  filter(part %in% c("muscle", "muscle + skin")) %>% 
  group_by(species1, feeding_mode, EnvTemp, DemersPelag, BodyShapeI, part, realm) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) 


cal2$log_length <- scale(cal2$log_length)
cal2$bulk_trophic_level <- scale(cal2$bulk_trophic_level)
cal2$DepthRangeDeep <- scale(cal2$DepthRangeDeep)
cal2$AgeMatMin <- scale(cal2$AgeMatMin)


mod1a <- lm(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag +
              DepthRangeDeep + AgeMatMin + BodyShapeI + realm, data = cal2)
summary(mod1a)

confints_zinc <- data.frame(confint(mod1a), estimate = coef(mod1a)) %>% 
  mutate(term = rownames(.)) %>% 
  rename(lower = X2.5..) %>% 
  rename(upper = X97.5..)


cf_zinc <- confints_zinc %>% 
  mutate(nutrient = "zinc")

cf_epa <- confints_epa %>% 
  mutate(nutrient = "epa")

cf_dha <- confints_dha %>% 
  mutate(nutrient = "dha")


cf_cal <- confints_cal %>% 
  mutate(nutrient = "calcium")

cf_iron <- confints_iron %>% 
  mutate(nutrient = "iron")

all_cf <- bind_rows(cf_cal, cf_iron, cf_epa, cf_dha, cf_zinc)


all_cf %>% 
  filter(term != "(Intercept)") %>% 
  mutate(nutrient = factor(nutrient, levels = c("calcium", "iron", "zinc", "epa", "dha"))) %>% 
  # filter(nutrient %in% c("iron", "zinc", "calcium")) %>% 
  ggplot(aes(x = term, y = estimate, color = nutrient)) + 
  geom_pointrange(aes(x = term, y = estimate, color = nutrient, ymin = lower, ymax = upper), position = position_dodge(width = 1)) +
  coord_flip() +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("dodgerblue", "cadetblue", "blue", "darkorange", "darkred"))
