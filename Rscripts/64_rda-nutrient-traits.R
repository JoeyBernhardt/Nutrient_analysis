

#### traits RDA

library(tidyverse)
library(vegan)

traits <- read_csv("data-processed/all-seanuts-may-24-2020-2.csv") %>% ### this is more updated with fixed parts and reksten data
  rename(species1 = Species) %>% 
  rename(feeding_level = Herbivory2) %>% 
  rename(feeding_mode = FeedingType) %>% 
  rename(length = Length) %>% 
  rename(bulk_trophic_level = FoodTroph) %>% 
  mutate(log_length = log(length)) %>% 
  mutate(log_concentration = log(concentration)) 


View(traits)
prot_table <- traits %>% 
  dplyr::select(species1, feeding_mode, EnvTemp, DemersPelag, BodyShapeI, part, realm, log_concentration,
                log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin, nutrient) %>% 
  mutate(log_length = scale(log_length)) %>% 
  mutate(bulk_trophic_level = scale(bulk_trophic_level)) %>% 
  mutate(DepthRangeDeep = scale(DepthRangeDeep)) %>% 
  mutate(AgeMatMin = scale(AgeMatMin))

trait_table <- traits %>% 
  dplyr::select(species1, feeding_mode, EnvTemp, DemersPelag, BodyShapeI, part, realm,
                log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) %>% 
  mutate(log_length = scale(log_length)) %>% 
  mutate(bulk_trophic_level = scale(bulk_trophic_level)) %>% 
  mutate(DepthRangeDeep = scale(DepthRangeDeep)) %>% 
  mutate(AgeMatMin = scale(AgeMatMin)) %>% 
  distinct(.keep_all = TRUE)

nut_table <- prot_table %>% 
  dplyr::select(species1, nutrient, log_concentration) %>% 
  mutate(concentration = exp(log_concentration)) %>% 
  group_by(species1, nutrient) %>% 
  summarise(mean_con = mean(concentration)) %>%
  spread(key = nutrient, value = mean_con) %>% 
  mutate(protein = ifelse(is.na(protein), protein_g, protein)) %>% 
  dplyr::select(species1, ca_mg, dha, epa, fe_mg, zn_mg) %>% 
  ungroup() %>% 
  filter(complete.cases(.))

trait_nuts_table <- left_join(nut_table, trait_table, by= "species1") %>% 
  filter(complete.cases(.))

prot2 <- trait_nuts_table %>% 
  dplyr::select(2:6)



spe <- dplyr::select(prot_table, -Treatment, -Strain)
FULL.cap <- capscale(prot2~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag +
                       DepthRangeDeep + AgeMatMin + BodyShapeI + realm, data=trait_nuts_table)
# FULL.cap <- capscale(spe ~ Treatment, data=prot_table)


anova.cca(FULL.cap, by = "axis", step = 1000)
anova.cca(FULL.cap)

coef(FULL.cap) ### gives the the equivalent of regression coefficients for each explanatory variable
# on each canonical axis

RsquareAdj(FULL.cap)$adj.r.squared
RsquareAdj(FULL.cap)$r.squared

dist_test_cap1 <- aov(lm(CAP1~bulk_trophic_level + log_length  + feeding_mode + DemersPelag +
                           DepthRangeDeep + AgeMatMin + BodyShapeI + realm, data=Res.dim))
summary(dist_test_cap1)
TukeyHSD(dist_test_cap1)

CAP1 <- scores(FULL.cap, display="wa", scaling=3)[,1]
CAP2 <- scores(FULL.cap, display="wa", scaling=3)[,2]
Res.dim <- as.data.frame(scores(FULL.cap, display="wa", scaling=3)[,1:2])
Res.dim$Strain <- prot_table$Strain
Res.dim$Treatment <- prot_table$Treatment
Res.dim
trait_vectors <- as.data.frame(scores(FULL.cap, choices = 1:2, display = "sp", scaling = 2)) %>% 
  mutate(trait = rownames(.)) 
dims_summary <- dims %>% 
  dplyr::group_by(Treatment) %>% 
  summarise(CAP1mean=mean(CAP1), CAP2mean=mean(CAP2), CAP1sd=sd(CAP1), CAP2sd=sd(CAP2)) 

ggplot() + 
  geom_segment(aes(x = 0, y = 0, xend = CAP1, yend = CAP2, text =  trait), data = trait_vectors, color = "black",
               arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  geom_hline(yintercept = 0, color = "grey") + geom_vline(xintercept = 0, color = "grey") +
  # geom_errorbar(aes(x = CAP1mean, ymin = CAP2mean - CAP2sd, ymax = CAP2mean + CAP2sd), width = 0, data = dims_summary, size = 1) +
  # geom_errorbarh(aes(y = CAP2mean, xmin = CAP1mean - CAP1sd, xmax = CAP1mean + CAP1sd), width = 0, data = dims_summary, size = 1) +
  geom_point(aes(x = CAP1, y = CAP2), data = dims, size = 4) +
  geom_point(aes(x = CAP1, y = CAP2), data = dims, size = 4, shape = 1, color = "black")

