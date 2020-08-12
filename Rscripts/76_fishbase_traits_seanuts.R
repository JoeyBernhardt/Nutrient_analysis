

require(phytools)
require(MCMCglmm)
library(tidyverse)
library(rotl)
library(nlme)
library(visreg)
library(caper)

library(piecewiseSEM)
library(tidyverse)
library(rotl)
library(ape)
library(stargazer)
library(janitor)
library(MuMIn)
library(broom)
library(forcats)
library(tidyverse)
library(xtable)
library(stargazer)
library(arm)
library(nlme)
library(cowplot)
library(visreg)

library(stargazer)
library(rotl)
library(rfishbase)
fishbase_species <- read_csv("data-processed/fishbase-species.csv")

library(rfishbase)
data <- fishbase_species %>% 
  rename(species1 = matched_name2)


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

write_csv(all_traits6, "data-processed/fishbase-traits-aug2020.csv")
all_traits6 <- read_csv("data-processed/fishbase-traits-aug2020.csv")
library(readxl)
seanuts <- read_excel("data-processed/seanuts-rebuild.xlsx")

calcium <- seanuts %>% 
  filter(subgroup == "Finfish") %>% 
  filter(!is.na(zn_mg)) %>% 
  filter(part_edited %in% c("muscle", "muscle_skinless")) %>% 
  left_join(., all_traits6, by = c("genus_species" = "user_supplied_name")) %>%
  rename(feeding_level = Herbivory2) %>% 
  rename(feeding_mode = FeedingType) %>% 
  rename(length = Length) %>% 
  rename(bulk_trophic_level = FoodTroph) %>% 
  mutate(log_length = log(length)) %>% 
  mutate(log_concentration = log(zn_mg)) %>% 
  group_by(Species, feeding_mode, DemersPelag, BodyShapeI, part_edited, realm, DepthRangeDeep, AgeMatMin) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level) %>% 
  ungroup() %>% 
  filter(complete.cases(.)) %>% 
  rename(part = part_edited) %>% 
  rename(species1 = Species)



calcium$species1 <- str_to_lower(calcium$species1)
unique(calcium$part)
length(unique(calcium$species1))

cal_taxa <- tnrs_match_names(unique(calcium$species1), context="Animals", names = unique(calcium$species1), do_approximate_matching = TRUE) 

tr_cal <- tol_induced_subtree(ott_ids = ott_id(cal_taxa), label_format="name") 
tr_bl_cal <- compute.brlen(tr_cal)
phylo <- tr_bl_cal
cal2 <- calcium %>% 
  left_join(., cal_taxa, by = c("species1" = "search_string")) %>% 
  mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl_cal$tip.label)) %>% 
  ungroup() %>% 
  # mutate(feeding_level = as.factor(feeding_level)) %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>%
  mutate(BodyShapeI = as.factor(BodyShapeI)) %>%
  # mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(realm = as.factor(realm))
cal2$log_length <- scale(cal2$log_length)
cal2$bulk_trophic_level <- scale(cal2$bulk_trophic_level)
cal2$DepthRangeDeep <- scale(cal2$DepthRangeDeep)
cal2$AgeMatMin <- scale(cal2$AgeMatMin)

data <- cal2
data$sp_name <- data$unique_name2
# check overlap between tree and species in data
data$Phylospecies <- "not in tree" 
for(i in 1:nrow(data)){
  species <- data$sp_name[i]
  if(rlang::is_empty(phylo$tip.label[grepl(species,phylo$tip.label)])){ # if sp is not in tree leave "not in tree"
  } else {data$Phylospecies[i]<-phylo$tip.label[grepl(species,phylo$tip.label)]} # else put the sp name from the tree
}

length(unique(data$Phylospecies[which(!data$Phylospecies=="not in tree")])) # species in tree


#prune tree to match overlapping taxa
nameslist <- phylo$tip.label
treenameslist <- as.data.frame(table(data$Phylospecies))
Speciestoretain <- intersect(treenameslist$Var1, nameslist)
pruned.tree <- drop.tip(phylo,phylo$tip.label[-match(Speciestoretain,phylo$tip.label)])
tree <- pruned.tree
plot(tree)
is.ultrametric(tree) # has to be TRUE
is.rooted(tree) # TRUE

any(duplicated(tree$node.label)) # FALSE

tree$node.label<-NULL

#prune data to match treetips
Phylodata <- data[(data$Phylospecies %in% tree$tip.label),]

Phylodata1 <- Phylodata %>% 
  mutate(part = as.factor(part)) %>% 
  rename(species = Phylospecies) 

calg <- Phylodata1 %>% 
  group_by(species, DemersPelag, 
           # feeding_level,
           feeding_mode, BodyShapeI, realm, part) %>% 
  summarise_each(funs(mean), DepthRangeDeep, log_concentration, log_length, AgeMatMin, bulk_trophic_level) %>% 
  ungroup() %>%
  distinct(species, .keep_all = TRUE) 



calg2 <- calg %>% 
  mutate(part = as.factor(part))

calg2 <- calg2[match(tree$tip.label, calg2$species),]
row.names(calg2) <- calg2$species



mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag +
               DepthRangeDeep + AgeMatMin + BodyShapeI + realm, 
             correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")
mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag +
               DepthRangeDeep + AgeMatMin + BodyShapeI + realm, correlation = corPagel(value = 0, phy = tree, fixed = FALSE), data = calg2, method = "ML")



### full model
mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag +
               DepthRangeDeep + AgeMatMin + BodyShapeI + realm, 
             correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")

### diet model
mod1 <- gls(log_concentration ~ bulk_trophic_level + feeding_mode + DemersPelag, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")

### life history model
mod2 <- gls(log_concentration ~ log_length + AgeMatMin + BodyShapeI, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")

### habitat model
mod3 <- gls(log_concentration ~   DepthRangeDeep + realm, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")
cal_sel <- model.sel(mod1a, mod1, mod2, mod3, rank = "AIC", extra = "rsquared") 

summary(mod1a)
rsquared(mod1a)
summary(mod1a)
# visreg(mod1a)

anova(mod1a)

confints_cal <- data.frame(confint(mod2), estimate = coef(mod2)) %>% 
  mutate(term = rownames(.)) %>% 
  rename(lower = X2.5..) %>% 
  rename(upper = X97.5..)

cal_plot <- confints_cal %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
  coord_flip() +
  geom_hline(yintercept = 0) + ggtitle("Calcium")
