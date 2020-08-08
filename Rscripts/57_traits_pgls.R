

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
traits <- read_csv("data-processed/cine-traits-new-species3.csv")
traits <- read_csv("data-processed/trait-nutrient-data-analysis.csv") ### this is the latest version as of May 24 2020
traits_nut <- read_csv("data-processed/all-seanuts-may-24-2020-2.csv") %>% ### this is more updated with fixed parts and reksten data
  rename(species1 = Species) %>% 
  rename(feeding_level = Herbivory2) %>% 
  rename(feeding_mode = FeedingType) %>% 
  rename(length = Length) %>% 
  rename(bulk_trophic_level = FoodTroph) %>% 
  mutate(log_length = log(length)) %>% 
  mutate(log_concentration = log(concentration)) 

unique(traits_nut$part)

  mtraits <- read_csv("data-processed/mtraits_nuts.csv") %>% ### this is more updated with fixed parts and reksten data
  rename(species1 = Species) %>% 
  rename(feeding_level = Herbivory2) %>% 
  rename(feeding_mode = FeedingType) %>% 
  rename(length = Length) %>% 
  rename(bulk_trophic_level = FoodTroph) %>% 
  mutate(log_length = log(length)) %>% 
  mutate(log_concentration = log(concentration)) %>% 
  rename(part = form_clean)

unique(mtraits$part)

mtraits %>% 
  filter(!is.na(concentration)) %>% 
  distinct(part) %>% View

mspecies <- mtraits %>% 
  group_by(species1, nutrient) %>% 
  filter(!is.na(concentration)) %>% 
  summarise(concentration = mean(concentration)) %>% 
  filter(!is.na(concentration)) %>% 
  distinct(species1)

 names(mtraits)
names(traits_nut)

traits1b <- bind_rows(traits_nut, mtraits)

str(traits)
parts_list <- c(unique(traits$part))


traits_old %>% 
  filter(nutrient == "ca_mg") %>% 
  filter(part == "muscle") %>% 
  dplyr::select(species1, feeding_mode, EnvTemp, DemersPelag, BodyShapeI, part, realm, log_concentration,
                log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) %>% 
  filter(complete.cases(.)) %>%
  distinct(species1) %>% 
  tally()

traits_new2 <- traits %>% 
  filter(nutrient == "ca_mg") %>% 
  filter(part %in% c("muscle", "muscle + skin")) %>% 
  dplyr::select(species1, feeding_mode, EnvTemp, DemersPelag, BodyShapeI, part, realm, log_concentration,
                log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin, nutrient) %>% 
  filter(complete.cases(.)) 


traits_nut <- traits1b
traits2 <- traits_nut %>% 
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

unique(traits2$nutrient)
unique(traits2$part)
unique(traits)

traits2 %>% 
  group_by(nutrient) %>% 
  tally() %>% View

traits2 %>% 
  filter(nutrient == "ca_mg") %>% View
  ggplot(aes(x = part, y = concentration)) + geom_point() +
  facet_wrap( ~ nutrient, nrow = 4)


traits2 %>% 
  mutate(outlier = ifelse(nutrient %in% c("epa", "dha") & concentration > 20, "outlier", "fine")) %>% 
  # mutate(outlier = ifelse(nutrient == "dha" & concentration > 100, "outlier", "fine")) %>% 
  filter(outlier != "outlier") %>% 
  filter(nutrient %in% c("protein_g", "fat_g", "ca_mg", "fe_mg", "zn_mg", "epa", "dha")) %>% 
  ggplot(aes(x = concentration)) + geom_histogram() +
  facet_wrap( ~ nutrient, scale = "free")
ggsave("figures/nutrient-ranges.png", width = 8, height = 6)

# Calcium muscle only -----------------------------------------------------


calcium <- traits2 %>% 
  filter(nutrient == "ca_mg") %>% 
  filter(!is.na(concentration)) %>% 
  filter(!grepl("spp", species1)) %>% 
  filter(!species1 %in% c("Pleuronectinae", "Petromyzontinae", "Ensis directus", "Osmerus mordax")) %>% 
  filter(part %in% c("muscle", "muscle + skin")) %>% 
  group_by(species1, feeding_mode, EnvTemp, DemersPelag, BodyShapeI, part, realm, feeding_level) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) %>% 
  ungroup() %>% 
  filter(complete.cases(.))



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
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
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
  group_by(species, EnvTemp, DemersPelag, 
           feeding_level,
           feeding_mode, BodyShapeI, realm, part) %>% 
  summarise_each(funs(mean), DepthRangeDeep, log_concentration, log_length, AgeMatMin, bulk_trophic_level) %>% 
  ungroup() %>%
  mutate(EnvTemp = as.character(EnvTemp)) %>% 
  distinct(species, .keep_all = TRUE) 

calg2 <- calg %>% 
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(part = as.factor(part))

calg2 <- calg2[match(tree$tip.label, calg2$species),]
row.names(calg2) <- calg2$species
cal_data <- calg2
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

confints_cal <- data.frame(confint(mod1a), estimate = coef(mod1a)) %>% 
  mutate(term = rownames(.)) %>% 
  rename(lower = X2.5..) %>% 
  rename(upper = X97.5..)
  
confints_cal %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
  coord_flip() +
  geom_hline(yintercept = 0) + ggtitle("Calcium")


lambda <- round(mod1a$modelStruct[[1]][[1]], digits = 2)

rsq_mod1a <- round(rsquared(mod1a)['R.squared'][[1]], digits = 2)

stargazer(mod1a, title = "", type = "html", out="tables/calcium-models-expanded-muscle-only-pgls-lambda-muscleskin4.htm", 
          add.lines = list(c("R2", rsq_mod1a), c("Lamba", lambda)), ci=TRUE, ci.level=0.95, digits = 2, single.row = TRUE)


# Iron muscle only --------------------------------------------------------

calcium <- traits2 %>% 
  filter(nutrient == "fe_mg") %>% 
  filter(!grepl("spp", species1)) %>% 
  filter(!species1 %in% c("Pleuronectinae", "Petromyzontinae", "Ensis directus", "Osmerus mordax")) %>% 
  filter(part %in% c("muscle", "muscle + skin")) %>%
  filter(!is.na(concentration)) %>% 
  group_by(species1, feeding_mode, feeding_level, EnvTemp, DemersPelag, BodyShapeI, part, realm) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) %>% 
  ungroup() %>%
  filter(complete.cases(.))




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
  mutate(feeding_level = as.factor(feeding_level)) %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>%
  mutate(BodyShapeI = as.factor(BodyShapeI)) %>%
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
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
  group_by(species, EnvTemp, DemersPelag, feeding_level, feeding_mode, BodyShapeI, realm, part) %>% 
  summarise_each(funs(mean), DepthRangeDeep, log_concentration, log_length, AgeMatMin, bulk_trophic_level) %>% 
  ungroup() %>%
  mutate(EnvTemp = as.character(EnvTemp)) %>% 
  distinct(species, .keep_all = TRUE) 

calg2 <- calg %>% 
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(part = as.factor(part))

calg2 <- calg2[match(tree$tip.label, calg2$species),]
row.names(calg2) <- calg2$species

iron_data <- calg2

##full model

mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length + feeding_mode + DemersPelag +
             + DepthRangeDeep + AgeMatMin + BodyShapeI + realm, correlation = corPagel(value = 0, phy = tree, fixed = TRUE),
             data = calg2, method = "ML")
mod1b <- gls(log_concentration ~ 1, correlation = corPagel(value = 0, phy = tree, fixed = TRUE),
             data = calg2, method = "ML")
AIC(mod1a, mod1b)

### diet model
mod1 <- gls(log_concentration ~ bulk_trophic_level + feeding_mode + DemersPelag, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")

### life history model
mod2 <- gls(log_concentration ~ log_length + AgeMatMin + BodyShapeI, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")

### habitat model
mod3 <- gls(log_concentration ~  DepthRangeDeep + realm, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")
iron_sel <- model.sel(mod1a, mod1, mod2, mod3, rank = "AIC") 

### for iron, it looks like the full model is the best
rsquared(mod3)

mod1b <- gls(log_concentration ~  1, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")

lambda <- round(mod1a$modelStruct[[1]][[1]], digits = 2)
rsq_mod1a <- round(rsquared(mod1a)['R.squared'][[1]], digits = 2)

stargazer(mod1a, title = "", type = "html", out="tables/iron-models-expanded-muscle-only-pgls-muscleskin4.htm", 
          add.lines = list(c("R2", rsq_mod1a), c("Lamba", lambda)), ci=TRUE, ci.level=0.95, digits = 2, single.row = TRUE)

anova(mod1a)

confints_iron <- data.frame(confint(mod1a), estimate = coef(mod1a)) %>% 
  mutate(term = rownames(.)) %>% 
  rename(lower = X2.5..) %>% 
  rename(upper = X97.5..)

iron_plot <- confints_iron %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
  coord_flip() +
  geom_hline(yintercept = 0) + ggtitle("Iron")


# Zinc muscle only --------------------------------------------------------


calcium <- traits2 %>% 
  filter(!is.na(concentration)) %>% 
  filter(nutrient == "zn_mg") %>% 
  filter(!grepl("spp", species1)) %>% 
  filter(!species1 %in% c("Pleuronectinae", "Petromyzontinae", "Ensis directus", "Osmerus mordax")) %>% 
  filter(part != "unknown") %>%
  filter(part != "unspecified") %>% 
  filter(part %in% c("muscle", "muscle + skin")) %>% 
  group_by(species1, feeding_mode, feeding_level, EnvTemp, DemersPelag, BodyShapeI, part, realm) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) %>% 
  ungroup() %>%
  filter(complete.cases(.))


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
  mutate(feeding_level = as.factor(feeding_level)) %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>%
  mutate(BodyShapeI = as.factor(BodyShapeI)) %>%
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
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
  group_by(species, EnvTemp, DemersPelag, feeding_level, feeding_mode, BodyShapeI, realm, part) %>% 
  summarise_each(funs(mean), DepthRangeDeep, log_concentration, log_length, AgeMatMin, bulk_trophic_level) %>% 
  ungroup() %>%
  mutate(EnvTemp = as.character(EnvTemp)) %>% 
  distinct(species, .keep_all = TRUE) 

calg2 <- calg %>% 
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(part = as.factor(part))

calg2 <- calg2[match(tree$tip.label, calg2$species),]
row.names(calg2) <- calg2$species

zinc_data <- calg2

mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length + feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")
mod1b <- gls(log_concentration ~  1, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")

model.sel(mod1a, mod1b, extra = "rsquared", rank = "AIC")

mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length + feeding_mode + DemersPelag
             + DepthRangeDeep + AgeMatMin + BodyShapeI + realm, correlation = corPagel(value = 0, phy = tree, fixed = TRUE),
             data = calg2, method = "ML")
### diet model
mod1 <- gls(log_concentration ~ bulk_trophic_level + feeding_mode + DemersPelag, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")

### life history model
mod2 <- gls(log_concentration ~ log_length + AgeMatMin + BodyShapeI, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")

### habitat model
mod3 <- gls(log_concentration ~  DepthRangeDeep + realm, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")
zinc_sel <- model.sel(mod1a, mod1, mod2, mod3, rank = "AIC", extra = "rsquared") 
confint(model.avg(mod2, mod3))
confint(mod1a)
lambda <- round(mod1a$modelStruct[[1]][[1]], digits = 2)

# visreg(mod1a)
rsq_mod1a <- round(rsquared(mod1a)['R.squared'][[1]], digits = 2)

stargazer(mod1a, title = "", type = "html", out="tables/zinc-models-expanded-muscle-only-pgls-muscle-skin6.htm", 
          add.lines = list(c("R2", rsq_mod1a), c("Lamba", lambda)), ci=TRUE, ci.level=0.95, digits = 2, single.row = TRUE)

anova(mod1a)

confints_zinc <- data.frame(confint(mod1a), estimate = coef(mod1a)) %>% 
  mutate(term = rownames(.)) %>% 
  rename(lower = X2.5..) %>% 
  rename(upper = X97.5..)

zinc_plot <- confints_zinc %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
  coord_flip() +
  geom_hline(yintercept = 0) + ggtitle("Zinc")



# epa muscle only --------------------------------------------------------


calcium <- traits2 %>% 
  filter(nutrient == "epa") %>% 
  mutate(concentration = ifelse(is.na(concentration), exp(log_concentration), concentration)) %>% 
  mutate(species1 = ifelse(is.na(species1), latin_name, species1)) %>% 
  filter(!grepl("spp", species1)) %>% 
  filter(!species1 %in% c("Pleuronectinae", "Petromyzontinae", "Ensis directus", "Osmerus mordax")) %>% 
  filter(part != "unknown") %>%
  filter(part != "unspecified") %>% 
  filter(part %in% c("muscle", "muscle + skin")) %>% 
  group_by(species1, feeding_mode, feeding_level, EnvTemp, DemersPelag, BodyShapeI, part, realm) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) %>% 
  ungroup() %>%
  filter(complete.cases(.))


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
  mutate(feeding_level = as.factor(feeding_level)) %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>%
  mutate(BodyShapeI = as.factor(BodyShapeI)) %>%
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
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
  group_by(species, EnvTemp, DemersPelag, feeding_level, feeding_mode, BodyShapeI, realm, part) %>% 
  summarise_each(funs(mean), DepthRangeDeep, log_concentration, log_length, AgeMatMin, bulk_trophic_level) %>% 
  ungroup() %>%
  mutate(EnvTemp = as.character(EnvTemp)) %>% 
  distinct(species, .keep_all = TRUE) 

calg2 <- calg %>% 
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(part = as.factor(part))

calg2 <- calg2[match(tree$tip.label, calg2$species),]
row.names(calg2) <- calg2$species
epa_data <- calg2

mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length+ feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm, correlation = corPagel(value = 0, phy = tree, fixed = FALSE), data = calg2, method = "ML")
mod1b <- gls(log_concentration ~  1, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")

model.sel(mod1a, mod1b, extra = "rsquared", rank = "AIC") 

mod1ab <- gls(log_concentration ~ bulk_trophic_level + log_length+ feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm, correlation = corPagel(value = 0.7, phy = tree, fixed = TRUE), data = calg2, method = "ML")


### diet model
mod1 <- gls(log_concentration ~ bulk_trophic_level + feeding_mode + DemersPelag, correlation = corPagel(value = 0.7, phy = tree, fixed = TRUE), data = calg2, method = "ML")

### life history model
mod2 <- gls(log_concentration ~ log_length + AgeMatMin + BodyShapeI, correlation = corPagel(value = 0.7, phy = tree, fixed = TRUE), data = calg2, method = "ML")

### habitat model
mod3 <- gls(log_concentration ~  DepthRangeDeep + realm, correlation = corPagel(value = 0.7, phy = tree, fixed = TRUE), data = calg2, method = "ML")
epa_sel <- model.sel(mod1ab, mod1, mod2, mod3, rank = "AIC", extra = "rsquared") 



summary(mod1a)
lambda <- round(mod1a$modelStruct[[1]][[1]], digits = 2)

# visreg(mod1a)
rsq_mod1a <- round(rsquared(mod1a)['R.squared'][[1]], digits = 2)

stargazer(mod1a, title = "", type = "html", out="tables/epa-models-expanded-muscle-only-pgls-muscle-skin.htm", 
          add.lines = list(c("R2", rsq_mod1a), c("Lamba", lambda)), ci=TRUE, ci.level=0.95, digits = 2)

anova(mod1a)

confints_epa <- data.frame(confint(mod1a), estimate = coef(mod1a)) %>% 
  mutate(term = rownames(.)) %>% 
  rename(lower = X2.5..) %>% 
  rename(upper = X97.5..)

epa_plot <- confints_epa %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
  coord_flip() +
  geom_hline(yintercept = 0) + ggtitle("EPA")


# dha muscle only --------------------------------------------------------


calcium <- traits2 %>% 
  dplyr::select(-seanuts_id2) %>% 
  filter(nutrient == "dha") %>% 
  filter(!grepl("spp", species1)) %>% 
  filter(!species1 %in% c("Pleuronectinae", "Petromyzontinae", "Ensis directus", "Osmerus mordax")) %>% 
  filter(part != "unknown") %>%
  filter(part != "unspecified") %>% 
  filter(part %in% c("muscle", "muscle + skin")) %>% 
  group_by(species1, feeding_mode, feeding_level, EnvTemp, DemersPelag, BodyShapeI, part, realm) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) %>% 
  ungroup() %>% 
  filter(complete.cases(.))


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
  mutate(feeding_level = as.factor(feeding_level)) %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>%
  mutate(BodyShapeI = as.factor(BodyShapeI)) %>%
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
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
  group_by(species, EnvTemp, DemersPelag, feeding_level, feeding_mode, BodyShapeI, realm, part) %>% 
  summarise_each(funs(mean), DepthRangeDeep, log_concentration, log_length, AgeMatMin, bulk_trophic_level) %>% 
  ungroup() %>%
  mutate(EnvTemp = as.character(EnvTemp)) %>% 
  distinct(species, .keep_all = TRUE) 

calg2 <- calg %>% 
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(part = as.factor(part))

calg2 <- calg2[match(tree$tip.label, calg2$species),]
row.names(calg2) <- calg2$species

dha_data <- calg2
mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length+ feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm, correlation = corPagel(value = 0.7, phy = tree, fixed = TRUE), data = calg2, method = "ML")
mod1b <- gls(log_concentration ~ bulk_trophic_level + log_length+ feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm, correlation = corPagel(value = 0.8, phy = tree, fixed = TRUE), data = calg2, method = "ML")
mod1c <- gls(log_concentration ~ bulk_trophic_level + log_length+ feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm, correlation = corPagel(value = 0.5, phy = tree, fixed = TRUE), data = calg2, method = "ML")
mod1d <- gls(log_concentration ~ bulk_trophic_level + log_length+ feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm, correlation = corPagel(value = 0.6, phy = tree, fixed = TRUE), data = calg2, method = "ML")
mod1e <- gls(log_concentration ~ bulk_trophic_level + log_length+ feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm, correlation = corPagel(value = 0.2, phy = tree, fixed = TRUE), data = calg2, method = "ML")
mod1f <- gls(log_concentration ~ bulk_trophic_level + log_length+ feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")


AIC(mod1a, mod1b, mod1c, mod1d, mod1e, mod1f)

mod1b <- gls(log_concentration ~  1, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")

model.sel(mod1a, mod1b, extra = "rsquared", rank = "AIC") %>% View

### this is the one
mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag +
               DepthRangeDeep + AgeMatMin + BodyShapeI + realm, correlation = corPagel(value = 0.6, phy = tree, fixed = TRUE), data = calg2, method = "ML")

### diet model
mod1 <- gls(log_concentration ~ bulk_trophic_level + feeding_mode + DemersPelag, correlation = corPagel(value = 0.6, phy = tree, fixed = TRUE), data = calg2, method = "ML")

### life history model
mod2 <- gls(log_concentration ~ log_length + AgeMatMin + BodyShapeI, correlation = corPagel(value = 0.6, phy = tree, fixed = TRUE), data = calg2, method = "ML")

### habitat model
mod3 <- gls(log_concentration ~  DepthRangeDeep + realm, correlation = corPagel(value = 0.6, phy = tree, fixed = TRUE), data = calg2, method = "ML")
dha_sel <- model.sel(mod1a, mod1, mod2, mod3, rank = "AIC", extra = "rsquared") 


####
mod1 <- gls(log_concentration ~ bulk_trophic_level + feeding_mode, correlation = corPagel(value = 0, phy = tree, fixed = FALSE), data = calg2, method = "ML")
mod2 <- gls(log_concentration ~ log_length + AgeMatMin + BodyShapeI, correlation = corPagel(value = 0, phy = tree, fixed = FALSE), data = calg2, method = "ML")
mod3 <- gls(log_concentration ~  DemersPelag + DepthRangeDeep + realm, correlation = corPagel(value = 0, phy = tree, fixed = FALSE), data = calg2, method = "ML")
mod4 <- gls(log_concentration ~  1, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")

model.sel(mod1a, mod1, mod2, mod3, mod4, rank = "AIC") %>% View



AIC(mod1a, mod1b)
summary(mod1a)
lambda <- round(mod1a$modelStruct[[1]][[1]], digits = 2)

# visreg(mod1a)
rsq_mod1a <- round(rsquared(mod1a)['R.squared'][[1]], digits = 2)

stargazer(mod1a, title = "", type = "html", out="tables/dha-models-expanded-muscle-only-pgls-muscle-skin.htm", 
          add.lines = list(c("R2", rsq_mod1a), c("Lamba", lambda)), ci=TRUE, ci.level=0.95, digits = 2)

anova(mod1a)
theme_set(theme_cowplot())
confints_dha <- data.frame(confint(mod1a), estimate = coef(mod1a)) %>% 
  mutate(term = rownames(.)) %>% 
  rename(lower = X2.5..) %>% 
  rename(upper = X97.5..)

dha_plot <- confints_dha %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
  coord_flip() +
  geom_hline(yintercept = 0) + ggtitle("DHA")



# plot all micronutrients -------------------------------------------------
library(patchwork)
micro_plots <- epa_plot + dha_plot + zinc_plot + cal_plot + iron_plot
ggsave(plot = micro_plots, filename = "figures/microplots.png", width = 30, height = 14)

devtools::install_github("ricardo-bion/ggradar", 
                         dependencies = TRUE)

library(ggradar)

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
write_csv(all_cf, "data-processed/all-cfs-traits-pgls-cine.csv")

all_cf <- read_csv("data-processed/all-cfs-traits-pgls-cine.csv")
all_cf %>% 
  filter(term != "(Intercept)") %>% 
  filter(nutrient %in% c("iron", "zinc", "calcium")) %>% 
  ggplot(aes(x = term, y = estimate, color = nutrient)) + 
  geom_pointrange(aes(x = term, y = estimate, color = nutrient, ymin = lower, ymax = upper), position = position_dodge(width = 1)) +
  coord_flip() +
  geom_hline(yintercept = 0) +
  scale_color_viridis_d(option = "inferno", begin = 0.6, end = 0.8)
ggsave("figures/coef-iron-zinc-cal.png", width = 8, height = 10)

all_cf %>% 
  filter(term != "(Intercept)") %>% 
  filter(nutrient %in% c("epa", "dha")) %>% 
  ggplot(aes(x = term, y = estimate, color = nutrient)) + 
  geom_pointrange(aes(x = term, y = estimate, color = nutrient, ymin = lower, ymax = upper), position = position_dodge(width = 1)) +
  coord_flip() +
  geom_hline(yintercept = 0) +
  scale_color_viridis_d(option = "inferno", begin = 0.2, end = 0.5)
ggsave("figures/coef-epa-dha.png", width = 8, height = 10)

write_csv(all_cf, "data-processed/old-traits-coefs-new.csv")
all_cf2 <- read_csv("data-processed/old-traits-coefs.csv")
theme_set(theme_cowplot())


terms <- data.frame(terms = unique(all_cf$term))
write_csv(terms, "data-processed/terms-traits.csv")

new_terms <- read_csv("data-processed/terms-traits-new.csv") %>% 
  filter(terms %in% c(all_cf2$term))
all_cf$term2 <- new_terms$new_term



all_cf2 %>% View
  mutate(variable = NA) %>% 
  mutate(nutrient = str_replace(nutrient, "epa", "EPA")) %>% 
  mutate(nutrient = str_replace(nutrient, "dha", "DHA")) %>% 
  mutate(nutrient = str_replace(nutrient, "calcium", "Calcium")) %>% 
  mutate(nutrient = str_replace(nutrient, "zinc", "Zinc")) %>% 
  mutate(nutrient = str_replace(nutrient, "iron", "Iron")) %>% 
mutate(nutrient_type = ifelse(nutrient %in% c("EPA", "DHA"), "fatty acids", "elements")) %>% 
  mutate(variable = case_when(grepl("feeding", term) ~ "diet",
                              grepl("trophic", term) ~ "diet",
                              grepl("Demers", term) ~ "diet",
                              grepl("Depth", term) ~ "habitat",
                              grepl("realm", term) ~ "habitat",
                              grepl("length", term) ~ "morphology",
                              grepl("Age", term) ~ "morphology",
                              grepl("Shape", term) ~ "morphology",
                         TRUE ~ "NA")) %>% 
  mutate(variable = factor(variable, levels = c("morphology", "habitat", "diet"))) %>% 
  filter(term != "(Intercept)") %>% 
  # #filter(variable %in% c("diet", "morphology")) %>% 
  #  filter(variable %in% c("habitat")) %>% 
  mutate(nutrient = factor(nutrient, levels = c("Calcium", "Iron", "Zinc", "EPA", "DHA"))) %>% 
  # filter(nutrient %in% c("iron", "zinc", "calcium")) %>% 
  ggplot(aes(x = term2, y = estimate, color = variable)) + 
  geom_pointrange(aes(x = term2, y = estimate, color = variable, ymin = lower, ymax = upper), position = position_dodge(width = 1)) +
  
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("dodgerblue", "cadetblue", "blue", "darkorange", "darkred")) +
  theme(text = element_text(size=20),
        axis.text=element_text(size=16)) + 
  theme(axis.title.y=element_blank()) +
  facet_grid(variable ~ nutrient, scales = "free", space = "free_y") +
  theme(strip.background =element_rect(fill="transparent"))+
  coord_flip() +
  # theme(panel.spacing = unit(8, "lines")) +
  theme(legend.position = "none") + xlab("Estimate")
ggsave("figures/coef-all-micronutrients-all-colors-up.png", width = 14, height = 7)
ggsave("figures/coef-all-micronutrients-diet-morph.png", width = 8, height = 8)
ggsave("figures/coef-all-micronutrients-habitat.png", width = 4, height = 8)
ggsave("figures/coef-all-micronutrients-2new.png", width = 8, height = 14)

ggradar(mtcars_radar)

# protein muscle only --------------------------------------------------------


calcium <- traits2 %>% 
  dplyr::select(-seanuts_id2) %>% 
  filter(nutrient %in% c("protein", "protein_g")) %>% 
  filter(!grepl("spp", species1)) %>% 
  filter(!species1 %in% c("Pleuronectinae", "Petromyzontinae", "Ensis directus", "Osmerus mordax")) %>% 
  filter(part != "unknown") %>%
  filter(part != "unspecified") %>% 
  filter(part == "muscle") %>% 
  group_by(species1, feeding_mode, feeding_level, EnvTemp, DemersPelag, BodyShapeI, part, realm) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) %>% 
  ungroup() %>%
  filter(complete.cases(.))
str(calcium)

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
  mutate(feeding_level = as.factor(feeding_level)) %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>%
  mutate(BodyShapeI = as.factor(BodyShapeI)) %>%
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
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
  group_by(species, EnvTemp, DemersPelag, feeding_level, feeding_mode, BodyShapeI, realm, part) %>% 
  summarise_each(funs(mean), DepthRangeDeep, log_concentration, log_length, AgeMatMin, bulk_trophic_level) %>% 
  ungroup() %>%
  mutate(EnvTemp = as.character(EnvTemp)) %>% 
  distinct(species, .keep_all = TRUE) 

calg2 <- calg %>% 
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(part = as.factor(part))

calg2 <- calg2[match(tree$tip.label, calg2$species),]

row.names(calg2) <- calg2$species

mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length + feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")
mod1b <- gls(log_concentration ~  1, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")

model.sel(mod1a, mod1b, extra = "rsquared", rank = "AIC") %>% View
AIC(mod1a, mod1b)
summary(mod1a)
lambda <- round(mod1a$modelStruct[[1]][[1]], digits = 2)

# visreg(mod1a)
rsq_mod1a <- round(rsquared(mod1a)['R.squared'][[1]], digits = 2)

stargazer(mod1a, title = "", type = "html", out="tables/protein-models-expanded-muscle-only-pgls.htm", 
          add.lines = list(c("R2", rsq_mod1a), c("Lamba", lambda)), ci=TRUE, ci.level=0.95, digits = 2)

anova(mod1a)


# fat muscle only --------------------------------------------------------


calcium <- traits2 %>% 
  dplyr::select(-seanuts_id2) %>% 
  filter(nutrient == "fat_g") %>% 
  filter(!grepl("spp", species1)) %>% 
  filter(!species1 %in% c("Pleuronectinae", "Petromyzontinae", "Ensis directus", "Osmerus mordax")) %>% 
  filter(part != "unknown") %>%
  filter(part != "unspecified") %>% 
  filter(part == "muscle") %>% 
  group_by(species1, feeding_mode, feeding_level, EnvTemp, DemersPelag, BodyShapeI, part, realm) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) %>% 
  ungroup() %>%
  filter(complete.cases(.))
str(calcium)

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
  mutate(feeding_level = as.factor(feeding_level)) %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>%
  mutate(BodyShapeI = as.factor(BodyShapeI)) %>%
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
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
  group_by(species, EnvTemp, DemersPelag, feeding_level, feeding_mode, BodyShapeI, realm, part) %>% 
  summarise_each(funs(mean), DepthRangeDeep, log_concentration, log_length, AgeMatMin, bulk_trophic_level) %>% 
  ungroup() %>%
  mutate(EnvTemp = as.character(EnvTemp)) %>% 
  distinct(species, .keep_all = TRUE) 

calg2 <- calg %>% 
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(part = as.factor(part))

calg2 <- calg2[match(tree$tip.label, calg2$species),]
row.names(calg2) <- calg2$species


mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length+ feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm, correlation = corPagel(value = 0, phy = tree, fixed = FALSE), data = calg2, method = "ML")
mod1b <- gls(log_concentration ~  1, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")

model.sel(mod1a, mod1b, extra = "rsquared", rank = "AIC") %>% View
AIC(mod1a, mod1b)
summary(mod1a)
lambda <- round(mod1a$modelStruct[[1]][[1]], digits = 2)

# visreg(mod1a)
rsq_mod1a <- round(rsquared(mod1a)['R.squared'][[1]], digits = 2)

stargazer(mod1a, title = "", type = "html", out="tables/fat-models-expanded-muscle-only-pgls.htm", 
          add.lines = list(c("R2", rsq_mod1a), c("Lamba", lambda)), ci=TRUE, ci.level=0.95, digits = 2)

anova(mod1a)
# Calcium all parts -------------------------------------------------------

traits_one_part <- traits2 %>% 
  group_by(species1, nutrient) %>% 
  top_n(n = 1, wt = part)

calcium <- traits1b %>% 
  filter(part != "unknown") %>%
  filter(part != "unspecified") %>% 
  filter(part != "not specified") %>% 
  filter(nutrient == "ca_mg") %>% 
  filter(!is.na(concentration)) %>% 
  filter(!grepl("spp", species1)) %>% 
  filter(!species1 %in% c("Pleuronectinae", "Petromyzontinae", "Ensis directus", "Osmerus mordax")) %>% 
  group_by(species1, feeding_mode, EnvTemp, DemersPelag, BodyShapeI, part, realm, feeding_level) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) %>% 
  ungroup() %>% 
  # distinct(species1, feeding_mode, EnvTemp, DemersPelag, BodyShapeI, realm, feeding_level, .keep_all = TRUE) %>% 
  filter(complete.cases(.)) %>% 
  group_by(species1) %>% 
  top_n(n = 1, wt = part) 

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
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
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
  group_by(species, EnvTemp, DemersPelag, 
           feeding_level,
           feeding_mode, BodyShapeI, realm, part) %>% 
  summarise_each(funs(mean), DepthRangeDeep, log_concentration, log_length, AgeMatMin, bulk_trophic_level) %>% 
  ungroup() %>%
  mutate(EnvTemp = as.character(EnvTemp)) %>% 
  distinct(species, .keep_all = TRUE) 

calg2 <- calg %>% 
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(part = as.factor(part))

calg2 <- calg2[match(tree$tip.label, calg2$species),]
row.names(calg2) <- calg2$species
cal_data <- calg2
### full model
mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag +
               DepthRangeDeep + AgeMatMin + BodyShapeI + realm + part, 
             correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")

lambda <- round(mod1a$modelStruct[[1]][[1]], digits = 2)

rsq_mod1a <- round(rsquared(mod1a)['R.squared'][[1]], digits = 2)

stargazer(mod1a, title = "", type = "html", out="tables/1calcium-models-expanded-all-parts-pgls-lambda.htm", 
          add.lines = list(c("R2", rsq_mod1a), c("Lamba", lambda)), ci=TRUE, ci.level=0.95, digits = 2, single.row = TRUE)

# Iron all parts pgls -----------------------------------------------------
calcium <- traits1b %>% 
  filter(part != "unknown") %>%
  filter(part != "unspecified") %>% 
  filter(part != "not specified") %>% 
  filter(nutrient == "fe_mg") %>% 
  filter(!is.na(concentration)) %>% 
  filter(!grepl("spp", species1)) %>% 
  filter(!species1 %in% c("Pleuronectinae", "Petromyzontinae", "Ensis directus", "Osmerus mordax")) %>% 
  group_by(species1, feeding_mode, EnvTemp, DemersPelag, BodyShapeI, part, realm, feeding_level) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) %>% 
  ungroup() %>% 
  distinct(species1, feeding_mode, EnvTemp, DemersPelag, BodyShapeI, realm, feeding_level, .keep_all = TRUE) %>% 
  filter(complete.cases(.)) 
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
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
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
  group_by(species, EnvTemp, DemersPelag, 
           feeding_level,
           feeding_mode, BodyShapeI, realm, part) %>% 
  summarise_each(funs(mean), DepthRangeDeep, log_concentration, log_length, AgeMatMin, bulk_trophic_level) %>% 
  ungroup() %>%
  mutate(EnvTemp = as.character(EnvTemp)) %>% 
  distinct(species, .keep_all = TRUE) 

calg2 <- calg %>% 
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(part = as.factor(part))

calg2 <- calg2[match(tree$tip.label, calg2$species),]
row.names(calg2) <- calg2$species
cal_data <- calg2
### full model
mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag +
               DepthRangeDeep + AgeMatMin + BodyShapeI + realm + part, 
             correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")

lambda <- round(mod1a$modelStruct[[1]][[1]], digits = 2)

rsq_mod1a <- round(rsquared(mod1a)['R.squared'][[1]], digits = 2)

stargazer(mod1a, title = "", type = "html", out="tables/1iron-models-expanded-all-parts-pgls-lambda.htm", 
          add.lines = list(c("R2", rsq_mod1a), c("Lamba", lambda)), ci=TRUE, ci.level=0.95, digits = 2, single.row = TRUE)

##########

# zinc all parts pgls -----------------------------------------------------

calcium <- traits1b %>% 
  filter(part != "unknown") %>%
  filter(part != "unspecified") %>% 
  filter(nutrient == "zn_mg") %>% 
  filter(!is.na(concentration)) %>% 
  filter(!grepl("spp", species1)) %>% 
  filter(!species1 %in% c("Pleuronectinae", "Petromyzontinae", "Ensis directus", "Osmerus mordax")) %>% 
  group_by(species1, feeding_mode, EnvTemp, DemersPelag, BodyShapeI, part, realm, feeding_level) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) %>% 
  ungroup() %>% 
  distinct(species1, feeding_mode, EnvTemp, DemersPelag, BodyShapeI, realm, feeding_level, .keep_all = TRUE) %>% 
  filter(complete.cases(.))

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
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
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
  group_by(species, EnvTemp, DemersPelag, 
           feeding_level,
           feeding_mode, BodyShapeI, realm, part) %>% 
  summarise_each(funs(mean), DepthRangeDeep, log_concentration, log_length, AgeMatMin, bulk_trophic_level) %>% 
  ungroup() %>%
  mutate(EnvTemp = as.character(EnvTemp)) %>% 
  distinct(species, .keep_all = TRUE) 

calg2 <- calg %>% 
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(part = as.factor(part))

calg2 <- calg2[match(tree$tip.label, calg2$species),]
row.names(calg2) <- calg2$species
cal_data <- calg2
### full model
mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag +
               DepthRangeDeep + AgeMatMin + BodyShapeI + realm + part, 
             correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")

lambda <- round(mod1a$modelStruct[[1]][[1]], digits = 2)

rsq_mod1a <- round(rsquared(mod1a)['R.squared'][[1]], digits = 2)

stargazer(mod1a, title = "", type = "html", out="tables/zinc-models-expanded-all-parts-pgls-lambda.htm", 
          add.lines = list(c("R2", rsq_mod1a), c("Lamba", lambda)), ci=TRUE, ci.level=0.95, digits = 2, single.row = TRUE)



# zinc partialregressions all parts ---------------------------------------
mod1 <- mod1a
plot1 <- visreg(mod1, "DemersPelag", gg = TRUE, size = 4) +
  ylab("log(zinc) mg/100g ") + xlab("Habitat association") +
  theme(axis.text.x = element_text(angle = 90))
plot2 <- visreg(mod1, "log_length", gg = TRUE, size = 4) +
  ylab("log(zinc) mg/100g ") + xlab("Log length (cm)")  +
  theme(axis.text.x = element_text(angle = 90))
plot3 <- visreg(mod1, "AgeMatMin", gg = TRUE, size = 4) +
  ylab("log(zinc) mg/100g ") + xlab("Age at Maturity (years)") +
  theme(axis.text.x = element_text(angle = 90))
plot4 <- visreg(mod1, "DepthRangeDeep", gg = TRUE, size = 4) +
  ylab("log(zinc) mg/100g ") + xlab("Depth (m)") +
  theme(axis.text.x = element_text(angle = 90))
plot5 <- visreg(mod1, "bulk_trophic_level", gg = TRUE, size = 4) +
  ylab("log(zinc) mg/100g ") + xlab("Trophic position") +
  theme(axis.text.x = element_text(angle = 90))
plot6 <- visreg(mod1, "part", gg = TRUE, size = 4) +
  ylab("log(zinc) mg/100g ") + xlab("Body part") +
  theme(axis.text.x = element_text(angle = 90))
plot7 <- visreg(mod1, "BodyShapeI", gg = TRUE, size = 4) +
  ylab("log(zinc) mg/100g ") + xlab("Body shape") +
  theme(axis.text.x = element_text(angle = 90))
plot8 <- visreg(mod1, "feeding_mode", gg = TRUE, size = 4) +
  ylab("log(zinc) mg/100g ") + xlab("Feeding mode") +
  theme(axis.text.x = element_text(angle = 90))
plot9 <- visreg(mod1, "feeding_level", gg = TRUE, size = 4) +
  ylab("log(zinc) mg/100g ") + xlab("Feeding level") +
  theme(axis.text.x = element_text(angle = 90))


plot_all <- plot1 + plot2 + plot3 + plot4 + plot5 + plot7 + plot8 + plot6 +
  plot_annotation(tag_levels = 'A') + plot_layout(ncol = 4)
ggsave("figures/all-perts-pgls-zinc-partial-regressions.pdf", plot = plot_all, width = 14, height = 10)
ggsave("figures/all-parts-pgls-zinc-partial-regressions-mac.png", plot = plot_all, width = 14, height = 10)


# epa all parts pgls ------------------------------------------------------

calcium <- traits1b %>% 
  filter(part != "unknown") %>%
  filter(part != "unspecified") %>% 
  filter(nutrient == "epa") %>% 
  filter(!is.na(concentration)) %>% 
  filter(!grepl("spp", species1)) %>% 
  filter(!species1 %in% c("Pleuronectinae", "Petromyzontinae", "Ensis directus", "Osmerus mordax")) %>% 
  group_by(species1, feeding_mode, EnvTemp, DemersPelag, BodyShapeI, part, realm, feeding_level) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) %>% 
  ungroup() %>% 
  distinct(species1, feeding_mode, EnvTemp, DemersPelag, BodyShapeI, realm, feeding_level, .keep_all = TRUE) %>% 
  filter(complete.cases(.))

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
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
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
  group_by(species, EnvTemp, DemersPelag, 
           feeding_level,
           feeding_mode, BodyShapeI, realm, part) %>% 
  summarise_each(funs(mean), DepthRangeDeep, log_concentration, log_length, AgeMatMin, bulk_trophic_level) %>% 
  ungroup() %>%
  mutate(EnvTemp = as.character(EnvTemp)) %>% 
  distinct(species, .keep_all = TRUE) 

calg2 <- calg %>% 
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(part = as.factor(part))

calg2 <- calg2[match(tree$tip.label, calg2$species),]
row.names(calg2) <- calg2$species
cal_data <- calg2
### full model
mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag +
               DepthRangeDeep + AgeMatMin + BodyShapeI + realm + part, 
             correlation = corPagel(value = 0.7, phy = tree, fixed = TRUE), data = calg2, method = "ML")

lambda <- round(mod1a$modelStruct[[1]][[1]], digits = 2)

rsq_mod1a <- round(rsquared(mod1a)['R.squared'][[1]], digits = 2)

stargazer(mod1a, title = "", type = "html", out="tables/epa-models-expanded-all-parts-pgls-lambda.htm", 
          add.lines = list(c("R2", rsq_mod1a), c("Lamba", lambda)), ci=TRUE, ci.level=0.95, digits = 2, single.row = TRUE)
stargazer(mod1a, title = "", type = "html", out="tables/epa-models-expanded-all-parts-pgls-lambda-aug3.htm", 
          add.lines = list(c("R2", rsq_mod1a), c("Lamba", lambda)), ci=TRUE, ci.level=0.95, digits = 2, single.row = TRUE)
summary(mod1a)
# epa partial regressions all parts ---------------------------------------

mod1 <- mod1a
plot1 <- visreg(mod1, "DemersPelag", gg = TRUE, size = 4) +
  ylab("log(EPA) g/100g ") + xlab("Habitat association") +
  theme(axis.text.x = element_text(angle = 90))
plot2 <- visreg(mod1, "log_length", gg = TRUE, size = 4) +
  ylab("log(EPA) g/100g ") + xlab("Log length (cm)")  +
  theme(axis.text.x = element_text(angle = 90))
plot3 <- visreg(mod1, "AgeMatMin", gg = TRUE, size = 4) +
  ylab("log(EPA) g/100g ") + xlab("Age at Maturity (years)") +
  theme(axis.text.x = element_text(angle = 90))
plot4 <- visreg(mod1, "DepthRangeDeep", gg = TRUE, size = 4) +
  ylab("log(EPA) g/100g ") + xlab("Depth (m)") +
  theme(axis.text.x = element_text(angle = 90))
plot5 <- visreg(mod1, "bulk_trophic_level", gg = TRUE, size = 4) +
  ylab("log(EPA) g/100g ") + xlab("Trophic position") +
  theme(axis.text.x = element_text(angle = 90))
plot6 <- visreg(mod1, "part", gg = TRUE, size = 4) +
  ylab("log(EPA) g/100g ") + xlab("Body part") +
  theme(axis.text.x = element_text(angle = 90))
plot7 <- visreg(mod1, "BodyShapeI", gg = TRUE, size = 4) +
  ylab("log(EPA) g/100g ") + xlab("Body shape") +
  theme(axis.text.x = element_text(angle = 90))
plot8 <- visreg(mod1, "feeding_mode", gg = TRUE, size = 4) +
  ylab("log(EPA) g/100g ") + xlab("Feeding mode") +
  theme(axis.text.x = element_text(angle = 90))

plot9 <- visreg(mod1, "feeding_level", gg = TRUE, size = 4) +
  ylab("log(EPA) g/100g ") + xlab("Feeding level") +
  theme(axis.text.x = element_text(angle = 90))


plot_all <- plot1 + plot2 + plot3 + plot4 + plot5 + plot7 + plot8 + plot6 +
  plot_annotation(tag_levels = 'A') + plot_layout(ncol = 4)
ggsave("figures/EPA-partial-regressions-all.pdf", plot = plot_all, width = 14, height = 10)
ggsave("figures/EPA-partial-regressions-mac-all.png", plot = plot_all, width = 14, height = 10)



# dha all parts pgls ------------------------------------------------------

calcium <- traits1b %>% 
  filter(part != "unknown") %>%
  filter(part != "unspecified") %>% 
  filter(nutrient == "dha") %>% 
  filter(!is.na(concentration)) %>% 
  filter(!grepl("spp", species1)) %>% 
  filter(!species1 %in% c("Pleuronectinae", "Petromyzontinae", "Ensis directus", "Osmerus mordax")) %>% 
  group_by(species1, feeding_mode, EnvTemp, DemersPelag, BodyShapeI, part, realm, feeding_level) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) %>% 
  ungroup() %>% 
  distinct(species1, feeding_mode, EnvTemp, DemersPelag, BodyShapeI, realm, feeding_level, .keep_all = TRUE) %>% 
  filter(complete.cases(.))

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
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
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
  group_by(species, EnvTemp, DemersPelag, 
           feeding_level,
           feeding_mode, BodyShapeI, realm, part) %>% 
  summarise_each(funs(mean), DepthRangeDeep, log_concentration, log_length, AgeMatMin, bulk_trophic_level) %>% 
  ungroup() %>%
  mutate(EnvTemp = as.character(EnvTemp)) %>% 
  distinct(species, .keep_all = TRUE) 

calg2 <- calg %>% 
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(part = as.factor(part))

calg2 <- calg2[match(tree$tip.label, calg2$species),]
row.names(calg2) <- calg2$species
cal_data <- calg2
### full model
mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag +
               DepthRangeDeep + AgeMatMin + BodyShapeI + realm + part, 
             correlation = corPagel(value = 0.6, phy = tree, fixed = TRUE), data = calg2, method = "ML")


lambda <- round(mod1a$modelStruct[[1]][[1]], digits = 2)

rsq_mod1a <- round(rsquared(mod1a)['R.squared'][[1]], digits = 2)

stargazer(mod1a, title = "", type = "html", out="tables/dha-models-expanded-all-parts-pgls-lambda.htm", 
          add.lines = list(c("R2", rsq_mod1a), c("Lamba", lambda)), ci=TRUE, ci.level=0.95, digits = 2, single.row = TRUE)


# dha pgls partial regressions --------------------------------------------
mod1 <- mod1a
plot1 <- visreg(mod1, "DemersPelag", gg = TRUE, size = 4) +
  ylab("log(DHA) g/100g ") + xlab("Habitat association") +
  theme(axis.text.x = element_text(angle = 90))
plot2 <- visreg(mod1, "log_length", gg = TRUE, size = 4) +
  ylab("log(DHA) g/100g ") + xlab("Log length (cm)")  +
  theme(axis.text.x = element_text(angle = 90))
plot3 <- visreg(mod1, "AgeMatMin", gg = TRUE, size = 4) +
  ylab("log(DHA) g/100g ") + xlab("Age at Maturity (years)") +
  theme(axis.text.x = element_text(angle = 90))
plot4 <- visreg(mod1, "DepthRangeDeep", gg = TRUE, size = 4) +
  ylab("log(DHA) g/100g ") + xlab("Depth (m)") +
  theme(axis.text.x = element_text(angle = 90))
plot5 <- visreg(mod1, "bulk_trophic_level", gg = TRUE, size = 4) +
  ylab("log(DHA) g/100g ") + xlab("Trophic position") +
  theme(axis.text.x = element_text(angle = 90))
plot6 <- visreg(mod1, "part", gg = TRUE, size = 4) +
  ylab("log(DHA) g/100g ") + xlab("Body part") +
  theme(axis.text.x = element_text(angle = 90))
plot7 <- visreg(mod1, "BodyShapeI", gg = TRUE, size = 4) +
  ylab("log(DHA) g/100g ") + xlab("Body shape") +
  theme(axis.text.x = element_text(angle = 90))
plot8 <- visreg(mod1, "feeding_mode", gg = TRUE, size = 4) +
  ylab("log(DHA) g/100g ") + xlab("Feeding mode") +
  theme(axis.text.x = element_text(angle = 90))

plot9 <- visreg(mod1, "feeding_level", gg = TRUE, size = 4) +
  ylab("log(DHA) g/100g ") + xlab("Feeding level") +
  theme(axis.text.x = element_text(angle = 90))


plot_all <- plot1 + plot2 + plot3 + plot4 + plot5 + plot7 + plot8 + plot6 +
  plot_annotation(tag_levels = 'A') + plot_layout(ncol = 4)
ggsave("figures/DHA-partial-regressions-all.pdf", plot = plot_all, width = 14, height = 10)
ggsave("figures/DHA-partial-regressions-mac-all.png", plot = plot_all, width = 14, height = 10)





str(calcium)
mod_cal <- lm(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin
              + BodyShapeI + realm + part, data = calcium)
summary(mod_cal)
visreg(mod_cal)

tidy(mod_cal, conf.int = TRUE) %>% View

mod_part <- lm(log_concentration ~ part, data = calcium)
model.sel(mod_cal, mod_part, rank = "AIC") %>% View

stargazer(mod_cal, title = "", type = "html", out="tables/calcium-models-expanded-all-parts-ols3.htm")



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
  mutate(feeding_level = as.factor(feeding_level)) %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>%
  mutate(BodyShapeI = as.factor(BodyShapeI)) %>%
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
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
  group_by(species, EnvTemp, DemersPelag, feeding_level, feeding_mode, BodyShapeI, realm, part) %>% 
  summarise_each(funs(mean), DepthRangeDeep, log_concentration, log_length, AgeMatMin, bulk_trophic_level) %>% 
  ungroup() %>%
  mutate(EnvTemp = as.character(EnvTemp)) %>% 
  distinct(species, .keep_all = TRUE) 

calg2 <- calg %>% 
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(part = as.factor(part))

calg2 <- calg2[match(tree$tip.label, calg2$species),]
row.names(calg2) <- calg2$species


mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm + part,  
             correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")
mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + DepthRangeDeep + BodyShapeI + part,  correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")
mod1b <- gls(log_concentration ~  1, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")
mod1c <- gls(log_concentration ~  part, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")

AIC(mod1a, mod1b, mod1c)
summary(mod1a)
model.sel(mod1a, mod1c, extra = "rsquared", rank = "AIC") %>% View
summary(mod1b)
AICc(mod1a, mod1b, mod1c)


mod1a <- lm(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm + part, data = calg2)
mod1b <- lm(log_concentration ~  part, data = calg2)

AIC(mod1a, mod1b)

summary(mod1a)
rsquared(mod1a)

visreg(mod1a)
rsquared(mod1a)
stargazer(mod1a, title = "", type = "html", out="tables/calcium-models-expanded-muscle-only-pgls.htm")



# iron all parts -------------------------------------------------------

traits_one_part <- traits2 %>% 
  group_by(species1, nutrient) %>% 
  top_n(n = 1, wt = part)

calcium <- traits2 %>% 
  dplyr::select(-seanuts_id2) %>% 
  filter(nutrient == "fe_mg") %>% 
  filter(!grepl("spp", species1)) %>% 
  filter(!species1 %in% c("Pleuronectinae", "Petromyzontinae", "Ensis directus", "Osmerus mordax")) %>% 
  filter(part != "unknown") %>%
  filter(part != "unspecified") %>% 
  # filter(part == "muscle") %>% 
  group_by(species1, feeding_mode, feeding_level, EnvTemp, DemersPelag, BodyShapeI, part, realm) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) %>% 
  ungroup() %>%
  # filter(complete.cases(.)) %>% 
  mutate(part = as.character(part))

str(calcium)
mod_iron <- lm(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm + part, data = calcium)
mod_part <- lm(log_concentration ~ part, data = calcium)
model.sel(mod_cal, mod_part, rank = "AIC") %>% View

stargazer(mod_cal, title = "", type = "html", out="tables/iron-models-expanded-all-parts-ols.htm")

tidy(mod_iron, conf.int = TRUE) %>% View

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
  mutate(feeding_level = as.factor(feeding_level)) %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>%
  mutate(BodyShapeI = as.factor(BodyShapeI)) %>%
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
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
  group_by(species, EnvTemp, DemersPelag, feeding_level, feeding_mode, BodyShapeI, realm, part) %>% 
  summarise_each(funs(mean), DepthRangeDeep, log_concentration, log_length, AgeMatMin, bulk_trophic_level) %>% 
  ungroup() %>%
  mutate(EnvTemp = as.character(EnvTemp)) %>% 
  distinct(species, .keep_all = TRUE) 

calg2 <- calg %>% 
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(part = as.factor(part))

calg2 <- calg2[match(tree$tip.label, calg2$species),]
row.names(calg2) <- calg2$species


# mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm + part,  correlation = corPagel(value = 0, phy = tree, fixed = FALSE), data = calg2, method = "ML")
mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + DepthRangeDeep + BodyShapeI + part,  correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")
mod1b <- gls(log_concentration ~  1, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")
mod1c <- gls(log_concentration ~  part, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")

AIC(mod1a, mod1b, mod1c)
summary(mod1a)
model.sel(mod1a, mod1c, extra = "rsquared", rank = "AIC") %>% View
summary(mod1b)
AICc(mod1a, mod1b, mod1c)




# zinc all parts -------------------------------------------------------

traits_one_part <- traits2 %>% 
  group_by(species1, nutrient) %>% 
  top_n(n = 1, wt = part)

calcium <- traits2 %>% 
  dplyr::select(-seanuts_id2) %>% 
  filter(nutrient == "zn_mg") %>% 
  filter(!grepl("spp", species1)) %>% 
  filter(!species1 %in% c("Pleuronectinae", "Petromyzontinae", "Ensis directus", "Osmerus mordax")) %>% 
  filter(part != "unknown") %>%
  filter(part != "unspecified") %>% 
  # filter(part == "muscle") %>% 
  group_by(species1, feeding_mode, feeding_level, EnvTemp, DemersPelag, BodyShapeI, part, realm) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) %>% 
  ungroup() %>%
  filter(complete.cases(.)) %>% 
  mutate(part = as.character(part))

str(calcium)
mod_zinc <- lm(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm + part, data = calcium)
summary(mod_zinc)

mod_part <- lm(log_concentration ~ part, data = calcium)
model.sel(mod_cal, mod_part, rank = "AIC") %>% View

stargazer(mod_cal, title = "", type = "html", out="tables/zinc-models-expanded-all-parts-ols.htm")

tidy(mod_zinc, conf.int = TRUE) %>% View

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
  mutate(feeding_level = as.factor(feeding_level)) %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>%
  mutate(BodyShapeI = as.factor(BodyShapeI)) %>%
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
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
  group_by(species, EnvTemp, DemersPelag, feeding_level, feeding_mode, BodyShapeI, realm, part) %>% 
  summarise_each(funs(mean), DepthRangeDeep, log_concentration, log_length, AgeMatMin, bulk_trophic_level) %>% 
  ungroup() %>%
  mutate(EnvTemp = as.character(EnvTemp)) %>% 
  distinct(species, .keep_all = TRUE) 

calg2 <- calg %>% 
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(part = as.factor(part))

calg2 <- calg2[match(tree$tip.label, calg2$species),]
row.names(calg2) <- calg2$species


# mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm + part,  correlation = corPagel(value = 0, phy = tree, fixed = FALSE), data = calg2, method = "ML")
mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + DepthRangeDeep + BodyShapeI + part,  correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")
mod1b <- gls(log_concentration ~  1, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")
mod1c <- gls(log_concentration ~  part, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")

AIC(mod1a, mod1b, mod1c)
summary(mod1a)
model.sel(mod1a, mod1c, extra = "rsquared", rank = "AIC") %>% View
summary(mod1b)
AICc(mod1a, mod1b, mod1c)



# epa all parts -------------------------------------------------------

traits_one_part <- traits2 %>% 
  group_by(species1, nutrient) %>% 
  top_n(n = 1, wt = part)

calcium <- traits_one_part %>% 
  dplyr::select(-seanuts_id2) %>% 
  filter(nutrient == "epa") %>% 
  filter(!grepl("spp", species1)) %>% 
  filter(!species1 %in% c("Pleuronectinae", "Petromyzontinae", "Ensis directus", "Osmerus mordax")) %>% 
  filter(part != "unknown") %>%
  filter(part != "unspecified") %>% 
  # filter(part == "muscle") %>% 
  group_by(species1, feeding_mode, feeding_level, EnvTemp, DemersPelag, BodyShapeI, part, realm) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) %>% 
  ungroup() %>%
  filter(complete.cases(.)) %>% 
  mutate(part = as.character(part))

str(calcium)
mod_cal <- lm(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm + part, data = calcium)
mod_part <- lm(log_concentration ~ part, data = calcium)
model.sel(mod_cal, mod_part, rank = "AIC") %>% View

# stargazer(mod_cal, title = "", type = "html", out="tables/iron-models-expanded-all-parts-ols.htm")



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
  mutate(feeding_level = as.factor(feeding_level)) %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>%
  mutate(BodyShapeI = as.factor(BodyShapeI)) %>%
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
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
  group_by(species, EnvTemp, DemersPelag, feeding_level, feeding_mode, BodyShapeI, realm, part) %>% 
  summarise_each(funs(mean), DepthRangeDeep, log_concentration, log_length, AgeMatMin, bulk_trophic_level) %>% 
  ungroup() %>%
  mutate(EnvTemp = as.character(EnvTemp)) %>% 
  distinct(species, .keep_all = TRUE) 

calg2 <- calg %>% 
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(part = as.factor(part))

calg2 <- calg2[match(tree$tip.label, calg2$species),]
row.names(calg2) <- calg2$species


# mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm + part,  correlation = corPagel(value = 0, phy = tree, fixed = FALSE), data = calg2, method = "ML")
mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + DepthRangeDeep + BodyShapeI + part,  correlation = corPagel(value = 0, phy = tree, fixed = FALSE), data = calg2, method = "ML")
mod1b <- gls(log_concentration ~  1, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")
mod1c <- gls(log_concentration ~  part, correlation = corPagel(value = 0, phy = tree, fixed = FALSE), data = calg2, method = "ML")

AIC(mod1a, mod1b, mod1c)
summary(mod1a)
model.sel(mod1a, mod1c, extra = "rsquared", rank = "AIC") %>% View
summary(mod1b)
AICc(mod1a, mod1b, mod1c)

lambda <- round(mod1a$modelStruct[[1]][[1]], digits = 2)

# visreg(mod1a)
rsq_mod1a <- round(rsquared(mod1a)['R.squared'][[1]], digits = 2)

stargazer(mod1a, title = "", type = "html", out="tables/epa-models-expanded-all-parts-pgls.htm", 
          add.lines = list(c("R2", rsq_mod1a), c("Lamba", lambda)), ci=TRUE, ci.level=0.95, digits = 2)


# dha all parts -------------------------------------------------------

traits_one_part <- traits2 %>% 
  group_by(species1, nutrient) %>% 
  top_n(n = 1, wt = part)

calcium <- traits_one_part  %>% 
  dplyr::select(-seanuts_id2) %>% 
  filter(nutrient == "dha") %>% 
  filter(!grepl("spp", species1)) %>% 
  filter(!species1 %in% c("Pleuronectinae", "Petromyzontinae", "Ensis directus", "Osmerus mordax")) %>% 
  filter(part != "unknown") %>%
  filter(part != "unspecified") %>% 
  # filter(part == "muscle") %>% 
  group_by(species1, feeding_mode, feeding_level, EnvTemp, DemersPelag, BodyShapeI, part, realm) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) %>% 
  ungroup() %>%
  filter(complete.cases(.)) %>% 
  mutate(part = as.character(part))

str(calcium)
mod_cal <- lm(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm + part, data = calcium)
mod_part <- lm(log_concentration ~ part, data = calcium)
model.sel(mod_cal, mod_part, rank = "AIC") %>% View

# stargazer(mod_cal, title = "", type = "html", out="tables/iron-models-expanded-all-parts-ols.htm")



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
  mutate(feeding_level = as.factor(feeding_level)) %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>%
  mutate(BodyShapeI = as.factor(BodyShapeI)) %>%
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
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
  group_by(species, EnvTemp, DemersPelag, feeding_level, feeding_mode, BodyShapeI, realm, part) %>% 
  summarise_each(funs(mean), DepthRangeDeep, log_concentration, log_length, AgeMatMin, bulk_trophic_level) %>% 
  ungroup() %>%
  mutate(EnvTemp = as.character(EnvTemp)) %>% 
  distinct(species, .keep_all = TRUE) 

calg2 <- calg %>% 
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(part = as.factor(part))

calg2 <- calg2[match(tree$tip.label, calg2$species),]
row.names(calg2) <- calg2$species


# mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm + part,  correlation = corPagel(value = 0, phy = tree, fixed = FALSE), data = calg2, method = "ML")
mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + DepthRangeDeep + BodyShapeI + part,  correlation = corPagel(value = 0, phy = tree, fixed = FALSE), data = calg2, method = "ML")
mod1b <- gls(log_concentration ~  1, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")
mod1c <- gls(log_concentration ~  part, correlation = corPagel(value = 0, phy = tree, fixed = FALSE), data = calg2, method = "ML")

AIC(mod1a, mod1b, mod1c)
summary(mod1a)
model.sel(mod1a, mod1c, extra = "rsquared", rank = "AIC") %>% View
summary(mod1b)
AICc(mod1a, mod1b, mod1c)

lambda <- round(mod1a$modelStruct[[1]][[1]], digits = 2)

# visreg(mod1a)
rsq_mod1a <- round(rsquared(mod1a)['R.squared'][[1]], digits = 2)

stargazer(mod1a, title = "", type = "html", out="tables/dha-models-expanded-all-parts-pgls.htm", 
          add.lines = list(c("R2", rsq_mod1a), c("Lamba", lambda)), ci=TRUE, ci.level=0.95, digits = 2)



# extra code --------------------------------------------------------------



mod1b <- gls(log_concentration ~  1,  data = calg2, method = "ML")

mod1b <- lm(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm, data = calg2)
anova(mod1a, mod1b)
summary(mod1)

mod1b <- pgls(log_concentration ~  bulk_trophic_level + log_length  + feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm, data = com, lambda = "ML")
mod1bc <- pgls(log_concentration ~  1, data = com, lambda = "ML")
model.sel(mod1b, mod1bc) %>% View

summary(mod1bc)
mod1c <- lm(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm, data = calg2)
mod1d <- lm(log_concentration ~ realm, data = calg2)

lrtest<-function(model1,model2){
  lik1<-logLik(model1)
  lik2<-logLik(model2)
  LR<--2*(lik1-lik2)
  degf<-attr(lik2,"df")-attr(lik1,"df")
  P<-pchisq(LR,df=degf,lower.tail=FALSE)
  cat(paste("Likelihood ratio = ",
            signif(LR,5),"(df=",degf,") P =",
            signif(P,4),"\n",sep=" "))
  invisible(list(likelihood.ratio=LR,p=P))
}

## run likelihood-ratio test
lrtest(mod1c, mod1d)

anova(mod1c, mod1d) 
confint(mod1c)

moda <- phylolm(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm, data = calg2, phy = tree, model = "lambda", lower.bound = 0, upper.bound = 2)
modab <- phylolm(log_concentration ~ realm, data = calg2, phy = tree, model = "lambda", lower.bound = 0, upper.bound = 2)

summary(moda)
model.sel(moda, modab, extra = "rsquared") %>% View
mod1 <- gls(log_concentration ~ realm + EnvTemp, correlation = corPagel(value = 0, phy = tree, fixed = FALSE), data = calg2, method = "ML")

mod6 <- gls(log_concentration ~ EnvTemp + realm, data = calg2, method = "ML")
AICc(mod1, mod1b)
summary(mod1)
