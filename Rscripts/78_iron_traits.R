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
library(readxl)
library(rr2)


all_traits6 <- read_csv("data-processed/fishbase-traits-aug-26-2020.csv")
s2 <- all_traits6 %>% 
  # filter(biblio_id != 27) %>% 
  gather(17:23, key = nutrient, value = concentration) %>% 
  rename(feeding_level = Herbivory2) %>% 
  rename(feeding_mode = FeedingType) %>% 
  rename(length = Length) %>% 
  rename(bulk_trophic_level = FoodTroph) %>% 
  mutate(log_length = log(length)) %>% 
  mutate(log_concentration = log(concentration))


iron <-  s2 %>% 
  filter(nutrient == "fe_mg") %>% 
  filter(!is.na(concentration)) %>% 
  filter(body_part %in% c("muscle", "muscle_skinless")) %>% 
  group_by(Species, feeding_mode, DemersPelag, realm, EnvTemp) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level) %>% 
  ungroup() %>% 
  filter(complete.cases(.)) %>% 
  # rename(part = part_edited) %>% 
  rename(species1 = Species)


# PGLS iron muscle only ------------------------------------------------------------


iron$species1 <- str_to_lower(iron$species1)
# unique(iron$part)
length(unique(iron$species1))

iron_taxa <- tnrs_match_names(unique(iron$species1), context="Animals", names = unique(iron$species1), do_approximate_matching = TRUE) 

tr_iron <- tol_induced_subtree(ott_ids = ott_id(iron_taxa), label_format="name") 
tr_bl_iron <- compute.brlen(tr_iron)
phylo <- tr_bl_iron
iron2 <- iron %>% 
  left_join(., iron_taxa, by = c("species1" = "search_string")) %>% 
  mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl_iron$tip.label)) %>% 
  ungroup() %>% 
  # mutate(feeding_level = as.factor(feeding_level)) %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>%
  # mutate(BodyShapeI = as.factor(BodyShapeI)) %>%
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(realm = as.factor(realm))
iron2$log_length <- scale(iron2$log_length)
iron2$bulk_trophic_level <- scale(iron2$bulk_trophic_level)
# iron2$DepthRangeDeep <- sirone(iron2$DepthRangeDeep)
# iron2$AgeMatMin <- sirone(iron2$AgeMatMin)

data <- iron2
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
iron_tree <- pruned.tree
# plot(tree)
# is.ultrametric(tree) # has to be TRUE
# is.rooted(tree) # TRUE

any(duplicated(iron_tree$node.label)) # FALSE

iron_tree$node.label<-NULL

#prune data to match treetips
Phylodata <- data[(data$Phylospecies %in% iron_tree$tip.label),]

Phylodata1 <- Phylodata %>% 
  # mutate(part = as.factor(part)) %>% 
  rename(species = Phylospecies) 

irong <- Phylodata1 %>% 
  group_by(species, DemersPelag, 
           # feeding_level,
           feeding_mode, realm, EnvTemp) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level) %>% 
  ungroup() %>%
  distinct(species, .keep_all = TRUE) 




irong2 <- irong 
irong2 <- irong2[match(iron_tree$tip.label, irong2$species),]
row.names(irong2) <- irong2$species

library(MuMIn)
library(phylolm)


mod1 <- phylolm(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm, phy = iron_tree, data = irong2, model = "lambda")
mod2 <- phylolm(log_concentration ~  feeding_mode + log_length + EnvTemp + bulk_trophic_level + realm, phy = iron_tree, data = irong2, model = "lambda")

mod3 <- phylolm(log_concentration ~  feeding_mode + DemersPelag + bulk_trophic_level, phy = iron_tree, data = irong2, model = "lambda")
mod4 <- phylolm(log_concentration ~  log_length + DemersPelag + bulk_trophic_level, phy = iron_tree, data = irong2, model = "lambda")

mod5 <- phylolm(log_concentration ~  log_length + bulk_trophic_level, phy = iron_tree, data = irong2, model = "lambda")
mod6 <- phylolm(log_concentration ~  log_length + feeding_mode, phy = iron_tree, data = irong2, model = "lambda")
mod7 <- phylolm(log_concentration ~  bulk_trophic_level + feeding_mode, phy = iron_tree, data = irong2, model = "lambda")
mod8 <- phylolm(log_concentration ~  log_length + DemersPelag, phy = iron_tree, data = irong2, model = "lambda")
mod9 <- phylolm(log_concentration ~  feeding_mode + DemersPelag, phy = iron_tree, data = irong2, model = "lambda")
mod10 <- phylolm(log_concentration ~  bulk_trophic_level + DemersPelag, phy = iron_tree, data = irong2, model = "lambda")
mod11 <- phylolm(log_concentration ~  log_length + EnvTemp, phy = iron_tree, data = irong2, model = "lambda")
mod12 <- phylolm(log_concentration ~  feeding_mode + EnvTemp, phy = iron_tree, data = irong2, model = "lambda")
mod13 <- phylolm(log_concentration ~  bulk_trophic_level + EnvTemp, phy = iron_tree, data = irong2, model = "lambda")
mod14 <- phylolm(log_concentration ~  1, phy = iron_tree, data = irong2, model = "lambda")

model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, mod13, mod14, rank = AICc) %>%  
  mutate(model_number = rownames(.)) %>% 
  mutate(cumsum = cumsum(weight)) %>% View

summary(mod5)
confint(mod5)
visreg(mod1)

model.avg(mod5, mod6,mod7)
# confint(model.avg(mod5, mod7))
# coef(model.avg(mod5, mod7))
# 
# R2(mod7, phy = iron_tree)

model.avg(mod5, mod14)
  confints_iron <- data.frame(confint(MuMIn::model.avg(mod5, mod14, mod4, mod8, mod10)), estimate = coef(model.avg(mod5, mod14, mod4, mod8, mod10))) %>% 
  mutate(term = rownames(.)) %>% 
  rename(lower = X2.5..) %>% 
  rename(upper = X97.5..)

iron_plot <- confints_iron %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
  coord_flip() +
  geom_hline(yintercept = 0) + ggtitle("Iron")
iron_plot


# iron all tisues ---------------------------------------------------------
iron_tissues <-  s2 %>% 
  filter(nutrient == "fe_mg") %>% 
  filter(!is.na(concentration)) %>% 
  mutate(body_part = ifelse(body_part == "muscle_skinless", "muscle", body_part)) %>% 
  # filter(body_part %in% c("muscle", "muscle_skinless")) %>% 
  group_by(Species, feeding_mode, DemersPelag, realm, EnvTemp, body_part) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level) %>% 
  ungroup() %>% 
  filter(complete.cases(.)) %>% 
  # rename(part = part_edited) %>% 
  rename(species1 = Species)


# PGLS iron muscle only ------------------------------------------------------------


iron_tissues$species1 <- str_to_lower(iron_tissues$species1)
# unique(iron_tissues$part)
length(unique(iron_tissues$species1))

iron_tissues_taxa <- tnrs_match_names(unique(iron_tissues$species1), context="Animals", names = unique(iron_tissues$species1), do_approximate_matching = TRUE) 

tr_iron_tissues <- tol_induced_subtree(ott_ids = ott_id(iron_tissues_taxa), label_format="name") 
tr_bl_iron_tissues <- compute.brlen(tr_iron_tissues)
phylo <- tr_bl_iron_tissues
iron_tissues2 <- iron_tissues %>% 
  left_join(., iron_tissues_taxa, by = c("species1" = "search_string")) %>% 
  mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl_iron_tissues$tip.label)) %>% 
  ungroup() %>% 
  # mutate(feeding_level = as.factor(feeding_level)) %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>%
  # mutate(BodyShapeI = as.factor(BodyShapeI)) %>%
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(realm = as.factor(realm))
iron_tissues2$log_length <- scale(iron_tissues2$log_length)
iron_tissues2$bulk_trophic_level <- scale(iron_tissues2$bulk_trophic_level)
# iron_tissues2$DepthRangeDeep <- siron_tissuese(iron_tissues2$DepthRangeDeep)
# iron_tissues2$AgeMatMin <- siron_tissuese(iron_tissues2$AgeMatMin)

data <- iron_tissues2
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
iron_tissues_tree <- pruned.tree
# plot(tree)
# is.ultrametric(tree) # has to be TRUE
# is.rooted(tree) # TRUE

any(duplicated(iron_tissues_tree$node.label)) # FALSE

iron_tissues_tree$node.label<-NULL

#prune data to match treetips
Phylodata <- data[(data$Phylospecies %in% iron_tissues_tree$tip.label),]

Phylodata1 <- Phylodata %>% 
  # mutate(part = as.factor(part)) %>% 
  rename(species = Phylospecies) 

iron_tissuesg <- Phylodata1 %>% 
  group_by(species, DemersPelag, 
           # feeding_level,
           feeding_mode, realm, EnvTemp, body_part) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level) %>% 
  ungroup() %>%
  distinct(species, .keep_all = TRUE) 




iron_tissuesg2 <- iron_tissuesg 
iron_tissuesg2 <- iron_tissuesg2[match(iron_tissues_tree$tip.label, iron_tissuesg2$species),]
row.names(iron_tissuesg2) <- iron_tissuesg2$species




mod1 <- phylolm(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + body_part, phy = iron_tissues_tree, data = iron_tissuesg2, model = "lambda")
AICc(mod1, mod2)
summary(mod2)
summary(mod1)
visreg(mod1)
R2(mod1, phy = iron_tissues_tree)
