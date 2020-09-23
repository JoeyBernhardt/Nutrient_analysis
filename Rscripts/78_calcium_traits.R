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



## calcium traits





calcium <-  s2 %>% 
  filter(nutrient == "ca_mg") %>% 
  filter(!is.na(concentration)) %>% 
  filter(body_part %in% c("muscle", "muscle_skinless")) %>% 
  group_by(Species, feeding_mode, DemersPelag, realm, EnvTemp) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level) %>% 
  ungroup() %>% 
  filter(complete.cases(.)) %>% 
  # rename(part = part_edited) %>% 
  rename(species1 = Species)


# PGLS calcium muscle only ------------------------------------------------------------


calcium$species1 <- str_to_lower(calcium$species1)
# unique(calcium$part)
length(unique(calcium$species1))

calcium_taxa <- tnrs_match_names(unique(calcium$species1), context="Animals", names = unique(calcium$species1), do_approximate_matching = TRUE) 

tr_calcium <- tol_induced_subtree(ott_ids = ott_id(calcium_taxa), label_format="name") 
tr_bl_calcium <- compute.brlen(tr_calcium)
phylo <- tr_bl_calcium
calcium2 <- calcium %>% 
  left_join(., calcium_taxa, by = c("species1" = "search_string")) %>% 
  mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl_calcium$tip.label)) %>% 
  ungroup() %>% 
  # mutate(feeding_level = as.factor(feeding_level)) %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>%
  # mutate(BodyShapeI = as.factor(BodyShapeI)) %>%
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(realm = as.factor(realm))
calcium2$log_length <- scale(calcium2$log_length)
calcium2$bulk_trophic_level <- scale(calcium2$bulk_trophic_level)
# calcium2$DepthRangeDeep <- scalciume(calcium2$DepthRangeDeep)
# calcium2$AgeMatMin <- scalciume(calcium2$AgeMatMin)

data <- calcium2
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
calcium_tree <- pruned.tree
# plot(tree)
# is.ultrametric(tree) # has to be TRUE
# is.rooted(tree) # TRUE

any(duplicated(calcium_tree$node.label)) # FALSE

calcium_tree$node.label<-NULL

#prune data to match treetips
Phylodata <- data[(data$Phylospecies %in% calcium_tree$tip.label),]

Phylodata1 <- Phylodata %>% 
  # mutate(part = as.factor(part)) %>% 
  rename(species = Phylospecies) 

calciumg <- Phylodata1 %>% 
  group_by(species, DemersPelag, 
           # feeding_level,
           feeding_mode, realm, EnvTemp) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level) %>% 
  ungroup() %>%
  distinct(species, .keep_all = TRUE) 




calciumg2 <- calciumg 
calciumg2 <- calciumg2[match(calcium_tree$tip.label, calciumg2$species),]
row.names(calciumg2) <- calciumg2$species


mod1 <- phylolm(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm, phy = calcium_tree, data = calciumg2, model = "lambda")
# mod1 <- gls(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm, correlation = corPagel(1, calcium_tree), data = calciumg2, method = "ML")
class(mod1)
dd <- dredge(mod1)
importance(model.avg(dd, subset = cumsum(weight) <= .95))

mod2 <- phylolm(log_concentration ~  feeding_mode + log_length + EnvTemp + bulk_trophic_level + realm, phy = calcium_tree, data = calciumg2, model = "lambda")

model.avg(mod1)
?model.avg

mod3 <- phylolm(log_concentration ~  feeding_mode + DemersPelag + bulk_trophic_level, phy = calcium_tree, data = calciumg2, model = "lambda")
mod4 <- phylolm(log_concentration ~  log_length + DemersPelag + bulk_trophic_level, phy = calcium_tree, data = calciumg2, model = "lambda")

mod5 <- phylolm(log_concentration ~  log_length + bulk_trophic_level, phy = calcium_tree, data = calciumg2, model = "lambda")
mod6 <- phylolm(log_concentration ~  log_length + feeding_mode, phy = calcium_tree, data = calciumg2, model = "lambda")
mod7 <- phylolm(log_concentration ~  bulk_trophic_level + feeding_mode, phy = calcium_tree, data = calciumg2, model = "lambda")
mod8 <- phylolm(log_concentration ~  log_length + DemersPelag, phy = calcium_tree, data = calciumg2, model = "lambda")
mod9 <- phylolm(log_concentration ~  feeding_mode + DemersPelag, phy = calcium_tree, data = calciumg2, model = "lambda")
mod10 <- phylolm(log_concentration ~  bulk_trophic_level + DemersPelag, phy = calcium_tree, data = calciumg2, model = "lambda")
mod11 <- phylolm(log_concentration ~  log_length + EnvTemp, phy = calcium_tree, data = calciumg2, model = "lambda")
mod12 <- phylolm(log_concentration ~  feeding_mode + EnvTemp, phy = calcium_tree, data = calciumg2, model = "lambda")
mod13 <- phylolm(log_concentration ~  bulk_trophic_level + EnvTemp, phy = calcium_tree, data = calciumg2, model = "lambda")
mod14 <- phylolm(log_concentration ~  1, phy = calcium_tree, data = calciumg2, model = "lambda")

R2(mod1, phy = calcium_tree)

model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, mod13, mod14, rank = AICc) %>%  
  mutate(model_number = rownames(.)) %>% 
  mutate(cumsum = cumsum(weight)) %>% View

mod1 <- lm(log_concentration ~ bulk_trophic_level, data = calciumg2)
mod2 <- lm(log_concentration ~ feeding_mode + bulk_trophic_level, data = calciumg2)
importance(model.avg(mod1, mod2)) 
library(MuMIn)
summary(model.avg(mod5, mod11, mod6))
confint(model.avg(mod14, mod9, mod12))
coef(model.avg(mod9, mod10, mod6, mod7))

R2(mod7, phy = calcium_tree)
logLik(mod1)


confints_calcium <- data.frame(confint(model.avg(mod5, mod11, mod6)), estimate = coef(model.avg(mod5, mod11, mod6))) %>% 
  mutate(term = rownames(.)) %>% 
  rename(lower = X2.5..) %>% 
  rename(upper = X97.5..)

calcium_plot <- confints_calcium %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
  coord_flip() +
  geom_hline(yintercept = 0) + ggtitle("calcium")
calcium_plot
