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
	mutate(DemersPelag = ifelse(DemersPelag == "pelagic", "pelagic-oceanic", DemersPelag)) %>%
	mutate(DemersPelag = ifelse(DemersPelag == "bathydemersal", "demersal", DemersPelag)) %>%
	mutate(DemersPelag = ifelse(DemersPelag == "bathypelagic", "pelagic-oceanic", DemersPelag)) %>%
	mutate(DemersPelag = ifelse(DemersPelag == "reef-associated", "pelagic-neritic", DemersPelag)) %>%
	mutate(feeding_mode = ifelse(feeding_mode == "filtering plankton", "selective plankton feeding", feeding_mode)) %>% 
	mutate(feeding_mode = ifelse(feeding_mode == "grazing on aquatic plants", "variable", feeding_mode)) %>% 
	mutate(feeding_mode = ifelse(feeding_mode == "browsing on substrate", "variable", feeding_mode)) %>% 
	mutate(EnvTemp = ifelse(EnvTemp == "deep-water", "polar", EnvTemp)) %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>%
  # mutate(BodyShapeI = as.factor(BodyShapeI)) %>%
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(realm = as.factor(realm))
iron2$log_length <- scale(iron2$log_length)
iron2$bulk_trophic_level <- scale(iron2$bulk_trophic_level)
# iron2$DepthRangeDeep <- sirone(iron2$DepthRangeDeep)
# iron2$AgeMatMin <- sirone(iron2$AgeMatMin)

table(iron2$DemersPelag)

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
table(irong2$DemersPelag)


# model selection iron ----------------------------------------------------
library(phylolm)
mod1p <- phylolm(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + EnvTemp, phy = iron_tree, data = irong2, model = "lambda")
summary(mod1p) #### tells us lamba ~ 0

mod1 <- gls(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + EnvTemp, correlation = corPagel(value = 0, phy = iron_tree, fixed = TRUE), data = irong2, method = "ML")
mod2 <- gls(log_concentration ~  feeding_mode + log_length + EnvTemp + bulk_trophic_level + realm, correlation = corPagel(value = 0, phy = iron_tree, fixed = TRUE), data = irong2, method = "ML")
mod2b <- gls(log_concentration ~  feeding_mode + log_length + EnvTemp + bulk_trophic_level, correlation = corPagel(value = 0, phy = iron_tree, fixed = TRUE), data = irong2, method = "ML")
mod3 <- gls(log_concentration ~  feeding_mode + DemersPelag + bulk_trophic_level, correlation = corPagel(value = 0, phy = iron_tree, fixed = TRUE), data = irong2, method = "ML")
mod4 <- gls(log_concentration ~  log_length + DemersPelag + bulk_trophic_level, correlation = corPagel(value = 0, phy = iron_tree, fixed = TRUE), data = irong2, method = "ML")
mod5 <- gls(log_concentration ~  log_length + bulk_trophic_level, correlation = corPagel(value = 0, phy = iron_tree, fixed = TRUE), data = irong2, method = "ML")
mod6 <- gls(log_concentration ~  log_length + feeding_mode, correlation = corPagel(value = 0, phy = iron_tree, fixed = TRUE), data = irong2, method = "ML")
mod7 <- gls(log_concentration ~  bulk_trophic_level + feeding_mode, correlation = corPagel(value = 0, phy = iron_tree, fixed = TRUE), data = irong2, method = "ML")
mod8 <- gls(log_concentration ~  log_length + DemersPelag, correlation = corPagel(value = 0, phy = iron_tree, fixed = TRUE), data = irong2, method = "ML")
mod9 <- gls(log_concentration ~  feeding_mode + DemersPelag, correlation = corPagel(value = 0, phy = iron_tree, fixed = TRUE), data = irong2, method = "ML")
mod10 <- gls(log_concentration ~  bulk_trophic_level + DemersPelag, corPagel(value = 0, phy = iron_tree, fixed = TRUE), data = irong2, method = "ML")
mod11 <- gls(log_concentration ~  log_length + EnvTemp, correlation = corPagel(value = 0, phy = iron_tree, fixed = TRUE), data = irong2, method = "ML")
mod12 <- gls(log_concentration ~  feeding_mode + EnvTemp, correlation = corPagel(value = 0, phy = iron_tree, fixed = TRUE), data = irong2, method = "ML")
mod13 <- gls(log_concentration ~  bulk_trophic_level + EnvTemp, correlation = corPagel(value = 0, phy = iron_tree, fixed = TRUE), data = irong2, method = "ML")
mod14 <- gls(log_concentration ~  1, corPagel(value = 0, phy = iron_tree, fixed = TRUE), data = irong2, method = "ML")




R2(mod11, phy = iron_tree)

### model selection
msel_iron <- model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, mod13, mod2b, mod14, rank = AICc) %>% 
	mutate(model_num = rownames(.)) %>% 
	mutate(cum_weight = cumsum(weight))



confints_iron <- data.frame(confint(model.avg(get.models(msel_iron, subset = cumsum(weight) <= .95))),
							estimate = coef(model.avg(get.models(msel_iron, subset = cumsum(weight) <= .95)))) %>% 
	mutate(term = rownames(.)) %>% 
	rename(lower = X2.5..) %>% 
	rename(upper = X97.5..) %>% 
	mutate(nutrient = "iron")
write_csv(confints_iron, "data-processed/iron-traits-confints.csv")

iron_plot <- confints_iron %>% 
	filter(term != "(Intercept)") %>% 
	ggplot(aes(x = term, y = estimate)) + 
	geom_pointrange(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
	coord_flip() +
	geom_hline(yintercept = 0) + ggtitle("iron")
iron_plot


##### end model selection here. 


# partial regression plots ------------------------------------------------

iron_all <-  s2 %>% 
  filter(nutrient == "fe_mg") %>% 
  filter(body_part != "skin") %>% 
  mutate(body_part = ifelse(body_part %in% c("eggs", "liver"), "eggs or liver", body_part)) %>%
  mutate(DemersPelag = ifelse(DemersPelag == "pelagic", "pelagic-oceanic", DemersPelag)) %>%
  mutate(DemersPelag = ifelse(DemersPelag == "pelagic", "pelagic-oceanic", DemersPelag)) %>%
  mutate(DemersPelag = ifelse(DemersPelag == "bathydemersal", "demersal", DemersPelag)) %>%
  mutate(DemersPelag = ifelse(DemersPelag == "bathypelagic", "pelagic-oceanic", DemersPelag)) %>%
  mutate(DemersPelag = ifelse(DemersPelag == "reef-associated", "pelagic-neritic", DemersPelag)) %>%
  mutate(feeding_mode = ifelse(feeding_mode == "filtering plankton", "selective plankton feeding", feeding_mode)) %>% 
  mutate(feeding_mode = ifelse(feeding_mode == "grazing on aquatic plants", "variable", feeding_mode)) %>% 
  mutate(feeding_mode = ifelse(feeding_mode == "browsing on substrate", "variable", feeding_mode)) %>% 
  mutate(EnvTemp = ifelse(EnvTemp == "deep-water", "polar", EnvTemp)) %>% 
  mutate(feeding_mode = ifelse(feeding_mode == "hunting macrofauna (predator)", "predator", feeding_mode)) %>%
  mutate(feeding_mode = ifelse(feeding_mode == "selective plankton feeding", "plankton feeding", feeding_mode)) %>%
  mutate(body_part = ifelse(body_part == "muscle_skinless", "muscle", body_part)) %>% 
  mutate(body_part = ifelse(body_part == "whole", "muscle_organs", body_part)) %>% 
  mutate(body_part = ifelse(body_part == "muscle_organs", "muscle & organs", body_part)) %>% 
  filter(!is.na(concentration)) %>%
  rename(species1 = Species) %>% 
  group_by(species1, feeding_mode, EnvTemp, DemersPelag, body_part, realm, feeding_level) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level) %>% 
  ungroup() %>% 
  distinct(species1, feeding_mode, EnvTemp, DemersPelag, realm, feeding_level, .keep_all = TRUE) %>% 
  filter(complete.cases(.))

table(iron_all$body_part)


# iron all parts pgls -----------------------------------------------------

iron_all$species1 <- str_to_lower(iron_all$species1)
# unique(iron_all$part)
length(unique(iron_all$species1))

iron_all_taxa <- tnrs_match_names(unique(iron_all$species1), context="Animals", names = unique(iron_all$species1), do_approximate_matching = TRUE) 

tr_iron_all <- tol_induced_subtree(ott_ids = ott_id(iron_all_taxa), label_format="name") 
tr_bl_iron_all <- compute.brlen(tr_iron_all)
phylo <- tr_bl_iron_all
iron_all2 <- iron_all %>% 
  left_join(., iron_all_taxa, by = c("species1" = "search_string")) %>% 
  mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl_iron_all$tip.label)) %>% 
  ungroup() %>% 
  mutate(DemersPelag = ifelse(DemersPelag == "pelagic", "pelagic-oceanic", DemersPelag)) %>%
  mutate(DemersPelag = ifelse(DemersPelag == "bathydemersal", "demersal", DemersPelag)) %>%
  mutate(DemersPelag = ifelse(DemersPelag == "bathypelagic", "pelagic-oceanic", DemersPelag)) %>%
  mutate(DemersPelag = ifelse(DemersPelag == "reef-associated", "pelagic-neritic", DemersPelag)) %>%
  mutate(feeding_mode = ifelse(feeding_mode == "filtering plankton", "selective plankton feeding", feeding_mode)) %>% 
  mutate(feeding_mode = ifelse(feeding_mode == "grazing on aquatic plants", "variable", feeding_mode)) %>% 
  mutate(feeding_mode = ifelse(feeding_mode == "browsing on substrate", "variable", feeding_mode)) %>% 
  mutate(EnvTemp = ifelse(EnvTemp == "deep-water", "polar", EnvTemp)) %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>%
  # mutate(BodyShapeI = as.factor(BodyShapeI)) %>%
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(realm = as.factor(realm)) %>% 
  mutate(body_part = as.factor(body_part))
iron_all2$log_length <- scale(iron_all2$log_length)
iron_all2$bulk_trophic_level <- scale(iron_all2$bulk_trophic_level)
# iron_all2$DepthRangeDeep <- siron_alle(iron_all2$DepthRangeDeep)
# iron_all2$AgeMatMin <- siron_alle(iron_all2$AgeMatMin)

table(iron_all2$DemersPelag)

data <- iron_all2
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
iron_all_tree <- pruned.tree
# plot(tree)
# is.ultrametric(tree) # has to be TRUE
# is.rooted(tree) # TRUE

any(duplicated(iron_all_tree$node.label)) # FALSE

iron_all_tree$node.label<-NULL

#prune data to match treetips
Phylodata <- data[(data$Phylospecies %in% iron_all_tree$tip.label),]

Phylodata1 <- Phylodata %>% 
  # mutate(part = as.factor(part)) %>% 
  rename(species = Phylospecies) 

iron_allg <- Phylodata1 %>% 
  group_by(species, DemersPelag, 
           # feeding_level,
           feeding_mode, realm, EnvTemp, body_part) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level) %>% 
  ungroup() %>%
  distinct(species, .keep_all = TRUE) 

View(iron_allg)


iron_allg2 <- iron_allg 
iron_allg2 <- iron_allg2[match(iron_all_tree$tip.label, iron_allg2$species),]
row.names(iron_allg2) <- iron_allg2$species
table(iron_allg2$DemersPelag)


# model selection iron ----------------------------------------------------
library(phylolm)
library(rr2)
mod1p <- phylolm(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + EnvTemp + body_part, phy = iron_all_tree, data = iron_allg2, model = "lambda")
summary(mod1p)
R2(mod1p, phy = iron_all_tree)
mod1p <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + realm + EnvTemp + body_part, 
             correlation = corPagel(value = 0, phy = iron_all_tree, fixed = TRUE), data = iron_allg2, method = "ML")



# mod1p <- phylolm(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + EnvTemp + body_part, data = iron_all)
summary(mod1p)
R2(mod1p)

plot1 <- visreg(mod1p, "DemersPelag", gg = TRUE, size = 6) +
  ylab("log(iron) mg/100g") + xlab("Habitat association") +
  theme(axis.text.x = element_text(angle = 90))
plot2 <- visreg(mod1p, "log_length", gg = TRUE, size = 6) +
  ylab("log(iron) mg/100g") + xlab("Log length (cm)")  +
  theme(axis.text.x = element_text(angle = 90))
plot3 <- visreg(mod1p, "realm", gg = TRUE, size = 6) +
  ylab("log(iron) mg/100g") + xlab("Realm") +
  theme(axis.text.x = element_text(angle = 90))
plot4 <- visreg(mod1p, "bulk_trophic_level", gg = TRUE, size = 6) +
  ylab("log(iron) mg/100g") + xlab("Trophic position") +
  theme(axis.text.x = element_text(angle = 90))
plot5 <- visreg(mod1p, "EnvTemp", gg = TRUE, size = 6) +
  ylab("log(iron) mg/100g") + xlab("Thermal regime") +
  theme(axis.text.x = element_text(angle = 90))
plot6 <- visreg(mod1p, "feeding_mode", gg = TRUE, size = 6) +
  ylab("log(iron) mg/100g") + xlab("Feeding mode") +
  theme(axis.text.x = element_text(angle = 90))
plot7 <- visreg(mod1p, "body_part", gg = TRUE, size = 6) +
  ylab("log(iron) mg/100g") + xlab("Body part") +
  theme(axis.text.x = element_text(angle = 90))

plot_all <- plot1 + plot2 + plot3 + plot4 + plot5 + plot6 + plot7+
  plot_annotation(tag_levels = 'A') + plot_layout(ncol = 3)
ggsave("figures/all-parts-pgls-iron-partial-regressions-oct.pdf", plot = plot_all, width = 8, height = 11)


#################### END OF ANALYSIS SCRIPT ##############

#### Extra code

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
