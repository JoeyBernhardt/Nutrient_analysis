
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





epa <-  s2 %>% 
  filter(nutrient == "epa") %>% 
  filter(!is.na(concentration)) %>% 
  filter(body_part %in% c("muscle", "muscle_skinless")) %>% 
  group_by(Species, feeding_mode, DemersPelag, realm, EnvTemp) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level) %>% 
  ungroup() %>% 
  filter(complete.cases(.)) %>% 
  # rename(part = part_edited) %>% 
  rename(species1 = Species)


# PGLS calcium muscle only ------------------------------------------------------------


epa$species1 <- str_to_lower(epa$species1)
# unique(epa$part)
length(unique(epa$species1))

epa_taxa <- tnrs_match_names(unique(epa$species1), context="Animals", names = unique(epa$species1), do_approximate_matching = TRUE) 

tr_epa <- tol_induced_subtree(ott_ids = ott_id(epa_taxa), label_format="name") 
tr_bl_epa <- compute.brlen(tr_epa)
phylo <- tr_bl_epa
epa2 <- epa %>% 
  left_join(., epa_taxa, by = c("species1" = "search_string")) %>% 
  mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl_epa$tip.label)) %>% 
  ungroup() %>% 
	mutate(DemersPelag = ifelse(DemersPelag == "pelagic", "pelagic-oceanic", DemersPelag)) %>%
	mutate(EnvTemp = ifelse(EnvTemp == "boreal", "polar", EnvTemp)) %>%
	mutate(EnvTemp = ifelse(EnvTemp == "deep-water", "polar", EnvTemp)) %>%
	mutate(DemersPelag = ifelse(DemersPelag == "bathypelagic", "pelagic-oceanic", DemersPelag)) %>%
	mutate(DemersPelag = ifelse(DemersPelag == "bathydemersal", "demersal", DemersPelag)) %>%
	mutate(DemersPelag = ifelse(DemersPelag == "reef-associated", "pelagic-neritic", DemersPelag)) %>%
	mutate(feeding_mode = ifelse(feeding_mode == "other", "variable", feeding_mode)) %>%
	mutate(feeding_mode = ifelse(feeding_mode == "browsing on substrate", "variable", feeding_mode)) %>%
	mutate(feeding_mode = ifelse(feeding_mode == "selective plankton feeding", "filtering plankton", feeding_mode)) %>%
  # mutate(feeding_level = as.factor(feeding_level)) %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>%
  # mutate(BodyShapeI = as.factor(BodyShapeI)) %>%
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(realm = as.factor(realm))
epa2$log_length <- scale(epa2$log_length)
epa2$bulk_trophic_level <- scale(epa2$bulk_trophic_level)

# epa2$DepthRangeDeep <- sepae(epa2$DepthRangeDeep)
# epa2$AgeMatMin <- sepae(epa2$AgeMatMin)

table(epa2$DemersPelag)
data <- epa2
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
epa_tree <- pruned.tree
# plot(tree)
# is.ultrametric(tree) # has to be TRUE
# is.rooted(tree) # TRUE

any(duplicated(epa_tree$node.label)) # FALSE

epa_tree$node.label<-NULL
write.tree(epa_tree, "data-to-share/epa-seafood-species-muscle.tre")


#prune data to match treetips
Phylodata <- data[(data$Phylospecies %in% epa_tree$tip.label),]

Phylodata1 <- Phylodata %>% 
  # mutate(part = as.factor(part)) %>% 
  rename(species = Phylospecies) 

epag <- Phylodata1 %>% 
  group_by(species, DemersPelag, 
           # feeding_level,
           feeding_mode, realm, EnvTemp) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level) %>% 
  ungroup() %>%
  distinct(species, .keep_all = TRUE) 




epag2 <- epag 
epag2 <- epag2[match(epa_tree$tip.label, epag2$species),]
row.names(epag2) <- epag2$species






# model selection epa ----------------------------------------------------
mod1p <- phylolm(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + EnvTemp, phy = epa_tree, data = epag2, model = "lambda")
summary(mod1p) #### tells us lamba ~ 0

mod1 <- gls(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + EnvTemp, correlation = corPagel(value = 0.43, phy = epa_tree, fixed = TRUE), data = epag2, method = "ML")
mod2 <- gls(log_concentration ~  feeding_mode + log_length + EnvTemp + bulk_trophic_level + realm, correlation = corPagel(value = 0.43, phy = epa_tree, fixed = TRUE), data = epag2, method = "ML")
mod2b <- gls(log_concentration ~  feeding_mode + log_length + EnvTemp + bulk_trophic_level, correlation = corPagel(value = 0.43, phy = epa_tree, fixed = TRUE), data = epag2, method = "ML")
mod3 <- gls(log_concentration ~  feeding_mode + DemersPelag + bulk_trophic_level, correlation = corPagel(value = 0.43, phy = epa_tree, fixed = TRUE), data = epag2, method = "ML")
mod4 <- gls(log_concentration ~  log_length + DemersPelag + bulk_trophic_level, correlation = corPagel(value = 0.43, phy = epa_tree, fixed = TRUE), data = epag2, method = "ML")
mod5 <- gls(log_concentration ~  log_length + bulk_trophic_level, correlation = corPagel(value = 0.43, phy = epa_tree, fixed = TRUE), data = epag2, method = "ML")
mod6 <- gls(log_concentration ~  log_length + feeding_mode, correlation = corPagel(value = 0.43, phy = epa_tree, fixed = TRUE), data = epag2, method = "ML")
mod7 <- gls(log_concentration ~  bulk_trophic_level + feeding_mode, correlation = corPagel(value = 0.43, phy = epa_tree, fixed = TRUE), data = epag2, method = "ML")
mod8 <- gls(log_concentration ~  log_length + DemersPelag, correlation = corPagel(value = 0.43, phy = epa_tree, fixed = TRUE), data = epag2, method = "ML")
mod9 <- gls(log_concentration ~  feeding_mode + DemersPelag, correlation = corPagel(value = 0.43, phy = epa_tree, fixed = TRUE), data = epag2, method = "ML")
mod10 <- gls(log_concentration ~  bulk_trophic_level + DemersPelag, corPagel(value = 0.43, phy = epa_tree, fixed = TRUE), data = epag2, method = "ML")
mod11 <- gls(log_concentration ~  log_length + EnvTemp, correlation = corPagel(value = 0.43, phy = epa_tree, fixed = TRUE), data = epag2, method = "ML")
mod12 <- gls(log_concentration ~  feeding_mode + EnvTemp, correlation = corPagel(value = 0.43, phy = epa_tree, fixed = TRUE), data = epag2, method = "ML")
mod13 <- gls(log_concentration ~  bulk_trophic_level + EnvTemp, correlation = corPagel(value = 0.43, phy = epa_tree, fixed = TRUE), data = epag2, method = "ML")
mod14 <- gls(log_concentration ~  1, corPagel(value = 0.43, phy = epa_tree, fixed = TRUE), data = epag2, method = "ML")




R2(mod11, phy = epa_tree)

### model selection
msel_epa <- model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, mod13, mod2b, mod14, rank = AICc) %>% 
	mutate(model_num = rownames(.)) %>% 
	mutate(cum_weight = cumsum(weight))



confints_epa <- data.frame(confint(model.avg(get.models(msel_epa, subset = cumsum(weight) <= .95))),
							estimate = coef(model.avg(get.models(msel_epa, subset = cumsum(weight) <= .95)))) %>% 
	mutate(term = rownames(.)) %>% 
	rename(lower = X2.5..) %>% 
	rename(upper = X97.5..) %>% 
	mutate(nutrient = "epa")

confints_epa <- data.frame(confint(mod2),
						   estimate = coef(mod2)) %>% 
	mutate(term = rownames(.)) %>% 
	rename(lower = X2.5..) %>% 
	rename(upper = X97.5..) %>% 
	mutate(nutrient = "epa")
write_csv(confints_epa, "data-processed/epa-traits-confints.csv")

epa_plot <- confints_epa %>% 
	filter(term != "(Intercept)") %>% 
	ggplot(aes(x = term, y = estimate)) + 
	geom_pointrange(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
	coord_flip() +
	geom_hline(yintercept = 0) + ggtitle("epa")
epa_plot

mod1 <- phylolm(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm, phy = epa_tree, data = epag2, model = "lambda")
mod2 <- phylolm(log_concentration ~  feeding_mode + log_length + EnvTemp + bulk_trophic_level + realm, phy = epa_tree, data = epag2, model = "lambda")

mod3 <- phylolm(log_concentration ~  feeding_mode + DemersPelag + bulk_trophic_level, phy = epa_tree, data = epag2, model = "lambda")
mod4 <- phylolm(log_concentration ~  log_length + DemersPelag + bulk_trophic_level, phy = epa_tree, data = epag2, model = "lambda")

mod5 <- phylolm(log_concentration ~  log_length + bulk_trophic_level, phy = epa_tree, data = epag2, model = "lambda")
mod6 <- phylolm(log_concentration ~  log_length + feeding_mode, phy = epa_tree, data = epag2, model = "lambda")
mod7 <- phylolm(log_concentration ~  bulk_trophic_level + feeding_mode, phy = epa_tree, data = epag2, model = "lambda")
mod8 <- phylolm(log_concentration ~  log_length + DemersPelag, phy = epa_tree, data = epag2, model = "lambda")
mod9 <- phylolm(log_concentration ~  feeding_mode + DemersPelag, phy = epa_tree, data = epag2, model = "lambda")
mod10 <- phylolm(log_concentration ~  bulk_trophic_level + DemersPelag, phy = epa_tree, data = epag2, model = "lambda")
mod11 <- phylolm(log_concentration ~  log_length + EnvTemp, phy = epa_tree, data = epag2, model = "lambda")
mod12 <- phylolm(log_concentration ~  feeding_mode + EnvTemp, phy = epa_tree, data = epag2, model = "lambda")
mod13 <- phylolm(log_concentration ~  bulk_trophic_level + EnvTemp, phy = epa_tree, data = epag2, model = "lambda")
mod14 <- phylolm(log_concentration ~  1, phy = epa_tree, data = epag2, model = "lambda")

model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, mod13, mod14, rank = AICc, extra = "rsquared") %>%  
  mutate(model_number = rownames(.)) %>% 
  mutate(cumsum = cumsum(weight)) %>% View
# summary(model.avg(mod9, mod2))
# confint(model.avg(mod9, mod2))
# coef(model.avg(mod9, mod2))
# 
# R2(mod2, phy = epa_tree)


confints_epa <- data.frame(confint(model.avg(mod2, mod13, mod11)), estimate = coef(model.avg(mod2, mod13, mod11))) %>% 
  mutate(term = rownames(.)) %>% 
  rename(lower = X2.5..) %>% 
  rename(upper = X97.5..)

epa_plot <- confints_epa %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
  coord_flip() +
  geom_hline(yintercept = 0) + ggtitle("epa")
epa_plot


# EPA all parts -----------------------------------------------------------

epa_all <-  s2 %>% 
  filter(nutrient == "epa") %>% 
  # filter(body_part != "skin") %>% 
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
  # mutate(body_part = ifelse(body_part == "muscle_organs", "muscle & organs", body_part)) %>% 
  filter(!is.na(concentration)) %>%
  rename(species1 = Species) %>% 
  group_by(species1, feeding_mode, EnvTemp, DemersPelag, body_part, realm, feeding_level) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level) %>% 
  ungroup() %>% 
  distinct(species1, feeding_mode, EnvTemp, DemersPelag, realm, feeding_level, .keep_all = TRUE) %>% 
  filter(complete.cases(.))

table(epa_all$body_part)


# epa all parts pgls -----------------------------------------------------

epa_all$species1 <- str_to_lower(epa_all$species1)
# unique(epa_all$part)
length(unique(epa_all$species1))

epa_all_taxa <- tnrs_match_names(unique(epa_all$species1), context="Animals", names = unique(epa_all$species1), do_approximate_matching = TRUE) 

tr_epa_all <- tol_induced_subtree(ott_ids = ott_id(epa_all_taxa), label_format="name") 
tr_bl_epa_all <- compute.brlen(tr_epa_all)
phylo <- tr_bl_epa_all
epa_all2 <- epa_all %>% 
  left_join(., epa_all_taxa, by = c("species1" = "search_string")) %>% 
  mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl_epa_all$tip.label)) %>% 
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
epa_all2$log_length <- scale(epa_all2$log_length)
epa_all2$bulk_trophic_level <- scale(epa_all2$bulk_trophic_level)
# epa_all2$DepthRangeDeep <- sepa_alle(epa_all2$DepthRangeDeep)
# epa_all2$AgeMatMin <- sepa_alle(epa_all2$AgeMatMin)

table(epa_all2$DemersPelag)

data <- epa_all2
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
epa_all_tree <- pruned.tree
write.tree(epa_all_tree, "data-to-share/epa-seafood-species-all-tissues.tre")

# plot(tree)
# is.ultrametric(tree) # has to be TRUE
# is.rooted(tree) # TRUE

any(duplicated(epa_all_tree$node.label)) # FALSE

epa_all_tree$node.label<-NULL

#prune data to match treetips
Phylodata <- data[(data$Phylospecies %in% epa_all_tree$tip.label),]

Phylodata1 <- Phylodata %>% 
  # mutate(part = as.factor(part)) %>% 
  rename(species = Phylospecies) 

epa_allg <- Phylodata1 %>% 
  group_by(species, DemersPelag, 
           # feeding_level,
           feeding_mode, realm, EnvTemp, body_part) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level) %>% 
  ungroup() %>%
  distinct(species, .keep_all = TRUE) 

View(epa_allg)


epa_allg2 <- epa_allg 
epa_allg2 <- epa_allg2[match(epa_all_tree$tip.label, epa_allg2$species),]
row.names(epa_allg2) <- epa_allg2$species
table(epa_allg2$DemersPelag)


# model selection epa ----------------------------------------------------
library(phylolm)
library(rr2)
mod1p <- phylolm(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + EnvTemp + body_part, phy = epa_all_tree, data = epa_allg2, model = "lambda")
summary(mod1p)
R2(mod1p, phy = epa_all_tree)
mod1p <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + realm + EnvTemp + body_part, 
             correlation = corPagel(value = 0.43, phy = epa_all_tree, fixed = TRUE), data = epa_allg2, method = "ML")



# mod1p <- phylolm(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + EnvTemp + body_part, data = epa_all)
summary(mod1p)
R2(mod1p)

plot1 <- visreg(mod1p, "DemersPelag", gg = TRUE, size = 6) +
  ylab("log(epa) mg/100g") + xlab("Habitat association") +
  theme(axis.text.x = element_text(angle = 90))
plot2 <- visreg(mod1p, "log_length", gg = TRUE, size = 6) +
  ylab("log(epa) mg/100g") + xlab("Log length (cm)")  +
  theme(axis.text.x = element_text(angle = 90))
plot3 <- visreg(mod1p, "realm", gg = TRUE, size = 6) +
  ylab("log(epa) mg/100g") + xlab("Realm") +
  theme(axis.text.x = element_text(angle = 90))
plot4 <- visreg(mod1p, "bulk_trophic_level", gg = TRUE, size = 6) +
  ylab("log(epa) mg/100g") + xlab("Trophic position") +
  theme(axis.text.x = element_text(angle = 90))
plot5 <- visreg(mod1p, "EnvTemp", gg = TRUE, size = 6) +
  ylab("log(epa) mg/100g") + xlab("Thermal regime") +
  theme(axis.text.x = element_text(angle = 90))
plot6 <- visreg(mod1p, "feeding_mode", gg = TRUE, size = 6) +
  ylab("log(epa) mg/100g") + xlab("Feeding mode") +
  theme(axis.text.x = element_text(angle = 90))
plot7 <- visreg(mod1p, "body_part", gg = TRUE, size = 6) +
  ylab("log(epa) mg/100g") + xlab("Body part") +
  theme(axis.text.x = element_text(angle = 90))

plot_all <- plot1 + plot2 + plot3 + plot4 + plot5 + plot6 + plot7+
  plot_annotation(tag_levels = 'A') + plot_layout(ncol = 3)
ggsave("figures/all-parts-pgls-epa-partial-regressions-oct.pdf", plot = plot_all, width = 8, height = 11)



# model summary table -----------------------------------------------------

lambda <- round(mod1p$modelStruct[[1]][[1]], digits = 2)

# visreg(mod1a)
rsq_mod1p <- round(rsquared(mod1p)['R.squared'][[1]], digits = 2)

stargazer(mod1p, title = "", type = "html", out="tables/epa-pgls-all-parts-oct.htm", 
          add.lines = list(c("R2", rsq_mod1p), c("Lamba", lambda)), ci=TRUE, ci.level=0.95, digits = 2, single.row = TRUE)





##### epa all parrts model sel

mod1 <- gls(log_concentration ~  body_part + feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + EnvTemp, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod2 <- gls(log_concentration ~  body_part + feeding_mode + log_length + EnvTemp + bulk_trophic_level + realm, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod2b <- gls(log_concentration ~  body_part + feeding_mode + log_length + EnvTemp + bulk_trophic_level, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod3 <- gls(log_concentration ~  body_part + feeding_mode + DemersPelag + bulk_trophic_level, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod4 <- gls(log_concentration ~  body_part + log_length + DemersPelag + bulk_trophic_level, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod5 <- gls(log_concentration ~  body_part + log_length + bulk_trophic_level, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod5b <- gls(log_concentration ~   log_length + bulk_trophic_level, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")

mod6 <- gls(log_concentration ~  body_part + log_length + feeding_mode, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod7 <- gls(log_concentration ~  body_part + bulk_trophic_level + feeding_mode, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod8 <- gls(log_concentration ~  body_part + log_length + DemersPelag, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod9 <- gls(log_concentration ~  body_part + feeding_mode + DemersPelag, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod10 <- gls(log_concentration ~  body_part + bulk_trophic_level + DemersPelag, corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod11 <- gls(log_concentration ~  body_part + log_length + EnvTemp, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod12 <- gls(log_concentration ~  body_part + feeding_mode + EnvTemp, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod13 <- gls(log_concentration ~  body_part + bulk_trophic_level + EnvTemp, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")


mod1b <- gls(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + EnvTemp, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod2b <- gls(log_concentration ~  feeding_mode + log_length + EnvTemp + bulk_trophic_level + realm, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod2bb <- gls(log_concentration ~  feeding_mode + log_length + EnvTemp + bulk_trophic_level, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod3b <- gls(log_concentration ~  feeding_mode + DemersPelag + bulk_trophic_level, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod4b <- gls(log_concentration ~  log_length + DemersPelag + bulk_trophic_level, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod5b <- gls(log_concentration ~  log_length + bulk_trophic_level, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod6b <- gls(log_concentration ~  log_length + feeding_mode, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod7b <- gls(log_concentration ~ bulk_trophic_level + feeding_mode, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod8b <- gls(log_concentration ~  log_length + DemersPelag, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod9b <- gls(log_concentration ~  feeding_mode + DemersPelag, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod10b <- gls(log_concentration ~  bulk_trophic_level + DemersPelag, corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod11b <- gls(log_concentration ~  log_length + EnvTemp, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod12b <- gls(log_concentration ~  feeding_mode + EnvTemp, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")
mod13b <- gls(log_concentration ~  bulk_trophic_level + EnvTemp, correlation = corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")



mod14 <- gls(log_concentration ~  1, corPagel(value = 0, phy = epa_all_tree, fixed = FALSE), data = epa_allg2, method = "ML")




R2(mod11, phy = epa_all_tree)

### model selection
msel_epa_all <- model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, mod13, mod2b,
                           mod1b, mod3b, mod4b, mod5b, mod6b, mod7b, mod8b, mod9b, mod10b, mod11b, mod12b, mod13b, mod2bb, rank = AICc) 

msel_epa_all2 <- model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, mod13, mod2b,
                            mod1b, mod3b, mod4b, mod5b, mod6b, mod7b, mod8b, mod9b, mod10b, mod11b, mod12b, mod13b, mod2bb, rank = AICc, extra = "R2") %>% 
  mutate(model_num = rownames(.)) %>% 
  mutate(cum_weight = cumsum(weight)) %>%
  filter(cum_weight <= .95)

model_table_epa_all <- msel_epa_all2 %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(term = rownames(.)) %>% 
  dplyr::select(term, everything())

str(model_table_epa_all)


confints_epa_all <- data.frame(confint(model.avg(get.models(msel_epa_all, subset = cumsum(weight) <= .95))),
                                estimate = coef(model.avg(get.models(msel_epa_all, subset = cumsum(weight) <= .95)))) %>% 
  mutate(term = rownames(.)) %>% 
  rename(lower = X2.5..) %>% 
  rename(upper = X97.5..) %>% 
  mutate(nutrient = "epa") %>% 
  mutate(term2 = case_when(grepl("body_part", term) ~ "body_part",
                           grepl("feeding_mode", term) ~ "feeding_mode",
                           grepl("EnvTemp", term) ~ "EnvTemp",
                           grepl("DemersPelag", term) ~ "DemersPelag",
                           grepl("realm", term) ~ "realm",
                           TRUE ~ term))

View(confints_epa_all)

epa_all_weights <- data.frame(wip = sw(get.models(msel_epa_all, subset = cumsum(weight) <= .95))) %>% 
  mutate(term2 = rownames(.))

epa_all_outputs <- full_join(confints_epa_all, model_table_epa_all, by = c("term2" = "term")) %>% 
  left_join(., epa_all_weights) %>% 
  filter(term2 != "model_num") %>% 
  filter(term2 != "(Intercept)") %>% 
  filter(term2 != "AICc") %>% 
  filter(term2 != "logLik") %>% 
  filter(term2 != "df") %>% 
  filter(term2 != "R2.R2_lik") %>% 
  filter(term2 != "R2.R2_resid") %>% 
  mutate(term = ifelse(is.na(term), term2, term)) %>% 
  dplyr::select(term2, term, contains("V"), estimate, lower, upper, everything(), wip, -nutrient, -term2) %>% 
  mutate(across(.cols = where(is.numeric), .funs = round, .digits = 2))



#### top models are model 2 and model 1
summary(mod1) ## lambda = 0.43
summary(mod2) ### lambda = 0.43

write_csv(epa_all_outputs, "tables/epa-pgls-all-parts.csv")



epa_plot_all <- confints_epa_all %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
  coord_flip() +
  geom_hline(yintercept = 0) + ggtitle("epa")
epa_plot_all


top_models_epa_all <- get.models(msel_epa_all, subset = cumsum(weight) <= .95)
sw(top_models_epa_all)



