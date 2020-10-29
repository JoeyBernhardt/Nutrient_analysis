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



#### this script contains the phylogenetic regressions for calcium concentration as a function of species' traits

all_traits6 <- read_csv("data-processed/fishbase-traits-aug-26-2020.csv")
s2 <- all_traits6 %>% 
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
	mutate(feeding_mode = ifelse(feeding_mode == "browsing on substrate", "variable", feeding_mode)) %>%
	mutate(feeding_mode = ifelse(feeding_mode == "filtering plankton", "selective plankton feeding", feeding_mode)) %>%
	mutate(DemersPelag = ifelse(DemersPelag == "pelagic", "pelagic-oceanic", DemersPelag)) %>%
	mutate(DemersPelag = ifelse(DemersPelag == "bathydemersal", "demersal", DemersPelag)) %>%
	mutate(DemersPelag = ifelse(DemersPelag == "bathypelagic", "pelagic-oceanic", DemersPelag)) %>%
	mutate(DemersPelag = ifelse(DemersPelag == "reef-associated", "pelagic-neritic", DemersPelag)) %>%
	mutate(EnvTemp = ifelse(EnvTemp == "deep-water", "polar", EnvTemp)) %>%
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>%
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(realm = as.factor(realm))
calcium2$log_length <- scale(calcium2$log_length)
calcium2$bulk_trophic_level <- scale(calcium2$bulk_trophic_level)

table(calcium2$DemersPelag)




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
write.tree(calcium_tree, "data-to-share/calcium-seafood-species-muscle.tre")
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



# model averaging for calcium ------------------------------------------------


mod1_calcium <- gls(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + EnvTemp, correlation = corBrownian(phy = calcium_tree), data = calciumg2, method = "ML")
summary(mod1_calcium)
confint(mod1_calcium)
dd_calcium <- dredge(mod1_calcium, m.lim = c(2, 5), extra = "R2") %>% 
	mutate(mod_number = rownames(.)) %>% 
	mutate(cum_weight = cumsum(weight))
View(dd_calcium)
mods_calcium <- get.models(dd_calcium, subset= cumsum(weight) <= .95)
R2(mods_calcium$`5`)
sw(mods_calcium)
ci_avg_calcium <- rownames_to_column(as.data.frame(confint(model.avg(dd_calcium, subset = cumsum(weight) <= .95))), var = "term")
slopes_avg_calcium <- enframe(coef(model.avg(dd_calcium, subset = cumsum(weight) <= .95)), name = "term", value = "slope")

calcium_mod_out <- left_join(ci_avg_calcium, slopes_avg_calcium) %>% 
	rename(lower = `2.5 %`,
		   upper = `97.5 %`) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(nutrient = "calcium") %>% 
	mutate(conf_int_overlap_0 = ifelse(upper < 0 & slope < 0 | lower > 0 & slope > 0, "yes", "no")) 

calcium_mod_out %>% 
	ggplot(aes(x = term, y = slope)) + 
	geom_pointrange(aes(x = term, y = slope, ymin = lower, ymax = upper), fill = "transparent") +
	geom_point(aes(x = term, y = slope, shape = conf_int_overlap_0, color = conf_int_overlap_0)) +
	scale_color_manual(values = c("black", "white")) +
	scale_shape_manual(values = c(1, 19)) +
	geom_hline(yintercept = 0) + coord_flip() +
	theme(legend.position = "none")






mod1 <- phylolm(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm, phy = calcium_tree, data = calciumg2, model = "lambda")
R2(mod1, phy = calcium_tree)
mod2 <- phylolm(log_concentration ~  feeding_mode + log_length + EnvTemp + bulk_trophic_level + realm, phy = calcium_tree, data = calciumg2, model = "lambda")
mod2b <- phylolm(log_concentration ~  feeding_mode + log_length + EnvTemp + bulk_trophic_level, phy = calcium_tree, data = calciumg2, model = "lambda")
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

#### try the same with gls
library(caper)
com_calcium <- comparative.data(calcium_tree, as.data.frame(calciumg2), "species", vcv.dim = 3)
mod1 <- gls(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + EnvTemp, correlation = corPagel(value = 0, phy = calcium_tree, fixed = TRUE), data = calciumg2, method = "ML")
mod2 <- gls(log_concentration ~  feeding_mode + log_length + EnvTemp + bulk_trophic_level + realm, correlation = corPagel(value = 0, phy = calcium_tree, fixed = TRUE), data = calciumg2, method = "ML")
mod2b <- gls(log_concentration ~  feeding_mode + log_length + EnvTemp + bulk_trophic_level, correlation = corPagel(value = 0, phy = calcium_tree, fixed = TRUE), data = calciumg2, method = "ML")
mod3 <- gls(log_concentration ~  feeding_mode + DemersPelag + bulk_trophic_level, correlation = corPagel(value = 0, phy = calcium_tree, fixed = TRUE), data = calciumg2, method = "ML")
mod4 <- gls(log_concentration ~  log_length + DemersPelag + bulk_trophic_level, correlation = corPagel(value = 0, phy = calcium_tree, fixed = TRUE), data = calciumg2, method = "ML")
mod5 <- gls(log_concentration ~  log_length + bulk_trophic_level, correlation = corPagel(value = 0, phy = calcium_tree, fixed = TRUE), data = calciumg2, method = "ML")
mod6 <- gls(log_concentration ~  log_length + feeding_mode, correlation = corPagel(value = 0, phy = calcium_tree, fixed = TRUE), data = calciumg2, method = "ML")
mod7 <- gls(log_concentration ~  bulk_trophic_level + feeding_mode, correlation = corPagel(value = 0, phy = calcium_tree, fixed = TRUE), data = calciumg2, method = "ML")
mod8 <- gls(log_concentration ~  log_length + DemersPelag, correlation = corPagel(value = 0, phy = calcium_tree, fixed = TRUE), data = calciumg2, method = "ML")
mod9 <- gls(log_concentration ~  feeding_mode + DemersPelag, correlation = corPagel(value = 0, phy = calcium_tree, fixed = TRUE), data = calciumg2, method = "ML")
mod10 <- gls(log_concentration ~  bulk_trophic_level + DemersPelag, corPagel(value = 0, phy = calcium_tree, fixed = TRUE), data = calciumg2, method = "ML")
mod11 <- gls(log_concentration ~  log_length + EnvTemp, correlation = corPagel(value = 0, phy = calcium_tree, fixed = TRUE), data = calciumg2, method = "ML")
mod12 <- gls(log_concentration ~  feeding_mode + EnvTemp, correlation = corPagel(value = 0, phy = calcium_tree, fixed = TRUE), data = calciumg2, method = "ML")
mod13 <- gls(log_concentration ~  bulk_trophic_level + EnvTemp, correlation = corPagel(value = 0, phy = calcium_tree, fixed = TRUE), data = calciumg2, method = "ML")
mod14 <- gls(log_concentration ~  1, corPagel(value = 0, phy = calcium_tree, fixed = TRUE), data = calciumg2, method = "ML")




R2(mod11, phy = calcium_tree)

### model selection
msel_calcium <- model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, mod13, mod2b, mod14, rank = AICc, extra = "R2") %>% 
	mutate(model_num = rownames(.)) %>% 
	mutate(cum_weight = cumsum(weight))



confints_calcium <- data.frame(confint(model.avg(get.models(msel_calcium, subset = cumsum(weight) <= .95))),
							   estimate = coef(model.avg(get.models(msel_calcium, subset = cumsum(weight) <= .95)))) %>% 
  mutate(term = rownames(.)) %>% 
  rename(lower = X2.5..) %>% 
  rename(upper = X97.5..) %>% 
	mutate(nutrient = "calcium")
write_csv(confints_calcium, "data-processed/calcium-traits-confints.csv")

calcium_plot <- confints_calcium %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
  coord_flip() +
  geom_hline(yintercept = 0) + ggtitle("calcium")
calcium_plot


# calcium all parts -------------------------------------------------------



calcium_all <-  s2 %>% 
  filter(nutrient == "ca_mg") %>% 
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

table(calcium_all$body_part)
table(calcium_all$EnvTemp)
table(calcium_all$DemersPelag)


# calcium all parts pgls -----------------------------------------------------

calcium_all$species1 <- str_to_lower(calcium_all$species1)
# unique(calcium_all$part)
length(unique(calcium_all$species1))

calcium_all_taxa <- tnrs_match_names(unique(calcium_all$species1), context="Animals", names = unique(calcium_all$species1), do_approximate_matching = TRUE) 

tr_calcium_all <- tol_induced_subtree(ott_ids = ott_id(calcium_all_taxa), label_format="name") 
tr_bl_calcium_all <- compute.brlen(tr_calcium_all)
phylo <- tr_bl_calcium_all
calcium_all2 <- calcium_all %>% 
  left_join(., calcium_all_taxa, by = c("species1" = "search_string")) %>% 
  mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl_calcium_all$tip.label)) %>% 
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
calcium_all2$log_length <- scale(calcium_all2$log_length)
calcium_all2$bulk_trophic_level <- scale(calcium_all2$bulk_trophic_level)
# calcium_all2$DepthRangeDeep <- scalcium_alle(calcium_all2$DepthRangeDeep)
# calcium_all2$AgeMatMin <- scalcium_alle(calcium_all2$AgeMatMin)

table(calcium_all2$DemersPelag)

data <- calcium_all2
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
calcium_all_tree <- pruned.tree
write.tree(calcium_all_tree, "data-to-share/calcium-seafood-species-all-tissues.tre")

# plot(tree)
# is.ultrametric(tree) # has to be TRUE
# is.rooted(tree) # TRUE

any(duplicated(calcium_all_tree$node.label)) # FALSE

calcium_all_tree$node.label<-NULL

#prune data to match treetips
Phylodata <- data[(data$Phylospecies %in% calcium_all_tree$tip.label),]

Phylodata1 <- Phylodata %>% 
  # mutate(part = as.factor(part)) %>% 
  rename(species = Phylospecies) 

calcium_allg <- Phylodata1 %>% 
  group_by(species, DemersPelag, 
           # feeding_level,
           feeding_mode, realm, EnvTemp, body_part) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level) %>% 
  ungroup() %>%
  distinct(species, .keep_all = TRUE) 

View(calcium_allg)


calcium_allg2 <- calcium_allg 
calcium_allg2 <- calcium_allg2[match(calcium_all_tree$tip.label, calcium_allg2$species),]
row.names(calcium_allg2) <- calcium_allg2$species
table(calcium_allg2$DemersPelag)
length(unique(calcium_allg2$species))

# model selection calcium ----------------------------------------------------
library(phylolm)
library(rr2)
mod1p <- phylolm(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + EnvTemp + body_part, phy = calcium_all_tree, data = calcium_allg2, model = "lambda")
summary(mod1p)
R2(mod1p, phy = calcium_all_tree)
mod1p <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + realm + EnvTemp + body_part, 
             correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")



# mod1p <- phylolm(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + EnvTemp + body_part, data = calcium_all)
summary(mod1p)
R2(mod1p)

plot1 <- visreg(mod1p, "DemersPelag", gg = TRUE, size = 6) +
  ylab("log(calcium) mg/100g") + xlab("Habitat association") +
  theme(axis.text.x = element_text(angle = 90))
plot2 <- visreg(mod1p, "log_length", gg = TRUE, size = 6) +
  ylab("log(calcium) mg/100g") + xlab("Log length (cm)")  +
  theme(axis.text.x = element_text(angle = 90))
plot3 <- visreg(mod1p, "realm", gg = TRUE, size = 6) +
  ylab("log(calcium) mg/100g") + xlab("Realm") +
  theme(axis.text.x = element_text(angle = 90))
plot4 <- visreg(mod1p, "bulk_trophic_level", gg = TRUE, size = 6) +
  ylab("log(calcium) mg/100g") + xlab("Trophic position") +
  theme(axis.text.x = element_text(angle = 90))
plot5 <- visreg(mod1p, "EnvTemp", gg = TRUE, size = 6) +
  ylab("log(calcium) mg/100g") + xlab("Climate zone") +
  theme(axis.text.x = element_text(angle = 90))
plot6 <- visreg(mod1p, "feeding_mode", gg = TRUE, size = 6) +
  ylab("log(calcium) mg/100g") + xlab("Feeding mode") +
  theme(axis.text.x = element_text(angle = 90))
plot7 <- visreg(mod1p, "body_part", gg = TRUE, size = 6) +
  ylab("log(calcium) mg/100g") + xlab("Body part") +
  theme(axis.text.x = element_text(angle = 90))

plot_all <- plot1 + plot2 + plot3 + plot4 + plot5 + plot6 + plot7+
  plot_annotation(tag_levels = 'A') + plot_layout(ncol = 3)
ggsave("figures/all-parts-pgls-calcium-partial-regressions-oct2.pdf", plot = plot_all, width = 8, height = 12)


# model summary table -----------------------------------------------------

lambda <- round(mod1p$modelStruct[[1]][[1]], digits = 2)

# visreg(mod1a)
rsq_mod1p <- round(rsquared(mod1p)['R.squared'][[1]], digits = 2)

stargazer(mod1p, title = "", type = "html", out="tables/calcium-pgls-all-parts-oct.htm", 
          add.lines = list(c("R2", rsq_mod1p), c("Lamba", lambda)), ci=TRUE, ci.level=0.95, digits = 2, single.row = TRUE)


##### calcium all parrts model sel

mod1 <- gls(log_concentration ~  body_part + feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + EnvTemp, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod2 <- gls(log_concentration ~  body_part + feeding_mode + log_length + EnvTemp + bulk_trophic_level + realm, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod2b <- gls(log_concentration ~  body_part + feeding_mode + log_length + EnvTemp + bulk_trophic_level, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod3 <- gls(log_concentration ~  body_part + feeding_mode + DemersPelag + bulk_trophic_level, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod4 <- gls(log_concentration ~  body_part + log_length + DemersPelag + bulk_trophic_level, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod5 <- gls(log_concentration ~  body_part + log_length + bulk_trophic_level, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod5b <- gls(log_concentration ~   log_length + bulk_trophic_level, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")

mod6 <- gls(log_concentration ~  body_part + log_length + feeding_mode, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod7 <- gls(log_concentration ~  body_part + bulk_trophic_level + feeding_mode, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod8 <- gls(log_concentration ~  body_part + log_length + DemersPelag, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod9 <- gls(log_concentration ~  body_part + feeding_mode + DemersPelag, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod10 <- gls(log_concentration ~  body_part + bulk_trophic_level + DemersPelag, corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod11 <- gls(log_concentration ~  body_part + log_length + EnvTemp, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod12 <- gls(log_concentration ~  body_part + feeding_mode + EnvTemp, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod13 <- gls(log_concentration ~  body_part + bulk_trophic_level + EnvTemp, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")


mod1b <- gls(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + EnvTemp, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod2b <- gls(log_concentration ~  feeding_mode + log_length + EnvTemp + bulk_trophic_level + realm, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod2bb <- gls(log_concentration ~  feeding_mode + log_length + EnvTemp + bulk_trophic_level, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod3b <- gls(log_concentration ~  feeding_mode + DemersPelag + bulk_trophic_level, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod4b <- gls(log_concentration ~  log_length + DemersPelag + bulk_trophic_level, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod5b <- gls(log_concentration ~  log_length + bulk_trophic_level, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod6b <- gls(log_concentration ~  log_length + feeding_mode, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod7b <- gls(log_concentration ~ bulk_trophic_level + feeding_mode, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod8b <- gls(log_concentration ~  log_length + DemersPelag, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod9b <- gls(log_concentration ~  feeding_mode + DemersPelag, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod10b <- gls(log_concentration ~  bulk_trophic_level + DemersPelag, corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod11b <- gls(log_concentration ~  log_length + EnvTemp, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod12b <- gls(log_concentration ~  feeding_mode + EnvTemp, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")
mod13b <- gls(log_concentration ~  bulk_trophic_level + EnvTemp, correlation = corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")



mod14 <- gls(log_concentration ~  1, corPagel(value = 0, phy = calcium_all_tree, fixed = TRUE), data = calcium_allg2, method = "ML")




R2(mod11, phy = calcium_all_tree)

### model selection
msel_calcium_all <- model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, mod13, mod2b, mod14,
                           mod1b, mod3b, mod4b, mod5b, mod6b, mod7b, mod8b, mod9b, mod10b, mod11b, mod12b, mod13b, mod2bb, rank = AICc) 

msel_calcium_all2 <- model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, mod13, mod2b, mod14,
                            mod1b, mod3b, mod4b, mod5b, mod6b, mod7b, mod8b, mod9b, mod10b, mod11b, mod12b, mod13b, mod2bb, rank = AICc, extra = "R2") %>% 
  mutate(model_num = rownames(.)) %>% 
  mutate(cum_weight = cumsum(weight)) %>%
  filter(cum_weight <= .95)

model_table_calcium_all <- msel_calcium_all2 %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(term = rownames(.)) %>% 
  dplyr::select(term, everything())

str(model_table_calcium_all)


confints_calcium_all <- data.frame(confint(model.avg(get.models(msel_calcium_all, subset = cumsum(weight) <= .95))),
                                estimate = coef(model.avg(get.models(msel_calcium_all, subset = cumsum(weight) <= .95)))) %>% 
  mutate(term = rownames(.)) %>% 
  rename(lower = X2.5..) %>% 
  rename(upper = X97.5..) %>% 
  mutate(nutrient = "calcium") %>% 
  mutate(term2 = case_when(grepl("body_part", term) ~ "body_part",
                           grepl("feeding_mode", term) ~ "feeding_mode",
                           grepl("EnvTemp", term) ~ "EnvTemp",
                           grepl("DemersPelag", term) ~ "DemersPelag",
                           grepl("realm", term) ~ "realm",
                           TRUE ~ term))

View(confints_calcium_all)

calcium_all_weights <- data.frame(wip = sw(get.models(msel_calcium_all, subset = cumsum(weight) <= .95))) %>% 
  mutate(term2 = rownames(.))

calcium_all_outputs <- full_join(confints_calcium_all, model_table_calcium_all, by = c("term2" = "term")) %>% 
  left_join(., calcium_all_weights) %>% 
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

write_csv(calcium_all_outputs, "tables/calcium-pgls-all-parts.csv")



calcium_plot_all <- confints_calcium_all %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
  coord_flip() +
  geom_hline(yintercept = 0) + ggtitle("calcium")
calcium_plot_all


top_models_calcium_all <- get.models(msel_calcium_all, subset = cumsum(weight) <= .95)
sw(top_models_calcium_all)




# calcium small fish ------------------------------------------------------

# calcium all parts small fish -----------------------------------------------
calcium_small <-  s2 %>% 
  filter(nutrient == "ca_mg") %>% 
  filter(length < 50) %>% 
  # filter(body_part != "skin") %>% 
  mutate(body_part = ifelse(body_part %in% c("eggs", "liver", "oil"), "eggs, liver or oil", body_part)) %>%
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
  dplyr::select(Species, body_part, log_length, feeding_mode, DemersPelag, bulk_trophic_level, realm, EnvTemp, log_concentration) %>%
  filter(complete.cases(.))

table(calcium_small$body_part)



##### calcium all parrts model sel

mod1 <- lm(log_concentration ~  body_part + feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + EnvTemp, data = calcium_small)
mod2 <- lm(log_concentration ~  body_part + feeding_mode + log_length + EnvTemp + bulk_trophic_level + realm, data = calcium_small)
mod2b <- lm(log_concentration ~  body_part + feeding_mode + log_length + EnvTemp + bulk_trophic_level, data = calcium_small)
mod3 <- lm(log_concentration ~  body_part + feeding_mode + DemersPelag + bulk_trophic_level, data = calcium_small)
mod4 <- lm(log_concentration ~  body_part + log_length + DemersPelag + bulk_trophic_level, data = calcium_small)
mod5 <- lm(log_concentration ~  body_part + log_length + bulk_trophic_level, data = calcium_small)
mod5b <- lm(log_concentration ~   log_length + bulk_trophic_level, data = calcium_small)

mod6 <- lm(log_concentration ~  body_part + log_length + feeding_mode, data = calcium_small)
mod7 <- lm(log_concentration ~  body_part + bulk_trophic_level + feeding_mode, data = calcium_small)
mod8 <- lm(log_concentration ~  body_part + log_length + DemersPelag, data = calcium_small)
mod9 <- lm(log_concentration ~  body_part + feeding_mode + DemersPelag, data = calcium_small)
mod10 <- lm(log_concentration ~  body_part + bulk_trophic_level + DemersPelag, data = calcium_small)
mod11 <- lm(log_concentration ~  body_part + log_length + EnvTemp, data = calcium_small)
mod12 <- lm(log_concentration ~  body_part + feeding_mode + EnvTemp, data = calcium_small)
mod13 <- lm(log_concentration ~  body_part + bulk_trophic_level + EnvTemp, data = calcium_small)


mod1b <- lm(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + EnvTemp, data = calcium_small)
mod2b <- lm(log_concentration ~  feeding_mode + log_length + EnvTemp + bulk_trophic_level + realm, data = calcium_small)
mod2bb <- lm(log_concentration ~  feeding_mode + log_length + EnvTemp + bulk_trophic_level, data = calcium_small)
mod3b <- lm(log_concentration ~  feeding_mode + DemersPelag + bulk_trophic_level, data = calcium_small)
mod4b <- lm(log_concentration ~  log_length + DemersPelag + bulk_trophic_level, data = calcium_small)
mod5b <- lm(log_concentration ~  log_length + bulk_trophic_level, data = calcium_small)
mod6b <- lm(log_concentration ~  log_length + feeding_mode, data = calcium_small)
mod7b <- lm(log_concentration ~ bulk_trophic_level + feeding_mode, data = calcium_small)
mod8b <- lm(log_concentration ~  log_length + DemersPelag, data = calcium_small)
mod9b <- lm(log_concentration ~  feeding_mode + DemersPelag, data = calcium_small)
mod10b <- lm(log_concentration ~  bulk_trophic_level + DemersPelag, data = calcium_small)
mod11b <- lm(log_concentration ~  log_length + EnvTemp, data = calcium_small)
mod12b <- lm(log_concentration ~  feeding_mode + EnvTemp, data = calcium_small)
mod13b <- lm(log_concentration ~  bulk_trophic_level + EnvTemp, data = calcium_small)
mod14 <- lm(log_concentration ~  1, data = calcium_small)

summary(mod1)


### model selection
msel_calcium_small <- model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, mod13, mod2b, mod14,
                             mod1b, mod3b, mod4b, mod5b, mod6b, mod7b, mod8b, mod9b, mod10b, mod11b, mod12b, mod13b, mod2bb, rank = AICc) 

msel_calcium_small2 <- model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, mod13, mod2b, mod14,
                              mod1b, mod3b, mod4b, mod5b, mod6b, mod7b, mod8b, mod9b, mod10b, mod11b, mod12b, mod13b, mod2bb, rank = AICc, extra = "R2") %>% 
  mutate(model_num = rownames(.)) %>% 
  mutate(cum_weight = cumsum(weight)) %>%
  filter(cum_weight <= .95)

model_table_calcium_small <- msel_calcium_small2 %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(term = rownames(.)) %>% 
  dplyr::select(term, everything())

str(model_table_calcium_small)


confints_calcium_small <- data.frame(confint(model.avg(get.models(msel_calcium_small, subset = cumsum(weight) <= .95))),
                                  estimate = coef(model.avg(get.models(msel_calcium_small, subset = cumsum(weight) <= .95)))) %>% 
  mutate(term = rownames(.)) %>% 
  rename(lower = X2.5..) %>% 
  rename(upper = X97.5..) %>% 
  mutate(nutrient = "calcium") %>% 
  mutate(term2 = case_when(grepl("body_part", term) ~ "body_part",
                           grepl("feeding_mode", term) ~ "feeding_mode",
                           grepl("EnvTemp", term) ~ "EnvTemp",
                           grepl("DemersPelag", term) ~ "DemersPelag",
                           grepl("realm", term) ~ "realm",
                           TRUE ~ term))

# View(confints_calcium_small)

calcium_small_weights <- data.frame(wip = sw(get.models(msel_calcium_small, subset = cumsum(weight) <= .95))) %>% 
  mutate(term2 = rownames(.))

calcium_small_outputs <- full_join(confints_calcium_small, model_table_calcium_small, by = c("term2" = "term")) %>% 
  left_join(., calcium_small_weights) %>% 
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


write_csv(calcium_small_outputs, "tables/calcium-pgls-small-parts.csv")



calcium_plot_small <- confints_calcium_small %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
  coord_flip() +
  geom_hline(yintercept = 0) + ggtitle("calcium")
calcium_plot_small



#################### END OF ANALYSIS SCRIPT ##############

