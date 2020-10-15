
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
library(phylolm)
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





zinc <-  s2 %>% 
  filter(nutrient == "zn_mg") %>% 
  filter(!is.na(concentration)) %>% 
  filter(body_part %in% c("muscle", "muscle_skinless")) %>% 
  group_by(Species, feeding_mode, DemersPelag, realm, EnvTemp) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level) %>% 
  ungroup() %>% 
  filter(complete.cases(.)) %>% 
  # rename(part = part_edited) %>% 
  rename(species1 = Species)


# PGLS calcium muscle only ------------------------------------------------------------


zinc$species1 <- str_to_lower(zinc$species1)
# unique(zinc$part)
length(unique(zinc$species1))

zinc_taxa <- tnrs_match_names(unique(zinc$species1), context="Animals", names = unique(zinc$species1), do_approximate_matching = TRUE) 

tr_zinc <- tol_induced_subtree(ott_ids = ott_id(zinc_taxa), label_format="name") 
tr_bl_zinc <- compute.brlen(tr_zinc)
phylo <- tr_bl_zinc
zinc2 <- zinc %>% 
  left_join(., zinc_taxa, by = c("species1" = "search_string")) %>% 
  mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl_zinc$tip.label)) %>% 
  ungroup() %>% 
	mutate(DemersPelag = ifelse(DemersPelag == "pelagic", "pelagic-oceanic", DemersPelag)) %>% 
	mutate(DemersPelag = ifelse(DemersPelag == "bathypelagic", "pelagic-oceanic", DemersPelag)) %>% 
	mutate(EnvTemp = ifelse(EnvTemp == "deep-water", "polar", EnvTemp)) %>% 
	mutate(feeding_mode = ifelse(feeding_mode == "filtering plankton", "selective plankton feeding", feeding_mode)) %>% 
	mutate(feeding_mode = ifelse(feeding_mode == "grazing on aquatic plants", "variable", feeding_mode)) %>% 
	mutate(DemersPelag = ifelse(DemersPelag == "reef-associated", "pelagic-neritic", DemersPelag)) %>%
  # mutate(feeding_level = as.factor(feeding_level)) %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>%
  # mutate(BodyShapeI = as.factor(BodyShapeI)) %>%
  mutate(EnvTemp = as.factor(EnvTemp)) %>% 
  mutate(realm = as.factor(realm))
zinc2$log_length <- scale(zinc2$log_length)
zinc2$bulk_trophic_level <- scale(zinc2$bulk_trophic_level)
# zinc2$DepthRangeDeep <- szince(zinc2$DepthRangeDeep)
# zinc2$AgeMatMin <- szince(zinc2$AgeMatMin)

table(zinc2$EnvTemp)

data <- zinc2
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
zinc_tree <- pruned.tree
# plot(tree)
# is.ultrametric(tree) # has to be TRUE
# is.rooted(tree) # TRUE

any(duplicated(zinc_tree$node.label)) # FALSE

zinc_tree$node.label<-NULL

#prune data to match treetips
Phylodata <- data[(data$Phylospecies %in% zinc_tree$tip.label),]

Phylodata1 <- Phylodata %>% 
  # mutate(part = as.factor(part)) %>% 
  rename(species = Phylospecies) 

zincg <- Phylodata1 %>% 
  group_by(species, DemersPelag, 
           # feeding_level,
           feeding_mode, realm, EnvTemp) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level) %>% 
  ungroup() %>%
  distinct(species, .keep_all = TRUE) 




zincg2 <- zincg 
zincg2 <- zincg2[match(zinc_tree$tip.label, zincg2$species),]
row.names(zincg2) <- zincg2$species
table(zincg2$EnvTemp)


# model selection zinc ----------------------------------------------------

mod1 <- gls(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + EnvTemp, correlation = corPagel(value = 0, phy = zinc_tree, fixed = TRUE), data = zincg2, method = "ML")
mod2 <- gls(log_concentration ~  feeding_mode + log_length + EnvTemp + bulk_trophic_level + realm, correlation = corPagel(value = 0, phy = zinc_tree, fixed = TRUE), data = zincg2, method = "ML")
mod2b <- gls(log_concentration ~  feeding_mode + log_length + EnvTemp + bulk_trophic_level, correlation = corPagel(value = 0, phy = zinc_tree, fixed = TRUE), data = zincg2, method = "ML")
mod3 <- gls(log_concentration ~  feeding_mode + DemersPelag + bulk_trophic_level, correlation = corPagel(value = 0, phy = zinc_tree, fixed = TRUE), data = zincg2, method = "ML")
mod4 <- gls(log_concentration ~  log_length + DemersPelag + bulk_trophic_level, correlation = corPagel(value = 0, phy = zinc_tree, fixed = TRUE), data = zincg2, method = "ML")
mod5 <- gls(log_concentration ~  log_length + bulk_trophic_level, correlation = corPagel(value = 0, phy = zinc_tree, fixed = TRUE), data = zincg2, method = "ML")
mod6 <- gls(log_concentration ~  log_length + feeding_mode, correlation = corPagel(value = 0, phy = zinc_tree, fixed = TRUE), data = zincg2, method = "ML")
mod7 <- gls(log_concentration ~  bulk_trophic_level + feeding_mode, correlation = corPagel(value = 0, phy = zinc_tree, fixed = TRUE), data = zincg2, method = "ML")
mod8 <- gls(log_concentration ~  log_length + DemersPelag, correlation = corPagel(value = 0, phy = zinc_tree, fixed = TRUE), data = zincg2, method = "ML")
mod9 <- gls(log_concentration ~  feeding_mode + DemersPelag, correlation = corPagel(value = 0, phy = zinc_tree, fixed = TRUE), data = zincg2, method = "ML")
mod10 <- gls(log_concentration ~  bulk_trophic_level + DemersPelag, corPagel(value = 0, phy = zinc_tree, fixed = TRUE), data = zincg2, method = "ML")
mod11 <- gls(log_concentration ~  log_length + EnvTemp, correlation = corPagel(value = 0, phy = zinc_tree, fixed = TRUE), data = zincg2, method = "ML")
mod12 <- gls(log_concentration ~  feeding_mode + EnvTemp, correlation = corPagel(value = 0, phy = zinc_tree, fixed = TRUE), data = zincg2, method = "ML")
mod13 <- gls(log_concentration ~  bulk_trophic_level + EnvTemp, correlation = corPagel(value = 0, phy = zinc_tree, fixed = TRUE), data = zincg2, method = "ML")
mod14 <- gls(log_concentration ~  1, corPagel(value = 0, phy = zinc_tree, fixed = TRUE), data = zincg2, method = "ML")




R2(mod11, phy = zinc_tree)

### model selection
msel_zinc <- model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, mod13, mod2b, mod14, rank = AICc) %>% 
	mutate(model_num = rownames(.)) %>% 
	mutate(cum_weight = cumsum(weight))



confints_zinc <- data.frame(confint(model.avg(get.models(msel_zinc, subset = cumsum(weight) <= .95))),
							   estimate = coef(model.avg(get.models(msel_zinc, subset = cumsum(weight) <= .95)))) %>% 
	mutate(term = rownames(.)) %>% 
	rename(lower = X2.5..) %>% 
	rename(upper = X97.5..) %>% 
	mutate(nutrient = "zinc")
write_csv(confints_zinc, "data-processed/zinc-traits-confints.csv")

zinc_plot <- confints_zinc %>% 
	filter(term != "(Intercept)") %>% 
	ggplot(aes(x = term, y = estimate)) + 
	geom_pointrange(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
	coord_flip() +
	geom_hline(yintercept = 0) + ggtitle("zinc")
zinc_plot




# zinc all parts -------------------------------------------------------



zinc_all <-  s2 %>% 
  filter(nutrient == "zn_mg") %>% 
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

table(zinc_all$body_part)
table(zinc_all$EnvTemp)
table(zinc_all$DemersPelag)



# zinc all parts pgls -----------------------------------------------------

zinc_all$species1 <- str_to_lower(zinc_all$species1)
# unique(zinc_all$part)
length(unique(zinc_all$species1))

zinc_all_taxa <- tnrs_match_names(unique(zinc_all$species1), context="Animals", names = unique(zinc_all$species1), do_approximate_matching = TRUE) 

tr_zinc_all <- tol_induced_subtree(ott_ids = ott_id(zinc_all_taxa), label_format="name") 
tr_bl_zinc_all <- compute.brlen(tr_zinc_all)
phylo <- tr_bl_zinc_all
zinc_all2 <- zinc_all %>% 
  left_join(., zinc_all_taxa, by = c("species1" = "search_string")) %>% 
  mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl_zinc_all$tip.label)) %>% 
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
zinc_all2$log_length <- scale(zinc_all2$log_length)
zinc_all2$bulk_trophic_level <- scale(zinc_all2$bulk_trophic_level)
# zinc_all2$DepthRangeDeep <- szinc_alle(zinc_all2$DepthRangeDeep)
# zinc_all2$AgeMatMin <- szinc_alle(zinc_all2$AgeMatMin)

table(zinc_all2$DemersPelag)

data <- zinc_all2
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
zinc_all_tree <- pruned.tree
# plot(tree)
# is.ultrametric(tree) # has to be TRUE
# is.rooted(tree) # TRUE

any(duplicated(zinc_all_tree$node.label)) # FALSE

zinc_all_tree$node.label<-NULL

#prune data to match treetips
Phylodata <- data[(data$Phylospecies %in% zinc_all_tree$tip.label),]

Phylodata1 <- Phylodata %>% 
  # mutate(part = as.factor(part)) %>% 
  rename(species = Phylospecies) 

zinc_allg <- Phylodata1 %>% 
  group_by(species, DemersPelag, 
           # feeding_level,
           feeding_mode, realm, EnvTemp, body_part) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level) %>% 
  ungroup() %>%
  distinct(species, .keep_all = TRUE) 




zinc_allg2 <- zinc_allg 
zinc_allg2 <- zinc_allg2[match(zinc_all_tree$tip.label, zinc_allg2$species),]
row.names(zinc_allg2) <- zinc_allg2$species
table(zinc_allg2$DemersPelag)
length(unique(zinc_allg2$species))

# model selection zinc ----------------------------------------------------
library(phylolm)
library(rr2)
mod1p <- phylolm(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + EnvTemp + body_part, phy = zinc_all_tree, data = zinc_allg2, model = "lambda")
summary(mod1p)
R2(mod1p, phy = zinc_all_tree)
mod1p <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + realm + EnvTemp + body_part, 
             correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")



# mod1p <- phylolm(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + EnvTemp + body_part, data = zinc_all)
summary(mod1p)
R2(mod1p)

plot1 <- visreg(mod1p, "DemersPelag", gg = TRUE, size = 6) +
  ylab("log(zinc) mg/100g") + xlab("Habitat association") +
  theme(axis.text.x = element_text(angle = 90))
plot2 <- visreg(mod1p, "log_length", gg = TRUE, size = 6) +
  ylab("log(zinc) mg/100g") + xlab("Log length (cm)")  +
  theme(axis.text.x = element_text(angle = 90))
plot3 <- visreg(mod1p, "realm", gg = TRUE, size = 6) +
  ylab("log(zinc) mg/100g") + xlab("Realm") +
  theme(axis.text.x = element_text(angle = 90))
plot4 <- visreg(mod1p, "bulk_trophic_level", gg = TRUE, size = 6) +
  ylab("log(zinc) mg/100g") + xlab("Trophic position") +
  theme(axis.text.x = element_text(angle = 90))
plot5 <- visreg(mod1p, "EnvTemp", gg = TRUE, size = 6) +
  ylab("log(zinc) mg/100g") + xlab("Thermal regime") +
  theme(axis.text.x = element_text(angle = 90))
plot6 <- visreg(mod1p, "feeding_mode", gg = TRUE, size = 6) +
  ylab("log(zinc) mg/100g") + xlab("Feeding mode") +
  theme(axis.text.x = element_text(angle = 90))
plot7 <- visreg(mod1p, "body_part", gg = TRUE, size = 6) +
  ylab("log(zinc) mg/100g") + xlab("Body part") +
  theme(axis.text.x = element_text(angle = 90))

plot_all <- plot1 + plot2 + plot3 + plot4 + plot5 + plot6 + plot7+
  plot_annotation(tag_levels = 'A') + plot_layout(ncol = 3)
ggsave("figures/all-parts-pgls-zinc-partial-regressions-oct.pdf", plot = plot_all, width = 8, height = 12)


# model summary table -----------------------------------------------------

lambda <- round(mod1p$modelStruct[[1]][[1]], digits = 2)

# visreg(mod1a)
rsq_mod1p <- round(rsquared(mod1p)['R.squared'][[1]], digits = 2)

stargazer(mod1p, title = "", type = "html", out="tables/zinc-pgls-all-parts-oct.htm", 
          add.lines = list(c("R2", rsq_mod1p), c("Lamba", lambda)), ci=TRUE, ci.level=0.95, digits = 2, single.row = TRUE)





#################### END OF ANALYSIS SCRIPT ##############



# model averaging with dredge for zinc ------------------------------------------------


mod1_zinc <- gls(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + EnvTemp, correlation = corBrownian(phy = zinc_tree, value = 1), data = zincg2, method = "ML")
summary(mod1_zinc)
dd_zinc <- dredge(mod1_zinc, m.lim = c(1, 5), extra = "R2") %>% 
	mutate(mod_number = rownames(.)) %>% 
	mutate(cum_weight = cumsum(weight))
View(dd_zinc)
mods_zinc <- get.models(dd_zinc, subset= cumsum(weight) <= .95)
sw(mods_zinc)
ci_avg_zinc <- rownames_to_column(as.data.frame(confint(model.avg(dd_zinc, subset = cumsum(weight) <= .95))), var = "term")
slopes_avg_zinc <- enframe(coef(model.avg(dd_zinc, subset = cumsum(weight) <= .95)), name = "term", value = "slope")

zinc_mod_out <- left_join(ci_avg, slopes_avg) %>% 
	rename(lower = `2.5 %`,
		   upper = `97.5 %`) %>% 
	filter(term != "(Intercept)") %>% 
	mutate(nutrient = "zinc") %>% 
	mutate(conf_int_overlap_0 = ifelse(upper < 0 & slope < 0 | lower > 0 & slope > 0, "yes", "no")) 

zinc_mod_out %>% 
	ggplot(aes(x = term, y = slope)) + 
	geom_pointrange(aes(x = term, y = slope, ymin = lower, ymax = upper), fill = "transparent") +
	geom_point(aes(x = term, y = slope, shape = conf_int_overlap_0, color = conf_int_overlap_0)) +
	scale_color_manual(values = c("black", "white")) +
	scale_shape_manual(values = c(1, 19)) +
	geom_hline(yintercept = 0) + coord_flip() +
	theme(legend.position = "none")


# ok this is just extra from here on out ----------------------------------



summary(mod1)
mod2 <- gls(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm, correlation = corBrownian(phy = zinc_tree), data = zincg2, method = "ML")


dd <- dredge(mod1)


# mod2 <- phylolm(log_concentration ~  feeding_mode + log_length + EnvTemp + bulk_trophic_level + realm, phy = zinc_tree, data = zincg2, model = "lambda")

# mod2b <- phylolm(log_concentration ~  feeding_mode + log_length + EnvTemp + bulk_trophic_level, phy = zinc_tree, data = zincg2, model = "lambda")

mod3 <- phylolm(log_concentration ~  feeding_mode + DemersPelag + bulk_trophic_level, phy = zinc_tree, data = zincg2, model = "lambda")
mod4 <- phylolm(log_concentration ~  log_length + DemersPelag + bulk_trophic_level, phy = zinc_tree, data = zincg2, model = "lambda")

mod5 <- phylolm(log_concentration ~  log_length + bulk_trophic_level, phy = zinc_tree, data = zincg2, model = "lambda")
mod6 <- phylolm(log_concentration ~  log_length + feeding_mode, phy = zinc_tree, data = zincg2, model = "lambda")
mod7 <- phylolm(log_concentration ~  bulk_trophic_level + feeding_mode, phy = zinc_tree, data = zincg2, model = "lambda")
mod8 <- phylolm(log_concentration ~  log_length + DemersPelag, phy = zinc_tree, data = zincg2, model = "lambda")
mod9 <- phylolm(log_concentration ~  feeding_mode + DemersPelag, phy = zinc_tree, data = zincg2, model = "lambda")
mod10 <- phylolm(log_concentration ~  bulk_trophic_level + DemersPelag, phy = zinc_tree, data = zincg2, model = "lambda")
mod11 <- phylolm(log_concentration ~  log_length + EnvTemp, phy = zinc_tree, data = zincg2, model = "lambda")
mod12 <- phylolm(log_concentration ~  feeding_mode + EnvTemp, phy = zinc_tree, data = zincg2, model = "lambda")
mod13 <- phylolm(log_concentration ~  bulk_trophic_level + EnvTemp, phy = zinc_tree, data = zincg2, model = "lambda")
mod14 <- phylolm(log_concentration ~  1, phy = zinc_tree, data = zincg2, model = "lambda")

model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, mod13, mod14,mod2b, rank = AICc) %>%  
  mutate(model_number = rownames(.)) %>% 
  mutate(cumsum = cumsum(weight)) %>% View
# summary(model.avg(mod9, mod6))
# confint(model.avg(mod9, mod6))
# coef(model.avg(mod9, mod6))
# 
# R2(mod6, phy = zinc_tree)

summary(mod1)
confints_zinc <- data.frame(confint(model.avg(mod11, mod12, mod13)), estimate = coef(model.avg(mod11, mod12, mod13))) %>% 
  mutate(term = rownames(.)) %>% 
  rename(lower = X2.5..) %>% 
  rename(upper = X97.5..)

zinc_plot <- confints_zinc %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
  coord_flip() +
  geom_hline(yintercept = 0) + ggtitle("zinc")
zinc_plot



# zinc all parts ----------------------------------------------------------

##### zinc all parrts model sel

mod1 <- gls(log_concentration ~  body_part + feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + EnvTemp, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod2 <- gls(log_concentration ~  body_part + feeding_mode + log_length + EnvTemp + bulk_trophic_level + realm, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod2b <- gls(log_concentration ~  body_part + feeding_mode + log_length + EnvTemp + bulk_trophic_level, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod3 <- gls(log_concentration ~  body_part + feeding_mode + DemersPelag + bulk_trophic_level, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod4 <- gls(log_concentration ~  body_part + log_length + DemersPelag + bulk_trophic_level, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod5 <- gls(log_concentration ~  body_part + log_length + bulk_trophic_level, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod5b <- gls(log_concentration ~   log_length + bulk_trophic_level, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")

mod6 <- gls(log_concentration ~  body_part + log_length + feeding_mode, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod7 <- gls(log_concentration ~  body_part + bulk_trophic_level + feeding_mode, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod8 <- gls(log_concentration ~  body_part + log_length + DemersPelag, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod9 <- gls(log_concentration ~  body_part + feeding_mode + DemersPelag, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod10 <- gls(log_concentration ~  body_part + bulk_trophic_level + DemersPelag, corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod11 <- gls(log_concentration ~  body_part + log_length + EnvTemp, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod12 <- gls(log_concentration ~  body_part + feeding_mode + EnvTemp, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod13 <- gls(log_concentration ~  body_part + bulk_trophic_level + EnvTemp, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")


mod1b <- gls(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm + EnvTemp, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod2b <- gls(log_concentration ~  feeding_mode + log_length + EnvTemp + bulk_trophic_level + realm, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod2bb <- gls(log_concentration ~  feeding_mode + log_length + EnvTemp + bulk_trophic_level, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod3b <- gls(log_concentration ~  feeding_mode + DemersPelag + bulk_trophic_level, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod4b <- gls(log_concentration ~  log_length + DemersPelag + bulk_trophic_level, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod5b <- gls(log_concentration ~  log_length + bulk_trophic_level, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod6b <- gls(log_concentration ~  log_length + feeding_mode, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod7b <- gls(log_concentration ~ bulk_trophic_level + feeding_mode, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod8b <- gls(log_concentration ~  log_length + DemersPelag, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod9b <- gls(log_concentration ~  feeding_mode + DemersPelag, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod10b <- gls(log_concentration ~  bulk_trophic_level + DemersPelag, corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod11b <- gls(log_concentration ~  log_length + EnvTemp, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod12b <- gls(log_concentration ~  feeding_mode + EnvTemp, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")
mod13b <- gls(log_concentration ~  bulk_trophic_level + EnvTemp, correlation = corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")



mod14 <- gls(log_concentration ~  1, corPagel(value = 0, phy = zinc_all_tree, fixed = TRUE), data = zinc_allg2, method = "ML")




R2(mod11, phy = zinc_all_tree)

### model selection
msel_zinc_all <- model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, mod13, mod2b, mod14,
                              mod1b, mod3b, mod4b, mod5b, mod6b, mod7b, mod8b, mod9b, mod10b, mod11b, mod12b, mod13b, mod2bb, rank = AICc) 

msel_zinc_all2 <- model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, mod13, mod2b, mod14,
                               mod1b, mod3b, mod4b, mod5b, mod6b, mod7b, mod8b, mod9b, mod10b, mod11b, mod12b, mod13b, mod2bb, rank = AICc, extra = "R2") %>% 
  mutate(model_num = rownames(.)) %>% 
  mutate(cum_weight = cumsum(weight)) %>%
  filter(cum_weight <= .95)

model_table_zinc_all <- msel_zinc_all2 %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(term = rownames(.)) %>% 
  dplyr::select(term, everything())

str(model_table_zinc_all)


confints_zinc_all <- data.frame(confint(model.avg(get.models(msel_zinc_all, subset = cumsum(weight) <= .95))),
                                   estimate = coef(model.avg(get.models(msel_zinc_all, subset = cumsum(weight) <= .95)))) %>% 
  mutate(term = rownames(.)) %>% 
  rename(lower = X2.5..) %>% 
  rename(upper = X97.5..) %>% 
  mutate(nutrient = "zinc") %>% 
  mutate(term2 = case_when(grepl("body_part", term) ~ "body_part",
                           grepl("feeding_mode", term) ~ "feeding_mode",
                           grepl("EnvTemp", term) ~ "EnvTemp",
                           grepl("DemersPelag", term) ~ "DemersPelag",
                           grepl("realm", term) ~ "realm",
                           TRUE ~ term))

View(confints_zinc_all)

zinc_all_weights <- data.frame(wip = sw(get.models(msel_zinc_all, subset = cumsum(weight) <= .95))) %>% 
  mutate(term2 = rownames(.))

zinc_all_outputs <- full_join(confints_zinc_all, model_table_zinc_all, by = c("term2" = "term")) %>% 
  left_join(., zinc_all_weights) %>% 
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

write_csv(zinc_all_outputs, "tables/zinc-pgls-all-parts.csv")



zinc_plot_all <- confints_zinc_all %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
  coord_flip() +
  geom_hline(yintercept = 0) + ggtitle("zinc")
zinc_plot_all
