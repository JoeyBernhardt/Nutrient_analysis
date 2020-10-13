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
msel_calcium <- model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, mod13, mod2b, mod14, rank = AICc) %>% 
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
