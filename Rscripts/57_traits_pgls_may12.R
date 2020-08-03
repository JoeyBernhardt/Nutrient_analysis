
library(rotl)
library(stargazer)
traits <- read_csv("data-processed/trait-nutrient-data-analysis.csv")

traits2 <- traits %>% 
  mutate(part = ordered(part, levels = c("muscle", "muscle + skin", "muscle + small bones", "muscle, bone + inside","whole",
                                         "head, eyes, cheeks + soft bones", "tongues + cheeks", "skin", "liver", "offal", "eggs", "oil", NA)))

# Calcium muscle only -----------------------------------------------------

View(traits2)
traits_one_part <- traits2 %>% 
  group_by(species1, nutrient) %>% 
  top_n(n = 1, wt = part)

calcium <- traits2 %>% 
  dplyr::select(-seanuts_id2) %>% 
  filter(nutrient == "ca_mg") %>% 
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


mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag +
               DepthRangeDeep + AgeMatMin + BodyShapeI + realm, correlation = corPagel(value = 0, phy = tree, fixed = FALSE), data = calg2, method = "ML")


summary(mod1a)
lambda <- round(mod1a$modelStruct[[1]][[1]], digits = 2)

visreg(mod1a)
rsq_mod1a <- round(rsquared(mod1a)['R.squared'][[1]], digits = 2)

stargazer(mod1a, title = "", type = "html", out="tables/calcium-models-expanded-muscle-only-pgls-aug2020.htm", 
          add.lines = list(c("R2", rsq_mod1a), c("Lamba", lambda)), ci=TRUE, ci.level=0.95, digits = 2)


anova(mod1a)
# Calcium all parts -------------------------------------------------------

traits_one_part <- traits2 %>% 
  group_by(species1, nutrient) %>% 
  top_n(n = 1, wt = part)

calcium <- traits_one_part %>% 
  dplyr::select(-seanuts_id2) %>% 
  filter(nutrient == "ca_mg") %>% 
  filter(!grepl("spp", species1)) %>% 
  filter(!species1 %in% c("Pleuronectinae", "Petromyzontinae", "Ensis directus", "Osmerus mordax")) %>% 
  filter(part != "unknown") %>%
  filter(part != "unspecified") %>% 
  # filter(part == "muscle") %>% 
  group_by(species1, feeding_mode, feeding_level, EnvTemp, DemersPelag, BodyShapeI, part, realm) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) %>% 
  ungroup() %>%
  filter(complete.cases(.))


mod_cal <- lm(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm + part, data = calcium)
mod_part <- lm(log_concentration ~ part, data = calcium)


stargazer(mod_cal, title = "", type = "html", out="tables/calcium-models-expanded-all-parts-ols.htm")

model.sel(mod_cal, mod_part)
summary(mod_cal)
summary(mod_part)
AIC(mod_cal, mod_part)
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


# mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm + part,  correlation = corPagel(value = 0, phy = tree, fixed = FALSE), data = calg2, method = "ML")
mod1a <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + DepthRangeDeep + BodyShapeI + part,  correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")
mod1b <- gls(log_concentration ~  1, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")
mod1c <- gls(log_concentration ~  part, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")

model.sel(mod1a, mod1b, extra = "rsquared") %>% View
summary(mod1b)
AICc(mod1a, mod1b, mod1c)


mod1a <- lm(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + realm + part, data = calg2)
mod1b <- lm(log_concentration ~  part, data = calg2)

AIC(mod1a, mod1b)

summary(mod1a)

visreg(mod1a)
rsquared(mod1a)
stargazer(mod1a, title = "", type = "html", out="tables/calcium-models-expanded-muscle-only-pgls.htm")






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
