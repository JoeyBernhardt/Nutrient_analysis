

require(phytools)
require(MCMCglmm)
library(tidyverse)
library(rotl)
library(nlme)
library(visreg)

library(Rphylopars)
## find out which ones have multuple temps

cine_traits_new <- read_csv("data-processed/cine-traits-new-species2.csv") %>% 
  filter(part != "not specified") %>% 
  # read_csv("data-processed/all-traits-nuts.csv") %>% 
  # rename(species1 = Species) %>% 
  # rename(bulk_trophic_level = FoodTroph) %>% 
  # rename(feeding_level = Herbivory2) %>% 
  # rename(feeding_mode = FeedingType) %>% 
  mutate(log_concentration1 = log(concentration)) %>% 
  mutate(log_length1 = log(Length)) %>% 
  mutate(log_concentration = ifelse(is.na(log_concentration), log_concentration1, log_concentration)) %>% 
  mutate(log_length = ifelse(is.na(log_length), log_length1, log_length)) %>% 
  mutate(reference = as.character(reference)) %>% 
  mutate(species1 = ifelse(is.na(species1), latin_name, species1))


seanuts_traits2 <- read_csv("data-processed/all-traits-nuts.csv") %>% 
  filter(!is.na(seanuts_id2)) %>% 
  select(-part)

parts <- read_csv("data-processed/seanuts_parts.csv")

seanuts_traits3 <- left_join(seanuts_traits2, parts) %>% 
  rename(species1 = Species) %>%
  rename(bulk_trophic_level = FoodTroph) %>%
  rename(feeding_level = Herbivory2) %>%
  rename(feeding_mode = FeedingType) %>% 
  mutate(log_length = log(Length)) %>% 
  mutate(log_concentration = log(concentration))

traits <- bind_rows(cine_traits_new, seanuts_traits3) %>%
  select(species1, nutrient, concentration,feeding_mode, feeding_level, EnvTemp, DemersPelag, BodyShapeI, part,
         log_concentration, log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) %>% 
  mutate(part = ifelse(part == "muscle-skinless", "muscle", part))

unique(traits$part)

traits <- cine_traits_new

unique(traits$nutrient)

calcium <- traits %>% 
  filter(nutrient == "ca_mg") %>% 
  # filter(!is.na(concentration)) %>% 
  filter(!grepl("spp", species1)) %>% 
  filter(!species1 %in% c("Pleuronectinae", "Petromyzontinae", "Ensis directus", "Osmerus mordax")) %>% 
  filter(part != "unknown") %>%
  filter(part != "unspecified") %>% 
  group_by(species1, feeding_mode, feeding_level, EnvTemp, DemersPelag, BodyShapeI, part) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) %>% 
  ungroup() %>% 
  # distinct(species1, .keep_all = TRUE) %>%
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
  mutate(EnvTemp = as.factor(EnvTemp))
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

# length(unique(sp[which(data$Phylospecies=="not in tree")])) # not in tree

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


length(unique(Phylodata$Phylospecies))

Phylodata2 <- Phylodata %>% 
  # filter(part != "muscle") %>% 
  distinct(Phylospecies, .keep_all = TRUE) %>% 
  # dplyr::select(Phylospecies, log_concentration, log_length, part) %>% 
  rename(species = Phylospecies) %>% 
  mutate(part = as.factor(part))



# cal_mod <- gls(log_concentration ~ log_length + BodyShapeI + bulk_trophic_level + part +
#                  feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin, correlation = corBrownian(phy = tree), data = Phylodata2, method = "ML")
# summary(cal_mod)
# anova(cal_mod)
# visreg(cal_mod)
# 
# vr <- visreg(cal_mod, plot = FALSE)




library(Rphylopars)

Phylodata1 <- Phylodata %>% 
  mutate(part = as.factor(part)) %>% 
	rename(species = Phylospecies) 

unique(Phylodata1$EnvTemp)

?phylopars.lm
unique(Phylodata1$part)

(thing1 <- phylopars.lm(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode +
                          DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + part, trait_data = Phylodata1, tree = tree, pheno_error = FALSE))
thing1
(thing1b <- phylopars.lm(log_concentration ~ part, trait_data = Phylodata1 , tree = tree, pheno_error = FALSE))
(thing1bl <- lm(log_concentration ~ part, data = Phylodata1 ))
(thing1c <- phylopars.lm(log_concentration ~ 1, trait_data = Phylodata1 , tree = tree, pheno_error = FALSE))

summary(thing1b)
summary(thing1bl)
anova(thing1b)
anova(thing1bl)
class(tree)
calcium_vect <- Phylodata1$log_concentration
names(calcium_vect) <- Phylodata1$species

calg <- Phylodata1 %>% 
  group_by(species, EnvTemp, DemersPelag, feeding_level, feeding_mode, BodyShapeI, part) %>% 
  summarise_each(funs(mean), DepthRangeDeep, log_concentration, log_length, AgeMatMin, bulk_trophic_level) %>% 
  ungroup() %>%
  mutate(EnvTemp = as.character(EnvTemp)) %>% 
  distinct(species, .keep_all = TRUE) %>% 
  mutate(part = as.character(part))

calg_part <- Phylodata1 %>% 
  group_by(species, EnvTemp, DemersPelag, feeding_level, feeding_mode, BodyShapeI, part) %>% 
  summarise_each(funs(mean), DepthRangeDeep, log_concentration, log_length, AgeMatMin, bulk_trophic_level) %>% 
  ungroup() %>%
  mutate(EnvTemp = as.character(EnvTemp)) %>% 
  mutate(part = as.character(part))


calg_part %>% 
  filter(species == "Oncorhynchus_tshawytscha") %>% 
  ggplot(aes(x = part, y = log_concentration))+ geom_point()


com <- comparative.data(tree, as.data.frame(calg), "species")
fitg <- fitContinuous(phy = tree, dat = calcium_vect, model = "lambda")
phylosig(tree = tree, x = calcium_vect)
fitg$opt
library(caper)

calg2 <- calg

calg2 <- calg2[match(tree$tip.label, calg2$species),]
row.names(calg2) <- calg2$species

calg3 <- calg2 %>% 
  mutate(part = as.factor(part))

### full model
mod1 <- lm(log_concentration ~ bulk_trophic_level +  log_length  + feeding_mode +
             DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + part + EnvTemp, data = Phylodata1, na.action=na.exclude)

mod2 <-pgls(log_concentration ~ bulk_trophic_level +  log_length  + feeding_mode +
              DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + part, data = com,
                  lambda="ML")
summary(mod2)

mod2c <- phylopars.lm(log_concentration ~ bulk_trophic_level +  log_length  + feeding_mode +
                        DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + part, 
             trait_data = calg_part , tree = tree, pheno_error = FALSE, REML = FALSE)
summary(mod2c)

mod2c2 <- phylopars.lm(log_concentration ~ part, 
                      trait_data = calg3 , tree = tree, pheno_error = TRUE, REML = FALSE)

mod2c3 <- phylopars.lm(log_concentration ~ 1, 
                       trait_data = calg3 , tree = tree, pheno_error = TRUE, REML = FALSE)
summary(mod2c2)
AIC(mod2c, mod2c2, mod2c3)
mod2c
?phylopars.lm

modlm <- lm(log_concentration ~ bulk_trophic_level +  log_length  + feeding_mode +
              DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + part, data=calg2)
summary(modlm)



mod2 <-pgls(log_concentration ~ log_length + BodyShapeI + bulk_trophic_level + AgeMatMin + DepthRangeDeep + feeding_mode, data=com,
            lambda="ML")
mod2b <-gls(log_concentration ~ log_length + BodyShapeI + bulk_trophic_level + AgeMatMin + DepthRangeDeep + feeding_mode, correlation = corPagel(phy = tree, value = 0, fixed = FALSE),
            data = calg)

summary(mod2)
summary(mod2b)


calg2 <- calg2[match(tree$tip.label, calg2$species),]

mod2$param["lambda"]
mod.l <- pgls.profile(mod2, 'lambda')
plot(mod.l)

mod2b <-lm(log_concentration ~ log_length + BodyShapeI + bulk_trophic_level + AgeMatMin + DepthRangeDeep + feeding_level, data=calg)
summary(mod2b)


mod2c <- gls(log_concentration ~ log_length + BodyShapeI + bulk_trophic_level +
               AgeMatMin + DepthRangeDeep + feeding_level, correlation = corPagel(phy = tree, value = 1, fixed = FALSE),
             data = calg, method = "ML")
mod2cb <- gls(log_concentration ~ log_length + BodyShapeI + bulk_trophic_level +
               AgeMatMin + DepthRangeDeep + feeding_level, correlation = corPagel(phy = tree, value = 0, fixed = TRUE),
             data = Phylodata2, method = "ML")

summary(mod2c)
summary(mod2cb)
# mod2c <- gls(log_concentration ~ log_length + BodyShapeI + bulk_trophic_level +
#                AgeMatMin + DepthRangeDeep + feeding_level, data = Phylodata2)

corp <- coef(corPagel(1,tree, fixed=FALSE))

?corPagel

mod2a <-pgls(log_concentration ~ log_length + bulk_trophic_level, data=com,
            lambda="ML")

summary(mod2a)

cor(as.numeric(as.factor(calg$DemersPelag)), as.numeric(as.factor(calg$EnvTemp)))

library(mctest)
calg2 <- calg %>% 
  dplyr::select(log_concentration, everything()) %>% 
  mutate(EnvTemp = as.factor(EnvTemp))

omcdiag(x = data.matrix(calg2[, 3:11]), y = calg2$log_concentration)
imcdiag(x = data.matrix(calg2[, 3:11]), y = calg2$log_concentration)

library(ppcor)
pcor(data.matrix(calg2[, 3:11]),  method = "pearson")
cor(calg$bulk_trophic_level, as.numeric(as.factor(calg$BodyShapeI)))
?pcor

mod2b <-lm(log_concentration ~ log_length + EnvTemp +  bulk_trophic_level + DepthRangeDeep + feeding_level +
              feeding_mode + BodyShapeI + feeding_mode, data = calg)

visreg(mod2b)
summary(mod2a)
summary(mod2b)
mod2 <-pgls(log_concentration ~ EnvTemp, data=com,
            lambda="ML")

mod2b <-lm(log_concentration ~ log_length + BodyShapeI + bulk_trophic_level +
              feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin + feeding_level, data=calg)

mod3 <-lm(mean_cal ~ mean_length, data=calg)
summary(mod2)
summary(mod2b)

anova.pgls(mod2)

class(shorebird.data)
class(calg)



tree$node.label<-NULL
length(tree$tip.label)
length(shorebird.tree$tip.label)
rownames(calg) <- calg$species
data(shorebird)
shorebird <- comparative.data(shorebird.tree, shorebird.data, 'Species')
mod1 <- pgls(log(Egg.Mass) ~ log(M.Mass) * log(F.Mass), shorebird, lambda='ML')
mod1b <- lm(log(Egg.Mass) ~ log(M.Mass) * log(F.Mass), data = shorebird.data)
mod1c <- gls(log(Egg.Mass) ~ log(M.Mass) * log(F.Mass), correlation = corPagel(0,shorebird.tree, fixed=FALSE), data = shorebird.data, method = "ML")

summary(mod1)
summary(mod1c)

?corBrownian


mod.l <- pgls.profile(mod1, 'lambda')
plot(mod.l)
?pgls

summary(mod1)
summary(mod1b)
summary(mod1c)

mod2 <- pgls(log(Egg.Mass) ~ log(M.Mass), data=shorebird, lambda='ML', delta='ML')
summary(mod2)

?fitContinuous
AIC(thing1, thing1b, thing1c)

(thing2 <- phylopars.lm(log_concentration ~ log_length + BodyShapeI + bulk_trophic_level + part +
                          feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin, 
                        trait_data = Phylodata1 , tree = tree, pheno_error = FALSE))

(thing2_lm <- lm(log_concentration ~ log_length + BodyShapeI + bulk_trophic_level + part +
                          feeding_mode + DemersPelag + DepthRangeDeep + AgeMatMin, 
                        data = Phylodata1))
mod1 <- lm(log_concentration ~ bulk_trophic_level +  log_length  + feeding_mode +
             DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + part + EnvTemp, data = Phylodata1, na.action=na.exclude)
mod2 <- lm(log_concentration ~ part, data = Phylodata1, na.action=na.exclude)
mod3 <- lm(log_concentration ~ 1, data = Phylodata1, na.action=na.exclude)

anova(mod1)
summary(mod1)
confint(mod1)
summary(thing2)
summary(thing2_lm)
summary(mod2)

fitContinuous()

AIC(mod1, mod2, mod3)

anova(thing2)

(thing2 <- phylopars.lm(log_concentration ~ bulk_trophic_level + log_length +
                          feeding_mode + DemersPelag, 
                        trait_data = Phylodata1 , tree = tree, pheno_error = FALSE))

anova(thing2)

anova(thing2)
coef(thing1)
confint(thing1)

(thing1 <- phylopars.lm(log_concentration ~ EnvTemp + bulk_trophic_level + log_length + feeding_mode +
                          BodyShapeI, trait_data = Phylodata1 , tree = tree, pheno_error = FALSE))


sum <- summary(thing1)


anov <- anova(thing1)
library(stargazer)

stargazer(anov, dep.var.labels.include = TRUE, summary = FALSE, title = "", type = "html", out="tables/pgls-cine-cal-models-expanded-all-tissues.htm")


(thing1 <- phylopars.lm(V1 ~ V2 + V3, trait_data = thing4 , tree = tree, pheno_error = FALSE))
class(thing1)
str(thing1)
visreg::visreg(thing1)
plot(thing1)





confint(thing1)
class(thing1)
anova(thing1)

thing1$model


library(visreg)
visreg(thing1)

tree$edge.length
phylopars.lm

thing4 <- sim_data$trait_data
thing4$V1 <- Phylodata1$log_concentration
thing4$V2 <- Phylodata1$log_length
thing4$V3 <- Phylodata1$part
thing4$species <- Phylodata1$species
thing4$part <- Phylodata1$part

thing4 <- data.frame(species = thing4$species, V1 = thing4$V1, V2 = thing4$V2, V3 = thing4$V3)

all.equal(tree$tip.label, Phylodata1$species)
intersect(tree$tip.label, Phylodata1$species)

class(Phylodata1$species)
# tree$tip.label
trees <- sim_data2$tree
trees$edge <- tree$edge
trees$edge.length <- tree$edge.length
trees$Nnode <- tree$Nnode
trees$tip.label <- tree$tip.label


rownames(thing4) <- Phylodata1$species

anova(thing1)
tree$node.label <- NULL

tree2 <- tree[1,4,3,2]

summary.phylopars.lm(thing1)

### example


data(bird.families) 
class(bird.families)

phylo.effect<-rbv(bird.families, 1, nodes="TIPS") 
phenotype<-phylo.effect+rnorm(dim(phylo.effect)[1], 0, 1)  

# simulate phylogenetic and residual effects with unit variance

test.data<-data.frame(phenotype=phenotype, taxon=row.names(phenotype))

Ainv<-inverseA(bird.families)$Ainv
dim(Ainv1)
# inverse matrix of shared phyloegnetic history

prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))

model2<-MCMCglmm(phenotype~1, random=~taxon, ginverse=list(taxon=Ainv),
                 data=test.data, prior=prior, verbose=FALSE, nitt=1300, burnin=300, thin=1)

Ainv1<-inverseA(tree)$Ainv


str(tree)
str(bird.families)

Phylodata1 <- Phylodata %>% 
  distinct(species1, .keep_all = TRUE) %>% 
  dplyr::select(Phylospecies, log_concentration, log_length) %>% 
  rename(animal = Phylospecies) %>% 
  mutate(animal = as.character(animal))

rownames(Phylodata1) <- Phylodata1$animal

model2<-MCMCglmm(log_concentration~1, random=~animal, pedigree = tree,
                 data=Phylodata1, prior=prior, verbose=FALSE, nitt=1300, burnin=300, thin=1)

str(ginverse)
ginverse$animal@Dimnames


function (formula, trait_data, tree, model = "BM", pheno_error, 
		  phylo_correlated = TRUE, pheno_correlated = TRUE, REML = TRUE, 
		  full_alpha = TRUE, phylocov_start, phenocov_start, model_par_start, 
		  phylocov_fixed, phenocov_fixed, model_par_fixed, skip_optim = FALSE, 
		  skip_EM = FALSE, EM_Fels_limit = 1000, repeat_optim_limit = 1, 
		  EM_missing_limit = 50, repeat_optim_tol = 0.01, model_par_evals = 10, 
		  max_delta = 10000, EM_verbose = FALSE, optim_verbose = FALSE, 
		  npd = FALSE, nested_optim = FALSE, usezscores = TRUE, phenocov_list = list(), 
		  ret_args = FALSE, ret_level = 1) 
{
	args <- as.list(match.call())
	args <- args[3:length(args)]
	trait_data <- trait_data[, c(which(colnames(trait_data) == 
									   	"species"), which(colnames(trait_data) != "species"))]
	original_data <- trait_data
	original_option <- getOption("na.action")
	options(na.action = "na.pass")
	mod.mat <- model.matrix(object = formula, data = trait_data)
	intercept <- attr(terms(formula, data = trait_data), "intercept") == 
		1
	if (!intercept) 
		stop("Intercept-free PGLS not currently supported.")
	y_var <- model.frame(formula, data = trait_data)
	var_name <- colnames(y_var)[1]
	y_var <- y_var[, 1, drop = FALSE]
	mod.mat <- cbind(mod.mat, y_var)
	colnames(mod.mat)[ncol(mod.mat)] <- var_name
	if (intercept) {
		trait_data <- data.frame(species = trait_data$species, 
								 mod.mat[, 2:ncol(mod.mat)])
		colnames(trait_data) <- c("species", colnames(mod.mat)[2:ncol(mod.mat)])
	}
	else {
		trait_data <- data.frame(species = trait_data$species, 
								 mod.mat[, 1:ncol(mod.mat)])
		colnames(trait_data) <- c("species", colnames(mod.mat)[1:ncol(mod.mat)])
	}
	options(na.action = original_option)
	args$trait_data <- trait_data
	PPE <- do.call(phylopars, args)
	n <- nspecies <- length(PPE$tree$tip.label)
	means <- PPE$anc_recon[nspecies + 1, ]
	trait_data <- PPE$trait_data
	df.int <- as.integer(intercept)
	k <- ncol(PPE$pars[[1]])
	rdf <- n - k
	if (PPE$model[[1]] != "mvOU" & PPE$model[[1]] != "OU") 
		covX <- PPE$pars[[1]]
	else covX <- PPE$model[[3]]
	npred <- ncol(covX) - 1
	y_pos <- ncol(covX)
	if (ncol(covX) == 1 & intercept) {
		R2 <- 0
		ts <- ps <- SEs <- NA
	}
	else {
		coefs <- solve(covX[1:npred, 1:npred, drop = FALSE]) %*% 
			covX[1:npred, y_pos, drop = FALSE]
		R2 <- as.double(sum(covX[1:npred, y_pos, drop = FALSE] * 
								coefs)/covX[y_pos, y_pos, drop = FALSE])
	}
	R2adj <- 1 - (1 - R2) * (n - df.int)/(rdf)
	SST <- as.double(covX[y_pos, y_pos]) * (n - 1)
	SSreg <- SST * R2
	SSres <- SST - SSreg
	MSres <- SSres/((rdf))
	sigma <- sqrt(MSres)
	if (!(ncol(covX) == 1 & intercept)) {
		SEs <- sqrt(diag(solve((covX)[1:npred, 1:npred, drop = FALSE]) * 
						 	MSres/(n - 1)))
		ts <- coefs/SEs
		ps <- 2 * (1 - pt(abs(ts), rdf))
	}
	if (intercept == 1) {
		if (ncol(covX) == 1 & intercept) 
			coefs <- setNames(means[y_pos], "(Intercept)")
		else {
			coefs <- as.double(c(Intercept = means[y_pos] - means[1:npred] %*% 
								 	solve(covX[1:npred, 1:npred, drop = FALSE]) %*% 
								 	covX[1:npred, y_pos, drop = FALSE], coefs))
			SEs <- c(NA, as.double(SEs))
			ts <- c(NA, as.double(ts))
			ps <- c(NA, as.double(ps))
			names(coefs) <- c("(Intercept)", colnames(covX)[1:npred])
		}
	}
	else names(coefs) <- colnames(covX)[1:npred]
	Fstat <- rdf/(k - df.int) * R2/(1 - R2)
	pval <- as.double(pf(Fstat, k - df.int, rdf, lower.tail = FALSE))
	logdet <- three.point.compute(tree, cbind(setNames(rep(1, 
														   n), tree$tip.label)))$logd
	ll <- -n/2 * log(2 * pi) - n/2 * log((n - k) * MSres/n) - 
		logdet/2 - n/2
	if (any(is.na(trait_data))) 
		ll <- NA
	ret <- list(coefficients = coefs, SEs = SEs, ts = ts, ps = ps, 
				R2 = R2, R2adj = R2adj, sigma = sigma, Fstat = Fstat, 
				pval = pval, df1 = k, df2 = rdf, dims = list(N = n, p = npred, 
															 REML = PPE$REML, df.int = df.int), model = formula, 
				SST = SST, SSres = SSres, SSreg = SSreg, logLik = ll, 
				PPE = PPE, original_data = original_data, covX = covX)
	class(ret) <- "phylopars.lm"
	ret
}


