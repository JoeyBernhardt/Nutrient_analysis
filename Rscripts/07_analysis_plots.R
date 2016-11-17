#### Re-do of the nutrients analysis
#### November 17 2016
#### Joey Bernhardt



# load libraries ----------------------------------------------------------

library(stringr)
library(tidyverse)
library(broom)
library(coefplot)
suppressPackageStartupMessages(library(vegan))



# read in data ------------------------------------------------------------

a22 <- read_csv("data-processed/all_nuts_working22.csv")
n.long <- read.csv("/Users/Joey/Documents/Nutrient_Analysis/data/aq.long.csv") ## this is latest working version from round 1.


# do a bit of finagling to get it into long form --------------------------

n.long <- a22 %>% 
  select(species_name, subgroup, prot_g, protcnt_g, epa, dha, fapun3, ca_mg, fat_g, zn_mg, fe_mg, seanuts_id, tl, food_item_id) %>% 
  gather(key = "nutrient", value = "concentration", prot_g, protcnt_g, epa, dha, fapun3, ca_mg, fat_g, zn_mg, fe_mg) %>% 
  filter(!is.na(concentration)) 


# begin plots and analysis ------------------------------------------------

g <- ggplot(n.long, aes(concentration)) + geom_histogram(binwidth = 0.07)
g + facet_grid(nutrient ~ ., scales = "free_y") + theme_bw() + scale_x_log10()



# find the min and max for each nutrient ----------------------------------

n.long %>% 
  group_by(nutrient) %>% 
  distinct(species_name) %>%
  count() %>% View


a22 %>% 
  filter(subgroup == "Crustacean") %>% 
  select(ca_mg, fe_mg, zn_mg, everything()) %>% 
  arrange(desc(ca_mg)) %>% View


# onto multivariate stuff -------------------------------------------------

ntbl.minerals <- a22 %>% 
  dplyr::select(subgroup, species_name, ca_mg, fe_mg, zn_mg)

ntbl.minerals %>% 
  arrange(desc(fe_mg)) %>% View

unique(ntbl.minerals$fe_mg)


## ok let's just get rid of the one super outlier ca and fe measurement for now

ntbl.minerals$ca_mg[ntbl.minerals$ca_mg == 41206.00000] <- NA
ntbl.minerals$fe_mg[ntbl.minerals$fe_mg > 40939.00000] <- NA

minerals <- ntbl.minerals
minerals$subgroup <- as.factor(minerals$subgroup)
minerals$species_name <- as.factor(minerals$species_name)

minerals <- minerals %>% 
  filter(!is.na(species_name))

min.mat <- minerals %>% 
  group_by(species_name) %>% 
  summarise(mean.CA = mean(ca_mg*1000, na.rm = TRUE),
            mean.ZN = mean(zn_mg*1000, na.rm = TRUE), 
            mean.FE = mean(fe_mg*1000, na.rm = TRUE)) %>%
  filter(!is.na(mean.CA)) %>%
  filter(!is.na(mean.ZN)) %>%
  filter(!is.na(mean.FE)) 

matrix.min <- data.matrix(min.mat[, 2:4])
rownames(matrix.min) <- min.mat$species_name 

min.taxon <- minerals %>% 
  dplyr::distinct(species_name, subgroup) 
  

min.env <- semi_join(min.taxon, min.mat, by = "species_name") 
min.env <- as.data.frame(min.env)

min.env <- min.env %>%
  dplyr::filter(!is.na(species_name)) 

rownames(min.env) <- min.env$species_name 
dim(min.env)
min.env <- as.matrix(min.env)
View(min.env)

#### begin ordination!

ord.mine <- metaMDS(matrix.min, distance="bray", trymax=100)
ord.mine$stress
plot(ord.mine, type = "t",cex=.5)
site.scaling <- as.data.frame(ord.mine$points)
points(site.scaling,pch=16)


site.scaling$nfi_plot <- row.names(site.scaling)
site.scaling$species_name <- row.names(site.scaling)

min.env$nfi_plot <- row.names(min.env)

new.compiled <- full_join(site.scaling, min.env)


plot(ord.mine, type = "t", cex=1) ### looks like Metacarcinus magister is an outlier here
# points(new.compiled$MDS1, new.compiled$MDS2, pch= as.integer(new.compiled$Subgroup), cex = 1)
points(new.compiled$MDS1, new.compiled$MDS2, col = (as.integer(new.compiled$subgroup)), pch= as.integer(new.compiled$subgroup), cex = 1.2)
legend('topleft', legend = levels(new.compiled$subgroup), col = 1:3, pch = 16, cex = 0.8)


ordiplot(ord.mine, type = "text")
ordiellipse(ord.mine, draw = "polygon", new.compiled$subgroup, conf = 0.95, label = T)
ordihull(ord.mine, draw = "polygon", new.compiled$subgroup, conf = 0.95, label = T)
# ordicluster(ord.mine, draw = "polygon", new.compiled$subgroup, conf = 0.95, label = T)
ordispider(ord.mine, new.compiled$Subgroup,col="grey")
legend('topleft', legend = levels(new.compiled$subgroup), col = 1:3, pch = 16, cex = 0.8)


### calculate Bray-Curtis distance among samples

comm.bc.dist <- vegdist(matrix.min, method = "bray")
hist(comm.bc.dist)

# cluster communities using average-linkage algorithm
comm.bc.clust <- hclust(comm.bc.dist, method = "average")

# plot cluster diagram
plot(comm.bc.clust, ylab = "Bray-Curtis dissimilarity")

## Use betadisper to test the significance of the multivariate groups
min.subgroup <- min.env$subgroup
mod <- betadisper(comm.bc.dist, min.subgroup)

## Perform test
anova(mod)

## Permutation test for F
permutest(mod, pairwise = TRUE, permutations = 200)


#### Use adonis to ask whether the group means in multivariate space are different from each other ####

min.subgroup %>% 
  data_frame(subgrp = .) %>% 
  adonis(comm.bc.dist ~ subgrp, data = .)
