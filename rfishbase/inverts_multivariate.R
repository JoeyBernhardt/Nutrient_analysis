### Minerals

library(dplyr)
library(vegan)
ntbl.raw <- read.csv("/Users/Joey/Documents/Nutrient_Analysis/data/ntbl.csv")
inverts.new <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data/aquatic_inverts_micronutrients.csv")

#### What I did here was to run the same multivariate analysis using the new inverts data, just for CA, FE and ZN.
ntbl.minerals <- ntbl.raw %>% 
  select(Subgroup, species, CA_mg, FE_mg, ZN_mg)
table(ntbl.minerals$Subgroup)

inverts.minerals <- inverts.new %>% 
  select(Subgroup, species, CA_mg, FE_mg, ZN_mg) %>% 
  filter(Subgroup != "Echinoderm") 

minerals <- bind_rows(ntbl.minerals, inverts.minerals)

table(minerals$Subgroup)
hist(minerals$CA_mg)
str(minerals)

minerals %>% 
  group_by(Subgroup) %>% 
  filter(Subgroup != "Echinoderm") %>% 
  summarise(avgCA = mean(CA_mg, na.rm = TRUE))

min.mat <- minerals %>% 
  group_by(species) %>% 
  summarise(mean.CA = mean(CA_mg*1000, na.rm = TRUE),
            mean.ZN = mean(ZN_mg*1000, na.rm = TRUE), 
            mean.FE = mean(FE_mg*1000, na.rm = TRUE)) %>%
  filter(!is.na(mean.CA)) %>%
  filter(!is.na(mean.ZN)) %>%
  filter(!is.na(mean.FE)) 

matrix.min <- data.matrix(min.mat[, 2:4])
rownames(matrix.min) <- min.mat$species 

dim(min.mat)

min.taxon <- minerals %>% 
  group_by(species) %>% 
  select(Subgroup) %>% 
  distinct(species) %>% View
dim(min.taxon)

min.env <- semi_join(min.taxon, min.mat, by = "species")
rownames(min.env) <- min.env$species 
dim(min.env)

mydist <- function(x) dist(x, method = "euclidian")
myhclust <- function(x) hclust(x, method = "average")
tree <- myhclust(mydist(matrix.min))


#### Following Katie's code

ord.mine <- metaMDS(matrix.min,distance="euclidean",trymax=100)
plot(ord.mine, type = "n",cex=.25)
site.scaling <- as.data.frame(ord.mine$points)
points(site.scaling,pch=16)

ef2 <- envfit(ord.mine, ntbl.all)
ef2
plot(ef2, p.max = 0.05,cex=.55)

site.scaling$nfi_plot <- row.names(site.scaling)
min.env$nfi_plot <- row.names(min.env)

View(site.scaling)
View(ntbl.all)
new.compiled <- merge(site.scaling, min.env, by=c("nfi_plot"))
new.compiled$Subgroup <- as.factor(new.compiled$Subgroup)
View(min.env)
View(new.compiled)

##now replot ordination, with sites colour-coded##
png(filename = "subgroupMDS.png", width = 10, height = 8, units = 'in', res = 300)
plot(ord.mine, type = "n", cex=1.5)
points(new.compiled$MDS1, new.compiled$MDS2, col= new.compiled$Subgroup, pch= 16, cex = 1.5)
legend('topright', legend = levels(new.compiled$Subgroup), col = 1:3, cex = 0.8, pch = 1)

dev.off()
points(new.compiled$MDS1,new.compiled$MDS2, col= c("cadetblue", "sienna", "pink", "green"), pch= 16, cex = 1.5)

min.subgroup <- min.env$Subgroup

# calculate Bray-Curtis distance among samples
comm.bc.dist <- vegdist(matrix.min, method = "bray")
hist(comm.bc.dist)

# cluster communities using average-linkage algorithm
comm.bc.clust <- hclust(comm.bc.dist, method = "average")

# plot cluster diagram
plot(comm.bc.clust, ylab = "Bray-Curtis dissimilarity")
png(filename = "BC.dendro.png", width = 6, height = 4, units = 'in', res = 300)
ggdendrogram(comm.bc.clust, rotate = FALSE, size = 2, theme_dendro = TRUE) + geom_text(colour = "purple")
dev.off()

### Convert dengrogram to phylo tree, so can use ggtree to plot
treephylo <- as.phylo(comm.bc.clust)
sum(treephylo$edge.length)
summary(treephylo)
treeheight(treephylo)


### Use betadisper to test the significance of the multivariate groups

mod <- betadisper(comm.bc.dist, min.subgroup)
?betadisper

## Perform test
anova(mod)

## Permutation test for F
permutest(mod, pairwise = TRUE, permutations = 99)

## Tukey's Honest Significant Differences
(mod.HSD <- TukeyHSD(mod))
plot(mod.HSD)
boxplot(mod)

?betadisper
## Using group centroids
mod3 <- betadisper(comm.bc.dist, ntbl.subgroup, bias.adjust = TRUE)
mod3
permutest(mod3, permutations = 99)
anova(mod3)
plot(mod3)
boxplot(mod3)
plot(TukeyHSD(mod3))

?adonis

#### Use adonis to ask whether the group means in multivariate space are different from each other ####
min.subgroup %>% 
  data_frame(subgrp = .) %>% 
  adonis(comm.bc.dist ~ subgrp, data = .)
