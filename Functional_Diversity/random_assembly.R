

### from http://kembellab.ca/r-workshop/biodivR/SK_Biodiversity_R.html
library(picante)
library(dplyr)
library(ggplot2)
library(devtools)
library(ggtree)
library(ggdendro)

### Load data
rdimat <- read.csv("RDImat.csv")
ntbl.raw <- read.csv("ntbl.csv")

### Create RDI matrix, for trait analysis
ntbl.mic.um <- ntbl.raw %>% 
  group_by(species) %>% 
  summarise(mean.CA = mean(CA_mg*1000, na.rm = TRUE),
            mean.EPA = mean(EPA_g*1000, na.rm = TRUE), 
            mean.DHA = mean(DHA_g*1000, na.rm = TRUE), 
            mean.ZN = mean(ZN_mg*1000, na.rm = TRUE), 
            mean.FE = mean(FE_mg*1000, na.rm = TRUE)) %>%
  filter(!is.na(mean.CA)) %>%
  filter(!is.na(mean.EPA)) %>%
  filter(!is.na(mean.DHA)) %>%
  filter(!is.na(mean.ZN)) %>%
  filter(!is.na(mean.FE)) 
# ?complete.cases
  matrix.mic <- data.matrix(ntbl.mic.um[, 2:6])
rownames(matrix.mic) <- ntbl.mic.um$species 
View(matrix.mic)

 ### Create matrices with grouping variables 
  ntbl.env <- ntbl.raw %>% 
  group_by(species) %>% 
  # select(taxon, TL, Habitat, Subgroup, Abs_lat, max_size) %>%
  summarise(mean.CA = mean(CA_mg*1000, na.rm = TRUE),
            mean.EPA = mean(EPA_g*1000, na.rm = TRUE), 
            mean.DHA = mean(DHA_g*1000, na.rm = TRUE), 
            mean.ZN = mean(ZN_mg*1000, na.rm = TRUE), 
            mean.FE = mean(FE_mg*1000, na.rm = TRUE),
            mean.TL = mean(TL, na.rm = TRUE),
            mean.size = mean(max_size, na.rm = TRUE),
            mean.lat = mean(Abs_lat, na.rm = TRUE)) %>% 
  filter(!is.na(mean.CA)) %>%
  filter(!is.na(mean.EPA)) %>%
  filter(!is.na(mean.DHA)) %>%
  filter(!is.na(mean.ZN)) %>%
  filter(!is.na(mean.FE)) %>% View

ntbl.taxon <- ntbl.raw %>% 
  group_by(species) %>% 
  select(taxon, Habitat, Subgroup) %>% 
  distinct(species) %>% View

ntbl.all <- left_join(ntbl.env, ntbl.taxon, by = "species") %>%
  select(Subgroup, Habitat, taxon)
rownames(ntbl.all) <- ntbl.env$species 
View(ntbl.all)

ntbl.env <- data.matrix(ntbl.all)
rownames(ntbl.env) <- ntbl.all$species 

View(ntbl.env)
###Create subgroup list
ntbl.subgroup <- ntbl.all$Subgroup

###Standardize the trait values??
cent.matrix.mic <- scale(matrix.mic) ### standardize the traits 


#### Create and plot dendrogram
mydist <- function(x) dist(x, method = "euclidian")
myhclust <- function(x) hclust(x, method = "average")
tree <- myhclust(mydist(matrix.mic))
treeheight(tree)
ggdendrogram(tree, rotate = FALSE, size = 2, theme_dendro = TRUE) + geom_text(colour = "purple") 
ggsave("standard.nut.dendro.png")


#### Following Katie's code

ord.mine <- metaMDS(matrix.mic,distance="euclidean",trymax=100)
plot(ord.mine, type = "n",cex=.25)
site.scaling <- as.data.frame(ord.mine$points)
points(site.scaling,pch=16)

ef2 <- envfit(ord.mine, ntbl.all)
ef2
plot(ef2, p.max = 0.05,cex=.55)

site.scaling$nfi_plot <- row.names(site.scaling)
ntbl.all$nfi_plot <- row.names(ntbl.all)

View(site.scaling)
View(ntbl.all)
new.compiled <- merge(site.scaling, ntbl.all, by=c("nfi_plot"))
View(new.compiled)

##now replot ordination, with sites colour-coded##
png(filename = "subgroupMDS.png", width = 10, height = 8, units = 'in', res = 300)
plot(ord.mine, type = "n", cex=1.5)
points(new.compiled$MDS1, new.compiled$MDS2, col=new.compiled$Subgroup, pch= 16, cex = 1.5)
dev.off()
points(new.compiled$MDS1,new.compiled$MDS2, col= c("cadetblue", "sienna", "sienna"), pch= 16, cex = 1.5)

########## Calculate beta diversity ####


# calculate Bray-Curtis distance among samples
comm.bc.dist <- vegdist(matrix.mic, method = "bray")
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
mod <- betadisper(comm.bc.dist, ntbl.subgroup)
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
ntbl.subgroup %>% 
  data_frame(subgrp = .) %>% 
  adonis(comm.bc.dist ~ subgrp, data = .)

# ### steepness of curve--can't get this work, but inspired by this vignette, page 8: https://cran.r-project.org/web/packages/vegan/vignettes/diversity-vegan.pdf
# z <- betadiver(matrix.mic, "z")
# str(z)
# betadis <- betadisper(z)
# 
# z <- betadiver(comm.bc.dist, "z")
# quantile(z)
# mod <- with(ntbl.subgroup, betadisper(z))
# mod
# ?betadiver


The metaMDS function automatically transforms data and checks solution
# robustness
comm.bc.mds <- metaMDS(matrix.mic, dist = "bray")
png(filename = "nut.MDS.png", width = 8, height = 6, units = 'in', res = 400)
plot(comm.bc.mds, type = "t")
dev.off()
# text(x, y, labels = row.names(comm.bc.mds$points), cex=.7) #tried to add labels of the species, didn't work
shnam <- make.cepnames(names(matrix.mic))
shnam[1:5]
pl <- plot(comm.bc.mds, dis = "sp")
identify(pl, "sp", labels = shnam)


(comm.bc.mds$points)
site.scaling <- as.data.frame(comm.bc.mds$points)
points(site.scaling,pch=16)

site.scaling$nfi_plot <- row.names(site.scaling)



# Assess goodness of ordination fit (stress plot)
stressplot(comm.bc.mds)

# calculate trait distance - Euclidean distance among scaled trait values -
# we want the full distance matrix
trait.dist <- as.matrix(dist(matrix.mic, method = "euclidean"))
# calculate trait ses.mpd
comm.sesmpd.traits <- ses.mpd(matrix.mic, ntbl.env, null.model = "richness", abundance.weighted = FALSE, runs = 999)
# compare trait ses.mpd between habitats
plot(comm.sesmpd.traits$mpd.obs.z ~ metadata$habitat, xlab = "Habitat", ylab = "Trait SES(MPD)")
abline(h = 0, col = "gray")

#####From vegan package vignette
## There is no data set on species properties yet, and therefore
## the example uses taxonomy 
data(dune.env)
View(dune)
View(dune.env)
# d <- taxa2dist(dune.taxon, varstep=TRUE)
# cl <- hclust(d, "aver")
# treedive(dune, cl)
## Significance test using Null model communities.
## The current choice fixes only site totals.
oecosimu(dune, treedive, "r0", tree = cl)
## Clustering of tree distances
dtree <- treedist(dune, cl)
plot(hclust(dtree, "aver"))

nutcomm <- read_csv("nutcomm.csv")



?"ggdendro"

p <- ggtree(treephylo, color="steelblue", size=0.5, linetype="dotted")
p + geom_text(aes(x=branch, label=label), size=3, color="purple", vjust=-0.3)
p + geom_text(aes(label=label), size=5, color="purple", hjust=-0.3)
p + geom_text(aes(x=branch, label=label), size=3, color="purple", vjust=-0.3)
ggtree(treephylo) + geom_text(aes(label=node)) %>% hilight(node= 57, fill="darkgreen", alpha=.6)


treephylo <- as.phylo(tree)
sum(treephylo$edge.length)
summary(treephylo)


source("https://bioconductor.org/biocLite.R")
biocLite("ggtree")
plot(tree)
treeheight(tree)

oecosimu(sm, "r0", tree = cl)
?oecosimu
treedive(nutcomm, tree, match.force = TRUE)
View(tree)
View(nutcomm)


###null model in vegan
## non-sequential nullmodel
(nm <- nullmodel(matrix.mic, "r00"))
(sm <- simulate(nm, nsim=10))

###
####################
## RANDOM ASSEMBLY from Nathan Kraft's website #
####################

## Randomly removes species, weighted by abundance, from the species pool until a final_richness value is reached	
# random_assembly=function(pool=pool, final_richness, abund=NULL){
#   size<-nrow(pool)
#   n_victim<-size-final_richness
#   if(n_victim<1){
#     print("pool has equal or lesser richness to final size- can't do random assembly")
#     return(pool)
#   }
#   
#   
#   if(is.null(abund)){
#     alive<-sample(1:size, final_richness)
#     return(pool[alive,])
#   }
#   
#   merge(pool, abund)->poolPlus
#   
#   alive<-sample((1:nrow(poolPlus)), final_richness, prob=poolPlus$abund)
#   
#   return(poolPlus[alive,c(1,2)])
#   
#   
#   
# }

