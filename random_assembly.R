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


### from http://kembellab.ca/r-workshop/biodivR/SK_Biodiversity_R.html


library(picante)
library(dplyr)

rdimat <- read.csv("RDImat.csv")
ntbl.raw <- read.csv("ntbl.csv")
View(rdimat)

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

matrix.mic <- data.matrix(ntbl.mic.um[, 2:6])
rownames(matrix.mic) <- ntbl.mic.um$species 

# calculate Bray-Curtis distance among samples
comm.bc.dist <- vegdist(matrix.mic, method = "bray")
# cluster communities using average-linkage algorithm
comm.bc.clust <- hclust(comm.bc.dist, method = "average")
# plot cluster diagram
plot(comm.bc.clust, ylab = "Bray-Curtis dissimilarity")

The metaMDS function automatically transforms data and checks solution
# robustness
comm.bc.mds <- metaMDS(matrix.mic, dist = "bray")
# Assess goodness of ordination fit (stress plot)
stressplot(comm.bc.mds)

# # calculate trait distance - Euclidean distance among scaled trait values -
# # we want the full distance matrix
# trait.dist <- as.matrix(dist(matrix.mic, method = "euclidean"))
# # calculate trait ses.mpd
# comm.sesmpd.traits <- ses.mpd(matrix.mic, trait.dist, null.model = "richness", abundance.weighted = FALSE, runs = 999)
# # compare trait ses.mpd between habitats
# plot(comm.sesmpd.traits$mpd.obs.z ~ metadata$habitat, xlab = "Habitat", ylab = "Trait SES(MPD)")
# abline(h = 0, col = "gray")


## There is no data set on species properties yet, and therefore
## the example uses taxonomy 
data(dune)
View(dune)
View(dune.taxon)
d <- taxa2dist(dune.taxon, varstep=TRUE)
cl <- hclust(d, "aver")
treedive(dune, cl)
## Significance test using Null model communities.
## The current choice fixes only site totals.
oecosimu(dune, treedive, "r0", tree = cl)
## Clustering of tree distances
dtree <- treedist(dune, cl)
plot(hclust(dtree, "aver"))

nutcomm <- read_csv("nutcomm.csv")

View(matrix.mic)
mydist <- function(x) dist(x, method = "euclidian")
myhclust <- function(x) hclust(x, method = "average")
tree <- myhclust(mydist(matrix.mic))
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




nullmodel(matrix.mic)

?taxa2dist
?treedive
