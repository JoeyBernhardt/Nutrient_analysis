### Minerals
library(plyr)
library(dplyr)
library(vegan)
library(readr)
library(ggplot2)
library(tidyr)
library(ggthemes)

ntbl.raw <- read.csv("/Users/Joey/Documents/Nutrient_Analysis/data/ntbl2.csv")
inverts.new <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data/aquatic_inverts_micronutrients.csv")

#### What I did here was to run the same multivariate analysis using the new inverts data, just for CA, FE and ZN.
ntbl.minerals <- ntbl.raw %>% 
  select(Subgroup, species, CA_mg, FE_mg, ZN_mg)
table(ntbl.minerals$Subgroup)

inverts.minerals <- inverts.new %>% 
  select(Subgroup, species, CA_mg, FE_mg, ZN_mg)
  # filter(Subgroup != "Echinoderm") 

minerals <- bind_rows(ntbl.minerals, inverts.minerals)

minerals.long <- minerals %>% 
  gather(nutrient, concentration, 3:5) %>% View
summary(minerals$CA_mg)
1503/2.99
(max(minerals$ZN_mg))/(min(minerals$ZN_mg))
max(minerals$ZN_mg)

g <- ggplot(minerals.long, aes(x = nutrient, y = log(concentration)))
g + geom_boxplot() +
  geom_point(aes(colour = Subgroup, size = 1, jitter = TRUE)) + theme_pander()
ggsave("/Users/Joey/Documents/Nutrient_Analysis/figures/nutrange_boxplot.png")

g <- ggplot(minerals.long, aes(x = nutrient, y = log(concentration), fill = Subgroup))
g + geom_boxplot() +
  geom_point(aes(colour = Subgroup, size = 1, jitter = TRUE)) + theme_pander()


hist(ntbl.raw$FAT)

ntbl.long <- ntbl.raw %>% 
  gather(nutrient, concentration, 7:14) %>% View

g <- ggplot(subset(ntbl.long, nutrient %in% c("FAT", "PROTEIN")), aes(x = nutrient, y = log(concentration)))
g + geom_boxplot() +
  geom_point(aes(colour = Subgroup, jitter = TRUE), size = 2) + theme_pander()



###  center the data

ntbl.scale <- ntbl.raw %>% mutate_each_(funs(scale),vars=c("CA_mg","FE_mg", "HG_mcg", "FAT", "ZN_mg", "PROTEIN", "EPA_g", "DHA_g")) 

ntbl.scale.long <- ntbl.scale %>% 
  gather(nutrient, concentration, 7:14)

g <- ggplot(ntbl.scale.long, aes(x = nutrient, y = log(concentration)))
# g + geom_pointrange(ymin=min(), ymax=max())
g + geom_boxplot() +
  stat_boxplot(geom ='errorbar') +
  geom_point(aes(colour = Subgroup), size = 2) + theme_minimal()
ggsave("/Users/Joey/Documents/Nutrient_Analysis/figures/log-nuts-range.png")

g <- ggplot(ntbl.long, aes(x = nutrient, y = concentration))
# g + geom_pointrange(ymin=min(), ymax=max())
g + geom_boxplot() +
  
  geom_point(aes(colour = Subgroup), size = 2, position = "jitter") + theme_pander()
?stat_boxplot

ntbl.long$RDI <- ntbl.long$nutrient

#### Here I add another column with the RDI values for each nutrient ####
ntbl.long <- ntbl.long %>% mutate(RDI = revalue(RDI,
                                         c("CA_mg" = "1200",
                                           "ZN_mg" = "11",
                                           "FE_mg" = "18",
                                           "EPA_g" = "1",
                                           "DHA_g" = "1",
                                           "PROTEIN" = "56",
                                           "FAT" = "70"))) %>% View
str(ntbl.scale.long)
ntbl.scale.long$RDI <- as.double(ntbl.scale.long$RDI)

write_csv(ntbl.long, "/Users/Joey/Documents/Nutrient_Analysis/data/ntbl.long.csv")
n.long <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data/ntbl.long.csv")


#### add a column for 25% of RDI
n.long <- n.long %>% 
  mutate(RDI.25per = (concentration/(RDI/4)),
         RDI.per = (concentration/RDI),
         RDI.20per = (concentration/(RDI/5)),
         RDI.15per = (concentration/(RDI/6)))

g <- ggplot(n.long, aes(x = nutrient, y = RDI.25per))
# g + geom_pointrange(ymin=min(), ymax=max())
g + geom_boxplot() +
  stat_boxplot(geom ='errorbar') +
  geom_point(aes(colour = Subgroup), size = 2) + theme_minimal()
ggsave("/Users/Joey/Documents/Nutrient_Analysis/figures/log-nuts-range.png")


#### body.prop: If max size is less than 100g, gets whole, if not, gets part. bones.body is a combo of fish less than 100g and those from Cambodia where study noted that bones were eaten.

n.long <- n.long %>% 
  mutate(body.whole = (max_size < 0.1),
         eat.bones = (Abs_lat == 12.265754 | Abs_lat == 11.066667),
         bones.body = (max_size < 0.1 | Abs_lat == 12.265754 | Abs_lat == 11.066667),
         bones.body.invert = (max_size < 0.1 | Abs_lat == 12.265754 | Abs_lat == 11.066667 | Subgroup != "Finfish"))

n.long %>% 
  filter(!is.na(bones.body)) %>% 
  # arrange(desc(nutrient)) %>% 
  ggplot(., aes(x = nutrient, y = log(RDI.per), fill = bones.body, geom = "boxplot")) +
 geom_boxplot() +
  theme_minimal() +
  geom_hline(yintercept=log(.15)) +
  ylab("percentage of RDI in edible portion, log scale")
ggsave("/Users/Joey/Documents/Nutrient_Analysis/figures/bones.body-RDI.png")

# n.long$nutrient <- as.factor(n.long$nutrient)
# levels(n.long$nutrient) <- c("var1", "var4", "var5", "var6", "var2", "var8", "var7", "var3")
# n.long$nutrient2 <- factor(n.long$nutrient, levels = c("var1", "var4", "var5", "var6", "var2", "var8", "var7", "var3"))

whole <- lm(log(RDI.15per) ~ bones.body, data = subset(n.long, nutrient == "DHA_g"))
summary(whole)

whole.aov <- aov(log(RDI.15per) ~ bones.body, data = subset(n.long, nutrient == "ZN_mg"))
whole.aov
posthoc <- TukeyHSD(x=whole.aov, 'bones.body', conf.level=0.95)
posthoc
plot(RDI.15per ~ bones.body, data = n.long)
  
g <- ggplot(n.long, aes(x = nutrient, y = log(concentration), fill = body.whole))
g + geom_boxplot() +
  stat_boxplot(geom ='errorbar') +
  # geom_point(aes(colour = Subgroup), size = 2) +
  theme_minimal()

length(unique(n.long$body.whole))

summary(n.long$body.whole)

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
?metaMDS
ord.mine <- metaMDS(matrix.min,distance="bray",trymax=100)
ord.mine$stress
plot(ord.mine, type = "t",cex=.5)
site.scaling <- as.data.frame(ord.mine$points)
points(site.scaling,pch=16)

ef2 <- envfit(ord.mine, min.env)

ef2
plot(ef2, p.max = 0.05,cex=.55)
?envfit
?vectorfit
vectorfit(ef2, min.env)

site.scaling$nfi_plot <- row.names(site.scaling)
min.env$nfi_plot <- row.names(min.env)

View(site.scaling)
View(ntbl.all)
new.compiled <- merge(site.scaling, min.env, by=c("nfi_plot"))
new.compiled$Subgroup <- as.factor(new.compiled$Subgroup)
View(min.env)
View(new.compiled)

##now replot ordination, with sites colour-coded##
png(filename = "subgroupMDSwMoll3.png", width = 10, height = 8, units = 'in', res = 300)
plot(ord.mine, type = "n", cex=1)
points(new.compiled$MDS1, new.compiled$MDS2, pch= as.integer(new.compiled$Subgroup), cex = 1)
points(new.compiled$MDS1, new.compiled$MDS2, col = (as.integer(new.compiled$Subgroup)), pch= as.integer(new.compiled$Subgroup), cex = 1)
points(new.compiled$MDS1, new.compiled$MDS2, col = jColors, pch = 16, cex = 1)
# add confidence ellipses around habitat types
ordiellipse(ord.mine, draw = "polygon", new.compiled$Subgroup, conf = 0.95, label = T)
ordispider(ord.mine, new.compiled$Subgroup,col="grey")
?ordispider

jColors <- c('chartreuse3', 'cornflowerblue', 'darkgoldenrod1', 'peachpuff3')

legend('topleft', legend = levels(new.compiled$Subgroup), col = 1:3, pch = 16, cex = 0.8)

dev.off()
points(new.compiled$MDS1,new.compiled$MDS2, col= c("cadetblue", "sienna", "pink", "green"), pch= 16, cex = 1.5)

min.subgroup <- min.env$Subgroup

?monoMDS

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



#### SAC
ntbl.RDI <- minerals %>% 
  group_by(species) %>% 
  summarise(mean.CA = mean(CA_mg, na.rm = TRUE),
            mean.ZN = mean(ZN_mg, na.rm = TRUE), 
            mean.FE = mean(FE_mg, na.rm = TRUE)) %>% 
  mutate(RDI.CA = ifelse(mean.CA > 250, 1, 0)) %>% 
  mutate(RDI.FE = ifelse(mean.FE > 4.5, 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(mean.ZN > 2.75, 1, 0)) %>%
  mutate(RDI.micro.tot = rowSums(.[5:7])) %>% 
  filter(!is.na(RDI.micro.tot)) %>% 
  # arrange(., RDI.micro.tot) %>% 
  # select(., c(1,5:7))
  select(., 5:7)

ntbl.RDI.noMoll <- minerals %>% 
  group_by(species) %>% 
  filter(Subgroup != "Molluscs") %>% 
  summarise(mean.CA = mean(CA_mg, na.rm = TRUE),
            mean.ZN = mean(ZN_mg, na.rm = TRUE), 
            mean.FE = mean(FE_mg, na.rm = TRUE)) %>% 
  mutate(RDI.CA = ifelse(mean.CA > 250, 1, 0)) %>% 
  mutate(RDI.FE = ifelse(mean.FE > 4.5, 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(mean.ZN > 2.75, 1, 0)) %>%
  mutate(RDI.micro.tot = rowSums(.[5:7])) %>% 
  filter(!is.na(RDI.micro.tot)) %>% 
  # arrange(., RDI.micro.tot) %>% 
  # select(., c(1,5:7))
  select(., 5:7)

spa.rand <- specaccum(ntbl.RDI, method = "random")
png(filename = "sac.full.vs.noMoll.png", width = 6, height = 4, units = 'in', res = 300)
plot(spa.rand, col = "cadetblue", lwd = 2, ci = 1, ci.type = "bar", ci.lty = 3,  ci.col = "cadetblue", ylim = c(0,4), xlim = c(0,80), xlab = "number of fish species in diet", ylab = "number of distinct RDI targets reached", main = "25% RDI targets")
abline( v= 15, col = "cadetblue")
abline( v = 26, col = "pink")
dev.off()
summary(spa.rand)

png(filename = "/Users/Joey/Documents/Nutrient_Analysis/figures/sac.full.vs.noMoll.png", width = 10, height = 8, units = 'in', res = 300)
spa.rand.noMoll <- specaccum(ntbl.RDI.noMoll, method = "random")
plot(spa.rand, col = "black", lwd = 4, ci = 1, ci.type = "polygon", ci.lty = 3,  ci.col = "blue", ylim = c(0,3.5), xlim = c(0,60), xlab = "number of fish species in diet", ylab = "number of distinct RDI targets reached", main = "micronutrients: 25% RDI targets")
plot(spa.rand.noMoll, add = TRUE, col = "darkgrey", lwd = 4, ci = 1, ci.type = "polygon", ci.lty = 3,  ci.col = "darkgrey", ylim = c(0,3.5), xlim = c(0,60), xlab = "number of fish species in diet", ylab = "number of distinct RDI targets reached", main = "25% RDI targets")
dev.off()
legend('bottomright', legend = c("full", "no molluscs"), lty=c(1,1), lwd=c(4.4), col=c('black', 'darkgrey')) # gives the legend lines the correct color and width))








