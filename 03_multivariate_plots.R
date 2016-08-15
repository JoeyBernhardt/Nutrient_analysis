with(min.env, levels(Subgroup))
scl <- 3 ## scaling = 3
colvec <- c("slateblue1", "purple4", "orchid2")
plot(ord.mine, type = "n", scaling = scl)
with(min.env, points(ord.mine, display = "sites", col = colvec[Subgroup],
                      scaling = scl, pch = 21, cex = 1.5, bg = colvec[Subgroup]))
text(ord.mine, display = "species", scaling = scl, cex = 0.8, col = "darkcyan")
with(min.env, legend("topleft", legend = levels(Subgroup), bty = "n",
                      col = colvec, pch = 21, pt.bg = colvec))

ordihull(ord.mine, min.env$Subgroup, conf = 0.95, label = F)
?ordiellipse
####

What I did here was to run the same multivariate analysis using the new inverts data, just for CA, FE and ZN.
```{r}
ntbl.micro <- ntbl.raw %>% 
  dplyr::select(Subgroup, species, CA_mg, FE_mg, ZN_mg, EPA_g, DHA_g)
```
```{r}
inverts.micro <- inverts.new %>% 
  dplyr::select(Subgroup, species, CA_mg, FE_mg, ZN_mg, EPA_g, DHA_g) %>% 
  filter(Subgroup != "Echinoderm") 
```

```{r}
micro <- bind_rows(ntbl.micro, inverts.micro) 
micro$Subgroup <- as.factor(micro$Subgroup)
micro$species <- as.factor(micro$species)


micro <- micro %>% 
  filter(!is.na(species))

min.mat <- micro %>% 
  group_by(species) %>% 
  summarise(mean.CA = mean(CA_mg*1000, na.rm = TRUE),
            mean.ZN = mean(ZN_mg*1000, na.rm = TRUE), 
            mean.FE = mean(FE_mg*1000, na.rm = TRUE),
            mean.EPA = mean(EPA_g*1000, na.rm = TRUE),
            mean.DHA = mean(DHA_g*1000, na.rm = TRUE)) %>%
  filter(!is.na(mean.CA)) %>%
  filter(!is.na(mean.ZN)) %>%
  filter(!is.na(mean.FE)) %>% 
  filter(!is.na(mean.EPA)) %>% 
  filter(!is.na(mean.DHA))

matrix.min <- data.matrix(min.mat[, 2:6])
rownames(matrix.min) <- min.mat$species 
```
```{r}
names(micro)

str(micro)
min.taxon <- micro %>% 
  dplyr::group_by(species) %>% 
  dplyr::select(Subgroup) %>% 
  distinct(species)


min.env <- semi_join(min.taxon, min.mat, by = "species") 
min.env <- as.data.frame(min.env)

```
##### ordination
```{r}
ord.mine <- metaMDS(matrix.min, distance="bray", trymax=100)
ord.mine$stress
plot(ord.mine, type = "t",cex=.5)
site.scaling <- as.data.frame(ord.mine$points)
points(site.scaling,pch=16)

View(min.env)
site.scaling$nfi_plot <- row.names(site.scaling)
site.scaling$species <- row.names(site.scaling)
View(site.scaling)
min.env$nfi_plot <- row.names(min.env)

new.compiled <- merge(site.scaling, min.env, by=c("nfi_plot"))
##new april11
new.compiled <- merge(site.scaling, min.env, by=c("species"))
new.compiled$Subgroup <- as.factor(new.compiled$Subgroup)
```

####now replot ordination, with groups colour-coded##
```{r}
plot(ord.mine, type = "n", cex=1)
# points(new.compiled$MDS1, new.compiled$MDS2, pch= as.integer(new.compiled$Subgroup), cex = 1)
points(new.compiled$MDS1, new.compiled$MDS2, col = (as.integer(new.compiled$Subgroup)), pch= as.integer(new.compiled$Subgroup), cex = 1.2)

## add confidence ellipses around subgroups
