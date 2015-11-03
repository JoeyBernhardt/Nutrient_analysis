# FD

Hi Mary!

Load packages

```r
library(ggplot2)
library(broom)
library(plyr)
suppressPackageStartupMessages(library(dplyr))
library(knitr)
library(tidyr)
library(readr)
```

Import and clean data


```r
nut_dec3 <- read.csv("~/Desktop/Nutrient_databases/nut_dec3.csv", comment.char="#")
ntbl <- tbl_df(nut_dec3)
ntbl <- ntbl %>%
  mutate(HG_mcg = as.numeric(HG_mcg)) %>% 
  mutate(PROTCNT_g = as.numeric(PROTCNT_g)) %>% 
   rename(species = ASFIS.Scientific.name,
         taxon = ISSCAAP_cat,
         max_length = SLMAX,
         FAT = FAT.g.) %>% 
   mutate(max_size = (lwA * (max_length^lwB)/1000)) %>% 
            mutate(species = revalue(species,
                               c("Oreochromis _=Tilapia spp" = "Tilapia spp",
                                 "Tilapia\x86zillii\x86\x86" = "Tilapia zillii",
                                 "Thaleichthys\x86pacificus" = "Thaleichthys pacificus", 
                                 "Zungaro\x86zungaro" = "Zungaro zungaro",
                                 "Pinirampus\x86pirinampu\x86" = "Pinirampus pirinampu",
                                 "Platichthys\x86stellatus" = "Platichthys stellatus",
                                 "Parambassis\x86wolffii" = "Parambassis wolffii",
                                 "Oncorhynchus\x86mykiss" = "Oncorhynchus mykiss",
                                 "Oncorhynchus\x86tshawytscha" = "Oncorhynchus tshawytscha",
                                 "Oncorhynchus\x86keta" = "Oncorhynchus keta",
                                 "Oncorhynchus\x86nerka\x86" = "Oncorhynchus nerka"))) %>%
  select(species, taxon, max_size, max_length, TL, CA_mg, EPA_g, DHA_g, FE_mg, ZN_mg, HG_mcg, FAT, PROTCNT_g, lwA, lwB, Habitat, Subgroup, Abs_lat)
```


```r
ntbl.RDI.mac <- ntbl%>% 
  group_by(species) %>% 
    summarise(mean.FAT = mean(FAT, na.rm = TRUE),
              mean.PRO = mean(PROTCNT_g, na.rm = TRUE)) %>% 
  mutate(RDI.FAT = (mean.FAT > 7)) %>% 
  mutate(RDI.PRO = (mean.PRO > 5)) %>% 
  mutate(RDI.macro.tot = rowSums(.[4:5])) %>% 
  filter(!is.na(RDI.macro.tot)) %>% 
ggplot(., aes(x = reorder(species, RDI.macro.tot), y = RDI.macro.tot, na.rm = TRUE, color = species)) + geom_point(size = 3) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position="none") + ylim(0,2)
ggsave("RDI.tot.mac.png")
```

```
## Saving 7 x 5 in image
```

```r
ntbl.RDI.mic <- ntbl %>% 
  group_by(species) %>% 
    summarise(mean.CA = mean(CA_mg, na.rm = TRUE),
            mean.EPA = mean(EPA_g, na.rm = TRUE), 
            mean.DHA = mean(DHA_g, na.rm = TRUE), 
            mean.ZN = mean(ZN_mg, na.rm = TRUE), 
            mean.FE = mean(FE_mg, na.rm = TRUE)) %>% 
  mutate(RDI.CA = (mean.CA > 300)) %>% 
  mutate(RDI.FE = (mean.FE > 4.5)) %>% 
  mutate(RDI.ZN = (mean.ZN > 2.75)) %>% 
  mutate(RDI.EPA = (mean.EPA > 0.25)) %>% 
  mutate(RDI.DHA = (mean.DHA > 0.25)) %>% 
  mutate(RDI.micro.tot = rowSums(.[7:11])) %>% 
  filter(!is.na(RDI.micro.tot)) %>%
    ggplot(., aes(x = reorder(species, RDI.micro.tot), y = RDI.micro.tot, na.rm = TRUE, color = species)) + geom_point(size = 3) + theme(axis.text.x = element_text(angle = 75, hjust = 1)) + theme(legend.position="none") + ylim(0,5)
ggsave("RDI.tot.mic.png")
```

```
## Saving 7 x 5 in image
```

![](RDI.tot.mic.png)
![](RDI.tot.mac.png)


How many species reach RDI targets for macro vs. micronutrients?

```r
macro.100 <- ntbl%>% 
  group_by(species) %>% 
    summarise(mean.FAT = mean(FAT, na.rm = TRUE),
              mean.PRO = mean(PROTCNT_g, na.rm = TRUE)) %>% 
              # mean.size = mean(max_size, na.rm = TRUE)) %>% 
  mutate(RDI.FAT = (mean.FAT > 7)) %>% 
  mutate(RDI.PRO = (mean.PRO > 5)) %>% 
  mutate(RDI.macro.tot = rowSums(.[4:5])) %>% 
  filter(!is.na(RDI.macro.tot)) %>% 
  filter(RDI.macro.tot == 2) #' 15 out of 113 hit 2/2 RDI targets for protein and fat, and 98 out of 113 hit the protein RDI target.
 
macro.species <- macro.100$species %>% droplevels()


intersect(macro.100$species, ntbl$species)
```

```
##  [1] "Alosa alosa"               "Ariomma bondi"            
##  [3] "Belone belone"             "Brevoortia spp"           
##  [5] "Clupeonella cultriventris" "Cyclopterus lumpus"       
##  [7] "Engraulis encrasicolus"    "Merlangius merlangus"     
##  [9] "Pinirampus pirinampu"      "Sarda sarda"              
## [11] "Sorubim lima"              "Spicara smaris"           
## [13] "Sprattus sprattus"         "Xiphias gladius"          
## [15] "Zungaro zungaro"
```

```r
macro.species
```

```
##  [1] Alosa alosa               Ariomma bondi            
##  [3] Belone belone             Brevoortia spp           
##  [5] Clupeonella cultriventris Cyclopterus lumpus       
##  [7] Engraulis encrasicolus    Merlangius merlangus     
##  [9] Pinirampus pirinampu      Sarda sarda              
## [11] Sorubim lima              Spicara smaris           
## [13] Sprattus sprattus         Xiphias gladius          
## [15] Zungaro zungaro          
## 15 Levels: Alosa alosa Ariomma bondi Belone belone ... Zungaro zungaro
```

```r
ntbl.macro <- ntbl %>% 
  filter(species %in% c("Alosa alosa", "Ariomma bondi", "Belone belone", "Brevoortia spp", "Clupeonella cultriventris", "Cyclopterus lumpus", "Engraulis encrasicolus", "Merlangius merlangus", "Pinirampus pirinampu", "Sarda sarda", "Sorubim lima", "Spicara smaris", "Sprattus sprattus", "Xiphias gladius", "Zungaro zungaro"))


summary(ntbl.macro$max_size)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
##    0.0293    0.0767    5.4180   59.2200    8.3570 2533.0000
```

```r
summary(ntbl$max_size)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
##    0.000    0.680    2.997   30.280    9.862 6077.000      205
```

```r
ntbl$species <- macro.species[ntbl$species]





# inner_join(ntbl,macro.species)
# macro.100per <- ntbl %>%
#     filter(grepl(macro.species, ntbl$species))



subset(ntbl, species == c("macro.species"))
```

```
## Source: local data frame [0 x 18]
## 
## Variables not shown: species (fctr), taxon (fctr), max_size (dbl),
##   max_length (dbl), TL (dbl), CA_mg (dbl), EPA_g (dbl), DHA_g (dbl), FE_mg
##   (dbl), ZN_mg (dbl), HG_mcg (dbl), FAT (dbl), PROTCNT_g (dbl), lwA (dbl),
##   lwB (dbl), Habitat (fctr), Subgroup (fctr), Abs_lat (dbl)
```

```r
ntbl.micro <- ntbl %>% 
  group_by(species) %>% 
    summarise(mean.CA = mean(CA_mg, na.rm = TRUE),
            mean.EPA = mean(EPA_g, na.rm = TRUE), 
            mean.DHA = mean(DHA_g, na.rm = TRUE), 
            mean.ZN = mean(ZN_mg, na.rm = TRUE), 
            mean.FE = mean(FE_mg, na.rm = TRUE)) %>% 
  mutate(RDI.CA = (mean.CA > 300)) %>% 
  mutate(RDI.FE = (mean.FE > 4.5)) %>% 
  mutate(RDI.ZN = (mean.ZN > 2.75)) %>% 
  mutate(RDI.EPA = (mean.EPA > 0.25)) %>% 
  mutate(RDI.DHA = (mean.DHA > 0.25)) %>% 
  mutate(RDI.micro.tot = rowSums(.[7:11])) %>% 
  filter(!is.na(RDI.micro.tot)) %>% 
  filter(RDI.micro.tot == 3) 

  ntbl.micro.species <- ntbl.micro[, 1]

ntbl %>% filter(species == "ntbl.micro.species")
```

```
## Source: local data frame [0 x 18]
## 
## Variables not shown: species (fctr), taxon (fctr), max_size (dbl),
##   max_length (dbl), TL (dbl), CA_mg (dbl), EPA_g (dbl), DHA_g (dbl), FE_mg
##   (dbl), ZN_mg (dbl), HG_mcg (dbl), FAT (dbl), PROTCNT_g (dbl), lwA (dbl),
##   lwB (dbl), Habitat (fctr), Subgroup (fctr), Abs_lat (dbl)
```

```r
(ntbl.micro.species)
```

```
## Source: local data frame [0 x 1]
## 
## Variables not shown: species (fctr)
```

```r
ntbl.micro.2 <- ntbl %>% filter(species %in% c("Cipangopaludina chinensis", "Clarias gariepinus", "Cyclopterus lumpus", "Oncorhynchus tshawytscha", "Oreochromis niloticus", "Sarda sarda", "Thunnus alalunga"))

summary(ntbl.micro.2$max_size)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   6.763   6.763   6.763   6.763   6.763   6.763       1
```

```r
summary(ntbl$max_size, subset = species == c("Rapana spp", "Trachurus trachurus"))
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
##    0.000    0.680    2.997   30.280    9.862 6077.000      205
```

```r
# mean size of species that reach 3/5 nutrients' RDI is 30.28, mean size of those that reach 2 of RDI is 13.88

#' 2 hit 3/5 of RDI micronutrient targets, 7 hit 2/5 targets, 16 hit 1/5 targets  #' (out of 55)
```


```r
# ?FD
# library(FD)
# ntbl.RDI.mic
# ntbl.matrix.mic <- data.matrix(ntbl.RDI.mic[, 2:6])
# rownames(ntbl.matrix.mic) <- ntbl.RDI.mic$species
# 
# FD.mic <- dbFD(ntbl.matrix.mic)
# FD.mic
# 
# 
# ntbl.mac.matrix <- data.matrix(ntbl.RDI.mac[, 2:3])
# rownames(ntbl.mac.matrix) <- ntbl.RDI.mac$species
# 
# FD.mac <- dbFD(ntbl.mac.matrix)
# (as.data.frame(FD.mac))
# (as.data.frame(FD.mic))
```


```r
# ntbl.mic.matrix <- data.matrix(ntbl.RDI.mic[, 2:6])
# rownames(ntbl.mic.matrix) <- ntbl.RDI.mic$species
# 
# ntbl.mic.matrix
# 
# mydist <- function(x) dist(x, method = "euclidian")
# myhclust <- function(x) hclust(x, method = "average")
# 
# tree <- myhclust(mydist(ntbl.mic.matrix))
# plot(tree)
# rect.hclust(tree, k = 7)
```

