

require(XML)
require(RCurl)
library(devtools)
library(rfishbase)

## set rcurl timeout
# RCurl::curlSetOpt(timeout = 60)

#####
library(plyr)
suppressPackageStartupMessages(library(dplyr))
library(knitr)
library(tidyr)
library(readr)
library(rfishbase)

###### import and clean data
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


View(ntbl$species)
nspecies <- unique(ntbl$species)
str(nspecies)
str(FishSpecies)

validate_names(nspecies)

GenusSpecies <- unite(fishbase, GenusSpecies, Genus, Species, sep = " ")

FishSpecies <- as.factor(GenusSpecies$GenusSpecies)
View(nspecies)

ntbl.fb.species <- intersect(nspecies,FishSpecies)

head(ntbl.fb.species)

#### Playing around with rfishbase functions


View(species_fields("Abramis brama"))

View(stocks("Abramis brama", c("TempMin", "TempMax", "StockDefs")))
?"rfishbase"

View(diet("Oreochromis niloticus"))

getSize("Ageneiosus brevifilis")


ecology(c("Oreochromis niloticus", "Salmo trutta"),
        fields=c("SpecCode", "FoodTroph", "FoodSeTroph", "DietTroph", "DietSeTroph"))

?fooditems

View(fooditems("Oreochromis niloticus"))

tables <- docs()
# Describe the diet table
dplyr::filter(tables, table == "diet")$description
species_fields()

str(ecosystem("Oreochromis niloticus"))

species()species_fields(c("Labroides bicolor", "Bolbometopon muricatum"))
species(c("Labroides bicolor", "Bolbometopon muricatum"), fields = species_fields$Weight)
View(stocks(common_to_sci("trout")))
trout.temp <- (stocks(common_to_sci("trout"), c("TempMin", "TempMax", "StockDefs")))
trout.temp

# I thought this would be a good alternative to the stocks function to get the temp data, but it threw a bunch of errors.
# trout.temp.2 <- length_freq(common_to_sci("trout"), c("TempMin", "TempMax"))
# View(trout.temp.2)



View(length_freq("Oreochromis niloticus"))

trout.temp %>% 
  group_by(sciname) %>% 
  summarize_each_(., "mean", c("TempMin", "TempMax")) %>% 
  View()


####### Import Cambodia spp

cambodia.spp <- read_csv("cambodia.spp.csv")



ntbl.cambodia.spp <- list(c(intersect(nspecies,cambodia.spp$Species)))
cam.spp <- c("Anabas testudineus", "Anguilla bicolor", "Anguilla japonica", "Channa marulius", "Channa micropeltes", "Channa striata")

cam.spp <- c("Anabas testudineus", "Anguilla bicolor", "Anguilla japonica", "Channa marulius", "Channa micropeltes", "Channa striata", "Clarias gariepinus", "Clarias macrocephalus", "Clupeoides borneensis", "Cyclocheilichthys apogon", "Cyprinus carpio", "Henicorhynchus siamensis", "Lates calcarifer", "Lutjanus argentimaculatus", "Monopterus albus", "Mastacembelus armatus", "Nandus nandus", "Notopterus notopterus", "Oreochromis niloticus", "Parambassis wolffii", "Plotosus canius", "Puntioplites proctozystron", "Rasbora tornieri", "Sillago sihama", "Thynnichthys thynnoides", "Xenentodon cancila") 



(ntbl.cambodia.spp)

#### FD on cambodia spp

library(FD)
?FD

cam.RDI.micro <- ntbl %>% 
  filter(species %in% cam.spp) %>% 
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
  filter(!is.na(RDI.micro.tot))
 
  
cam.matrix.mic <- data.matrix(cam.RDI.micro[, 2:6])
rownames(cam.matrix.mic) <- cam.RDI.micro$species
  
  cam.FD.mic <- as.data.frame(dbFD(cam.matrix.mic))
 (cam.FD.mic)


#### USA species

USA.spp <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data/USAspp.csv")

ntbl.USA.spp <- c(intersect(nspecies,USA.spp$Species))
str(ntbl.USA.spp)
class(ntbl.USA.spp)

USA.RDI.micro <- ntbl %>% 
  filter(species %in% ntbl.USA.spp) %>% 
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
  filter(!is.na(RDI.micro.tot))


USA.matrix.mic <- data.matrix(USA.RDI.micro[, 2:6])
rownames(USA.matrix.mic) <- USA.RDI.micro$species
View(USA.RDI.micro)


USA.FD.mic <- as.data.frame(dbFD(USA.matrix.mic))
(USA.FD.mic)
  
# 
# > (USA.FD.mic)
# nbsp sing.sp     FRic qual.FRic      FEve      FDiv     FDis     RaoQ CWM.mean.CA
# Community1   29      29 7.179277         1 0.4502677 0.6243078 1.468984 4.827586    61.02158
# CWM.mean.EPA CWM.mean.DHA CWM.mean.ZN CWM.mean.FE
# Community1    0.1521608    0.2846802   0.7996209   0.7374655
# > 
##### FD on all species in ntbl
 
  
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
    filter(!is.na(RDI.micro.tot))

  ntbl.matrix.mic <- data.matrix(ntbl.RDI.mic[, 2:6])
  rownames(ntbl.matrix.mic) <- ntbl.RDI.mic$species
  
  FD.mic <- as.data.frame(dbFD(ntbl.matrix.mic))
(FD.mic)
  
  
  #### How does FDis differ between the entire dataset and just the Cambodian species?
# # > (FD.mic)
#   nbsp sing.sp     FRic qual.FRic      FEve      FDiv     FDis     RaoQ CWM.mean.CA
#   Community1   55      55 54.39447         1 0.4758244 0.6539231 1.495851 4.909091     137.922
#   CWM.mean.EPA CWM.mean.DHA CWM.mean.ZN CWM.mean.FE
#   Community1    0.1287276    0.2247737    1.466092    2.481542
#   > 
    
#   > (cam.FD.mic)
#   nbsp sing.sp     FRic qual.FRic      FEve      FDiv     FDis RaoQ CWM.mean.CA
#   Community1    4       4 1.300584         1 0.8004751 0.9047908 1.920415 3.75    458.3708
#   CWM.mean.EPA CWM.mean.DHA CWM.mean.ZN CWM.mean.FE
#   Community1   0.04803744    0.1268583    4.125948    1.698333
#   > 
  
# 
# ntbl.mac.matrix <- data.matrix(ntbl.RDI.mac[, 2:3])
# rownames(ntbl.mac.matrix) <- ntbl.RDI.mac$species
# 
# FD.mac <- dbFD(ntbl.mac.matrix)
# (as.data.frame(FD.mac))
# (as.data.frame(FD.mic))