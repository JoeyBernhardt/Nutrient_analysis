## Checking out rfishbase
## November 2015


#### Load packages ####
library(devtools)
library(rfishbase)
library(ggplot2)
suppressPackageStartupMessages(library(dplyr))
library(plyr)
library(knitr)
library(tidyr)
library(readr)
library(ggthemes)

#### import and clean data ####
ntbl <- read_csv("ntbl.csv") 
ntbl <- tbl_df(ntbl)
ntbl <- ntbl %>% filter(Subgroup == "Finfish") ## take out finfish only, since that's what in fb
length(unique(ntbl$species))
nspecies <- unique(ntbl$species)
(nspecies)
#  Find matching and unmatching species in fb, I tried using validate_names(nspecies) ## really slow
fishbase ## loads the fishbase data
GenusSpecies <- unite(fishbase, GenusSpecies, Genus, Species, sep = " ")
FishSpecies <- as.factor(GenusSpecies$GenusSpecies)

#### FB species matching ####
ntbl.fb.species <- intersect(nspecies,FishSpecies) ## find matching species
ntbl.nfb.species <- setdiff(nspecies,FishSpecies) ## find unmatched species (there are about 71)
View(as.data.frame(ntbl.nfb.species))

## investigate on problem fish, export as csv and do some googling to find alternate names (i.e. the ones in FB for the unmatched species)
missing.fishbase.species <- as.data.frame(ntbl.nfb.species)
write_csv(missing.fishbase.species, "FB-missing-species.csv")

View(missing.fishbase.species)
View(as.data.frame(ntbl.fb.species))

### After googling, import new csv with all the re-matches ####

fb.names <- read_csv("ntbl.FB.renames.csv")
View(fb.names)

#### Renaming to match fb names ####
  oldvalues <- as.vector(fb.names$ntbl.nfb.species)
  newvalues <- as.vector(fb.names$fishbase.names) 
  length(oldvalues)
  length(newvalues)
#   ntbl$species <- newvalues[ match(ntbl$species, oldvalues) ] ## replace old species names with new FB matches, this doesn't work
#   data  
  ## try with mapvalues
  ntbl$species <- mapvalues(ntbl$species, oldvalues, newvalues) ## change names in ntbl to match FB names
  View(as.data.frame(ntbl))
  write_csv(ntbl, "ntbl_FBrenames.csv")
  
  #### import new fb matches ####
  ntbl.fb <- read_csv("ntbl_FBrenames.csv") ## this is ntbl with names matched to fb
  View(ntbl.fb)
length(unique(ntbl.fb$species))
table(ntbl.fb$Subgroup)

#### Use stocks function to get the temp min and temp max ####
nspecies <- unique(ntbl.fb$species)

length(nspecies)
temps.fb <- stocks(nspecies, c("TempMin", "TempMax", "StockDefs"))
write_csv(temps.fb, "temps.fb.csv")

temps.fb <- read_csv("temps.fb.csv")

str(temps.fb)

mintemp.fb <- temps.fb %>% 
  filter(!is.na(TempMin)) %>%
  # dplyr::rename(species = sciname) %>% 
  mutate(species = as.factor(species))

str(mintemp.fb)
str(ntbl)

ntbl.temps <- inner_join(ntbl.fb, mintemp.fb, by = "species")
View(ntbl.temps)

library(broom)
EPA.temp <- ntbl.temps %>% 
  group_by(species) %>% 
  # lm(log(EPA_g) ~ TempMin, data = .) %>% 
  # tidy(., conf.int = TRUE) %>% 
  ggplot(., aes(x = TempMax, y=log(EPA_g), geom = "point"))


EPA.temp <- lm(log(EPA_g) ~ TempMin, data = ntbl.temps)
summary(EPA.temp)

library(visreg)
library(ggplot2)
visreg(EPA.temp, xvar = "TempMin")

p <- ggplot(ntbl.temps, aes(x = TempMax, y=log(EPA_g)))
p + stat_summary(aes(y = log(EPA_g)), fun.y=mean, geom = "point", color = species) +
  geom_hline(aes(yintercept=log(0.5))) +
  stat_smooth(method = "lm") +
  theme_pander() +
  xlab("TempMin") + 
  ylab("log EPA content, g/100g portion")

#### weird, I seem to get getting results for the fatty acids that are opposite from what I would have expected!


#### Explore diet data ####
View(diet("Oreochromis niloticus"))

ntbl.diet <- ecology(nspecies,
        fields=c("SpecCode", "FoodTroph", "FoodSeTroph", "DietTroph", "DietSeTroph"))
View(ntbl.diet)

ntbl.diet <- ntbl.diet %>% 
  dplyr::rename(species = sciname) %>% 
  mutate(species = as.factor(species))

ndiet <- inner_join(ntbl.fb, ntbl.diet, by = "species")
View(ndiet)
write.csv(ndiet, file = "ntbl.diet.csv")

ndiet <- read_csv("ntbl.diet.csv")


diet <- diet %>% filter(HG_mcg > 1) 
  
hg.diet <- lm(HG_mcg ~ FoodTroph, data = diet)
summary(hg.diet)

hg.diet <- diet %>% 
  group_by(species) %>% 
  lm(HG_mcg ~ FoodTroph, data = .) %>% 
  tidy(., conf.int = TRUE) %>% View

visreg(hg.diet, xvar = "FoodTroph")

  ggplot(diet, aes(x = FoodTroph, y= HG_mcg)) + geom_point()

p <- ggplot(diet, aes(x = FoodTroph, y= HG_mcg)) +geom_point()
p + stat_summary(aes(y = HG_mcg, fun.y= mean, geom = "point", color = species)) +
  # geom_hline(aes(yintercept=log(0.5))) +
  stat_smooth(method = "lm") +
  theme_pander() +
  xlab("FoodTroph") + 
  ylab("log HG content, mg/100g portion")



tables <- docs()
# Describe the diet table
dplyr::filter(tables, table == "diet")$description

##### Here I pull out the 'ecology' tables for all the ntbl species in fb. ####
ecology.ntbl <- ecology(nspecies)
ecology("Salmo trutta")
write.csv(ecology.ntbl, file = "FB.ecology.csv")

str(ecology.ntbl)

### Rename the sciname to species in fd ecology data ####
ecology.fb <- ecology.ntbl %>% 
  dplyr::rename(species = sciname) %>% 
  mutate(species = as.factor(species))

str(ecology.fb)
str(ntbl)

#### join the ecology data from fb and ntbl ####
ecn <- inner_join(ntbl.fb, ecology.fb, by = "species")
View(ecn)
write.csv(ecn, file = "ntbl.ecology.csv")
n.ecology <- read_csv("ntbl.ecology.csv")
?rfishbase

#### Herbivory 2 ####
n.ecology$Herbivory2 <- as.factor(n.ecology$Herbivory2)
levels(n.ecology$Herbivory2)
summary(n.ecology$Herbivory2)
table(n.ecology$Herbivory2)


hg.herb <- lm(HG_mcg ~ Herbivory2, data = ecn)
summary(hg.herb)


##### FeedingType ####
str(ecn$FeedingType)
ecn$FeedingType <- as.factor(ecn$FeedingType)
summary(ecn$FeedingType)
ca.feed <- lm(log(CA_mg) ~ FeedingType, data = ecn)
summary(ca.feed)


##### DietTroph
summary(ecology.ntbl$DietTroph)
hist(ecology.ntbl$DietTroph)

hg.diet <- lm(HG_mcg ~ DietTroph, data = ecn)
summary(hg.diet)

visreg(hg.diet, xvar = "DietTroph")

##### FoodTroph
summary(ecn$FoodTroph)
hist(ecn$FoodTroph)

p <- ggplot(ecn, aes(x = FoodTroph, y= HG_mcg))
p + stat_summary(aes(y = HG_mcg, fun.y= mean, geom = "point", color = species)) +
  # geom_hline(aes(yintercept=log(0.5))) +
  stat_smooth(method = "lm") +
  theme_pander() +
  xlab("FoodTrop") + 
  ylab("log HG content, mg/100g portion")


p <- ggplot(ecn, aes(x = FoodTroph, y=log(HG_mcg)))
p + stat_summary(aes(y = log(HG_mcg)), fun.y=mean, geom = "point", color = species)
  + # geom_hline(aes(yintercept=log(0.5))) +
  + stat_smooth(method = "lm")
  + theme_pander()
  + xlab("FoodTrop")
  + ylab("log HG content, mg/100g portion")

#### body size and basic FB trait data ####
info <-species(nspecies)
View(info)

info.fb <- info %>% 
  dplyr::rename(species = sciname) %>% 
  mutate(species = as.factor(species))

#### join the ecology data from fb and ntbl ####
fb.basic <- inner_join(ntbl.fb, info.fb, by = "species")
write_csv(fb.basic, "fb_basic_traits.csv") ### save basic trait data 
summary((fb.basic$max_length))

####combine diet data with basic trait data ####
fb.traits <- inner_join(fb.basic, ntbl.diet, by = "species")
fb.all <- inner_join(fb.traits, ecology.fb, by = "species")

write_csv(fb.all, "fb.all.csv")

fb.all <- read_csv("fb.all.csv")

names(fb.all)

CA.weight <- lm(log(CA_mg) ~ Weight, data = fb.basic)
summary(CA.weight)


#### SeaLifeBase ####
options(FISHBASE_API = "http://fishbase.ropensci.org/sealifebase")
View(sealifebase)
kingcrab <- common_to_sci("king crab")
kingcrab
species("Mytilus edulis")
ecology(kingcrab)
options(FISHBASE_API = "http://fishbase.ropensci.org")
ecology("Salmo trutta")

data(sealifebase)
sealifebase
#### models to test trait associations ####
table(fb.all$Herbivory2)

CA.size <- lm(log(CA_mg) ~ Length, data = fb.basic)
CA.size <- lm(log(CA_mg) ~ max_size + DemersPelag + FoodTroph.x + Herbivory2 + taxon + Abs_lat, data = fb.all)
tidy.fits.lm <- (tidy(CA.size, conf.int = TRUE)) %>% View()
summary(CA.size)



summary(fb.basic$DemersPelag)

fb.basic$DemersPelag <- as.factor(fb.basic$DemersPelag)

summary(fb.basic$Length)

CA.size <- ntbl %>% 
  group_by(species) %>% 
  lm(log(CA_mg) ~ max_size, data = .) %>% 
  tidy(., conf.int = TRUE) %>% View
summary(CA.size)

summary(ecn$HG_mcg)
#### fishbase species summary ####




View(species("Oreochromis niloticus"))

# I thought this would be a good alternative to the stocks function to get the temp data, but it threw a bunch of errors.
# trout.temp.2 <- length_freq(common_to_sci("trout"), c("TempMin", "TempMax"))
# View(trout.temp.2)


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
View(USA.FD.mic)
  
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