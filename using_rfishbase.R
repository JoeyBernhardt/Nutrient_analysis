
library(devtools)
install_github("rfishbase", "ropensci")
rm(list=ls())
require(rfishbase)
require(XML)
require(RCurl)


library(devtools)
install_github("rfishbase", "ropensci")

install.packages("rfishbase", 
                 repos = c("http://packages.ropensci.org", "http://cran.rstudio.com"), 
                 type="source")
library(rfishbase)
## set rcurl timeout
# RCurl::curlSetOpt(timeout = 60)

httr::set_config(httr::timeout(seconds = 80))
species(c("Labroides bicolor", "Bolbometopon muricatum"))


options(FISHBASE_API = "http://fishbase.ropensci.org")
common_names(c("Labroides bicolor", "Bolbometopon muricatum"))

labrids <- species_list(Family="Labridae")
(labrids)

fish <- species_list(Genus = "Labroides")
fish
species(fish[1:2])
?curl
RCurl::listCurlOptions()

dat <- species(fish, fields=c("SpecCode", "PriceCateg", "Vulnerability"))
dat

list_fields("Resilience")


#####
library(plyr)
suppressPackageStartupMessages(library(dplyr))
library(knitr)
library(tidyr)
library(readr)
library(rfishbase)
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

#"Osteochilus hasselti" 
species_list(Genus = "Osteochilus")

GenusSpecies <- unite(fishbase, GenusSpecies, Genus, Species, sep = " ")

FishSpecies <- as.factor(GenusSpecies$GenusSpecies)
View(nspecies)

setdiff(nspecies,FishSpecies)
