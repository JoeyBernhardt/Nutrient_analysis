##Bringing in new inverts data
library(rfishbase)
library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(coefplot2)


options(FISHBASE_API = "http://fishbase.ropensci.org/sealifebase")

inverts.new <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data/aquatic_inverts_micronutrients.csv")
ntbl <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data/ntbl2.csv")
ntbl.inv <- ntbl %>% filter(Subgroup != "Finfish") ## take out inverts
table(ntbl.inv$Subgroup)

GenusSpecies <- unite(sealifebase, GenusSpecies, Genus, Species, sep = " ")
InvSpecies <- as.factor(GenusSpecies$GenusSpecies)

ntbl.inv.spp <- unique(ntbl.inv$species)
ntbl.slb.sp <- intersect(ntbl.inv.spp,InvSpecies) ## find matching species
ntbl.non.match <- setdiff(ntbl.inv.spp, InvSpecies) ## finds non matching species
ntbl.non.match <- as.data.frame(ntbl.non.match)
write_csv(ntbl.non.match, "/Users/Joey/Documents/Nutrient_Analysis/data/ntbl.inv.nonmatch.csv")

inverts.new$length.reported_cm <- as.numeric(inverts.new$length.reported_cm)

hist(inverts.new$length.reported_cm)


new.spp <- unique(inverts.new$SLB_species)

new.slb.sp <- intersect(new.spp,InvSpecies) ## find matching species
non.match <- setdiff(new.spp, InvSpecies) ## finds non matching species
non.match

new.slb.sp

slb.basic <- species(new.slb.sp)

slb.ecology <- ecology(new.slb.sp)

length(!is.na(slb.ecology$FoodTroph))

### after some googling, import the new ntbl matches

ntbl.inv.match <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data/ntbl.inv.nonmatch.csv")


#### Renaming to match slb names ####
oldvalues <- as.vector(ntbl.inv.match$ntbl.non.match)
newvalues <- as.vector(ntbl.inv.match$slb.name) 
length(oldvalues)
length(newvalues)
#   ntbl$species <- newvalues[ match(ntbl$species, oldvalues) ] ## replace old species names with new FB matches, this doesn't work
#   data  
## try with mapvalues
ntbl.inv$species <- mapvalues(ntbl.inv$species, oldvalues, newvalues) ## change names in ntbl to match FB names

length(unique(ntbl.inv$species))

write_csv(ntbl.inv, "/Users/Joey/Documents/Nutrient_Analysis/data/ntbl.inv.csv")


## in excel, I just took the rows from the new inverts file and added them to ntbl inv. Now import new version of ntbl.inv
## update March 14 2016: this is the most recent version of the ntbl.inv as downloaded from google drive
ntbl.inv <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data/ntbl.inv.csv")


#### For now, I'll just take out the rows which have spp not in sealife base ####
ntbl.inv.spp <- unique(ntbl.inv$species)
ntbl.inv.spp

ntbl.non.match <- setdiff(ntbl.inv.spp, InvSpecies) ## finds non matching species
ntbl.non.match

# ntbl.inv %>% filter (species != c(ntbl.non.match)) 

ntbl.slb.sp <- intersect(ntbl.inv.spp,InvSpecies) ## find matching species

### get the basic species info for the matching species
intbl.species <- species(ntbl.slb.sp) %>%
  rename(species = sciname)

### merge the ntbl.inv with the slb basic data ####

ntbl.basic <- inner_join(intbl.species, ntbl.inv, by = "species")
ntbl.basic <- ntbl.basic[ , !duplicated(colnames(ntbl.basic))] ## to remove duplicated Subgroup column
names(ntbl.basic)


intbl.ecology <- ecology(ntbl.slb.sp) %>% 
  rename(species = sciname) %>% 
  select(species, StockCode, Herbivory2, DietTroph, FoodTroph, EcoTroph, SpecCode)

names(intbl.ecology)

intbl.all <- inner_join(intbl.ecology, ntbl.basic, by = "species") %>% 
  select(species, Length, Weight, StockCode, Herbivory2, DietTroph, FoodTroph, EcoTroph, SpecCode.x, Food.Item.ID:max_length_study)

str(intbl.all) 

names(intbl.all)
### this file contains all the invert data (nutrients, FB ecology and FB species)
write_csv(intbl.all, "/Users/Joey/Documents/Nutrient_Analysis/data/intbl.all.csv")
intbl.all <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data/intbl.all.csv")

intbl.all$CA_mg <- as.numeric(intbl.all$CA_mg)
intbl.all$Subgroup <- as.factor(intbl.all$Subgroup)

intbl.all %>% 
  select(`CA_mg`) %>% View
unique(intbl.all$CA_mg)

str(intbl.all)


### extract, from aq.wide, the inverts that don't have length data 
  no.length <- aq.wide %>% 
    filter(Subgroup != "Finfish") %>% 
    filter(is.na(max_length)) %>% 
    select(species) %>% 
    distinct(species)
  
class(no.length)

dat <- ecology(no.length, fields = c("FoodTroph", "SpecCode"))
  
  