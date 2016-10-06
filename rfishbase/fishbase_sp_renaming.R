
# load libraries ----------------------------------------------------------

library(tidyverse)

### ntbl data cleaning

nut_dec3 <- read.csv("~/Desktop/Nutrient_databases/nut_dec3.csv", comment.char="#")
ntbl_raw <- tbl_df(nut_dec3) 
View(ntbl_raw)

ntbl <- ntbl_raw %>%
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
  select(species, taxon, max_size, max_length, TL, CA_mg, EPA_g, DHA_g,
         FE_mg, ZN_mg, HG_mcg, FAT, PROTCNT_g, lwA, lwB, Habitat, Subgroup, Abs_lat) %>% 
  filter(Subgroup == "Finfish")

###### import and clean data
ntbl <- read_csv("ntbl.csv") 
ntbl <- tbl_df(ntbl)
ntbl <- ntbl %>% filter(Subgroup == "Finfish") ## take out finfish only, since that's what in fb
nspecies <- unique(ntbl$species)
(nspecies)
#  Find matching and unmatching species in fb, I tried using validate_names(nspecies) ## really slow
fishbase ## loads the fishbase data
GenusSpecies <- unite(fishbase, GenusSpecies, Genus, Species, sep = " ")
FishSpecies <- as.factor(GenusSpecies$GenusSpecies)

ntbl.fb.species <- intersect(nspecies,FishSpecies) ## find matching species
ntbl.nfb.species <- setdiff(nspecies,FishSpecies) ## find unmatched species (there are about 71)
View(as.data.frame(ntbl.nfb.species))

## investigate on problem fish, export as csv and do some googling to find alternate names (i.e. the ones in FB for the unmatched species)
missing.fishbase.species <- as.data.frame(ntbl.nfb.species)
write_csv(missing.fishbase.species, "FB-missing-species.csv")

View(missing.fishbase.species)
View(as.data.frame(ntbl.fb.species))

### After googling, import new csv with all the re-matches

fb.names <- read_csv("ntbl.FB.renames.csv")

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

#### Andrew's fb renaming code ####



# joey_syn <- ntbl.nfb.species %>% 
#   .[1:20] %>% 
#   data_frame(joey_sp = .) %>% 
#   group_by(joey_sp) %>% 
#   do(synonyms(.$joey_sp))
# 
# syn <- synonyms(ntbl.nfb.species[1:20])
# spec.code <- syn["SpecCode"]
# spec.code
# # syn[["SpecCode"]] ## get the raw code
# glimpse(fishbase)
# 
# length(spec.code[[1]])
# View(semi_join(x = fishbase, y = spec.code))
# FishSpecies["Oncorhyncus nerka"]
