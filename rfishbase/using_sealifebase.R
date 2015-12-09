##Bringing in new inverts data
library(rfishbase)
options(FISHBASE_API = "http://fishbase.ropensci.org/sealifebase")

inverts.new <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data/aquatic_inverts_micronutrients.csv")

inverts.new$length.reported_cm <- as.numeric(inverts.new$length.reported_cm)

hist(inverts.new$length.reported_cm)

GenusSpecies <- unite(sealifebase, GenusSpecies, Genus, Species, sep = " ")
InvSpecies <- as.factor(GenusSpecies$GenusSpecies)

new.spp <- unique(inverts.new$species)

new.slb.sp <- intersect(new.spp,InvSpecies) ## find matching species
non.match <- setdiff(new.spp, InvSpecies) ## finds non matching species
non.match
validate_names('Meretrix lusoria')

chinese <- common_to_sci("Chinese mitten crab")
chinese
