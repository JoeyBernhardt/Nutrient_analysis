

### new traits analysis
library(rfishbase)
library(tidyverse)


data <- read_csv("data-processed/nutrients-traits-for-pgls.csv") %>% 
  mutate(species_name = str_replace(species_name, "(juvenile)", "")) %>% 
  mutate(species_name = str_replace(species_name, "()", ""))

more_traits <- species(data$species1) ## contains BodyShapeI, DemersPelag, AnaCat, DepthRangeDeep
fec <- fecundity(data$species1) 
ecosys <- ecosystem(data$species1) 
mat <- maturity(data$species1) ### contains age at maturity
stocks1 <- stocks(data$species1) ###contains EnvTemp
pop <- popchar(data$species1) 
pop2 <- popqb(data$species1) ### contains K
poplw2 <- poplw(data$species1) ### c
popllf2 <- poplf(data$species1) ### c
popll <- popll(data$species1) ### c


rep1 <- reproduction(data$species1) 

more_traits %>% 
  select(BodyShapeI, DemersPelag, AnaCat, DepthRangeDeep, everything()) %>% View
  filter(is.na(BodyShapeI)) %>% 

stocks(trout$Species, fields=c("Species", "Resilience", "StockDefs"))
trout <- common_to_sci("trout")

species(common_to_sci("trout"))
thing <- species("Salmo obtusirostris") 
thing2 <- popqb("Salmo obtusirostris") 

thing %>% 
  select(contains("Demers")) %>% V

sum(grepl("Depth", names(thing)))
