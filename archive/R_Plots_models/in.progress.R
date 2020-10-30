library(dplyr)
library(readr)

n.long <- read.csv("/Users/Joey/Documents/Nutrient_Analysis/data/aq.long.csv")
aq.long <- read.csv("~/Documents/Nutrient_Analysis/data/aq.long.csv")
aq.wide <- read.csv("/Users/Joey/Documents/Nutrient_Analysis/data/aq.wide.csv")

aq.long %>% 
  group_by(nutrient, Subgroup) %>% 
  table(.$Subgroup)

str(n.long)
class(aq.long$RDI)
unique(aq.long$RDI)
n.long$RDI
summary(n.long$RDI)


n.long$RDI <- as.numeric(as.character(n.long$RDI))

n.long.RDI <- n.long %>% 
  mutate(RDI.target = (concentration > (RDI/10))) %>%
  filter(!is.na(RDI.target)) %>%
  ggplot(., aes(RDI.target)) + geom_bar() + facet_wrap( ~ Subgroup)

n.long.RDI <- n.long %>% 
  dplyr::group_by(nutrient) %>% 
  mutate(RDI.target = (concentration > (RDI/10))) %>%
  filter(!is.na(RDI.target)) %>%
  group_by(species, RDI.target) %>% View()
 tally() %>% View
  

str(aq.wide)

ntbl.RDI.tot2 <- aq.wide %>% 
  group_by(species) %>% 
  summarise(mean.CA = mean(CA_mg, na.rm = TRUE),
            mean.ZN = mean(ZN_mg, na.rm = TRUE), 
            mean.FE = mean(FE_mg, na.rm = TRUE)) %>% 
  mutate(RDI.CA = ifelse(mean.CA > 300, 1, 0)) %>% 
  mutate(RDI.FE = ifelse(mean.FE > 4.5, 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(mean.ZN > 2.75, 1, 0)) %>%
  mutate(RDI.micro.tot = rowSums(.[5:7])) %>% 
  filter(!is.na(RDI.micro.tot)) %>% View

sp.subgroup <- aq.wide %>%
  dplyr::group_by(species) %>% 
  dplyr::select(Subgroup, species) %>% 
  distinct(species)

ntbl.RDI.sub2 <- dplyr::left_join(ntbl.RDI.tot2, sp.subgroup, by = "species") 


ggplot(ntbl.RDI.sub2, aes(RDI.micro.tot)) + geom_bar(binwidth = .5) + facet_wrap(~ Subgroup, scales = "free_y")  
