

library(tidyverse)
library(rotl)
library(ape)
library(stargazer)


fish_species <- read_csv("data-processed/fish_species.csv")
traits_analysis_raw <- read_csv("data-processed/traits_for_analysis.csv") %>% 
  filter(subgroup == "finfish")

traits <- traits_analysis_raw %>% 
  mutate(species = species_name) %>% 
  mutate(species = str_replace(species, "(juvenile)", "")) %>% 
  # mutate(species = str_replace(species, " ", "")) %>%
  mutate(species = str_replace(species, "()", "")) %>%
 mutate(species = ifelse(species == "Pangasianodon hypophthalmus ()", "Pangasianodon hypophthalmus", species)) %>% 
  mutate(species = ifelse(species == "Scomber japonicus/colias", "Scomber japonicus", species)) %>% 
  mutate(species = ifelse(species == "Tenualosa ilisha ()", "Tenualosa ilisha", species)) %>% 
  mutate(species = ifelse(species == "Oreochromis niloticus ()", "Oreochromis niloticus", species)) %>% 
  mutate(species = str_replace(species, "Travin, 1951_", "")) %>% 
  mutate(species = ifelse(species == "Prochilodus reticulatus magdalenae", "Prochilodus reticulatus", species)) 
  
  fish_species_sub <- traits %>% 
  distinct(species) %>% 
  top_n(n = 200) %>% 
  filter(species != "Parambassis wolffii")

species <- fish_species_sub$species

taxa <- tnrs_match_names(species, context="Animals", names = species, do_approximate_matching = TRUE) 
# taxa_sub <- taxa[taxa$ott_id %in% c(ti$`taxa$ott_id`), ]

str(taxa)
class(taxa)

taxa2 <- taxa %>% 
  filter(ott_id == 374316)

  
# taxa3 <- tnrs_match_names(taxa2$search_string, context="Animals", names = taxa2$unique_name, do_approximate_matching = TRUE) 

tr <- tol_induced_subtree(ott_ids = ott_id(taxa), label_format="name")

library(phylolm)
library(fuzzyjoin)

calcium <- traits %>% 
  # filter(species != "Scorpena_scrofa") %>% 
  filter(species != "Parambassis wolffii") %>% 
  filter(species_name %in% c(species)) %>% 
  filter(nutrient == "ca_mg") %>% 
  filter(subgroup == "finfish") %>% 
  rename(species1 = species_name) %>% 
  group_by(species1, feeding_mode, feeding_level) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, abs_lat) 

 calcium$species1 <- str_to_lower(calcium$species1)

 cal_taxa <- tnrs_match_names(calcium$species1, context="Animals", names = calcium$species1, do_approximate_matching = TRUE) 
 tr_cal <- tol_induced_subtree(ott_ids = ott_id(cal_taxa), label_format="name") 

species <- unique(calcium$species)
species <- unique(things$species)

tr_bl <- compute.brlen(tr_cal)
plot(tr)

length(tr_bl$tip.label)
cal_taxa

cal2 <- calcium %>% 
  left_join(., cal_taxa, by = c("species1" = "search_string")) %>% 
  mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl$tip.label)) %>% 
  group_by(unique_name2, feeding_mode, feeding_level) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, abs_lat)

rownames(cal2) <- cal2$unique_name2


mod1 <- phylolm(log_concentration ~ log_length + bulk_trophic_level + abs_lat + feeding_mode + feeding_level, phy = tr_bl, data = cal2)
sums <- summary(mod1)
sums$coefficients
stargazer(sums$coefficients, type = "html")


# Extra crap --------------------------------------------------------------


setdiff(cal2$unique_name2, tr_bl$tip.label)
length(unique(cal2$unique_name2))

tr_species <- data.frame(species = tr$tip.label) %>% 
  mutate(species2 = species) %>% 
  separate(species2, into = c("speciesa", "speciesb", "speciesc", sep = "_")) %>% 
  mutate(speciesc = str_replace(speciesc, "species", "")) %>% 
  unite(species, speciesa, speciesb) %>% 
  mutate(species = str_replace(species, "Sprattus_sprattus", "Sprattus_sprattus_sprattus")) %>% 
  mutate(species = str_replace(species, "_", " ")) %>% 
  dplyr::select(species) 
  
length(taxa$search_string)

calcium %>% View

names(things) <- rownames()
tr_species$species <- str_to_lower(tr_species$species)

calcium2 <- calcium %>% 
  filter(species1 %in% c(tr_species$species))

calcium2 <- calcium[calcium$species1 %in% c(tr_species$species), ]

calcium3 <- calcium2 %>% 
  group_by(species1) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, abs_lat) 

tr_bl <- compute.brlen(tr)
tr_bl$tip.label

length(unique(calcium3$species1))

calcium <- traits %>% 
  filter(species_name %in% c(species)) %>% 
  filter(nutrient == "ca_mg") %>% 
  filter(subgroup == "finfish") %>% 
  rename(species1 = species_name) 

calcium$species1 <- str_to_lower(calcium$species1)

things <- stringdist_inner_join(calcium, phy_species, by = c("species" = "species")) %>% 
  rename(species = species.x) %>% 
  mutate(species = str_replace(species, " ", "_")) %>% 
  filter(species != "Scorpena_scrofa") %>% 
  filter(species %in% c(tr_bl$tip.label)) %>% 
  distinct(species, .keep_all = TRUE) 

rownames(things) <- things$species

mod1 <- phylolm(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, phy = tr_bl, data = things)
summary(mod1)

R2(mod1, phy = tr_bl)
library(broom)
library(visreg)
library(rr2)


mod1

visreg(mod1, "log_length")

phy_species <- data.frame(species = tr_bl$tip.label)

things <- stringdist_inner_join(calcium, phy_species, by = c("species" = "species")) %>% 
  rename(species = species.x) %>% 
  mutate(species = str_replace(species, " ", "_")) %>% 
  filter(species != "Scorpena_scrofa") %>% 
  filter(species %in% c(tr_bl$tip.label)) %>% 
  distinct(species, .keep_all = TRUE) 

rownames(things) <- things$species

length(unique(things$species))
length(unique(tr_bl$tip.label))

setdiff(things$species,tr_bl$tip.label)

?fuzzy_left_join

?phylolm

plot(tr)

class(tr)
class(tr_bl)
str(tr_bl)
tr_bl <- compute.brlen(tr)
tr_bl$tip.label
calcium$species

tr_bl$tip.label
tr_bl$node.label
tr_bl$edge.length

?compute.brlen
