

library(tidyverse)
library(rotl)
library(ape)
library(stargazer)
library(cowplot)
library(stringr)
library(purrr)
library(janitor)
library(MuMIn)
library(broom)
library(forcats)
library(tidyverse)
library(xtable)
library(stargazer)
library(arm)
library(nlme)
library(cowplot)
library(visreg)

theme_set(theme_cowplot())


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
  #filter(species_name %in% c(species)) %>% 
  filter(nutrient == "ca_mg") %>% 
  # filter(subgroup == "finfish") %>% 
  rename(species1 = species) %>% 
  mutate(species1 = str_replace(species1, "Channa striatus", "Channa striata")) %>% 
  mutate(species1 = str_replace(species1, "Johnius argentatus", "Pennahia argentata")) %>% 
  mutate(species1 = str_replace(species1, "Rutilus frisii kutum", "Rutilus frisii")) %>% 
  mutate(species1 = str_replace(species1, "Salvelinus naresi", "Salvelinus alpinus alpinus")) %>% 
  mutate(species1 = str_replace(species1, "Scorpena scrofa", "Scorpaena scrofa")) %>% 
  # mutate(species1 = str_replace(species1, "Mormyrus rume", "Mormyrus rume rume")) %>% 
  group_by(species1, feeding_mode, feeding_level, subgroup) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, abs_lat) 

 calcium$species1 <- str_to_lower(calcium$species1)

 cal_taxa <- tnrs_match_names(calcium$species1, context="Animals", names = calcium$species1, do_approximate_matching = TRUE) 
 tr_cal <- tol_induced_subtree(ott_ids = ott_id(cal_taxa), label_format="name") 
 tr_bl_cal <- compute.brlen(tr_cal)
 str(tr_bl_cal)
 cal_sp <- cal_taxa$search_string
 
 cal2 <- calcium %>% 
   left_join(., cal_taxa, by = c("species1" = "search_string")) %>%  
   mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
   filter(unique_name2 %in% c(tr_bl_cal$tip.label)) %>% 
   group_by(unique_name2, feeding_mode, feeding_level) %>% 
   summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, abs_lat)
 
 rownames(cal2) <- cal2$unique_name2
 
 f_phy <- fishtree_phylogeny(species = calcium$species1)
 
 miss <- c("Salvelinus alpinus", "Salvelinus naresi")
 missing <- fishtree_phylogeny(species = miss)
 f_phy
 str(f_phy)

 f_phy <-  tr_bl_cal
 
 cal_species <-  str_replace_all(f_phy$tip.label, "_", " ")
 cal_species1 <-  str_replace_all(tr_bl_cal$tip.label, "_", " ")
 
 setdiff(cal_species, calcium$species1)
 
 calcium2 <- calcium %>% 
   filter(species1 %in% cal_species) %>% 
   as.data.frame() %>% 
   mutate(tip_labels = str_replace_all(species1, " ", "_"))
 
 calcium3 <- calcium %>% 
   filter(species1 %in% cal_species1) %>% 
   as.data.frame() %>% 
   mutate(tip_labels = str_replace_all(species1, " ", "_"))
 
setdiff(cal_species1, cal_species)
cal_species1[grepl("Isurus", cal_species1)]
cal_species[grepl("Salv", cal_species)]
calcium$species1[grepl("Liza", calcium$species1)]
 
 
rownames(calcium2) <- calcium2$tip_labels
 
calcium2$log_length <- scale(calcium2$log_length)
calcium2$abs_lat <- scale(calcium2$abs_lat)
calcium2$bulk_trophic_level <- scale(calcium2$bulk_trophic_level)
# calcium2$log_concentration <- scale(calcium2$log_concentration)
 
 mod1 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, correlation = corBrownian(phy = f_phy), data = calcium2, method = "ML")
 mod2 <- gls(log_concentration ~ log_length, correlation = corBrownian(phy = f_phy), data = calcium2, method = "ML")
 model.sel(mod1, mod2)
 
library(ggplot2)
cal2 %>% 
  ggplot(aes(x = log_length, y = log_concentration)) + geom_point() +
  geom_smooth(method = "lm")



summary(mod3)
intervals(mod3)
mod1 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod2 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod3 <- gls(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod4 <- gls(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod6 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML") 
mod7 <- gls(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
ddfer <- model.sel(mod1, mod2, mod3, mod4, mod6, mod7)
ddfer

confint(mod1)
coef(model.avg(ddfer, subset = cumsum(weight) <= .95))
summary(model.avg(ddfer, subset = cumsum(weight) <= .95))

CI_average <- rownames_to_column(as.data.frame(confint(model.avg(ddfer, subset = cumsum(weight) <= .95)), var = "term")) %>%
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`) %>% 
  rename(term = rowname)
  
summary(mod1)
summary(mod2b)
intervals(mod2)
 
 str(f_phy)
 summary(mod2)
 anova(mod2)
 intervals(mod1)
 
 library(httr)
 library(jsonlite)
 # library(rgbif)
 library(rotl)
 library(phytools)
 
 library(fishtree)
 ?fishtree
 
 
 
 


cal2 <- calcium %>% 
  left_join(., cal_taxa, by = c("species1" = "search_string")) %>% 
  mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl_cal$tip.label)) %>% 
  group_by(unique_name2, feeding_mode, feeding_level) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, abs_lat)

rownames(cal2) <- cal2$unique_name2

library(phylolm)
?phylolm

mod1 <- phylolm(log_concentration ~ log_length + bulk_trophic_level + abs_lat + feeding_mode + feeding_level, phy = tr_bl, data = cal2, boot = 1000, model = "BM")
mod1b <- phylolm(log_concentration ~ log_length, phy = tr_bl, data = cal2, boot = 1000)
mod1m_iron <- lme(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = cal2) 


summary(mod1b)
summary(mod1)
mod1b$coefficients
mod1b$bootmean
mod1b$bootconfint95
sums <- summary(mod1)
summary(mod1)
sums$coefficients
stargazer(sums$coefficients, type = "html")

cal2 %>% 
  ggplot(aes(x = log_length, y = log_concentration)) + geom_point() +
  geom_smooth(method = "lm")
lm(log_concentration ~ log_length, data = cal2) %>% tidy(conf.int = TRUE)
lm(log_concentration ~ log_length, data = cal2) %>% summary


# Iron PGLS ---------------------------------------------------------------


iron <- traits %>% 
  # filter(species != "Scorpena_scrofa") %>% 
  filter(species != "Parambassis wolffii") %>% 
  filter(species_name %in% c(species)) %>% 
  filter(nutrient == "fe_mg") %>% 
  # filter(subgroup == "finfish") %>% 
  rename(species1 = species_name) %>% 
  group_by(species1, feeding_mode, feeding_level) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, abs_lat) 

iron$species1 <- str_to_lower(iron$species1)

iron_taxa <- tnrs_match_names(iron$species1, context="Animals", names = iron$species1, do_approximate_matching = TRUE) 
tr_iron <- tol_induced_subtree(ott_ids = ott_id(iron_taxa), label_format="name") 


tr_bl_iron <- compute.brlen(tr_iron)
plot(tr_iron)

length(tr_bl_iron$tip.label)


iron2 <- iron %>% 
  left_join(., iron_taxa, by = c("species1" = "search_string")) %>% 
  mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl_iron$tip.label)) %>% 
  group_by(unique_name2, feeding_mode, feeding_level) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, abs_lat)

rownames(iron2) <- iron2$unique_name2

length(iron2$unique_name2)
mod2 <- phylolm(log_concentration ~ log_length + bulk_trophic_level + abs_lat + feeding_mode + feeding_level,
                phy = tr_bl_iron, data = iron2, boot = 1000)
mod2b <- phylolm(log_concentration ~ log_length, phy = tr_bl_iron, data = iron2, boot = 1000)
mod2c <- lm(log_concentration ~ log_length + bulk_trophic_level + abs_lat + feeding_mode + feeding_level, data = iron2)
mod2c <- lm(log_concentration ~ log_length, data = iron2)
summary(mod2b)
summary(mod2)
mod2b$bootmean
mod2b$bootconfint95
summary(mod2)
summary(mod2c)

sums$coefficients
stargazer(sums$coefficients, type = "html")

iron2 %>% 
  ggplot(aes(x = log_length, y = log_concentration)) + geom_point() +
  geom_smooth(method = "lm")
lm(log_concentration ~ log_length, data = iron2) %>% tidy(conf.int = TRUE)
  lm(log_concentration ~ log_length, data = iron2) %>% summary

  
  traits %>% 
    filter(nutrient == "fe_mg") %>% 
    # filter(subgroup == "finfish") %>% 
    # group_by(species_name) %>% 
    # summarise_each(funs(mean), log_concentration, log_length) %>% 
    lm(log_concentration ~ log_length, data = .) %>% summary
# Zinc PGLS ---------------------------------------------------------------


zinc <- traits %>% 
  # filter(species != "Scorpena_scrofa") %>% 
  filter(species != "Parambassis wolffii") %>% 
  filter(species_name %in% c(species)) %>% 
  filter(nutrient == "zn_mg") %>% 
  # filter(subgroup == "finfish") %>% 
  rename(species1 = species_name) %>% 
  group_by(species1, feeding_mode, feeding_level) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, abs_lat) 

zinc$species1 <- str_to_lower(zinc$species1)

zinc_taxa <- tnrs_match_names(zinc$species1, context="Animals", names = zinc$species1, do_approximate_matching = TRUE) 
tr_zinc <- tol_induced_subtree(ott_ids = ott_id(zinc_taxa), label_format="name") 


tr_bl_zinc <- compute.brlen(tr_zinc)
plot(tr_zinc)

length(tr_bl_zinc$tip.label)


zinc2 <- zinc %>% 
  left_join(., zinc_taxa, by = c("species1" = "search_string")) %>% 
  mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl_zinc$tip.label)) %>% 
  group_by(unique_name2, feeding_mode, feeding_level) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, abs_lat)

rownames(zinc2) <- zinc2$unique_name2

length(zinc2$unique_name2)
mod3 <- phylolm(log_concentration ~ log_length + bulk_trophic_level + abs_lat + feeding_mode + feeding_level, phy = tr_bl_zinc, data = zinc2, boot  = 1000)
mod3b <- phylolm(log_concentration ~ log_length, phy = tr_bl_zinc, data = zinc2, boot = 1000)

mod3b$bootconfint95
mod3b$bootmean

mod3$bootconfint95
mod3$bootmean
summary(mod3b)
sums$coefficients
mod3$coefficients

stargazer(mod3, type = "html", out = "tables/zinc-pgls.htm")

lm(log_concentration ~ log_length, data = zinc2) %>% tidy(conf.int = TRUE)
lm(log_concentration ~ log_length, data = zinc2) %>% summary
zinc2 %>% 
  ggplot(aes(x = log_length, y = log_concentration)) + geom_point() +
  geom_smooth(method = "lm")

traits %>% 
  filter(nutrient == "zn_mg") %>% 
  filter(subgroup == "finfish") %>% 
  group_by(species_name) %>% 
  summarise_each(funs(mean), log_concentration, log_length) %>% 
  lm(log_concentration ~ log_length, data = .) %>% summary



# DHA PGLS ---------------------------------------------------------------
unique(traits$nutrient)

dha <- traits %>% 
  # filter(species != "Scorpena_scrofa") %>% 
  filter(species != "Parambassis wolffii") %>% 
  filter(species_name %in% c(species)) %>% 
  filter(nutrient == "dha") %>% 
  filter(subgroup == "finfish") %>% 
  rename(species1 = species_name) %>% 
  mutate(species1 = str_to_lower(species1)) %>% 
  dplyr::filter(species1 != "epinephelus spp.") %>%
  group_by(species1, feeding_mode, feeding_level) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, abs_lat) %>% 
  ungroup() %>% 
  dplyr::distinct(species1, .keep_all = TRUE)


dha$species1 <- str_to_lower(dha$species1)

length(unique(dha$species1))
length(dha$species1)

dha_taxa <- tnrs_match_names(dha$species1, context="Animals", names = dha$species1, do_approximate_matching = TRUE) 
tr_dha <- tol_induced_subtree(ott_ids = ott_id(dha_taxa), label_format="name") 


tr_bl_dha <- compute.brlen(tr_dha)
plot(tr_dha)

length(tr_bl_dha$tip.label)


dha2 <- dha %>% 
  left_join(., dha_taxa, by = c("species1" = "search_string")) %>% 
  mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl_dha$tip.label)) %>% 
  group_by(unique_name2, feeding_mode, feeding_level) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, abs_lat)

rownames(dha2) <- dha2$unique_name2

length(dha2$unique_name2)
mod4 <- phylolm(log_concentration ~ log_length + bulk_trophic_level + abs_lat + feeding_mode + feeding_level, phy = tr_bl_dha, data = dha2, boot = 1000)
mod4a <- phylostep(log_concentration ~ log_length + abs_lat, phy = tr_bl_dha, data = dha2, direction = "backward")

library(nlme)
library(MuMIn)
library(arm)

?standardize
?scale

vec <- c(2, 5, 7, 19, 44, 55, 99)
vec2 <- scale(vec)

dha3 <- dha2
dha3$abs_lat <- scale(dha3$abs_lat)
dha3$log_length <- scale(dha3$log_length)
dha3$bulk_trophic_level <- scale(dha3$bulk_trophic_level)

# dha3 <- dha2 %>% 
#   # mutate(log_concentration = scale(log_concentration, scale = TRUE, center = FALSE)) %>% 
#   # mutate(log_length = scale(log_length)) %>% 
#   mutate(abs_lat = scale(abs_lat)) %>% 
#   mutate(bulk_trophic_level = scale(bulk_trophic_level))

pgls <- gls(log_concentration ~ log_length + bulk_trophic_level + abs_lat + feeding_mode + feeding_level, correlation = corBrownian(phy = tr_bl_dha),
                 data = dha3, method = "ML")
rsquared(pgls)

library(nlme)
library(MuMIn)
sum(is.na(dha3$bulk_trophic_level))

mod1 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, correlation = corBrownian(phy = tr_bl_dha), data = dha3, method = "ML")
mod2 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, correlation = corBrownian(phy = tr_bl_dha), data = dha3, method = "ML")
mod3 <- gls(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, correlation = corBrownian(phy = tr_bl_dha), data = dha3, method = "ML")
mod4 <- gls(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, correlation = corBrownian(phy = tr_bl_dha), data = dha3, method = "ML")
mod6 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level, correlation = corBrownian(phy = tr_bl_dha), data = dha3, method = "ML") 
mod7 <- gls(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, correlation = corBrownian(phy = tr_bl_dha), data = dha3, method = "ML")

ddfe <- model.sel(mod1, mod2, mod3, mod4, mod6, mod7)
ddfe
coef(mod1)
summary(mod6)
intervals(mod6)
fe_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(ddfe, subset = cumsum(weight) <= .95)), var = "term")) %>%
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`) %>% 
  rename(term = rowname)
fe_slopes_average <- enframe(coef(model.avg(ddfe, subset = cumsum(weight) <= .95)), name = "term", value = "slope")
fe_results <- left_join(fe_CI_average, fe_slopes_average, by = "term") %>% 
  mutate(nutrient = "iron")

ddfe
summary(pgls)
anova(pgls)
library(piecewiseSEM)
source("Rscripts/rsquared.R")

rsquared.gls(mod1)
library(devtools)


### need to standardize variables.

pgls1 <- gls(log_concentration ~ log_length, correlation = corBrownian(phy = tr_bl_dha),
            data = dha2, method = "ML")

AIC(pgls1, pgls)
model.avg(pgls, pgls1)
model.sel(pgls, pgls1)
summary(pgls)
anova(pgls)

summary(pgls)
mod4b <- phylolm(log_concentration ~ log_length, phy = tr_bl_dha, data = dha2, boot = 1000)
AIC(mod5)

mod5 <- lm(log_concentration ~ log_length + bulk_trophic_level + abs_lat + feeding_mode + feeding_level, data = dha2)
summary(mod4)
summary(mod5)
tidy(mod5, conf.int = TRUE)
summary(mod4)
sums$coefficients
stargazer(sums$coefficients, type = "html")

lm(log_concentration ~ log_length, data = dha2) %>% summary

# EPA PGLS ---------------------------------------------------------------
unique(traits$nutrient)

epa <- traits %>% 
  # filter(species != "Scorpena_scrofa") %>% 
  filter(species != "Parambassis wolffii") %>% 
  filter(species_name %in% c(species)) %>% 
  filter(nutrient == "epa") %>% 
  filter(subgroup == "finfish") %>% 
  rename(species1 = species_name) %>% 
  mutate(species1 = str_to_lower(species1)) %>% 
  dplyr::filter(species1 != "epinephelus spp.") %>%
  group_by(species1, feeding_mode, feeding_level) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, abs_lat) %>% 
  ungroup() %>% 
  dplyr::distinct(species1, .keep_all = TRUE)


epa$species1 <- str_to_lower(epa$species1)

length(unique(epa$species1))
length(epa$species1)

epa_taxa <- tnrs_match_names(epa$species1, context="Animals", names = epa$species1, do_approximate_matching = TRUE) 
tr_epa <- tol_induced_subtree(ott_ids = ott_id(epa_taxa), label_format="name") 


tr_bl_epa <- compute.brlen(tr_epa)
plot(tr_epa)

length(tr_bl_epa$tip.label)


epa2 <- epa %>% 
  left_join(., epa_taxa, by = c("species1" = "search_string")) %>% 
  mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl_epa$tip.label)) %>% 
  group_by(unique_name2, feeding_mode, feeding_level) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, abs_lat)

rownames(epa2) <- epa2$unique_name2

length(epa2$unique_name2)
mod6 <- phylolm(log_concentration ~ log_length + bulk_trophic_level + abs_lat + feeding_mode + feeding_level, phy = tr_bl_epa, data = epa2, boot = 1000)
mod7 <- lm(log_concentration ~ log_length + bulk_trophic_level + abs_lat + feeding_mode + feeding_level, data = epa2)
summary(mod6)
tidy(mod7, conf.int = TRUE)
R2(mod6, phy = tr_bl_epa)
?R2
summary(mod6)
sums$coefficients
stargazer(sums$coefficients, type = "html")

lm(log_concentration ~ log_length, data = epa2) %>% summary



# new figure 4 ------------------------------------------------------------
cal2$nutrient <- "Calcium"
zinc2$nutrient <- "Zinc"
iron2$nutrient <- "Iron"

all_micro <- bind_rows(cal2, zinc2, iron2)

all_micro %>% 
  ggplot(aes(x = log_length, y = log_concentration)) + geom_point(alpha = 0.7) +
  facet_wrap( ~ nutrient, scales = "free") + geom_smooth(method = "lm", color = "black") +
  ylab("ln(nutrient concentration) \n (mg/100g)") + xlab("ln(length) (cm)")
ggsave("figures/cal-iron-zinc-length.png", width = 8, height = 3.5)


### slopes

cal_mod <- gls(log_concentration ~ log_length, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")

  


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
