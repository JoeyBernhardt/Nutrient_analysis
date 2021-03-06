
library(piecewiseSEM)
library(tidyverse)
library(rotl)
library(ape)
library(stargazer)
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

traits_analysis_raw <- read_csv("data-processed/traits_for_analysis.csv") 

traits <- traits_analysis_raw %>% 
  # filter(subgroup == "finfish") %>% 
  mutate(species = species_name) %>% 
  mutate(species = str_replace(species, "(juvenile)", "")) %>% 
  # mutate(species = str_replace(species, " ", "")) %>%
  mutate(species = str_replace(species, "()", "")) %>%
  mutate(species = ifelse(species == "Pangasianodon hypophthalmus ()", "Pangasianodon hypophthalmus", species)) %>% 
  mutate(species = ifelse(species == "Scomber japonicus/colias", "Scomber japonicus", species)) %>% 
  mutate(species = ifelse(species == "Tenualosa ilisha ()", "Tenualosa ilisha", species)) %>% 
  mutate(species = ifelse(species == "Oreochromis niloticus ()", "Oreochromis niloticus", species)) %>% 
  mutate(species = str_replace(species, "Travin, 1951_", "")) %>% 
  mutate(species = ifelse(species == "Prochilodus reticulatus magdalenae", "Prochilodus reticulatus", species)) %>% 
  rename(species1 = species) %>% 
  mutate(species1 = str_replace(species1, "Channa striatus", "Channa striata")) %>% 
  mutate(species1 = str_replace(species1, "Johnius argentatus", "Pennahia argentata")) %>% 
  mutate(species1 = str_replace(species1, "Rutilus frisii kutum", "Rutilus frisii")) %>% 
  mutate(species1 = str_replace(species1, "Salvelinus naresi", "Salvelinus alpinus alpinus")) %>% 
  mutate(species1 = str_replace(species1, "Scorpena scrofa", "Scorpaena scrofa")) %>% 
  filter(species1 != "Parambassis wolffii")

write_csv(traits, "data-processed/nutrients-traits-for-pgls.csv")


# calcium models ----------------------------------------------------------
traits <- read_csv("data-processed/nutrients-traits-for-pgls.csv") %>% 
  mutate(species_name = str_replace(species_name, "(juvenile)", "")) %>% 
  mutate(species_name = str_replace(species_name, "()", ""))

traits <- read_csv("data-processed/more_traits-finfish.csv")
traits <- read_csv("data-processed/all-traits-nuts.csv") %>% 
  rename(species1 = Species) %>% 
  rename(bulk_trophic_level = FoodTroph) %>% 
  rename(feeding_level = Herbivory2) %>% 
  rename(feeding_mode = FeedingType) %>% 
  mutate(log_concentration = log(concentration)) %>% 
  mutate(log_length = log(Length))
  ### update April 14 2020

calcium <- traits %>% 
  filter(nutrient == "ca_mg") %>% 
  filter(!is.na(concentration)) %>% 
  filter(!grepl("spp", species1)) %>% 
  filter(!species1 %in% c("Pleuronectinae", "Petromyzontinae", "Ensis directus", "Osmerus mordax")) %>% 
  filter(part != "unknown") %>% 
  group_by(species1, feeding_mode, feeding_level, EnvTemp, DemersPelag, BodyShapeI, part) %>%
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, DepthRangeDeep, AgeMatMin) %>% 
  ungroup() %>% 
  distinct(species1, .keep_all = TRUE) %>%
  filter(complete.cases(.))
calcium$species1 <- str_to_lower(calcium$species1)

length(unique(calcium$species1))

cal_taxa <- tnrs_match_names(calcium$species1, context="Animals", names = calcium$species1, do_approximate_matching = TRUE) 
tr_cal <- tol_induced_subtree(ott_ids = ott_id(cal_taxa), label_format="name") 
tr_bl_cal <- compute.brlen(tr_cal)

library(MuMIn)
full_mod <- lm(log_concentration ~ log_length + bulk_trophic_level + feeding_mode  + EnvTemp + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI, data = calcium)

library(phytools)
library(MCMCglmm)
summary(full_mod)
bio1_mod <- MCMCglmm(log_concentration ~ log_length + bulk_trophic_level + feeding_mode  + EnvTemp + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + part, random = ~ species1, data=cal2, pedigree=tr_bl_cal, nitt=20000)
?MCMCglmm

is.ultrametric(tr_bl_cal)
is.rooted(tr_bl_cal)
any(duplicated(tr_bl_cal$node.label)) 

mod1 <- lm(log_concentration ~ log_length + bulk_trophic_level + feeding_mode  + EnvTemp + DemersPelag + DepthRangeDeep + AgeMatMin, data = calcium)
mod2 <- lm(log_concentration ~ log_length + bulk_trophic_level + feeding_mode  + EnvTemp + DemersPelag + DepthRangeDeep + BodyShapeI, data = calcium)
mod3 <- lm(log_concentration ~ log_length + bulk_trophic_level + feeding_mode  + EnvTemp + DemersPelag + AgeMatMin + BodyShapeI, data = calcium)
mod4 <- lm(log_concentration ~ log_length + bulk_trophic_level + feeding_mode  + EnvTemp + DepthRangeDeep + AgeMatMin + BodyShapeI, data = calcium)
mod5 <- lm(log_concentration ~ log_length + bulk_trophic_level + feeding_mode  + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI, data = calcium)
mod6 <- lm(log_concentration ~ log_length + bulk_trophic_level  + EnvTemp + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI, data = calcium)
mod7 <- lm(log_concentration ~ log_length + feeding_mode  + EnvTemp + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI, data = calcium)
mod8 <- lm(log_concentration ~ log_length, data = calcium)

model.sel(full_mod, mod1, mod2, mod3, mod4, mod5, mod6, mod7) %>% View


mod1 <- lm(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + feeding_level + EnvTemp + DemersPelag + BodyShapeI, data = calcium)
model.sel(full_mod, mod1) %>% View
summary(mod1)
summary(full_mod)

### how do small fish compare to one another?

CINE_rename <- read_csv("data-processed/CINE-fish-nutrients-processed.csv")

cine_names <- unique(CINE_rename$latin_name)
(cine_names)

full_mod <- lm(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + feeding_level + abs_lat + EnvTemp + DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI, data = calcium)

calcium %>% View
  ggplot(aes(x = log_length, y = log_concentration)) + geom_point() + geom_smooth(method = "lm")


  tr_bl_cal$tip.label

 thing <-  calcium %>% 
    left_join(., cal_taxa, by = c("species1" = "search_string")) %>% 
    mutate(unique_name2 = str_replace_all(unique_name, " ", "_"))
 
 setdiff(thing$unique_name2, tr_bl_cal$tip.label)
  
  
cal2 <- calcium %>% 
  left_join(., cal_taxa, by = c("species1" = "search_string")) %>% 
  mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl_cal$tip.label)) %>% 
  ungroup() %>% 
  mutate(feeding_level = as.factor(feeding_level)) %>% 
  mutate(feeding_mode = as.factor(feeding_mode)) %>% 
  mutate(DemersPelag = as.factor(DemersPelag)) %>% 
  mutate(BodyShapeI = as.factor(BodyShapeI)) %>% 
  mutate(EnvTemp = as.factor(EnvTemp))
cal2$log_length <- scale(cal2$log_length)
# cal2$abs_lat <- scale(cal2$abs_lat)
cal2$bulk_trophic_level <- scale(cal2$bulk_trophic_level)
cal2$DepthRangeDeep <- scale(cal2$DepthRangeDeep)
cal2$AgeMatMin <- scale(cal2$AgeMatMin)

rownames(cal2) <- cal2$unique_name2

str(cal2)
# models to compare -------------------------------------------------------
library(visreg)


cal_mod <- gls(log_concentration ~ log_length, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
summary(cal_mod)
rsquared(cal_mod)
confint(cal_mod)
coef(cal_mod)

visreg(cal_mod)
visreg(cal_mod, "log_length")

calcium_points <- cal2
cal_plot2 <- calcium_points %>% 
  ggplot(aes(x = log_length, y = log_concentration)) + geom_point() +
  geom_smooth(method = "lm", color = "black")

cal3 <- cal2 %>% 
  filter(complete.cases(.))    
mod1 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + feeding_level + AgeMatMin + BodyShapeI + DemersPelag, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod2 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + feeding_level + AgeMatMin + DemersPelag, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod3 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + feeding_level + BodyShapeI + DemersPelag, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod4 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + BodyShapeI + DemersPelag + part, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod5 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + DemersPelag + part, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")

model.sel(mod1, mod2, mod3, mod4, mod5, extra = "rsquared") %>% View

summary(mod5)
anova(mod5)

mod1 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod2 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod2b <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level + DemersPelag, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")

mod3 <- gls(log_concentration ~ log_length + feeding_level + feeding_mode, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod4 <- gls(log_concentration ~ bulk_trophic_level + feeding_mode, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod5 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + feeding_level, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod6 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML") 
mod7 <- gls(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
ddfer <- model.sel(mod1, mod1b, mod2, mod3, mod4, mod5, mod6, mod7, mod2b, extra = "rsquared") %>% 
  dplyr::select(-"rsquared.Response") %>% 
  dplyr::select(-"rsquared.family") %>% 
  dplyr::select(-"rsquared.link") %>% 
  dplyr::select(-"rsquared.method")

ddfer <- model.sel(mod1, mod1b, extra = "rsquared") %>% 
  dplyr::select(-"rsquared.Response") %>% 
  dplyr::select(-"rsquared.family") %>% 
  dplyr::select(-"rsquared.link") %>% 
  dplyr::select(-"rsquared.method")

summary(mod1b)
confint(mod1b)
summary(mod1)
visreg(mod1b)
summary(mod2)
rsquared(mod1b)
rsquared(mod1)
confint(mod4)
coef(mod1)


dd_wide <- ddfer %>% 
  mutate(weight = round(weight, digits = 2)) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() 


tab1 <- tableHTML(ddfer, round = 2)
write_tableHTML(tab1, file = "tables/model-sel-calcium.htm")

ddfer
rsquared(mod6) 

stargazer(ddfer, type = "html", out="tables/model-sel-calcium.htm")
stargazer(mod1, mod3, mod4, mod7, mod5, mod2, mod6, title="", type = "html",
          align=TRUE, dep.var.labels= "Calcium concentration (mg/100g)",
          ci=TRUE, ci.level=0.95, 
          single.row=FALSE, digits = 2, dep.var.caption = "", out="tables/calcium-models-2.htm",
          add.lines = list(c("delta",  "0.00",  "0.39",  "0.89",
                             "2.45",  "2.79", "12.96", "20.38"),
                           c("weight", "0.33", "0.27", "0.21", "0.098", "0.083", "0.001", "0.00"),
                           c("R2", "0.28", "0.27", "0.30", "0.28", "0.28", "0.22", "0.074")))


avg_mod <- model.avg(ddfer, subset = cumsum(weight) <= .95)

CI_average <- rownames_to_column(as.data.frame(confint(model.avg(ddfer, subset = cumsum(weight) <= .95)), var = "term")) %>%
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`) %>% 
  rename(term = rowname)

slopes_average <- enframe(coef(model.avg(ddfer, subset = cumsum(weight) <= .95)), name = "term", value = "slope") 

results <- left_join(CI_average, slopes_average, by = "term") %>% 
  dplyr::select(term, slope, conf_low, conf_high) %>% 
  mutate(term = factor(term, levels = c("log_length", "bulk_trophic_level", 
                                        "feeding_modefiltering plankton",
                                        "feeding_modegrazing on aquatic plants",
                                        "feeding_modehunting macrofauna (predator)",
                                        "feeding_modeselective plankton feeding",
                                        "feeding_modevariable",
                                        "feeding_levelmainly plants/detritus (troph. 2-2.19)",
                                        "feeding_levelplants/detritus+animals (troph. 2.2-2.79)",
                                        "abs_lat",
                                       "(Intercept)" 
                                        ))) %>% 
  dplyr::arrange((term))



res_tab1 <- tableHTML(results, round = 2)
write_tableHTML(res_tab1, file = "tables/model-avg-calcium.htm")




# iron models -------------------------------------------------------------

calcium <- traits %>% 
  filter(nutrient == "fe_mg") %>% 
  group_by(species1, feeding_mode, feeding_level, subgroup) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, abs_lat)
calcium$species1 <- str_to_lower(calcium$species1)


cal_taxa <- tnrs_match_names(calcium$species1, context="Animals", names = calcium$species1, do_approximate_matching = TRUE) 
tr_cal <- tol_induced_subtree(ott_ids = ott_id(cal_taxa), label_format="name") 
tr_bl_cal <- compute.brlen(tr_cal)

cal2 <- calcium %>% 
  ungroup() %>% 
  left_join(., cal_taxa, by = c("species1" = "search_string")) %>%
  mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl_cal$tip.label)) %>% 
  mutate(feeding_level = as.factor(feeding_level)) %>% 
  mutate(feeding_mode = as.factor(feeding_mode))

iron2 <- cal2
cal2$log_length <- scale(cal2$log_length)
cal2$abs_lat <- scale(cal2$abs_lat)
cal2$bulk_trophic_level <- scale(cal2$bulk_trophic_level)

rownames(cal2) <- cal2$unique_name2
# models to compare -------------------------------------------------------

mod1 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod2 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod3 <- gls(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod4 <- gls(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod5 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + feeding_level + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod6 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML") 
mod7 <- gls(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
ddfer <- model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod7, extra = "rsquared") %>% 
  dplyr::select(-"rsquared.Response") %>% 
  dplyr::select(-"rsquared.family") %>% 
  dplyr::select(-"rsquared.link") %>% 
  dplyr::select(-"rsquared.method")

visreg(mod4, "abs_lat")
mod1b <- gls(log_concentration ~ log_length, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
summary(mod1b)
coef(mod1b)
confint(mod1b)

cal2 %>% 
  ggplot(aes(x = log_length, y = log_concentration)) + geom_point() +
  geom_smooth(method = "lm")

summary(mod1b)
coef(mod1b)
confint(mod1b)

dd_wide <- ddfer %>% 
  mutate(weight = round(weight, digits = 2)) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() 

dd_wide %>% 
  filter(rowname == "weight")


tab1 <- tableHTML(ddfer, round = 2)
write_tableHTML(tab1, file = "tables/model-sel-iron.htm")

rsquared(mod6) 
# mod1, 
# mod7, 
# mod3, 
# mod5, 
# mod2, 
# mod6
ddfer

stargazer(mod4, mod1, mod7, mod3, mod5, mod2, mod6, title="", type = "html",
          align=TRUE, dep.var.labels= "Iron concentration (mg/100g)",
          ci=TRUE, ci.level=0.95, 
          single.row=FALSE, digits = 2, dep.var.caption = "", out="tables/iron-models.htm",
          add.lines = list(c("delta",  "0.00",  "1.98",  "2.81",
                             "3.02",  "5.16", "17.35", "26.40"),
                           c("weight", "0.52", "0.19", "0.13", "0.12", "0.04", "0.00", "0.00"),
                           c("R2", "0.27", "0.27", "0.27", "0.26", "0.27", "0.23", "0.041")))


avg_mod <- model.avg(ddfer, subset = cumsum(weight) <= .95)

CI_average <- rownames_to_column(as.data.frame(confint(model.avg(ddfer, subset = cumsum(weight) <= .95)), var = "term")) %>%
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`) %>% 
  rename(term = rowname)

slopes_average <- enframe(coef(model.avg(ddfer, subset = cumsum(weight) <= .95)), name = "term", value = "slope") 

results <- left_join(CI_average, slopes_average, by = "term") %>% 
  dplyr::select(term, slope, conf_low, conf_high) %>% 
  mutate(term = factor(term, levels = c("log_length", "bulk_trophic_level", 
                                        "feeding_modefiltering plankton",
                                        "feeding_modegrazing on aquatic plants",
                                        "feeding_modehunting macrofauna (predator)",
                                        "feeding_modeselective plankton feeding",
                                        "feeding_modevariable",
                                        "feeding_levelmainly plants/detritus (troph. 2-2.19)",
                                        "feeding_levelplants/detritus+animals (troph. 2.2-2.79)",
                                        "abs_lat",
                                        "(Intercept)" 
  ))) %>% 
  dplyr::arrange((term))



res_tab1 <- tableHTML(results, round = 2)
write_tableHTML(res_tab1, file = "tables/model-avg-iron.htm")


# zinc models -------------------------------------------------------------


calcium <- traits %>% 
  filter(nutrient == "zn_mg") %>% 
  group_by(species1, feeding_mode, feeding_level, subgroup) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, abs_lat)
calcium$species1 <- str_to_lower(calcium$species1)


cal_taxa <- tnrs_match_names(calcium$species1, context="Animals", names = calcium$species1, do_approximate_matching = TRUE) 
tr_cal <- tol_induced_subtree(ott_ids = ott_id(cal_taxa), label_format="name") 
tr_bl_cal <- compute.brlen(tr_cal)




cal2 <- calcium %>% 
  left_join(., cal_taxa, by = c("species1" = "search_string")) %>%
  mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl_cal$tip.label))
zinc2 <- cal2

cal2$log_length <- scale(cal2$log_length)
cal2$abs_lat <- scale(cal2$abs_lat)
cal2$bulk_trophic_level <- scale(cal2$bulk_trophic_level)

rownames(cal2) <- cal2$unique_name2
# models to compare -------------------------------------------------------

mod1 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod2 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod3 <- gls(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod4 <- gls(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod5 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + feeding_level + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod6 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML") 
mod7 <- gls(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
ddfer <- model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod7, extra = "rsquared") %>% 
  dplyr::select(-"rsquared.Response") %>% 
  dplyr::select(-"rsquared.family") %>% 
  dplyr::select(-"rsquared.link") %>% 
  dplyr::select(-"rsquared.method")

coef(mod3)
confint(mod3)

rownames(zinc2) <- zinc2$unique_name2
mod1c <- gls(log_concentration ~ log_length, correlation = corBrownian(phy = tr_bl_cal), data = zinc2, method = "ML")
summary(mod1c)
coef(mod1c)
confint(mod1c)

zinc2 %>% 
  ggplot(aes(x = log_length, y = log_concentration)) + geom_point() +
  geom_smooth(method = "lm")


dd_wide <- ddfer %>% 
  mutate(weight = round(weight, digits = 2)) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() 

dd_wide %>% 
  filter(rowname == "weight")


tab1 <- tableHTML(ddfer, round = 2)
write_tableHTML(tab1, file = "tables/model-sel-zinc.htm")

rsquared(mod6) 
ddfer

stargazer(mod3, mod5, mod1, mod2, mod4, mod7, mod6, title="", type = "html",
          align=TRUE, dep.var.labels= "Zinc concentration (mg/100g)",
          ci=TRUE, ci.level=0.95, 
          single.row=FALSE, digits = 2, dep.var.caption = "", out="tables/zinc-models.htm",
          add.lines = list(c("delta",  "0.00",  "2.01",  "2.81",
                             "2.88",  "6.72", "8.12", "15.62"),
                           c("weight", "0.52", "0.19", "0.13", "0.12", "0.018", "0.009", "0.00"),
                           c("R2", "0.37", "0.37", "0.35", "0.39", "0.30", "0.31", "0.24")))


avg_mod <- model.avg(ddfer, subset = cumsum(weight) <= .95)

CI_average <- rownames_to_column(as.data.frame(confint(model.avg(ddfer, subset = cumsum(weight) <= .95)), var = "term")) %>%
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`) %>% 
  rename(term = rowname)

slopes_average <- enframe(coef(model.avg(ddfer, subset = cumsum(weight) <= .95)), name = "term", value = "slope") 

results <- left_join(CI_average, slopes_average, by = "term") %>% 
  dplyr::select(term, slope, conf_low, conf_high) %>% 
  mutate(term = factor(term, levels = c("log_length", "bulk_trophic_level", 
                                        "feeding_modefiltering plankton",
                                        "feeding_modegrazing on aquatic plants",
                                        "feeding_modehunting macrofauna (predator)",
                                        "feeding_modeselective plankton feeding",
                                        "feeding_modevariable",
                                        "feeding_levelmainly plants/detritus (troph. 2-2.19)",
                                        "feeding_levelplants/detritus+animals (troph. 2.2-2.79)",
                                        "abs_lat",
                                        "(Intercept)" 
  ))) %>% 
  dplyr::arrange((term))



res_tab1 <- tableHTML(results, round = 2)
write_tableHTML(res_tab1, file = "tables/model-avg-zinc.htm")



# plot all micronutrients -------------------------------------------------

cal2$nutrient <- "Calcium"
zinc2$nutrient <- "Zinc"
iron2$nutrient <- "Iron"

all_micro <- bind_rows(cal2, zinc2, iron2)

all_micro %>% 
  ggplot(aes(x = log_length, y = log_concentration)) + geom_point(alpha = 0.7) +
  facet_wrap( ~ nutrient, scales = "free") + geom_smooth(method = "lm", color = "black") +
  ylab("ln(nutrient concentration) \n (mg/100g)") + xlab("ln(length) (cm)")
ggsave("figures/cal-iron-zinc-length-all.png", width = 8, height = 3.5)


# epa models -------------------------------------------------------------


calcium <- traits %>% 
  mutate(species1 = str_replace(species1, "Merlangius merlangus euxinus", "Merlangius merlangus")) %>% 
  filter(nutrient == "epa") %>% 
  group_by(species1, feeding_mode, feeding_level, subgroup) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, abs_lat) %>% 
  ungroup() %>% 
  distinct(species1, .keep_all= TRUE) 
calcium$species1 <- str_to_lower(calcium$species1)
length(unique(calcium$species1))
cal_taxa

cal_taxa <- tnrs_match_names(context="Animals", names = calcium$species1, do_approximate_matching = TRUE) 
tr_cal <- tol_induced_subtree(ott_ids = ott_id(cal_taxa), label_format="name") 
tr_bl_cal <- compute.brlen(tr_cal)
str(tr_bl_cal)



cal2 <- calcium %>% 
  left_join(., cal_taxa, by = c("species1" = "search_string")) %>% 
  mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl_cal$tip.label)) %>% 
  dplyr::distinct(unique_name2, .keep_all = TRUE)

unames <- unique(cal2$unique_name2)
names_cal <- cal2$unique_name2
cal2$unique_name2[duplicated(cal2$unique_name2)]

setdiff(names_cal, unames)

cal2$log_length <- scale(cal2$log_length)
cal2$abs_lat <- scale(cal2$abs_lat)
cal2$bulk_trophic_level <- scale(cal2$bulk_trophic_level)


rownames(cal2) <- cal2$unique_name2

# models to compare -------------------------------------------------------

mod1 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod2 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod3 <- gls(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod4 <- gls(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod5 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + feeding_level + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod6 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML") 
mod7 <- gls(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
ddfer <- model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod7, extra = "rsquared") %>% 
  dplyr::select(-"rsquared.Response") %>% 
  dplyr::select(-"rsquared.family") %>% 
  dplyr::select(-"rsquared.link") %>% 
  dplyr::select(-"rsquared.method")



dd_wide <- ddfer %>% 
  mutate(weight = round(weight, digits = 2)) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() 

dd_wide %>% 
  filter(rowname == "weight")


tab1 <- tableHTML(ddfer, round = 2)
write_tableHTML(tab1, file = "tables/model-sel-epa.htm")

rsquared(mod5) 
ddfer

stargazer(mod6, mod2, mod4, mod1, mod7, mod3, mod5, title="", type = "html",
          align=TRUE, dep.var.labels= "EPA concentration (g/100g)",
          ci=TRUE, ci.level=0.95, 
          single.row=FALSE, digits = 2, dep.var.caption = "", out="tables/epa-models.htm",
          add.lines = list(c("delta",  "0.00",  "1.90",  "5.98",
                             "8.16",  "10.16", "12.34", "12.40"),
                           c("weight", "0.68", "0.26", "0.034", "0.12", "0.004", "0.001", "0.00"),
                           c("R2", "0.029", "0.03", "0.03", "0.03", "0.03", "0.02", "0.03")))


avg_mod <- model.avg(ddfer, subset = cumsum(weight) <= .95)

CI_average <- rownames_to_column(as.data.frame(confint(model.avg(ddfer, subset = cumsum(weight) <= .95)), var = "term")) %>%
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`) %>% 
  rename(term = rowname)

slopes_average <- enframe(coef(model.avg(ddfer, subset = cumsum(weight) <= .95)), name = "term", value = "slope") 

results <- left_join(CI_average, slopes_average, by = "term") %>% 
  dplyr::select(term, slope, conf_low, conf_high) %>% 
  mutate(term = factor(term, levels = c("log_length", "bulk_trophic_level", 
                                        "feeding_modefiltering plankton",
                                        "feeding_modegrazing on aquatic plants",
                                        "feeding_modehunting macrofauna (predator)",
                                        "feeding_modeselective plankton feeding",
                                        "feeding_modevariable",
                                        "feeding_levelmainly plants/detritus (troph. 2-2.19)",
                                        "feeding_levelplants/detritus+animals (troph. 2.2-2.79)",
                                        "abs_lat",
                                        "(Intercept)" 
  ))) %>% 
  dplyr::arrange((term))



res_tab1 <- tableHTML(results, round = 2)
write_tableHTML(res_tab1, file = "tables/model-avg-epa.htm")

# dha models -------------------------------------------------------------


calcium <- traits %>% 
  mutate(species1 = str_replace(species1, "Merlangius merlangus euxinus", "Merlangius merlangus")) %>% 
  filter(nutrient == "dha") %>% 
  group_by(species1, feeding_mode, feeding_level, subgroup) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, abs_lat) %>% 
  ungroup() %>% 
  distinct(species1, .keep_all= TRUE) 
calcium$species1 <- str_to_lower(calcium$species1)
length(unique(calcium$species1))


cal_taxa <- tnrs_match_names(context="Animals", names = calcium$species1, do_approximate_matching = TRUE) 
tr_cal <- tol_induced_subtree(ott_ids = ott_id(cal_taxa), label_format="name") 
tr_bl_cal <- compute.brlen(tr_cal)
str(tr_bl_cal)

?compute.brlen

cal2 <- calcium %>% 
  left_join(., cal_taxa, by = c("species1" = "search_string")) %>% 
  mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl_cal$tip.label)) %>% 
  dplyr::distinct(unique_name2, .keep_all = TRUE)

unames <- unique(cal2$unique_name2)
names_cal <- cal2$unique_name2
cal2$unique_name2[duplicated(cal2$unique_name2)]


cal2$log_length <- scale(cal2$log_length)
cal2$abs_lat <- scale(cal2$abs_lat)
cal2$bulk_trophic_level <- scale(cal2$bulk_trophic_level)


rownames(cal2) <- cal2$unique_name2

  # models to compare -------------------------------------------------------

mod1 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod2 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod3 <- gls(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod4 <- gls(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod5 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + feeding_level + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
mod6 <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML") 
mod7 <- gls(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, correlation = corBrownian(phy = tr_bl_cal), data = cal2, method = "ML")
ddfer <- model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod7, extra = "rsquared") %>% 
  dplyr::select(-"rsquared.Response") %>% 
  dplyr::select(-"rsquared.family") %>% 
  dplyr::select(-"rsquared.link") %>% 
  dplyr::select(-"rsquared.method")



dd_wide <- ddfer %>% 
  mutate(weight = round(weight, digits = 2)) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() 

dd_wide %>% 
  filter(rowname == "weight")


tab1 <- tableHTML(ddfer, round = 2)
write_tableHTML(tab1, file = "tables/model-sel-dha.htm")

rsquared(mod5) 
ddfer

stargazer(mod6, mod2, mod4, mod1, mod3, mod7, mod5, title="", type = "html",
          align=TRUE, dep.var.labels= "dha concentration (g/100g)",
          ci=TRUE, ci.level=0.95, 
          single.row=FALSE, digits = 2, dep.var.caption = "", out="tables/dha-models.htm",
          add.lines = list(c("delta",  "0.00",  "1.49",  "9.09",
                             "10.19",  "10.96", "11.09", "12.18"),
                           c("weight", "0.66", "0.32", "0.007", "0.004", "0.003", "0.003", "0.002"),
                           c("R2", "0.03", "0.04", "0.03", "0.036", "0.035", "0.035", "0.041")))


avg_mod <-mod6

CI_average <- rownames_to_column(as.data.frame(confint(avg_mod), var = "term")) %>%
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`) %>% 
  rename(term = rowname)

slopes_average <- enframe(coef(avg_mod), name = "term", value = "slope") 

results <- left_join(CI_average, slopes_average, by = "term") %>% 
  dplyr::select(term, slope, conf_low, conf_high) %>% 
  mutate(term = factor(term, levels = c("log_length", "bulk_trophic_level", 
                                        "feeding_modefiltering plankton",
                                        "feeding_modegrazing on aquatic plants",
                                        "feeding_modehunting macrofauna (predator)",
                                        "feeding_modeselective plankton feeding",
                                        "feeding_modevariable",
                                        "feeding_levelmainly plants/detritus (troph. 2-2.19)",
                                        "feeding_levelplants/detritus+animals (troph. 2.2-2.79)",
                                        "abs_lat",
                                        "(Intercept)" 
  ))) %>% 
  dplyr::arrange((term))



res_tab1 <- tableHTML(results, round = 2)
write_tableHTML(res_tab1, file = "tables/model-avg-dha.htm")
