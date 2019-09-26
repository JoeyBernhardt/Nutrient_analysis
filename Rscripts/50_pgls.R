
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



# calcium models ----------------------------------------------------------


calcium <- traits %>% 
  filter(nutrient == "ca_mg") %>% 
  group_by(species1, feeding_mode, feeding_level, subgroup) %>% 
  summarise_each(funs(mean), log_concentration, log_length, bulk_trophic_level, abs_lat)
calcium$species1 <- str_to_lower(calcium$species1)


cal_taxa <- tnrs_match_names(calcium$species1, context="Animals", names = calcium$species1, do_approximate_matching = TRUE) 
tr_cal <- tol_induced_subtree(ott_ids = ott_id(cal_taxa), label_format="name") 
tr_bl_cal <- compute.brlen(tr_cal)
str(tr_bl_cal)



cal2 <- calcium %>% 
  left_join(., cal_taxa, by = c("species1" = "search_string")) %>%
  mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl_cal$tip.label)) 
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
library(tableHTML)

dd_wide <- ddfer %>% 
  mutate(weight = round(weight, digits = 2)) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() 

dd_wide %>% 
  filter(rowname == "weight")

rsquared
summary(mod5)
class(mod5)
coef(avg_mod)
class(avg_mod)

tab1 <- tableHTML(ddfer, round = 2)
write_tableHTML(tab1, file = "tables/model-sel-calcium.htm")

ddfer
rsquared(mod6) 
# mod1, 
# mod7, 
# mod3, 
# mod5, 
# mod2, 
# mod6

stargazer(ddfer, type = "html", out="tables/model-sel-calcium.htm")
stargazer(mod1, mod3, mod4, mod7, mod5, mod2, mod6, title="", type = "html",
          align=TRUE, dep.var.labels= "Calcium concentration (mg/100g)",
          ci=TRUE, ci.level=0.95, 
          single.row=TRUE, digits = 2, dep.var.caption = "", out="tables/calcium-models.htm",
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

levels(results$term)

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
str(tr_bl_cal)



cal2 <- calcium %>% 
  left_join(., cal_taxa, by = c("species1" = "search_string")) %>%
  mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl_cal$tip.label)) 
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
          single.row=TRUE, digits = 2, dep.var.caption = "", out="tables/iron-models.htm",
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
str(tr_bl_cal)



cal2 <- calcium %>% 
  left_join(., cal_taxa, by = c("species1" = "search_string")) %>%
  mutate(unique_name2 = str_replace_all(unique_name, " ", "_")) %>% 
  filter(unique_name2 %in% c(tr_bl_cal$tip.label)) 
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
write_tableHTML(tab1, file = "tables/model-sel-zinc.htm")

rsquared(mod6) 
ddfer

stargazer(mod3, mod5, mod1, mod2, mod4, mod7, mod6, title="", type = "html",
          align=TRUE, dep.var.labels= "Zinc concentration (mg/100g)",
          ci=TRUE, ci.level=0.95, 
          single.row=TRUE, digits = 2, dep.var.caption = "", out="tables/zinc-models.htm",
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


View(cal_taxa)
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
          single.row=TRUE, digits = 2, dep.var.caption = "", out="tables/epa-models.htm",
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


View(cal_taxa)
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
          single.row=TRUE, digits = 2, dep.var.caption = "", out="tables/dha-models.htm",
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
