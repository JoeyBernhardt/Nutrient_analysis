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


traits_analysis_raw <- read_csv("data-processed/traits_for_analysis.csv")



mod_all <- traits_analysis_raw

feeding_mode_table <- as.data.frame(table(mod_all$feeding_mode))
write_csv(feeding_mode_table, "data-processed/feeding_mode_table.csv")

feeding_level_table <- as.data.frame(table(mod_all$feeding_level))
write_csv(feeding_level_table, "data-processed/feeding_level_table.csv")

# calcium -----------------------------------------------------------------
calcium <- mod_all %>% 
  filter(nutrient == "ca_mg") %>% 
  filter(subgroup == "finfish")

mod1fg_calcium <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, data = calcium, method = "ML") 
mod2fg_calcium <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, data = calcium, method = "ML") 
mod2fg2_calcium <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, data = calcium, method = "ML") 
mod3fg_calcium <- gls(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, data = calcium, method = "ML") 
mod4fg_calcium <- gls(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, data = calcium, method = "ML") 
mod5fg_calcium <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level, data = calcium, method = "ML") 
mod6fg_calcium <- gls(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, data = calcium, method = "ML") 
ddfeg_calcium <- model.sel(mod1fg_calcium, mod2fg_calcium, mod3fg_calcium, mod4fg_calcium, mod5fg_calcium, mod6fg_calcium, mod2fg2_calcium)

summary(mod6fg_calcium)
mod1m_calcium <- lme(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = calcium) 
mod2m_calcium <- lme(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, random = ~ 1 | reference, method = "ML", data = calcium) 
mod3m_calcium <- lme(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = calcium) 
mod4m_calcium <- lme(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = calcium)
mod5m_calcium <- lme(log_concentration ~ log_length + bulk_trophic_level + feeding_level, random = ~ 1 | reference, method = "ML", data = calcium) 
mod6m_calcium <- lme(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = calcium) 

ddfer_calcium <- model.sel(mod1m_calcium, mod2m_calcium, mod3m_calcium, mod4m_calcium, mod5m_calcium, mod6m_calcium)
anova(mod2fg_calcium, mod2m_calcium)
summary(mod6m_calcium)
intervals(mod2m_calcium)
r.squaredGLMM(mod6m_calcium)



  ca_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(ddfer_calcium, subset = cumsum(weight) <= .95)), var = "term")) %>%
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`) %>% 
  rename(term = rowname)

ca_slopes_average <- enframe(coef(model.avg(ddfer_calcium, subset = cumsum(weight) <= .95)), name = "term", value = "slope") %>% 
  mutate(type = "random")

ca_results <- left_join(ca_CI_average, ca_slopes_average, by = "term") %>% 
  mutate(nutrient = "calcium") %>% 
  mutate(type = "random")



# iron -----------------------------------------------------------------
iron <- mod_all %>% 
  filter(nutrient == "fe_mg") %>% 
  filter(subgroup == "finfish")

mod1fg_iron <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, data = iron, method = "ML") 
mod2fg_iron <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, data = iron, method = "ML") 
mod2fg2_iron <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, data = iron, method = "ML") 
mod3fg_iron <- gls(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, data = iron, method = "ML") 
mod4fg_iron <- gls(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, data = iron, method = "ML") 
mod5fg_iron <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level, data = iron, method = "ML") 
mod6fg_iron <- gls(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, data = iron, method = "ML") 
ddfeg_iron <- model.sel(mod1fg_iron, mod2fg_iron, mod3fg_iron, mod4fg_iron, mod5fg_iron, mod6fg_iron,mod2fg2_iron)

mod1m_iron <- lme(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = iron) 
mod2m_iron <- lme(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, random = ~ 1 | reference, method = "ML", data = iron) 
mod3m_iron <- lme(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = iron) 
mod4m_iron <- lme(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = iron)
mod5m_iron <- lme(log_concentration ~ log_length + bulk_trophic_level + feeding_level, random = ~ 1 | reference, method = "ML", data = iron) 
mod6m_iron <- lme(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = iron) 

ddfer_iron <- model.sel(mod1m_iron, mod2m_iron, mod3m_iron, mod4m_iron, mod5m_iron, mod6m_iron)
ddfer_iron

fe_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(ddfer_iron, subset = cumsum(weight) <= .95)), var = "term")) %>%
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`) %>% 
  rename(term = rowname)

fe_slopes_average <- enframe(coef(model.avg(ddfer_iron, subset = cumsum(weight) <= .95)), name = "term", value = "slope") %>% 
  mutate(type = "random")

fe_results <- left_join(fe_CI_average, fe_slopes_average, by = "term") %>% 
  mutate(nutrient = "iron") %>% 
  mutate(type = "random")


r.squaredGLMM(mod3m_iron)

# zinc -----------------------------------------------------------------
zinc <- mod_all %>% 
  filter(nutrient == "zn_mg") %>% 
  filter(subgroup == "finfish")

mod1fg_zinc <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, data = zinc, method = "ML") 
mod2fg_zinc <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, data = zinc, method = "ML") 
mod2fg2_zinc <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, data = zinc, method = "ML") 
mod3fg_zinc <- gls(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, data = zinc, method = "ML") 
mod4fg_zinc <- gls(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, data = zinc, method = "ML") 
mod5fg_zinc <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level, data = zinc, method = "ML") 
mod6fg_zinc <- gls(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, data = zinc, method = "ML") 
ddfeg_zinc <- model.sel(mod1fg_zinc, mod2fg_zinc, mod3fg_zinc, mod4fg_zinc, mod5fg_zinc, mod6fg_zinc,mod2fg2_zinc)

mod1m_zinc <- lme(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = zinc) 
mod2m_zinc <- lme(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, random = ~ 1 | reference, method = "ML", data = zinc) 
mod3m_zinc <- lme(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data =zinc) 
mod4m_zinc <- lme(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = zinc)
mod5m_zinc <- lme(log_concentration ~ log_length + bulk_trophic_level + feeding_level, random = ~ 1 | reference, method = "ML", data = zinc) 
mod6m_zinc <- lme(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = zinc) 

ddfer_zinc <- model.sel(mod1m_zinc, mod2m_zinc, mod3m_zinc, mod4m_zinc, mod5m_zinc, mod6m_zinc)

zn_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(ddfer_zinc, subset = cumsum(weight) <= .95)), var = "term")) %>%
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`) %>% 
  rename(term = rowname)

zn_slopes_average <- enframe(coef(model.avg(ddfer_zinc, subset = cumsum(weight) <= .95)), name = "term", value = "slope") %>% 
  mutate(type = "random")

zn_results <- left_join(zn_CI_average, zn_slopes_average, by = "term") %>% 
  mutate(nutrient = "zinc") %>% 
  mutate(type = "random")

ddfer_zinc
anova(mod1fg_zinc, mod4m_zinc)
summary(mod4m_zinc)
intervals(mod4m_zinc)
r.squaredGLMM(mod5m_zinc)



# EPA -----------------------------------------------------------------
EPA <- mod_all %>% 
  filter(nutrient == "epa") %>% 
  filter(subgroup == "finfish")

mod1fg_EPA <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, data = EPA, method = "ML") 
mod2fg_EPA <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, data = EPA, method = "ML") 
mod2fg2_EPA <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, data = EPA, method = "ML") 
mod3fg_EPA <- gls(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, data = EPA, method = "ML") 
mod4fg_EPA <- gls(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, data = EPA, method = "ML") 
mod5fg_EPA <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level, data = EPA, method = "ML") 
mod6fg_EPA <- gls(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, data = EPA, method = "ML") 
ddfeg_EPA <- model.sel(mod1fg_EPA, mod2fg_EPA, mod3fg_EPA, mod4fg_EPA, mod5fg_EPA, mod6fg_EPA,mod2fg2_EPA)

mod1m_EPA <- lme(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = EPA) 
mod2m_EPA <- lme(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, random = ~ 1 | reference, method = "ML", data =EPA) 
mod3m_EPA <- lme(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = EPA) 
mod4m_EPA <- lme(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = EPA)
mod5m_EPA <- lme(log_concentration ~ log_length + bulk_trophic_level + feeding_level, random = ~ 1 | reference, method = "ML", data = EPA) 
mod6m_EPA <- lme(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = EPA) 

ddfer_EPA <- model.sel(mod1m_EPA, mod2m_EPA, mod3m_EPA, mod4m_EPA, mod5m_EPA, mod6m_EPA)


epa_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(ddfer_EPA, subset = cumsum(weight) <= .95)), var = "term")) %>%
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`) %>% 
  rename(term = rowname)

epa_slopes_average <- enframe(coef(model.avg(ddfer_EPA, subset = cumsum(weight) <= .95)), name = "term", value = "slope") %>% 
  mutate(type = "random")
ddfer_EPA
summary(mod5m_EPA)
epa_results <- left_join(epa_CI_average, epa_slopes_average, by = "term") %>% 
  mutate(nutrient = "epa") %>% 
  mutate(type = "random")

r.squaredGLMM(mod6m_EPA)

# DHA -----------------------------------------------------------------
dha <- mod_all %>% 
  filter(nutrient == "dha") %>% 
  filter(subgroup == "finfish")

mod1fg_dha <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, data = dha, method = "ML") 
mod2fg_dha <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, data = dha, method = "ML") 
mod2fg2_dha <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, data = dha, method = "ML") 
mod3fg_dha <- gls(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, data = dha, method = "ML") 
mod4fg_dha <- gls(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, data = dha, method = "ML") 
mod5fg_dha <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level, data = dha, method = "ML") 
mod6fg_dha <- gls(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, data = dha, method = "ML") 
ddfeg_dha <- model.sel(mod1fg_dha, mod2fg_dha, mod3fg_dha, mod4fg_dha, mod5fg_dha, mod6fg_dha,mod2fg2_dha)

mod1m_dha <- lme(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = dha) 
mod2m_dha <- lme(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, random = ~ 1 | reference, method = "ML", data =dha) 
mod3m_dha <- lme(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = dha) 
mod4m_dha <- lme(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = dha)
mod5m_dha <- lme(log_concentration ~ log_length + bulk_trophic_level + feeding_level, random = ~ 1 | reference, method = "ML", data = dha) 
mod6m_dha <- lme(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = dha) 

ddfer_dha <- model.sel(mod1m_dha, mod2m_dha, mod3m_dha, mod4m_dha, mod5m_dha, mod6m_dha)


dha_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(ddfer_dha, subset = cumsum(weight) <= .95)), var = "term")) %>%
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`) %>% 
  rename(term = rowname)

dha_slopes_average <- enframe(coef(model.avg(ddfer_dha, subset = cumsum(weight) <= .95)), name = "term", value = "slope") %>% 
  mutate(type = "random")

dha_results <- left_join(dha_CI_average, dha_slopes_average, by = "term") %>% 
  mutate(nutrient = "dha") %>% 
  mutate(type = "random")

r.squaredGLMM(mod6m_dha)


# protein -----------------------------------------------------------------
protein <- mod_all %>% 
  filter(nutrient == "protein") %>% 
  filter(subgroup == "finfish")

mod1fg_protein <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, data = protein, method = "ML") 
mod2fg_protein <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, data = protein, method = "ML") 
mod2fg2_protein <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, data = protein, method = "ML") 
mod3fg_protein <- gls(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, data = protein, method = "ML") 
mod4fg_protein <- gls(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, data = protein, method = "ML") 
mod5fg_protein <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level, data = protein, method = "ML") 
mod6fg_protein <- gls(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, data = protein, method = "ML") 
ddfeg_protein <- model.sel(mod1fg_protein, mod2fg_protein, mod3fg_protein, mod4fg_protein, mod5fg_protein, mod6fg_protein,mod2fg2_protein)

mod1m_protein <- lme(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = protein) 
mod2m_protein <- lme(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, random = ~ 1 | reference, method = "ML", data =protein) 
mod3m_protein <- lme(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = protein) 
mod4m_protein <- lme(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = protein)
mod5m_protein <- lme(log_concentration ~ log_length + bulk_trophic_level + feeding_level, random = ~ 1 | reference, method = "ML", data = protein) 
mod6m_protein <- lme(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = protein) 

ddfer_protein <- model.sel(mod1m_protein, mod2m_protein, mod3m_protein, mod4m_protein, mod5m_protein, mod6m_protein)


protein_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(ddfer_protein, subset = cumsum(weight) <= .95)), var = "term")) %>%
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`) %>% 
  rename(term = rowname)

protein_slopes_average <- enframe(coef(model.avg(ddfer_protein, subset = cumsum(weight) <= .95)), name = "term", value = "slope") %>% 
  mutate(type = "random")

protein_results <- left_join(protein_CI_average, protein_slopes_average, by = "term") %>% 
  mutate(nutrient = "protein") %>% 
  mutate(type = "random")

r.squaredGLMM(mod5m_protein)


# fat -----------------------------------------------------------------
fat <- mod_all %>% 
  filter(nutrient == "fat_g") %>% 
  filter(subgroup == "finfish")

mod1fg_fat <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, data = fat, method = "ML") 
mod2fg_fat <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, data = fat, method = "ML") 
mod2fg2_fat <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, data = fat, method = "ML") 
mod3fg_fat <- gls(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, data = fat, method = "ML") 
mod4fg_fat <- gls(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, data = fat, method = "ML") 
mod5fg_fat <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level, data = fat, method = "ML") 
mod6fg_fat <- gls(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, data = fat, method = "ML") 
ddfeg_fat <- model.sel(mod1fg_fat, mod2fg_fat, mod3fg_fat, mod4fg_fat, mod5fg_fat, mod6fg_fat,mod2fg2_fat)

mod1m_fat <- lme(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = fat) 
mod2m_fat <- lme(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, random = ~ 1 | reference, method = "ML", data =fat) 
mod3m_fat <- lme(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = fat) 
mod4m_fat <- lme(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = fat)
mod5m_fat <- lme(log_concentration ~ log_length + bulk_trophic_level + feeding_level, random = ~ 1 | reference, method = "ML", data = fat) 
mod6m_fat <- lme(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, random = ~ 1 | reference, method = "ML", data = fat) 

ddfer_fat <- model.sel(mod1m_fat, mod2m_fat, mod3m_fat, mod4m_fat, mod5m_fat, mod6m_fat)


fat_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(ddfer_fat, subset = cumsum(weight) <= .95)), var = "term")) %>%
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`) %>% 
  rename(term = rowname)

fat_slopes_average <- enframe(coef(model.avg(ddfer_fat, subset = cumsum(weight) <= .95)), name = "term", value = "slope") %>% 
  mutate(type = "random")

fat_results <- left_join(fat_CI_average, fat_slopes_average, by = "term") %>% 
  mutate(nutrient = "fat") %>% 
  mutate(type = "random")

r.squaredGLMM(mod1m_fat)



# Merge all results -------------------------------------------------------

all_nuts_results <- bind_rows(protein_results, zn_results, fe_results, ca_results, epa_results, dha_results)

# calcium -----------------------------------------------------------------


modc <- mod_all %>% 
  filter(nutrient == "ca_mg") %>% 
  filter(subgroup == "finfish")

mod1 <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, data = modc), standardize.y = TRUE) 
mod2 <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, data = modc), standardize.y = TRUE) 
mod3 <- standardize(lm(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, data = modc), standardize.y = TRUE) 
mod4 <- standardize(lm(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, data = modc), standardize.y = TRUE) 
mod5 <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_level, data = modc), standardize.y = TRUE) 
mod6 <- standardize(lm(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, data = modc), standardize.y = TRUE) 

ddca <- model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod2m)


ca_CI_average <- rownames_to_column(as.data.frame(confint(mod2, var = "term"))) %>%
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`) %>% 
  rename(term = rowname)
ca_slopes_average <- enframe(coef(mod2), name = "term", value = "slope") %>% 
  mutate(type = "fixed")
ca_results <- left_join(ca_CI_average, ca_slopes_average, by = "term") %>% 
  mutate(nutrient = "calcium") %>% 
  mutate(type = "fixed")

mod1m <- standardize(lmer(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat + (1 |reference), data = modc), standardize.y = TRUE) 
mod2m <- standardize(lmer(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat + (1 |reference), data = modc), standardize.y = TRUE) 
mod2b <- standardize(lmer(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat + (1 |reference), data = modc), standardize.y = TRUE) 
mod3m <- standardize(lmer(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat + (1 |reference), data = modc), standardize.y = TRUE) 
mod4m <- standardize(lmer(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat + (1 |reference), data = modc), standardize.y = TRUE) 
mod5m <- standardize(lmer(log_concentration ~ log_length + bulk_trophic_level + feeding_level + (1 |reference), data = modc), standardize.y = TRUE) 
mod6m <- standardize(lmer(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat + (1 |reference), data = modc), standardize.y = TRUE) 
ddcar <- model.sel(mod1m, mod2m, mod2b, mod3m, mod4m, mod5m, mod6m)

car_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(ddcar, subset = cumsum(weight) <= .95)), var = "term")) %>%
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`) %>% 
  rename(term = rowname)
car_slopes_average <- enframe(coef(model.avg(ddcar, subset = cumsum(weight) <= .95)), name = "term", value = "slope") %>% 
  mutate(type = "random")
car_results <- left_join(car_CI_average, car_slopes_average, by = "term") %>% 
  mutate(nutrient = "calcium") %>% 
  mutate(type = "random")


bind_rows(car_results, ca_results) %>% View

r.squaredGLMM(mod2m)

# zinc --------------------------------------------------------------------

mod <- mod_all %>% 
  filter(nutrient == "zn_mg") %>% 
  filter(subgroup == "finfish")

mod1 <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 
mod2 <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, data = mod), standardize.y = TRUE) 
mod3 <- standardize(lm(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 
mod4 <- standardize(lm(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 
mod5 <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_level, data = mod), standardize.y = TRUE) 
mod6 <- standardize(lm(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 

ddzn <- model.sel(mod1, mod2, mod3, mod4, mod5, mod6)



zn_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(ddzn, subset = cumsum(weight) <= .95)), var = "term")) %>%
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`) %>% 
  rename(term = rowname)
zn_slopes_average <- enframe(coef(model.avg(ddzn, subset = cumsum(weight) <= .95)), name = "term", value = "slope") %>% 
  mutate(type = "fixed")
zn_results <- left_join(zn_CI_average, zn_slopes_average, by = "term") %>% 
  mutate(nutrient = "zinc") %>% 
  mutate(type = "fixed")

mod1m <- standardize(lmer(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat + (1 |reference), data = mod), standardize.y = TRUE) 
mod2m <- standardize(lmer(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat + (1 |reference), data = mod), standardize.y = TRUE) 
mod3m <- standardize(lmer(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat + (1 |reference), data = mod), standardize.y = TRUE) 
mod4m <- standardize(lmer(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat + (1 |reference), data = mod), standardize.y = TRUE) 
mod5m <- standardize(lmer(log_concentration ~ log_length + bulk_trophic_level + feeding_level + (1 |reference), data = mod), standardize.y = TRUE) 
mod6m <- standardize(lmer(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat + (1 |reference), data = mod), standardize.y = TRUE) 

ddznr <- model.sel(mod1m, mod2m, mod3m, mod4m, mod5m, mod6m)

znr_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(ddznr, subset = cumsum(weight) <= .95)), var = "term")) %>%
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`) %>% 
  rename(term = rowname)
znr_slopes_average <- enframe(coef(model.avg(ddznr, subset = cumsum(weight) <= .95)), name = "term", value = "slope") %>% 
  mutate(type = "random")
znr_results <- left_join(znr_CI_average, znr_slopes_average, by = "term") %>% 
  mutate(nutrient = "zinc") %>% 
  mutate(type = "random")


bind_rows(znr_results, zn_results) %>% View

summary(mod2m)
tidy(mod2m, conf.int = TRUE) %>% View

