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


n.long_lat3 <- read_csv("data-processed/n.long_lat3.csv")

mod_all <- n.long_lat3 %>% 
  filter(concentration > 0) %>% 
  mutate(anacat = ifelse(subgroup != "finfish", "non-migratory", anacat)) %>% 
  filter(!is.na(bulk_max_length), !is.na(bulk_trophic_level), !is.na(feeding_level), !is.na(feeding_mode), !is.na(abs_lat)) %>% 
  mutate(log_length = log(bulk_max_length),
         log_concentration = log(concentration)) %>% 
  filter(!grepl("^Mohanty, B. P.,", ref_info)) %>% 
  mutate(reference = ifelse(is.na(updated_ref_info), ref_info, updated_ref_info)) %>% 
  filter(!is.na(reference)) 

mod_all %>% 
  filter(is.na(reference)) %>% View


modf <- mod_all %>% 
  filter(nutrient == "fe_mg") %>% 
  filter(subgroup == "finfish")

mod1f <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, data = modf), standardize.y = TRUE) 
mod2f <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, data = modf), standardize.y = TRUE) 
mod3f <- standardize(lm(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, data = modf), standardize.y = TRUE) 
mod4f <- standardize(lm(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, data = modf), standardize.y = TRUE) 
mod5f <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_level, data = modf), standardize.y = TRUE) 
mod6f <- standardize(lm(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, data = modf), standardize.y = TRUE) 
ddfe <- model.sel(mod1f, mod2f, mod3f, mod4f, mod5f, mod6f)

mod1fg <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, data = modf) 
mod2fg <- gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, data = modf) 
mod3fg <- standardize(gls(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, data = modf), standardize.y = TRUE) 
mod4fg <- standardize(gls(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, data = modf), standardize.y = TRUE) 
mod5fg <- standardize(gls(log_concentration ~ log_length + bulk_trophic_level + feeding_level, data = modf), standardize.y = TRUE) 
mod6fg <- standardize(gls(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, data = modf), standardize.y = TRUE) 



summary(mod2)

fe_CI_average <- rownames_to_column(as.data.frame(confint(mod2), var = "term")) %>%
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`) %>% 
  rename(term = rowname)
fe_slopes_average <- enframe(coef(mod2), name = "term", value = "slope") %>% 
  mutate(type = "fixed")
fe_results <- left_join(fe_CI_average, fe_slopes_average, by = "term") %>% 
  mutate(nutrient = "iron") %>% 
  mutate(type = "fixed")

mod1m <- standardize(lmer(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat + (1 |reference), data = modf), standardize.y = TRUE) 
mod2m <- lme(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, random = ~ 1 | reference, data = modf) 
mod3m <- standardize(lmer(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat + (1 |reference), data = modf), standardize.y = TRUE) 
mod4m <- standardize(lmer(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat + (1 |reference), data = modf), standardize.y = TRUE) 
mod5m <- standardize(lmer(log_concentration ~ log_length + bulk_trophic_level + feeding_level + (1 |reference), data = modf), standardize.y = TRUE) 
mod6m <- standardize(lmer(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat + (1 |reference), data = modf), standardize.y = TRUE) 

ddfer <- model.sel(mod1m, mod2m, mod3m, mod4m, mod5m, mod6m)
summary(mod2m)
tidy(mod2m, conf.int = TRUE) %>% View

anova(mod2fg, mod2m)
AIC(mod2fg, mod2m)

fer_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(ddfer, subset = cumsum(weight) <= .95)), var = "term")) %>%
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`) %>% 
  rename(term = rowname)
fer_slopes_average <- enframe(coef(model.avg(ddfer, subset = cumsum(weight) <= .95)), name = "term", value = "slope") %>% 
  mutate(type = "random")
fer_results <- left_join(fer_CI_average, fer_slopes_average, by = "term") %>% 
  mutate(nutrient = "iron") %>% 
  mutate(type = "random")


bind_rows(fer_results, fe_results) %>% View




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


library(piecewiseSEM)
sem.model.fits(mod2m)
r.squaredGLMM(mod2m)
