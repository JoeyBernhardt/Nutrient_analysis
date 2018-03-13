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


n.long_lat3 <- read_csv("data-processed/n.long_lat3.csv")

mod_all <- n.long_lat3 %>% 
  filter(concentration > 0) %>% 
  mutate(anacat = ifelse(subgroup != "finfish", "non-migratory", anacat)) %>% 
  filter(!is.na(bulk_max_length), !is.na(bulk_trophic_level), !is.na(feeding_level), !is.na(feeding_mode), !is.na(abs_lat)) %>% 
  mutate(log_length = log(bulk_max_length),
         log_concentration = log(concentration)) %>% 
  filter(!grepl("^Mohanty, B. P.,", ref_info)) %>% 
  mutate(reference = ifelse(is.na(updated_ref_info), ref_info, updated_ref_info))


mod <- mod_all %>% 
  filter(nutrient == "fe_mg") %>% 
  filter(subgroup == "finfish")

mod1 <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 
mod2 <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, data = mod), standardize.y = TRUE) 
mod3 <- standardize(lm(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 
mod4 <- standardize(lm(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 
mod5 <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_level, data = mod), standardize.y = TRUE) 
mod6 <- standardize(lm(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 

ddfe <- model.sel(mod1, mod2, mod3, mod4, mod5, mod6)


fe_CI_average <- rownames_to_column(as.data.frame(confint(mod2), var = "term")) %>%
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`) %>% 
  rename(term = rowname)
fe_slopes_average <- enframe(coef(mod2), name = "term", value = "slope") %>% 
  mutate(type = "fixed")
fe_results <- left_join(fe_CI_average, fe_slopes_average, by = "term") %>% 
  mutate(nutrient = "iron") %>% 
  mutate(type = "fixed")

mod1m <- standardize(lmer(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat + (1 |reference), data = mod), standardize.y = TRUE) 
mod2m <- standardize(lmer(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat + (1 |reference), data = mod), standardize.y = TRUE) 
mod3m <- standardize(lmer(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat + (1 |reference), data = mod), standardize.y = TRUE) 
mod4m <- standardize(lmer(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat + (1 |reference), data = mod), standardize.y = TRUE) 
mod5m <- standardize(lmer(log_concentration ~ log_length + bulk_trophic_level + feeding_level + (1 |reference), data = mod), standardize.y = TRUE) 
mod6m <- standardize(lmer(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat + (1 |reference), data = mod), standardize.y = TRUE) 

ddfer <- model.sel(mod1m, mod2m, mod3m, mod4m, mod5m, mod6m)

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


mod <- mod_all %>% 
  filter(nutrient == "ca_mg") %>% 
  filter(subgroup == "finfish")

mod1 <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 
mod2 <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, data = mod), standardize.y = TRUE) 
mod3 <- standardize(lm(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 
mod4 <- standardize(lm(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 
mod5 <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_level, data = mod), standardize.y = TRUE) 
mod6 <- standardize(lm(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 

ddca <- model.sel(mod1, mod2, mod3, mod4, mod5, mod6)


ca_CI_average <- rownames_to_column(as.data.frame(confint(mod2, var = "term"))) %>%
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`) %>% 
  rename(term = rowname)
ca_slopes_average <- enframe(coef(mod2), name = "term", value = "slope") %>% 
  mutate(type = "fixed")
ca_results <- left_join(ca_CI_average, ca_slopes_average, by = "term") %>% 
  mutate(nutrient = "calcium") %>% 
  mutate(type = "fixed")

mod1m <- standardize(lmer(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat + (1 |reference), data = mod), standardize.y = TRUE) 
mod2m <- standardize(lmer(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat + (1 |reference), data = mod), standardize.y = TRUE) 
mod2b <- standardize(lmer(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat + (1 |reference), data = mod), standardize.y = TRUE) 
mod3m <- standardize(lmer(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat + (1 |reference), data = mod), standardize.y = TRUE) 
mod4m <- standardize(lmer(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat + (1 |reference), data = mod), standardize.y = TRUE) 
mod5m <- standardize(lmer(log_concentration ~ log_length + bulk_trophic_level + feeding_level + (1 |reference), data = mod), standardize.y = TRUE) 
mod6m <- standardize(lmer(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat + (1 |reference), data = mod), standardize.y = TRUE) 
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


# zinc --------------------------------------------------------------------

mod <- mod_all %>% 
  filter(nutrient == "zn_mg") %>% 
  filter(subgroup == "finfish")

mod1 <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 
mod2 <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, data = mod), standardize.y = TRUE) 
mod3 <- standardize(lm(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 
mod4 <- standardize(lm(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 
mod6 <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_level, data = mod), standardize.y = TRUE) 
mod7 <- standardize(lm(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 
mod8 <- standardize(lm(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 

ddzn <- model.sel(mod1, mod2, mod3, mod4, mod6, mod7, mod8)

zn_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(ddzn, subset = cumsum(weight) <= .95)), var = "term")) %>%
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`) %>% 
  rename(term = rowname)
zn_slopes_average <- enframe(coef(model.avg(ddzn, subset = cumsum(weight) <= .95)), name = "term", value = "slope") %>% 
  mutate(type = "fixed")
zn_results <- left_join(zn_CI_average, zn_slopes_average, by = "term") %>% 
  mutate(nutrient = "zinc") %>% 
  mutate(type = "fixed")

mod1m <- standardize(lmer(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat + (1|reference), data = mod), standardize.y = TRUE) 
mod2m <- standardize(lmer(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat + (1|reference), data = mod), standardize.y = TRUE) 
mod2b <- standardize(lmer(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat + (1|reference), data = mod), standardize.y = TRUE) 
mod2c <- standardize(lmer(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat + (1|reference), data = mod), standardize.y = TRUE) 
mod2d <- standardize(lmer(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat + (1|reference), data = mod), standardize.y = TRUE) 
mod3m <- standardize(lmer(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat + (1|reference), data = mod), standardize.y = TRUE) 
mod4m <- standardize(lmer(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat + (1|reference), data = mod), standardize.y = TRUE) 
mod6m <- standardize(lmer(log_concentration ~ log_length + bulk_trophic_level + feeding_level + (1|reference), data = mod), standardize.y = TRUE) ### correlated random intercept and slope
mod6b <- standardize(lmer(log_concentration ~ log_length + bulk_trophic_level + feeding_level + (1|reference), data = mod), standardize.y = TRUE) ### correlated random intercept and slope
mod6mb <- standardize(lmer(log_concentration ~ log_length + bulk_trophic_level + feeding_level + (1|reference), data = mod), standardize.y = TRUE) ### uncorrelated random intercept and slope
mod7m <- standardize(lmer(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat + (1|reference), data = mod), standardize.y = TRUE) 
mod8m <- standardize(lmer(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat + (1|reference), data = mod), standardize.y = TRUE) 
ddznr <- model.sel(mod1m, mod2m, mod3m, mod4m, mod6m, mod7m, mod8m, mod6mb, mod6b, mod2b, mod2c, mod2d)

znr_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(ddznr, subset = cumsum(weight) <= .95)), var = "term")) %>%
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`) %>% 
  rename(term = rowname)
znr_slopes_average <- enframe(coef(model.avg(ddfer, subset = cumsum(weight) <= .95)), name = "term", value = "slope") %>% 
  mutate(type = "random")
znr_results <- left_join(znr_CI_average, znr_slopes_average, by = "term") %>% 
  mutate(nutrient = "zinc") %>% 
  mutate(type = "random")


bind_rows(znr_results, zn_results) %>% View
