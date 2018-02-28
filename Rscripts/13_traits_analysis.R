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


trait_data <- read_csv("data-processed/n.long_lat3.csv")


mod_all <- trait_data %>% 
  filter(concentration > 0) %>% 
  mutate(anacat = ifelse(subgroup != "finfish", "non-migratory", anacat)) %>% 
  filter(!is.na(bulk_max_length), !is.na(bulk_trophic_level), !is.na(feeding_level), !is.na(feeding_mode), !is.na(abs_lat)) %>% 
  mutate(log_length = log(bulk_max_length),
         log_concentration = log(concentration)) %>% 
  filter(!grepl("^Mohanty, B. P.,", ref_info)) %>% 
  filter(nutrient %in% c("ca_mg", "zn_mg", "fe_mg", "fapun3", "protcnt_g", "fat_g", "epa", "dha"))


names(mod_all)

finfish_size <- mod_all %>% 
  filter(subgroup == "finfish") %>% 
  group_by(nutrient) %>% 
  do(glance(lm(log_concentration ~ log_length + bulk_trophic_level + abs_lat, data = .))) %>% 
  mutate(subgroup = "finfish") %>% 
  stargazer()

mod_all %>% 
  filter(subgroup == "finfish") %>% 
  group_by(nutrient) %>% 
  do(tidy(lm(log_concentration ~ log_length, data = .))) %>% 
  mutate(subgroup = "finfish") %>% 
  filter(term != "(Intercept)") %>% View

mod_all %>% 
  filter(subgroup == "finfish") %>% 
  filter(nutrient == "zn_mg") %>% 
  lm(log_concentration ~ log_length, data = .) %>% summary()


fin_ca<- mod_all %>% 
  filter(subgroup == "finfish") %>% 
  filter(nutrient == "ca_mg") %>% 
  lm(log_concentration ~ log_length + bulk_trophic_level + abs_lat, data = .) 
fin_zn <- mod_all %>% 
  filter(subgroup == "finfish") %>% 
  filter(nutrient == "zn_mg") %>% 
  lm(log_concentration ~ log_length + bulk_trophic_level + abs_lat, data = .) 
fin_fe <- mod_all %>% 
  filter(subgroup == "finfish") %>% 
  filter(nutrient == "fe_mg") %>% 
  lm(log_concentration ~ log_length + bulk_trophic_level + abs_lat, data = .) 
fin_epa <- mod_all %>% 
  filter(subgroup == "finfish") %>% 
  filter(nutrient == "epa") %>% 
  lm(log_concentration ~ log_length + bulk_trophic_level + abs_lat, data = .) 
fin_dha <- mod_all %>% 
  filter(subgroup == "finfish") %>% 
  filter(nutrient == "dha") %>% 
  lm(log_concentration ~ log_length + bulk_trophic_level + abs_lat, data = .) 
fin_fapun3 <- mod_all %>% 
  filter(subgroup == "finfish") %>% 
  filter(nutrient == "fapun3") %>% 
  lm(log_concentration ~ log_length + bulk_trophic_level + abs_lat, data = .)

stargazer(fin_ca, fin_zn, fin_fe, fin_epa, fin_dha, fin_fapun3, align = TRUE)
stargazer(fin_epa, fin_dha, fin_fapun3, align = TRUE)


summary(fin_dha)
stargazer(linear.1, linear.2, probit.model, title="Results", align=TRUE)


mollusc_size <- mod_all %>% 
  filter(subgroup == "mollusc") %>% 
  group_by(nutrient) %>% 
  do(glance(lm(log_concentration ~ log_length, data = .))) %>% 
  mutate(subgroup = "molluscs")

crustacean_size <- mod_all %>% 
  filter(subgroup == "crustacean") %>% 
  group_by(nutrient) %>% 
  do(glance(lm(log_concentration ~ log_length, data = .))) %>% 
  mutate(subgroup = "crustacean")

mod_all %>% 
  filter(subgroup == "finfish") %>% 
  filter(nutrient == "ca_mg") %>%  
  lm(log_concentration ~ log_length, data = .) %>% summary





mod_all %>% 
  filter(subgroup == "finfish") %>% 
  group_by(nutrient) %>% 
  do(glance(lm(log_concentration ~ abs_lat + log_length + feeding_mode + feeding_level + bulk_trophic_level, data = .))) %>% 
  dplyr::select(1, 3, 5, 6, 7, 11, 12) %>% 
  xtable() %>% 
  print()


mod_all %>% 
  filter(subgroup == "finfish") %>% 
  group_by(nutrient) %>% 
  do(glance(lm(log_concentration ~ log_length, data = .))) %>% View
  filter(term != "(Intercept)") %>% View
  
  
  
  #### bringing in the traits analysis from the Rmd
  
  mod <- mod_all %>% 
    filter(nutrient == "dha") %>% 
    filter(subgroup == "finfish")
  
  mod1 <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 
  mod2 <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, data = mod), standardize.y = TRUE) 
  mod3 <- standardize(lm(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 
  mod4 <- standardize(lm(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 
  mod6 <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_level, data = mod), standardize.y = TRUE) 
  mod7 <- standardize(lm(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 
  
  ddfe <- model.sel(mod1, mod2, mod3, mod4, mod6, mod7)
ddfe

model_table <- as.data.frame(ddfe)
model_table %>% 
  rename(latitude = z.abs_lat,
         trophic_level = z.bulk_trophic_level,
         ln_length = z.log_length) %>% 
  dplyr::select(-`(Intercept)`) %>% 
  t() %>%
  as.data.frame() %>% View
    xtable(type = "latex", digits = 2)

summary(mod6)

  
  stargazer(mod2)
confint(mod2)
summary(mod2)
 summary(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, data = mod))
  
  confint(model.avg(ddfe, subset = cumsum(weight) <= .95))
  summary(model.avg(ddfe, subset = cumsum(weight) <= .95))
  
  fe_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(ddfe, subset = cumsum(weight) <= .95)), var = "term")) %>%
    rename(conf_low = `2.5 %`,
           conf_high = `97.5 %`) %>% 
    rename(term = rowname)
  fe_slopes_average <- enframe(coef(model.avg(ddfe, subset = cumsum(weight) <= .95)), name = "term", value = "slope")
  fe_results <- left_join(fe_CI_average, fe_slopes_average, by = "term") %>% 
    mutate(nutrient = "iron")
  
  summary(mod1)
confint(mod2)
  
  
  library(listviewer)
  
  
  map(`[`, c("Richness", "fixed"))