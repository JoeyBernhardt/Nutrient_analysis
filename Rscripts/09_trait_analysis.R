### Seanuts traits analysis
## notes: this write_csv(n.long_lat3, "data-processed/n.long_lat3.csv") contains the fixed Bogard et al. 2015 epa and dha data, where I changed the units from mg/100g to g/100g

# load packages ----------------------------------------------------------


library(stringr)
library(purrr)
library(janitor)
library(MuMIn)
library(broom)
library(forcats)

library(tidyverse)
library(arm)

# load data ---------------------------------------------------------------

seanuts_ecology <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data-processed/seanuts_select_11.csv")

## ok let's just get rid of the one super outlier ca and fe measurement for now

seanuts_ecology$ca_mg[seanuts_ecology$ca_mg == 41206.00000] <- NA
seanuts_ecology$fe_mg[seanuts_ecology$fe_mg > 40939.00000] <- NA
seanuts_2 <- seanuts_ecology %>% 
  clean_names() 

# explore the dataset a bit -----------------------------------------------


## ok let's take an average of all the length measurements and use that for now
# this is now obsolete, since I've gone back and fixed the missing lengths.



hist(seanuts_2$bulk_mean_length)

### checking to see which species we don't have length data for:
no_length <- seanuts_2 %>% 
  filter(is.na(bulk_mean_length)) %>%
  distinct(species_name, .keep_all = TRUE) %>% 
  select(species_name, everything()) %>% View

no_length_withaverage <- seanuts_2 %>% 
group_by(species_name) %>% 
  summarise(mean_length_by_species = mean(avg_length, na.rm = TRUE)) %>%
  filter(is.na(mean_length_by_species))

write_csv(no_length_withaverage, "data-processed/no_length_withaverage.csv")

intersect(no_length_withaverage$species_name, no_length$species_name)


table(no_length$subgroup)

species_no_length <- unique(no_length$species_name) 


length <- seanuts_2 %>% 
  filter(!is.na(avg_length)) %>% 
  select(species_name, avg_length) %>% 
  distinct(species_name, .keep_all = TRUE)


write_csv(no_length, "data-processed/no_length.csv")



# turn the dataset into long form ----------------------


n.long <- seanuts_2 %>% 
  dplyr::select(ref_info, species_name, subgroup, prot_g, protein_g, fapun_all_g, fapun3, protcnt_g, epa, dha, ca_mg, fat_g,
                zn_mg, fe_mg, contains("length"), latitude, seanuts_id2, tl, food_item_id_2, country_region, season, isscaap_cat,
                abs_lat, contains("feeding"), demerspelag, food_name_clean, contains("Brack"), marine, fresh, contains("troph"), contains("weight"), contains("ana"), country_region) %>% 
  gather(key = "nutrient", value = "concentration", prot_g, fapun_all_g, protein_g, fapun3, protcnt_g, epa, dha, ca_mg, fat_g, zn_mg, fe_mg) %>% 
  filter(!is.na(concentration)) %>% 
  mutate(subgroup = as.factor(subgroup)) %>% 
  dplyr::select(-contains("_y")) %>% 
  dplyr::select(-contains("_x")) %>% 
  mutate(bulk_trophic_level = ifelse(is.na(bulk_trophic_level), trophic_level, bulk_trophic_level)) %>% 
  mutate(bulk_mean_length = ifelse(is.na(bulk_mean_length), mean_length, bulk_mean_length)) %>% 
  mutate(bulk_max_length = ifelse(is.na(bulk_max_length), max_length, bulk_max_length)) %>% 
  dplyr::select(ref_info, seanuts_id2, food_name_clean, species_name, nutrient, concentration, subgroup, abs_lat, latitude, contains("bulk"), feeding_mode, feeding_level, anacat, country_region) %>% 
  distinct(.data = ., .keep_all = TRUE)
  

names(n.long)

n.long <- n.long %>% 
  mutate(feeding_level = factor(feeding_level)) %>% 
  mutate(feeding_level = fct_relevel(feeding_level, "filtering plankton", "mainly plants/detritus (troph. 2-2.19)",
                                     "plants/detritus+animals (troph. 2.2-2.79)", "mainly animals (troph. 2.8 and up)"))


write_csv(n.long, "data-processed/n.long.csv")

# make some initial plots -------------------------------------------------
n.long <- read_csv("data-processed/n.long.csv")
n.long <- read_csv("data-processed/n.long_jan18.csv")

levels(n.long$feeding_level)
ggplot(data = n.long, aes(x = feeding_level, y = bulk_trophic_level)) + geom_point()

n.long %>% 
  filter(nutrient == "ca_mg") %>% 
  filter(concentration > 0) %>% 
  lm(log(concentration) ~ log(bulk_mean_length) + abs_lat + bulk_trophic_level + feeding_mode + feeding_level + anacat, data = .) %>% 
  tidy(., conf.int = TRUE) %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) + geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) + 
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### make mini data frame to fit model to
missing_lat <- n.long %>% 
  # filter(nutrient %in% c("fe_mg", "zn_mg", "ca_mg")) %>% 
	filter(concentration > 0) %>% 
	filter(is.na(abs_lat)) %>%
	dplyr::select(ref_info, seanuts_id2, species_name, country_region, nutrient, concentration) %>% 
	distinct()

write_csv(missing_lat, "data-processed/missing_lat.csv")
missing_lat$ref_info

## read in the newly updated and filled in missing_lat.csv, which is now missing_lat_completed

missing_lat_completed <- read_csv("data-processed/missing_lat_completed.csv")

missing_lat_select <- missing_lat_completed %>% 
  rename(location = country_region) 


## now join this new table back into n.long


n.long_lat <- left_join(n.long, missing_lat_select, by = c("seanuts_id2", "species_name", "nutrient", "concentration", "ref_info"))

n.long_lat2 <- n.long_lat %>% 
 mutate(abs_lat2 = ifelse(latitude.y < 0, latitude.y*-1, latitude.y)) %>% 
  mutate(abs_lat = ifelse(is.na(abs_lat), abs_lat2, abs_lat))


write_csv(n.long_lat2, "data-processed/n.long_lat2.csv")


## ok here, fix the g to mg issue in the bogard refs
n.long_lat2 <- read_csv("data-processed/n.long_lat2.csv")

n.long_lat3 <- n.long_lat2 %>% 
  mutate(concentration = ifelse(grepl("Bogard, J.R., Thilsted, S.H., Marks, G.C., Wahab", ref_info) & nutrient %in% c("epa", "dha"), concentration/1000, concentration))
  
write_csv(n.long_lat3, "data-processed/n.long_lat3.csv")

## prep the data
mod_all <- n.long_lat2 %>% 
  filter(concentration > 0) %>% 
  mutate(anacat = ifelse(subgroup != "finfish", "non-migratory", anacat)) %>% 
  filter(!is.na(bulk_max_length), !is.na(bulk_trophic_level), !is.na(feeding_level), !is.na(feeding_mode), !is.na(abs_lat)) %>% 
  mutate(log_length = log(bulk_max_length),
         log_concentration = log(concentration)) %>% 
  filter(!grepl("^Mohanty, B. P.,", ref_info))
 

## find where anacat is missing
n.long_lat2 %>% 
  filter(concentration > 0) %>% 
  filter(!is.na(bulk_max_length), !is.na(bulk_trophic_level), !is.na(feeding_level), !is.na(feeding_mode), !is.na(abs_lat)) %>% 
  mutate(log_length = log(bulk_max_length),
         log_concentration = log(concentration)) %>% 
  filter(subgroup != "finfish") %>% 
  filter(!is.na(anacat)) %>% View


table(mod_all$anacat)

### onto models!
mod <- mod_all %>% 
  filter(nutrient == "fe_mg")


mod1 <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 
mod2 <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, data = mod), standardize.y = TRUE) 
mod3 <- standardize(lm(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 
mod4 <- standardize(lm(log_concentration ~ bulk_trophic_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 
mod5 <- standardize(lm(log_concentration ~ log_length + feeding_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 
mod6 <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_level, data = mod), standardize.y = TRUE) 
mod7 <- standardize(lm(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 


mod8 <- standardize(lm(log_concentration ~ bulk_trophic_level + feeding_level + feeding_mode + abs_lat, data = mod), standardize.y = TRUE) 

model.sel(mod1, mod2, mod3, mod4, mod5, mod6, mod7)
confint(mod2)
summary(mod1)
summary(mod2)
summary(model.avg(mod1, mod2))
confint(model.avg(mod1, mod2))


ca_CI_average <- rownames_to_column(as.data.frame(confint(mod2), var = "term")) %>% 
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`,
         term = rowname)

ca_slopes_average <- enframe(coef(mod2), name = "term", value = "slope")

ca_results <- left_join(ca_CI_average, ca_slopes_average) %>% 
  mutate(nutrient = "calcium")


ca_results_plot <- ggplot(data = ca_results, aes(x = term, y = slope)) + geom_point(size = 3) + 
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.1) +
  coord_flip() + theme_bw()

### zn

zn_slopes_average <- enframe(coef(model.avg(mod1, mod2)), name = "term", value = "slope")


zn_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(mod1, mod2))), var = "term") %>% 
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`)


zn_results <- left_join(zn_CI_average, zn_slopes_average) %>%
  mutate(nutrient = "zinc")


zn_results_plot <- ggplot(data = zn_results, aes(x = term, y = slope)) + geom_point(size = 3) + 
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.1) +
  coord_flip() + theme_bw()



## iron results

fe_slopes_average <- enframe(coef(model.avg(mod1, mod2)), name = "term", value = "slope")


fe_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(mod1, mod2))), var = "term") %>% 
  rename(conf_low = `2.5 %`,
         conf_high = `97.5 %`)


fe_results <- left_join(fe_CI_average, fe_slopes_average) %>% 
  mutate(nutrient = "iron")


## all microelements 

microelements <- bind_rows(fe_results, zn_results, ca_results) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(term = str_replace(term, "z.log_length", "body size (log length)")) %>% 
  mutate(term = str_replace(term, "z.bulk_trophic_level", "fractional trophic position")) %>% 
  mutate(term = str_replace(term, "feeding_modefiltering plankton", "plankton feeder")) %>% 
  mutate(term = str_replace(term, "feeding_modegrazing on aquatic plants", "herbivore grazer")) %>% 
  mutate(term = str_replace(term, "feeding_modeselective plankton feeding", "selective filter feeder")) %>%
  mutate(term = str_replace(term, "feeding_modevariable", "variable feeding mode")) %>%
  mutate(term = str_replace(term, "z.abs_lat", "absolute latitude")) %>%
  mutate(term = ifelse(term == "feeding_levelplants/detritus+animals (troph. 2.2-2.79)", "omnivore", term)) %>%
  mutate(term = ifelse(term == "feeding_levelmainly animals (troph. 2.8 and up)", "carnivore", term)) %>% 
  mutate(term = ifelse(term == "feeding_modehunting macrofauna (predator)", "active predator", term))
  

ggplot(data = microelements, aes(x = term, y = slope)) + geom_point(size = 3) + 
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.1) +
  coord_flip() + theme_bw() + facet_wrap( ~ nutrient)
ggsave("trait_coefficients.png", width = 12, height = 6)

## fix the term names
microelements %>% 
distinct(term)



data <- n.long_lat2 %>% 
  filter(concentration > 0) %>% 
  # filter(!is.na(bulk_max_length), !is.na(bulk_trophic_level), !is.na(feeding_level), !is.na(feeding_mode), !is.na(abs_lat)) %>% 
  mutate(log_length = log(bulk_max_length),
         log_concentration = log(concentration)) %>% 
  filter(!grepl("^Mohanty, B. P.,", ref_info)) %>% 
  filter(nutrient == "zn_mg")


modfit <- standardize(lm(log_concentration ~ log_length + bulk_trophic_level + feeding_level + abs_lat, data = data), standardize.y = TRUE)
summary(modfit)
confint(modfit)

model <- lm(log_concentration ~ log_length + bulk_trophic_level + feeding_mode + abs_lat, data = mod)
summary(model)
arm::standardize(model)


### body size plots!
n.long %>% 
  # filter(nutrient == "ca_mg") %>% 
  filter(concentration > 0) %>%
  filter(nutrient %in% c("fe_mg", "ca_mg", "zn_mg")) %>% 
  group_by(nutrient) %>% 
ggplot(aes(y = log(concentration), x = log(bulk_mean_length)), data = .) + geom_point(size = 3, alpha = 0.5, color = "blue") + geom_smooth(method = "lm") +
  theme_bw() + ylab("ln nutrient concentration") + xlab("ln body length") + facet_wrap( ~ nutrient, scales = "free")
ggsave("figures/microelements_v_bodysize.png", width = 12, height = 4)


n.long %>% 
  # filter(nutrient == "ca_mg") %>% 
  filter(concentration > 0) %>%
  filter(nutrient %in% c("fe_mg", "ca_mg", "zn_mg")) %>% 
  group_by(nutrient) %>% 
  ggplot(aes(y = log(concentration), x = abs_lat), data = .) + geom_point(size = 3, alpha = 0.5, color = "blue") + geom_smooth(method = "lm") +
  theme_bw() + ylab("ln nutrient concentration") + xlab("absolute latitude") + facet_wrap( ~ nutrient, scales = "free")
ggsave("figures/microelements_v_latitude.png", width = 12, height = 4)

n.long %>% 
  # filter(nutrient == "ca_mg") %>% 
  filter(concentration > 0) %>%
  filter(nutrient %in% c("fe_mg", "ca_mg", "zn_mg")) %>% 
  group_by(nutrient) %>% 
  ggplot(aes(y = log(bulk_mean_length), x = abs_lat), data = .) + geom_point(size = 3, alpha = 0.5, color = "blue") + geom_smooth(method = "lm") +
  theme_bw() + ylab("ln body length") + xlab("absolute latitude") + facet_wrap( ~ nutrient, scales = "free")
ggsave("figures/size_v_latitude.png", width = 12, height = 4)




n.long %>% 
  filter(nutrient == "ca_mg") %>% 
  filter(concentration > 0) %>% 
  lm(log(concentration) ~ log(bulk_mean_length) + subgroup, data = .) %>% summary
  tidy(., conf.int = TRUE) %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) + geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) + 
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# zinc model comparisons! ------------------------------------------------------

n.long_nutrient <- n.long %>% 
  filter(nutrient == "zn_mg") %>% 
  filter(concentration > 0) %>% 
  filter(!is.na(bulk_mean_length), !is.na(bulk_trophic_level), !is.na(feeding_mode), !is.na(feeding_level), !is.na(abs_lat))


ggplot(data = n.long_nutrient, aes(x = feeding_level, y = bulk_trophic_level)) + geom_point()

mod_global <- lm(log(concentration) ~ log(bulk_mean_length) + abs_lat + bulk_trophic_level + feeding_mode + feeding_level, data = n.long_nutrient, na.action = na.fail)

dd <- dredge(mod_global)
summary(get.models(dd, 1)[[1]])
summary(model.avg(dd, subset = cumsum(weight) <= .95))
confint(model.avg(dd, subset = cumsum(weight) <= .95))
zn_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(dd, subset = cumsum(weight) <= .95))), var = "term")
zn_slopes_average <- enframe(coef(model.avg(dd, subset = cumsum(weight) <= .95)), name = "term", value = "slope")

zn_mod_out <- left_join(zn_CI_average, zn_slopes_average) %>% 
  rename(conf.low = `2.5 %`,
         conf.high = `97.5 %`) %>% 
  filter(term != "(Intercept)") %>% 
  arrange(desc(slope)) %>% 
  mutate(nutrient = "zinc")

ggplot(data = mod_out, aes(x = term, y = slope)) + geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  geom_hline(yintercept = 0) + coord_flip()



#'Best' model
confint(get.models(dd, 1)[[1]])
summary(get.models(dd, 1)[[1]])


# calcium model comparisons! ------------------------------------------------------

n.long_nutrient <- n.long %>% 
  filter(nutrient == "ca_mg") %>% 
  filter(concentration > 0) %>% 
  filter(!is.na(bulk_mean_length), !is.na(bulk_trophic_level), !is.na(feeding_mode), !is.na(anacat), !is.na(abs_lat), !is.na(feeding_level))

mod_global <- lm(log(concentration) ~ log(bulk_mean_length) + feeding_level + abs_lat + bulk_trophic_level + feeding_mode + anacat, data = n.long_nutrient, na.action = na.fail)

dd <- dredge(mod_global)
subset(dd, delta < 4)
summary(get.models(dd, 1)[[1]])

#or as a 95% confidence set:
summary(model.avg(dd, subset = cumsum(weight) <= .95)) # get averaged coefficients
ca_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(dd, subset = cumsum(weight) <= .95))), var = "term")
ca_slopes_average <- enframe(coef(model.avg(dd, subset = cumsum(weight) <= .95)), name = "term", value = "slope")



ca_mod_out <- left_join(ca_CI_average, ca_slopes_average) %>% 
  rename(conf.low = `2.5 %`,
         conf.high = `97.5 %`) %>% 
  filter(term != "(Intercept)") %>% 
  arrange(desc(slope)) %>% 
  mutate(nutrient = "calcium")

ggplot(data = mod_out, aes(x = term, y = slope)) + geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  geom_hline(yintercept = 0) + coord_flip()



# iron model comparisons! ------------------------------------------------------
n.long_nutrient <- n.long %>% 
  filter(nutrient == "fe_mg") %>% 
  filter(concentration > 0) %>% 
  filter(!is.na(bulk_mean_length), !is.na(bulk_trophic_level), !is.na(feeding_mode), !is.na(anacat), !is.na(abs_lat), !is.na(feeding_level))

mod_global <- lm(log(concentration) ~ log(bulk_mean_length) + feeding_level + abs_lat + bulk_trophic_level + feeding_mode + anacat, data = n.long_nutrient, na.action = na.fail)

dd <- dredge(mod_global)
summary(get.models(dd, 1)[[1]])
confint(get.models(dd, 1)[[1]])
summary(get.models(dd, 1)[[1]])
summary(model.avg(dd, subset = cumsum(weight) <= .95))
confint(model.avg(dd, subset = cumsum(weight) <= .95))

#or as a 95% confidence set:
# summary(model.avg(dd, subset = cumsum(weight) <= .95)) # get averaged coefficients
fe_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(dd, subset = cumsum(weight) <= .95))), var = "term")
fe_slopes_average <- enframe(coef(model.avg(dd, subset = cumsum(weight) <= .95)), name = "term", value = "slope")

fe_mod_out <- left_join(fe_CI_average, fe_slopes_average) %>% 
  rename(conf.low = `2.5 %`,
         conf.high = `97.5 %`) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(nutrient = "iron")

ggplot(data = mod_out, aes(x = term, y = slope)) + geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  geom_hline(yintercept = 0) + coord_flip()

#'Best' model
confint(get.models(dd, 1)[[1]])
summary(get.models(dd, 1)[[1]])

# epa model comparisons! ------------------------------------------------------

n.long_nutrient <- n.long %>% 
  filter(nutrient == "epa") %>% 
  filter(concentration > 0) %>% 
  filter(!is.na(bulk_mean_length), !is.na(bulk_trophic_level), !is.na(feeding_mode), !is.na(anacat), !is.na(abs_lat), !is.na(feeding_level))

mod_global <- lm(log(concentration) ~ log(bulk_mean_length) + feeding_level + abs_lat + bulk_trophic_level + feeding_mode + anacat, data = n.long_nutrient, na.action = na.fail)

dd <- dredge(mod_global)
# subset(dd, delta < 4)

#or as a 95% confidence set:
# summary(model.avg(dd, subset = cumsum(weight) <= .95)) # get averaged coefficients
epa_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(dd, subset = cumsum(weight) <= .95))), var = "term")
epa_slopes_average <- enframe(coef(model.avg(dd, subset = cumsum(weight) <= .95)), name = "term", value = "slope")


epa_mod_out <- left_join(epa_CI_average, epa_slopes_average) %>% 
  rename(conf.low = `2.5 %`,
         conf.high = `97.5 %`) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(nutrient = "EPA")

ggplot(data = mod_out, aes(x = term, y = slope)) + geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  geom_hline(yintercept = 0) + coord_flip()

#'Best' model
confint(get.models(dd, 1)[[1]])
summary(get.models(dd, 1)[[1]])


# dha model comparisons! ------------------------------------------------------

n.long_nutrient <- n.long %>% 
  filter(nutrient == "dha") %>% 
  filter(concentration > 0) %>% 
  filter(!is.na(bulk_mean_length), !is.na(bulk_trophic_level), !is.na(feeding_mode), !is.na(anacat), !is.na(abs_lat), !is.na(feeding_level))

mod_global <- lm(log(concentration) ~ log(bulk_mean_length) + feeding_level + abs_lat + bulk_trophic_level + feeding_mode + anacat, data = n.long_nutrient, na.action = na.fail)


dd <- dredge(mod_global)
# subset(dd, delta < 4)

#or as a 95% confidence set:
# summary(model.avg(dd, subset = cumsum(weight) <= .95)) # get averaged coefficients
dha_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(dd, subset = cumsum(weight) <= .95))), var = "term")
dha_slopes_average <- enframe(coef(model.avg(dd, subset = cumsum(weight) <= .95)), name = "term", value = "slope")



dha_mod_out <- left_join(dha_CI_average, dha_slopes_average) %>% 
  rename(conf.low = `2.5 %`,
         conf.high = `97.5 %`) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(nutrient = "DHA")

ggplot(data = mod_out, aes(x = term, y = slope)) + geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  geom_hline(yintercept = 0) + coord_flip()

#'Best' model
confint(get.models(dd, 1)[[1]])
summary(get.models(dd, 1)[[1]])


# protein model comparisons! ------------------------------------------------------

n.long_nutrient <- n.long %>% 
  filter(nutrient == "protcnt_g") %>% 
  filter(concentration > 0) %>% 
  filter(!is.na(bulk_mean_length), !is.na(bulk_trophic_level), !is.na(feeding_mode), !is.na(anacat), !is.na(abs_lat), !is.na(feeding_level))

mod_global <- lm(log(concentration) ~ log(bulk_mean_length) + feeding_level + abs_lat + bulk_trophic_level + feeding_mode + anacat, data = n.long_nutrient, na.action = na.fail)

dd <- dredge(mod_global)

table(n.long_nutrient$anacat)
length(unique(n.long_nutrient$species_name))

#or as a 95% confidence set:
# summary(model.avg(dd, subset = cumsum(weight) <= .95)) # get averaged coefficients
prot_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(dd, subset = cumsum(weight) <= .95))), var = "term")
prot_slopes_average <- enframe(coef(model.avg(dd, subset = cumsum(weight) <= .95)), name = "term", value = "slope")

prot_mod_out <- left_join(prot_CI_average, prot_slopes_average) %>% 
  rename(conf.low = `2.5 %`,
         conf.high = `97.5 %`) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(nutrient = "protein")

ggplot(data = mod_out, aes(x = term, y = slope)) + geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  geom_hline(yintercept = 0) + coord_flip()

#'Best' model
confint(get.models(dd, 1)[[1]])
summary(get.models(dd, 1)[[1]])


# fat model comparisons! ------------------------------------------------------

n.long_nutrient <- n.long %>% 
  filter(nutrient == "fat_g") %>% 
  filter(concentration > 0) %>% 
  filter(!is.na(bulk_mean_length), !is.na(bulk_trophic_level), !is.na(feeding_mode), !is.na(anacat), !is.na(abs_lat), !is.na(feeding_level))

mod_global <- lm(log(concentration) ~ log(bulk_mean_length) + feeding_level + abs_lat + bulk_trophic_level + feeding_mode + anacat, data = n.long_nutrient, na.action = na.fail)

dd <- dredge(mod_global)

table(n.long_nutrient$anacat)
length(unique(n.long_nutrient$species_name))

#or as a 95% confidence set:
# summary(model.avg(dd, subset = cumsum(weight) <= .95)) # get averaged coefficients
fat_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(dd, subset = cumsum(weight) <= .95))), var = "term")
fat_slopes_average <- enframe(coef(model.avg(dd, subset = cumsum(weight) <= .95)), name = "term", value = "slope")

fat_mod_out <- left_join(fat_CI_average, fat_slopes_average) %>% 
  rename(conf.low = `2.5 %`,
         conf.high = `97.5 %`) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(nutrient = "fat")

ggplot(data = mod_out, aes(x = term, y = slope)) + geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  geom_hline(yintercept = 0) + coord_flip()

#'Best' model
confint(get.models(dd, 1)[[1]])
summary(get.models(dd, 1)[[1]])

# plot all coefficients ---------------------------------------------------

coefs <- bind_rows(ca_mod_out, fe_mod_out, zn_mod_out, epa_mod_out, dha_mod_out, prot_mod_out, fat_mod_out) 


unique(coefs$term)


ggplot(data = coefs, aes(x = term, y = slope)) + geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  geom_hline(yintercept = 0) + coord_flip() + facet_wrap( ~ nutrient)
