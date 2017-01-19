### Seanuts traits analysis


# load packages ----------------------------------------------------------

library(tidyverse)
library(stringr)
library(purrr)
library(janitor)
library(MuMIn)
library(broom)

# load data ---------------------------------------------------------------

seanuts_ecology <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data-processed/seanuts_select_11.csv")

## ok let's just get rid of the one super outlier ca and fe measurement for now

seanuts_ecology$ca_mg[seanuts_ecology$ca_mg == 41206.00000] <- NA
seanuts_ecology$fe_mg[seanuts_ecology$fe_mg > 40939.00000] <- NA


# explore the dataset a bit -----------------------------------------------

## Here are the main variables we'll use in the traits analysis

seanuts_ecology %>% 
  select(species_name, contains("bulk"), contains("length")) %>% View

## let's figure out which length measurement makes sense to use


## ok let's take an average of all the length measurements and use that for now
# this is now obsolete, since I've gone back and fixed the missing lengths.

seanuts_2 <- seanuts_ecology %>% 
  clean_names() %>% 
  select(bulk_mean_length, bulk_max_length, everything())

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


library(rfishbase)
options(FISHBASE_API = "http://fishbase.ropensci.org/sealifebase")
species("Donax trunculus")
cor(seanuts_ecology$diettroph, seanuts_ecology$foodtroph)

ggplot(aes(x = diettroph, y = foodtroph), data = seanuts_ecology) + geom_point() +
  geom_abline(slope = 1, intercept = 0)


# check for completeness of other fishbase variables ----------------------

seanuts_2 %>% 
  # filter(is.na(feeding_mode)) %>% 
  select(species_name, feeding_mode, feeding_level, feeding_habit, herbivory2_x, herbivory2_y, everything()) %>% 
  arrange(species_name) %>% View



n.long <- seanuts_2 %>% 
  dplyr::select(species_name, subgroup, prot_g, protcnt_g, epa, dha, ca_mg, fat_g,
                zn_mg, fe_mg, contains("length"), seanuts_id2, tl, food_item_id_2,
                abs_lat, contains("feeding"), demerspelag, contains("Brack"), marine, fresh, contains("troph"), contains("weight"), contains("ana")) %>% 
  gather(key = "nutrient", value = "concentration", prot_g, protcnt_g, epa, dha, ca_mg, fat_g, zn_mg, fe_mg) %>% 
  filter(!is.na(concentration)) %>% 
  mutate(subgroup = as.factor(subgroup))

### fill in potentially missing trait values
n.long <- n.long %>% 
  mutate(bulk_trophic_level = ifelse(is.na(bulk_trophic_level), trophic_level, bulk_trophic_level)) %>% 
  mutate(bulk_mean_length = ifelse(is.na(bulk_mean_length), mean_length, bulk_mean_length)) %>% 
  mutate(bulk_max_length = ifelse(is.na(bulk_max_length), max_length, bulk_max_length))


n.long %>% 
  filter(nutrient == "fe_mg") %>% 
  filter(concentration > 0) %>% 
  lm(log(concentration) ~ log(bulk_mean_length) + abs_lat + bulk_trophic_level + feeding_mode + anacat, data = .) %>% 
  tidy(., conf.int = TRUE) %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) + geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) + 
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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


n.long %>% 
  filter(nutrient == "ca_mg") %>% 
  filter(concentration > 0) %>% 
ggplot(data = ., aes(x = log(bulk_max_length), y = log(concentration))) + geom_point() +
  geom_smooth(method = "lm")

n.long %>% 
  filter(nutrient == "ca_mg") %>% 
  filter(concentration > 0) %>% 
  lm(log(concentration) ~ log(bulk_max_length) + abs_lat + bulk_trophic_level + feeding_mode + anacat, data = .) %>% 
  summary


# zinc model comparisons! ------------------------------------------------------

n.long_nutrient <- n.long %>% 
  filter(nutrient == "zn_mg") %>% 
  filter(concentration > 0) %>% 
  filter(!is.na(bulk_mean_length), !is.na(bulk_trophic_level), !is.na(feeding_mode), !is.na(feeding_level), !is.na(anacat), !is.na(abs_lat))

mod_global <- lm(log(concentration) ~ log(bulk_mean_length) + abs_lat + bulk_trophic_level + feeding_mode + feeding_level + anacat, data = n.long_nutrient, na.action = na.fail)

dd <- dredge(mod_global)

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
# subset(dd, delta < 4)

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
