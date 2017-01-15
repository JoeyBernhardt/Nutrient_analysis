### Seanuts traits analysis


# load packages ----------------------------------------------------------

library(tidyverse)
library(stringr)
library(purrr)
library(janitor)
library(MuMIn)
library(broom)

# load data ---------------------------------------------------------------

seanuts_ecology <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data-processed/seanuts_select_8.csv")

## ok let's just get rid of the one super outlier ca and fe measurement for now

seanuts_ecology$ca_mg[seanuts_ecology$ca_mg == 41206.00000] <- NA
seanuts_ecology$fe_mg[seanuts_ecology$fe_mg > 40939.00000] <- NA


# explore the dataset a bit -----------------------------------------------


names_seanuts <- names(seanuts_ecology)
str_subset(names_seanuts, "length")
str_subset(names_seanuts, "Length")
str_subset(names_seanuts, "sl")
str_subset(names_seanuts, "BrackishWater")

sum(!is.na(seanuts_ecology$Length))
sum(!is.na(seanuts_ecology$slmax))
sum(!is.na(seanuts_ecology$length_from_study))
sum(!is.na(seanuts_ecology$CommonLength))





## let's figure out which length measurement makes sense to use

ggplot(aes(x = Length, y = slmax_nov28), data = seanuts_ecology) + geom_point() +
  geom_abline(slope = 1, intercept = 0)

seanuts_ecology %>% 
  filter(is.na(Length) | is.na(slmax) | is.na(slmax_nov28)) %>% 
  select(Length, slmax, slmax_nov28, everything()) %>% View

## ok let's take an average of all the length measurements and use that for now

seanuts_2 <- seanuts_ecology %>% 
  rowwise() %>% 
  mutate(avg_length = mean(c(Length, slmax), na.rm = TRUE)) %>% 
  select(avg_length, Length, slmax, everything()) %>% 
  clean_names()

seanuts_ecology <- seanuts_ecology %>% 
  clean_names()

### checking to see which species we don't have length data for:
no_length <- seanuts_2 %>% 
  filter(is.na(avg_length)) %>%
  distinct(species_name, .keep_all = TRUE) %>% 
  select(species_name, everything())

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


n.long <- seanuts_2 %>% 
  dplyr::select(species_name, subgroup, prot_g, protcnt_g, epa, dha, ca_mg, fat_g,
                zn_mg, fe_mg, slmax, seanuts_id2, tl, food_item_id_2,
                Length, avg_length, abs_lat, Herbivory2, DemersPelag, contains("Brack"), Marine, Fresh, FeedingType, contains("troph"), contains("length"), contains("weight"), contains("ana")) %>% 
  gather(key = "nutrient", value = "concentration", prot_g, protcnt_g, epa, dha, ca_mg, fat_g, zn_mg, fe_mg) %>% 
  filter(!is.na(concentration)) %>% 
  mutate(subgroup = as.factor(subgroup)) %>% 
  mutate(herbivory = as.factor(Herbivory2)) %>% 
  clean_names()

ggplot(aes(x = foodtroph, y = tl), data = n.long) + geom_point()

summary(sub$subgroup)

n.long %>% 
  filter(nutrient == "ca_mg") %>% 
  filter(concentration > 0) %>% 
  lm(log(concentration) ~ log(avg_length) + abs_lat + foodtroph + herbivory2 + anacat + feedingtype, data = .) %>% 
  tidy(., conf.int = TRUE) %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) + geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

n.long %>% 
  filter(nutrient == "fe_mg") %>% 
  filter(concentration > 0) %>% 
  lm(log(concentration) ~ log(avg_length) + FoodTroph + abs_lat, data = .) %>% 
  summary


# model comparisons! ------------------------------------------------------



model.sel(mod1, mod2, mod3, mod1b, mod1c, mod1d, mod1e, mod1f, mod1g)

mod1 <- n.long %>% 
  filter(nutrient == "ca_mg") %>% 
  filter(concentration > 0) %>% 
   lm(log(concentration) ~ log(avg_length) + abs_lat + foodtroph + herbivory2 + anacat + feedingtype, data = .)

summary(mod1)

mod1g <- n.long %>% 
  filter(nutrient == "ca_mg") %>% 
  filter(concentration > 0) %>% 
  lm(log(concentration) ~ log(avg_length) + abs_lat + herbivory2 + anacat + feedingtype, data = .)


 mod1f <- n.long %>% 
   filter(nutrient == "ca_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ log(avg_length) + foodtroph + herbivory2 + anacat + feedingtype, data = .)
 
 mod1e <- n.long %>% 
   filter(nutrient == "ca_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ abs_lat + foodtroph + herbivory2 + anacat + feedingtype, data = .)
 
 mod1b <- n.long %>% 
   filter(nutrient == "ca_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ log(avg_length) + abs_lat + foodtroph + herbivory2 + anacat, data = .)
 
 mod1c <- n.long %>% 
   filter(nutrient == "ca_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ log(avg_length) + abs_lat + foodtroph + herbivory2 + feedingtype, data = .)
 
 
 mod1d <- n.long %>% 
   filter(nutrient == "ca_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ log(avg_length) + foodtroph + herbivory2 + anacat + feedingtype, data = .)
 

 mod2 <- n.long %>% 
   filter(nutrient == "ca_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ log(avg_length) + abs_lat + foodtroph + herbivory2, data = .)
 
 mod3 <- n.long %>% 
   filter(nutrient == "ca_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ log(avg_length) + abs_lat + foodtroph, data = .)
 
 
 
 
# now for zinc ------------------------------------------------------------

 
 model.sel(mod2, mod3, mod1b, mod1c, mod1d, mod1e, mod1f, mod1g, mod1v)
 mod.avg <- model.avg(mod1g, mod1, mod1e)
 
 confint(mod.avg)
 
 summary(mod1g)
 summary(mod1e)
 
 tidy(mod1g, conf.int = TRUE)
 
 mod1v <- n.long %>% 
   filter(nutrient == "zn_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ log(avg_length) + abs_lat + foodtroph + herbivory2 + anacat + feedingtype, data = .)

 
 
 mod1g <- n.long %>% 
   filter(nutrient == "zn_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ abs_lat + herbivory2 + anacat + feedingtype, data = .)
 
 
 mod1f <- n.long %>% 
   filter(nutrient == "zn_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ log(avg_length) + foodtroph + herbivory2 + anacat + feedingtype, data = .)
 
 mod1e <- n.long %>% 
   filter(nutrient == "zn_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ abs_lat + foodtroph + herbivory2 + anacat + feedingtype, data = .)
 
 mod1b <- n.long %>% 
   filter(nutrient == "zn_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ log(avg_length) + abs_lat + foodtroph + herbivory2 + anacat, data = .)
 
 mod1c <- n.long %>% 
   filter(nutrient == "zn_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ log(avg_length) + abs_lat + foodtroph + herbivory2 + feedingtype, data = .)
 
 
 mod1d <- n.long %>% 
   filter(nutrient == "zn_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ log(avg_length) + foodtroph + herbivory2 + anacat + feedingtype, data = .)
 
 
 mod2 <- n.long %>% 
   filter(nutrient == "zn_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ log(avg_length) + abs_lat + foodtroph + herbivory2, data = .)
 
 mod3 <- n.long %>% 
   filter(nutrient == "zn_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ log(avg_length) + abs_lat + foodtroph, data = .)
 
 
 # now for iron ------------------------------------------------------------
 
 
 model.sel(mod1, mod2, mod3, mod1b, mod1c, mod1d, mod1e, mod1f, mod1g, mod1gb)
 model.sel(mod1, mod1g, mod1e)
 
 summary(mod1gb)
 summary(mod1g)
 
 mod1 <- n.long %>% 
   filter(nutrient == "fe_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ log(avg_length) + abs_lat + foodtroph + herbivory2 + anacat + feedingtype, data = .)
 
 
 mod1gb <- n.long %>% 
   filter(nutrient == "fe_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ log(avg_length) + diettroph + abs_lat + herbivory2 + feedingtype + anacat, data = .)
 
confint_tidy(mod1g)
 
 
 
 mod1g <- n.long %>% 
   filter(nutrient == "fe_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ log(avg_length) +  abs_lat + herbivory2 + feedingtype + anacat, data = .)
 
 
 mod1f <- n.long %>% 
   filter(nutrient == "fe_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ log(avg_length) + foodtroph + herbivory2 + anacat + feedingtype, data = .)
 
 mod1e <- n.long %>% 
   filter(nutrient == "fe_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ abs_lat + foodtroph + herbivory2 + anacat + feedingtype, data = .)
 
 mod1b <- n.long %>% 
   filter(nutrient == "fe_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ log(avg_length) + abs_lat + foodtroph + herbivory2 + anacat, data = .)
 
 mod1c <- n.long %>% 
   filter(nutrient == "fe_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ log(avg_length) + abs_lat + foodtroph + herbivory2 + feedingtype, data = .)
 
 
 mod1d <- n.long %>% 
   filter(nutrient == "fe_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ log(avg_length) + foodtroph + herbivory2 + anacat + feedingtype, data = .)
 
 
 mod2 <- n.long %>% 
   filter(nutrient == "fe_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ log(avg_length) + abs_lat + foodtroph + herbivory2, data = .)
 
 mod3 <- n.long %>% 
   filter(nutrient == "fe_mg") %>% 
   filter(concentration > 0) %>% 
   lm(log(concentration) ~ log(avg_length) + abs_lat + foodtroph, data = .)
 
 
 species <- read_csv("data-processed/species_seanuts_new.csv")
 ecology <- read_csv("data-processed/ecology_seanuts_new.csv")
 names(species)
 names(ecology)
 
 glimpse(ecology)
 