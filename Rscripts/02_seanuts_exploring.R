## Playing around with the giant seanuts dataframe
## Joey Bernhardt
## Last updated Nov 15 2016


# load libraries ----------------------------------------------------------


library(tidyverse)
library(janitor)
library(stringr)


# read in data ------------------------------------------------------------


seanuts <- read_csv("data-processed/all_nuts.csv")
ntbl2 <- read_csv("data/ntbl2.csv")
dec19_raw <- read_csv("~/Documents/Nutrient_Analysis/data/nut_dec19.csv")
inf_fish_raw <- read_csv("data/INF_fish.csv") ## this is the version that has different data than the dec19 version. Could merge it in?
anf_raw <- read_csv("/Users/Joey/Desktop/Nutrient_databases/AnFooD1.0_fish.csv") ## I think this is version of infoods that dec_19 is based on
anf_raw <- read_excel("data/AnFooD1.1.xlsx", sheet = "09 Fish & Shellfish")
nutrients_raw <- read_csv("data/Nutrient_data_October2016.csv")



# clean up the data a bit -------------------------------------------------

nutrients <- nutrients_raw %>% 
  clean_names()

names(nutrients)

length(unique(nutrients$asfis_sci_name))

# explore the ‘nutrients' more --------------------------------------------------

nutrients %>% 
  filter(database != "INFOODS") %>% View

# cleaning up the micronutrient data --------------------------------------

length(!is.na(nutrients$ca_mg))
class(nutrients$ca_mg)

unique(nutrients$ca_mg)
str_subset(nutrients2$ca_mg, "-")

## noticing that there are some entries in the ca_mg that are reported as ranges, so I'm taking an average
nutrients2 <- nutrients %>% 
  mutate(ca_mg = str_replace(ca_mg, "13-25", "19")) %>% 
  mutate(ca_mg = str_replace(ca_mg, "17-32", "24.5")) %>%
  mutate(ca_mg = str_replace(ca_mg, "6.27-11", "8.635")) %>%
  mutate(ca_mg = as.numeric(ca_mg)) %>% 
  arrange(desc(ca_mg)) 

nutrients3 <- nutrients2 %>% 
  filter(food_item_id != "0902188" | is.na(food_item_id)) ## this looks like an outlier? I'm removing it for now

hist(log(nutrients3$ca_mg))

summary(nutrients3$ca_mg)


# cleaning up the iron ----------------------------------------------------

unique(nutrients3$fe_mg)
str_subset(nutrients3$fe_mg, "-")

## replacing ranges with the average value
nutrients4 <- nutrients3 %>% 
  mutate(fe_mg = str_replace(fe_mg, "3-3", "3")) %>% 
  mutate(fe_mg = str_replace(fe_mg, "3-6", "4.5")) %>%
  mutate(fe_mg = str_replace(fe_mg, "0.223-0.43", "0.3265")) %>% 
  mutate(fe_mg = as.numeric(fe_mg)) %>% 
  arrange(desc(fe_mg)) %>% 
  select(fe_mg, fe_units, everything())

nutrients4$fe_mg[!is.na(nutrients4$fe_mg)]

hist(nutrients4$fe_mg)

summary(nutrients4$fe_mg)

names(nutrients4)


# now look at other data tables -------------------------------------------


anf <- anf_raw %>% 
  slice(2:n()) %>% 
  clean_names()

unique(anf$type)

dec19 <- dec19_raw %>% 
  clean_names()

inf_fish <- inf_fish_raw %>% 
  clean_names() %>% 
  mutate(type_farmed_wild = str_replace(type_farmed_wild, "F", "farmed")) %>%
  filter(type_farmed_wild != "farmed" | is.na(type_farmed_wild))

anf <- anf %>% 
  mutate(type = str_replace(Type, "F", "farmed"))

anf <- anf %>% 
  filter(type != "farmed" | is.na(type)) %>% 
  # filter(is.na(type) | type == "W") %>% 
  filter(Processing == "r")


dec19_foods <- unique(dec19$food_name_in_english)
nutrients_foods <- unique(nutrients4$food_name_in_english)
inf_foods <-  unique(inf_fish$food_name_in_english)
seanuts_foods <- unique(seanuts$food_name_in_english.x)
anf_foods <- unique(anf$food_name_in_english)

length(dec19_foods)

length(intersect(dec19_foods, nutrients_foods))
setdiff(nutrients_foods, dec19_foods)

str_detect(dec19_foods, ",")

### make a new food_name_in_english col that will match up!
dec19_foods <- unique(dec19$food_name_in_english)
nutrients_foods <- unique(nutrients4$food_name_in_english)



dec19foods2 <- str_replace_all(dec19_foods, '[,]', "_") 
dec19foods3 <- str_replace_all(dec19foods2, "[ ]", "_") 
dec19foods4 <- str_replace_all(dec19foods3, "__", "_") 
dec19foods5 <- str_replace_all(dec19foods4, "__", "_") 



nutrients_foods2 <- str_replace_all(nutrients_foods, '[,]', "_") 
nutrients_foods3 <- str_replace_all(nutrients_foods2, '[()]', "_") 
nutrients_foods4 <- str_replace_all(nutrients_foods3, '[ ]', "_") 
nutrients_foods5 <- str_replace_all(nutrients_foods4, "__", "_")
nutrients_foods6 <- str_replace_all(nutrients_foods5, "__", "_") 


str_subset(dec19foods5, "Yelloweye_rockfish")
str_subset(nutrients_foods6, "Yelloweye_rockfish")

nutrients_foods6
dec19foods5

setdiff(dec19foods5, nutrients_foods6) ### at this point it looks like we are only two species different between these two lists
intersect(dec19foods5, nutrients_foods6)


# go about doing these name changes in the full files ---------------------


dec19_foods <- unique(dec19$food_name_in_english)
nutrients_foods <- unique(nutrients4$food_name_in_english)

dec19_working <- dec19

dec19_working <- dec19_working %>% 
  mutate(food_name_clean = food_name_in_english) %>% 
  mutate(food_name_clean = str_replace_all(food_name_clean, '[,]', "_")) %>% 
  mutate(food_name_clean = str_replace_all(food_name_clean, "[ ]", "_")) %>% 
  mutate(food_name_clean = str_replace_all(food_name_clean, "__", "_")) %>% 
  mutate(food_name_clean = str_replace_all(food_name_clean, "__", "_")) 

nutrients_working <- nutrients4

nutrients_working <- nutrients_working %>% 
  mutate(food_name_clean = food_name_in_english) %>% 
  mutate(food_name_clean = str_replace_all(food_name_clean, '[,]', "_")) %>% 
  mutate(food_name_clean = str_replace_all(food_name_clean, '[()]', "_")) %>% 
  mutate(food_name_clean = str_replace_all(food_name_clean, '[ ]', "_")) %>% 
  mutate(food_name_clean = str_replace_all(food_name_clean, "__", "_")) %>% 
  mutate(food_name_clean = str_replace_all(food_name_clean, "__", "_")) 
  


# now try and join them! --------------------------------------------------

nuts_names <- unique(nutrients_working$food_name_clean)
dec_names <- unique(dec19_working$food_name_clean)
intersect(nuts_names, dec_names)
length(nuts_names)
length(dec_names)


nutrients_working$food_item_id <- as.character(nutrients_working$food_item_id)
dec19_working$food_item_id <- as.character(dec19_working$food_item_id)


str(nutrients_working)
nutrients_working$tl<- as.character(nutrients_working$food_item_id)
dec19_working$food_item_id <- as.character(dec19_working$food_item_id)



nuts_sel <- nutrients_working %>% 
  select(food_item_id, database, food_name_clean, nutrient_ref, biblioid)

dec_sel <- dec19_working %>% 
  select(food_item_id, food_name_clean)

anti_join(nuts_sel, dec_sel) %>% View


all_nuts <- left_join(dec19_working, nuts_sel)
 


# write out the latest most complete csv of the db ------------------------
write_csv(all_nuts, "data-processed/all_nutrients.csv")





# Nov 15 2016, now let’s check out that infoods data --------------------------

View(inf_fish_raw)

### pull in the most clean of of the infoods data set

all_nuts_inf <- read_csv("data-processed/all_nuts.csv")
class(all_nuts_inf$ca_mg)
class(all_nuts$ca_mg)

### ok, now let's merge them ALLLLL together :) Although at this point we are still missing the rows that were in nutrients_working
### that didn't make it into the dec19 file (i.e. the ones that weren't in any of the existing databases)

nuts_all <- full_join(all_nuts, all_nuts_inf, by = "food_item_id")

### ok, this is a super messy dataframe, but it's got almost everything we want at this point. Now onto cleaning it in a different file!

write_csv(nuts_all, "data-processed/nuts_all_messy.csv")





# extract the CINE rows from nutrients ------------------------------------

CINE <- nutrients %>% 
  filter(database == "CINE") %>% 
  select(1:13, 20:25, 28:111) 

write_csv(CINE, "data-processed/CINE.csv")


# extra code, not organized, but maybe useful! ----------------------------

## checking out the latest full ntbl dataset, which is the dec 19 version
str(dec19)
length(!is.na(dec19$food_name_in_english))
unique(dec19$food_name_in_english)

filter_join1 <- left_join(dec19, inf_fish, by = "food_name_in_english")
semi_join <- semi_join(dec19, inf_fish, by = "food_name_in_english")
filter_join <- left_join(dec19, inf_fish, by = "country_region")


### let's try this with the nutrients data file
filter_join1 <- left_join(dec19, inf_fish, by = "food_name_in_english")
semi_join <- semi_join(dec19, inf_fish, by = "food_name_in_english")
filter_join <- left_join(dec19, inf_fish, by = "country_region")



names(dec19)
Encoding(seanuts$subgroup)
setdiff(dec19_foods, inf_foods) ## these are in dec19_foods but not in anf_foods
intersect(dec19_foods, inf_foods) 
str_subset(dec19_foods, "bream")
str_subset(anf_foods, "Sharpsnout sea bream")

length(unique(anf$food_name_in_english))
length(unique(dec19$food_name_in_english))
str_subset(anf$food_item_id, "0901284")
str_subset(anf$country_region, "Iceland, North Atlantic")



dec19_foods
inf_foods
str_subset(inf_foods, "Lobster, wild, raw")
str_subset(dec19_foods, "Chinook salmon, wild, fillet, raw")
seanuts_foods[!str_detect(seanuts_foods, "raw")]
dec19_foods[!str_detect(dec19_foods, "raw")]


str_subset(seanuts_foods, "raw")




names(seanuts)

unique(filter_join$processing.x)
unique(inf_fish$processing)
unique(dec19$food_name_in_english)
foods_dec19 <- (unique(dec19$food_item_id))
foods_inf <- (unique(inf_fish$food_item_id))
foods_anf <- (unique(anf$food_item_id))
setdiff(foods_anf, foods_dec19)

anf_ca <- anf %>%
  filter(!is.na(`CA(mg)`)) %>%
  rename(ca_mg = `CA(mg)`)

dec_19_ca <- dec19 %>%
  filter(!is.na(CA_mg)) %>%
  rename(ca_mg = CA_mg)
names_dec <- names(dec_19_ca)

str_subset(names_dec, "Bib")
bibs <- unique(dec_19_ca$BiblioID.y)
bib2 <- unique(anf_ca$BiblioID)
intersect(bibs, bib2)
sum(is.na(dec_19_ca$BiblioID.y))

ntbl_sp <- unique(ntbl2$species)
seanuts_sp <- unique(seanuts$asfis_scientific_name_fishbase_swap.y)
str(ntbl_sp)
str(seanuts_sp)

fish_list <- intersect(seanuts_sp, ntbl_sp)
fish_list <- intersect(ntbl_sp, seanuts_sp)

?setdiff

fish_list
str_subset(ntbl2$species, "Pisaster ochraceus")


ntbl2 %>% 
  filter(species %in% fish_list) %>% View


inf_fish <- read_csv("data/INF_fish.csv")

nuts <- nut_dec19 %>% 
  rename(food_name = Food.name.in.English)
inf_nuts <- inf_fish %>% 
  rename(food_name = `Food name in English`) %>% 
  rename(food_item_id_nuts = `Food Item ID`)

both_join <- left_join(inf_nuts, nuts, by = "food_name")
length(unique(as.character(both_join$`Scientific name`)))
length(unique(as.character(both_join$`CA(mg)`)))
