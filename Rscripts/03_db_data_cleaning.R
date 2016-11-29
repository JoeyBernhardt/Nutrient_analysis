#### Cleaning up the horrendously messy all_nuts_messy.csv, which contains all the nutrient data from databases to date
#### Last updated November 15 2016
#### Still need to bring in the non database data



# load libraries ----------------------------------------------------------

library(tidyverse)
library(stringr)
library(janitor)


# read in data ------------------------------------------------------------

all <- read_csv("data-processed/nuts_all_messy.csv")
CINE <- read_csv("data-processed/CINE.csv")
str(all)


# start by pulling out the data we will use for now -----------------------
# namely the minerals, fats and fatty acids

names_all <- names(all)
names_all
str_subset(names_all, "database")
all$database

# how many columns do we have that have calcium data?
str_subset(names_all, "ca") ## ca_mg.x, ca_mg.y

unique(all$ca_mg.x)
unique(all$ca_mg.y)

all$ca_mg.y <- as.numeric(all$ca_mg.y)

all_working <- all

all_working2 <- all_working %>% 
  unite(ca_mg, ca_mg.y, ca_mg.x, remove = TRUE) %>% 
  mutate(ca_mg = str_replace(ca_mg, "_NA", "")) %>% 
  mutate(ca_mg = str_replace(ca_mg, "NA_", "")) %>% 
  mutate(ca_mg = as.numeric(ca_mg))

# how many columns do we have that have iron data?
str_subset(names_all, "fe") ## fe_mg.x, fe_mg.y


all_working3 <- all_working2 %>% 
  unite(fe_mg, fe_mg.y, fe_mg.x, remove = TRUE) %>% 
  mutate(fe_mg = str_replace(fe_mg, "_NA", "")) %>% 
  mutate(fe_mg = str_replace(fe_mg, "NA_", "")) %>% 
  mutate(fe_mg = as.numeric(fe_mg))

sum(!is.na(all_working3$fe_mg))


# how many columns do we have that have zinc data?
str_subset(names_all, "zn") 


all_working4 <- all_working3 %>% 
  unite(zn_mg, zn_mg.y, zn_mg.x, remove = TRUE) %>% 
  mutate(zn_mg = str_replace(zn_mg, "_NA", "")) %>% 
  mutate(zn_mg = str_replace(zn_mg, "NA_", "")) %>% 
  mutate(zn_mg = as.numeric(zn_mg))

sum(!is.na(all_working4$zn_mg))

# how many columns do we have that have mercurcy data?
str_subset(names_all, "hg") 


all_working5 <- all_working4 %>% 
  unite(hg_mcg, hg_mcg.y, hg_mcg.x, remove = TRUE) %>% 
  mutate(hg_mcg = str_replace(hg_mcg, "_NA", "")) %>% 
  mutate(hg_mcg = str_replace(hg_mcg, "NA_", "")) %>% 
  mutate(hg_mcg = as.numeric(hg_mcg))

sum(!is.na(all_working5$hg_mcg))
unique(all_working5$hg_mcg)

# how many columns do we have that have fat data?
str_subset(names_all, "fat") 


all_working5 <- all_working4 %>% 
  unite(hg_mcg, hg_mcg.y, hg_mcg.x, remove = TRUE) %>% 
  mutate(hg_mcg = str_replace(hg_mcg, "_NA", "")) %>% 
  mutate(hg_mcg = str_replace(hg_mcg, "NA_", "")) %>% 
  mutate(hg_mcg = as.numeric(hg_mcg))

sum(!is.na(all_working5$hg_mcg))

## check out the fat repeats, looks like it's safe to merge them
all_working5 %>% 
  select(contains("fat")) %>% 
  select(- fatrn_g.x) %>% 
  select(- fatrn_g.y) %>% 
  mutate_each(funs(as.numeric), 1:11) %>% 
  mutate(sum_value = rowSums(., na.rm = TRUE)) 

str_subset(names_all, "fat") 

### here trying to merge the redundant fat columns, but not quite there yet! (nov 15)
fats <- all_working5 %>% 
  select(- fatrn_g.x) %>% 
  select(- fatrn_g.y) %>% 
  select(- fat_g_2.y) %>% 
  select(- fat_g.y) %>% 
  select(- fat_g_2) %>% 
  select(- fatce_g.x) %>% 
  select(- fat_g.x) %>% 
  select(contains("fat"), everything()) %>%
  mutate(fatce_g_2 = str_replace(fatce_g_2, "0.4-1.9", "1.15")) %>% ## replace the ranges with the mean
  mutate(fatce_g_2 = str_replace(fatce_g_2, "1.3-5.2", "3.25")) %>%
  mutate(fatce_g_2 = as.numeric(fatce_g_2)) %>% 
  mutate(fat_g_3.y = as.numeric(fat_g_3.y)) %>% 
  mutate(fat_g_2.x = as.numeric(fat_g_2.x)) %>% 
  mutate(fatce_g.y = as.numeric(fatce_g.y)) %>% 
  select(contains("fat")) %>% 
  mutate(fat_g = rowSums(., na.rm = TRUE)) 

### doing something a bit dangerous here and going to bind cols the fats with the all_working5

all_working7 <- bind_cols(fats, all_working5) ### this looks good, so I'm going to get rid of the duplicated cols

all_working8 <- all_working7[, !duplicated(colnames(all_working7))]

### now get rid of the extraneous fat columns

all_working9 <- all_working8 %>% 
select(fat_g, 8:208) 

all_working9$fat_g[all_working9$fat_g == "0.00"] <- NA
all_working9$fat_g[all_working9$fat_g == "0"] <- NA


write_csv(all_working9, "data-processed/all_nuts_working9.csv")
a9 <- read_csv("data-processed/all_nuts_working9.csv")
# a9 <- all_working9

summary(a9$epa_g)

sum(!is.na(a9$fat_g))

sum(is.na(a9$food_name_clean))

a9 %>% 
  filter(is.na(food_name_clean)) %>%
  select(food_name_clean, starts_with("biblio"), contains("ref"), everything()) %>% View


a10 <- a9 %>% 
  arrange(desc(ca_mg)) %>%
  select(ca_mg, everything()) %>% 
  # filter(ca_mg != "41206.0000") ## this looks like an outlier

hist(a10$zn_mg)

write_csv(a10, "data-processed/all_nuts_working10.csv")

sum(!is.na(a10$fat_g))

names_all <- names(a10)

names_all
str_subset(names_all, "3")

sum(!is.na(a10$dha_g))

#### now let's deal with the EPAs
a10 %>% 
  select(contains("fapun3"), contains("3"), epa_g, everything()) %>% View

#### Ok let's try to deal with the two fapun3 columns

a11 <- a10 %>% 
  unite(fapun3, fapun3_g.x, fapun3_g.y, sep = "_" ) %>% 
  select(fapun3, everything()) %>%
  mutate(fapun3 = str_replace_all(fapun3, "NA", "")) %>% 
  mutate(fapun3 = str_replace_all(fapun3, "_", "")) %>%
  mutate(fapun3 = as.numeric(fapun3))


#### Ok let's try to deal with the two EPA columns
a12 <- a11 %>% 
  unite(epa, epa_g, f20d5n3_g, sep = "_" ) %>% 
  select(epa, everything()) %>%
  mutate(epa = str_replace_all(epa, "NA", "")) %>% 
  mutate(epa = str_replace_all(epa, "_", "")) %>% 
  mutate(epa = as.numeric(epa))


#### Ok let's try to deal with the two EPA columns
a13 <- a12 %>% 
  unite(dha, dha_g, f22d6n3_g, sep = "_" ) %>% 
  select(dha, everything()) %>%
  mutate(dha = str_replace_all(dha, "NA", "")) %>% 
  mutate(dha = str_replace_all(dha, "_", "")) %>% 
  mutate(dha = as.numeric(dha))

#### now onto the fapu's (polyunsaturated FAs)

a14 <- a13 %>% 
  select(-fapu_g_2) %>% 
  unite(fapun_all_g, fapu_g,fapu_g.x, sep = "_" ) %>% 
  select(fapun_all_g, everything()) %>% 
  mutate(fapun_all_g = str_replace_all(fapun_all_g, "NA", "")) %>% 
  mutate(fapun_all_g = str_replace_all(fapun_all_g, "_", "")) %>% 
  mutate(fapun_all_g = as.numeric(fapun_all_g))


write_csv(a14, "data-processed/all_nuts_working14.csv")


names_all <- names(a14)

names_all
str_subset(names_all, "fapu_")

a14 %>% 
  select(ca_mg, slmax, everything()) %>% View

a14 %>% 
  group_by(sci_name, slmax) %>% 
  summarise(mean_ca = mean(ca_mg)) %>% 
ggplot(data = ., aes(x = log(slmax), y = log(mean_ca))) + geom_point() +
  geom_smooth(method = "lm") + geom_jitter(width = 0.30)



# starting november 16 2016 -----------------------------------------------

a14 <- read_csv("data-processed/all_nuts_working14.csv")


#### now onto the proteins


names_all <- names(a14)

names_all
str_subset(names_all, "pro")

## get rid of empty columns with protein in them
a15 <- a14 %>% 
  select(- protcnp_g.x) %>% 
  select(- protcnp_g.y) %>% 
  select(- npro_g.y) %>% 
  select(contains("pro"), everything()) 



names_all <- names(a15)
str_subset(names_all, "pro")


class(a15$prot_g.y)

## this has [ protcnt_g.y


a16 <- a15 %>% 
  mutate(protcnt_g.y = str_replace_all(protcnt_g.y, "[\\[]", "")) %>% 
  mutate(protcnt_g.y = str_replace(protcnt_g.y, "[\\]]", "")) %>% 
  mutate(protcnt_g.y = str_replace(protcnt_g.y, "19.4-20.9", "20.15")) ## get rid of the ranges

names_all <- names(a16)
str_subset(names_all, "pro")
unique(a16$protcnt_g.y)

a17 <- a16 %>% 
unite(protein_g, protcnt_g.x, protcnt_g.y, sep = "_" ) %>% 
  select(protein_g, everything()) %>%
  mutate(protein_g = str_replace_all(protein_g, "NA", "")) %>% 
  mutate(protein_g = str_replace_all(protein_g, "_", "")) %>% 
  mutate(protcnt_g = as.numeric(protein_g)) %>% 
  select(-protein_g) %>% 
  unite(protein_g2, prot_g.x, prot_g.y, sep = "_" ) %>% 
  select(protein_g2, everything()) %>%
  mutate(protein_g2 = str_replace_all(protein_g2, "NA", "")) %>% 
  mutate(protein_g2 = str_replace_all(protein_g2, "_", "")) %>%  
mutate(prot_g = as.numeric(protein_g2)) %>% 
  select(-protein_g2) %>% 
  select(prot_g, protcnt_g, everything())

unique(a17$npro_g.x)
summary(a17$protcnt_g)
# finished fixing protein, so write out a17 as csv ------------------------

write_csv(a17, "data-processed/all_nuts_working17.csv")
names_all <- names(a17)
str_subset(names_all, "pro")



# now see if we still need to bring in the new fatty acid data ------------

inf_fatty_acids <- read_csv("data-processed/fatty_acids_cleaned_in_progress_googled.csv")

sum(is.na(inf_fatty_acids$food_item_id))
foods <- unique(inf_fatty_acids$food_item_id)
foods_a17 <- unique(a17$food_item_id)

intersect(foods, foods_a17) ### ok so it looks like all of the food item ids line up between the inf_fatty_acids and a17, so no need to incorporate it



# starting again — is there anything major left? --------------------------

a17 <- read_csv("data-processed/all_nuts_working17.csv")

#### before I go too much further, let's create a new uniqueID for a17, in case I go chopping things away in the future


a17$seanuts_id <- rownames(a17)

a17 %>% 
  select(seanuts_id, everything()) %>% View


names_a <- names(a17)

str_subset(names_a, "name")

setdiff(unique(a17$asfis_scientific_name_fishbase_swap.x), unique(a17$asfis_scientific_name_fishbase_swap.y))
sum(is.na(a17$asfis_scientific_name_fishbase_swap.y))
sum(is.na(a17$asfis_scientific_name_fishbase_swap.x))
sum(is.na(a17$scientific_name.y))

### let's make a single scientific name that matches with fishbase, call it species_name

a18 <- a17 %>% 
  # filter(is.na(sci_name)) %>%
  select(sci_name, asfis_scientific_name_fishbase_swap.x, asfis_scientific_name_fishbase_swap.y, everything()) %>% ### ok, for the time being let's take the asfis_scientific_name_fishbase_swap.x 
  unite(species_name, sci_name, asfis_scientific_name_fishbase_swap.x) %>% 
  select(species_name, everything()) %>%
  mutate(species_name = str_replace(species_name, "NA", "")) %>% 
  mutate(species_name = str_replace(species_name, "_", "")) 


sum(!is.na(a18$species_name))

write_csv(a18, "data-processed/all_nuts_working18.csv")
a18 <- read_csv( "data-processed/all_nuts_working18.csv")


### write out just the species list in order to use it to wrangle the fishbase data
seanuts_species <- as.data.frame(unique(a18$species_name))

write_csv(seanuts_species, "data-processed/seanuts_species.csv")



# wrangling the species data ----------------------------------------------


seanuts_species <- read_csv("data-processed/seanuts_species.csv") %>% 
  rename(species_name = `unique(a18$species_name)`)

a18 <- read_csv("data-processed/all_nuts_working18.csv")

seanuts_working %>% 
  select(seanuts_species)

fishbase_species_list <- read_csv("data-processed/fishbase_species_names.csv")
fishbase_list <- unique(fishbase_species_list$species_name)

bad_snippet <- anti_join(a18, fishbase_species_list) %>% 
  select(seanuts_id, everything()) 

str_subset(fishbase_list, "Anadara broughtonii")

### ok going about this in probably not the best way, but here goes nothing. The goal here is create a snippet of a18 that has the mismatches where there is a better match in the inprogress column


bad_snippet_corrected <- bad_snippet %>%
  filter(!is.na(asfis_scientific_name_fishbase_swap_in_progress)) %>% 
  mutate(species_name = asfis_scientific_name_fishbase_swap_in_progress) ## ok do the bold thing of just swapping out the species names

### ok now let's snip these rows out of seanuts_working!

rows_to_cut <- unique(bad_snippet_corrected$seanuts_id)

a19 <- a18 %>% 
  filter(!seanuts_id %in% rows_to_cut) 

## now add the written-over rows back in

a20 <- bind_rows(a19, bad_snippet_corrected)

### write out just the species list in order to use it to wrangle the fishbase data



write_csv(a20, "data-processed/all_nuts_working20.csv")

a20 <- read_csv("data-processed/all_nuts_working20.csv")
seanuts_species <- as.data.frame(unique(a20$species_name))

write_csv(seanuts_species, "data-processed/seanuts_species.csv")

#### November 17, pick up here after a short stint in the 03b_db_species_names_wrangling.R script, where did a bit more to clean up the species names
## the product of that file at the moment is data-processed/all_nuts_working21.csv

a21 <- read_csv("data-processed/all_nuts_working21.csv")
names_all <- names(a21)

### let's get the subgroup column in order!
str_subset(names_all, "subgroup")

a22 <- a21 %>% 
  select(contains("subgroup"), everything()) %>% 
  unite(subgroup, subgroup.x.x, subgroup.y, sep = "_" ) %>% 
  select(subgroup, everything()) %>% 
  mutate(subgroup = str_replace_all(subgroup, "NA", "")) %>% 
  mutate(subgroup = str_replace_all(subgroup, "_", ""))

sum(is.na(a22$subgroup))

write_csv(a22, "data-processed/all_nuts_working22.csv")


#### November 17 2016
#### now bringing in the new inverts data from last April
a22 <- read_csv("data-processed/all_nuts_working22.csv")
inverts.new <- read_csv("~/Documents/Nutrient_Analysis/data/aquatic_inverts_micronutrients.csv")
## or update March 14 2016 with new inverts data:
inverts.new2 <- read_csv("~/Documents/Nutrient_Analysis/data/ntbl.inv.csv")

names(inverts.new2)

inv <- inverts.new2 %>% 
  clean_names() %>% 
  filter(food_item_id < 118)
### update Nov 29
inv <- inverts.new %>% 
  clean_names() %>% 
  filter(food_item_id < 118)


inv_names <- names(inv)
a22_names <- names(a22)

setdiff(inv_names, a22_names)
str_subset(a22_names, "length")

a22 %>% 
  select(taxonkey) %>% 
  filter(!is.na(taxonkey))


### rename some columns so we get a good line up
inv2 <- inv %>% 
  rename(protein_g = protein_g_100g_ww, 
         dha = dha_g_100g, 
         fat_g = fat_g_100g_ww,
         epa = epa_g_100g,
         species_name = species) %>% 
  mutate(ca_mg = as.numeric(ca_mg))

str(inv2)

a23 <- bind_rows(a22, inv2)

inv2_selected <- inv2 %>% 
  select(-p_mg)

##update Nov 29
a23b <- bind_rows(a22, inv2_selected)

write_csv(a23, "data-processed/all_nuts_working23.csv")

write_csv(a23b, "data-processed/all_nuts_working23b.csv")

# adding the CINE data ----------------------------------------------------

CINE <- read_csv("data-processed/CINE.csv")
a23 <- read_csv("data-processed/all_nuts_working23.csv")
a23b <- read_csv("data-processed/all_nuts_working23b.csv")

names_CINE <- names(CINE)
str_subset(names_CINE, "dha")

# get the CINE names to align with the a23 names, so can then bind rows

CINE2 <- CINE %>%
  rename(species_name = scientific_name,
         dha = x22_6_n_3_dha_g, 
         epa = x20_5_n_3_epa, 
         fapun3 = total_n_3, 
         max_length = length)

intersect(names(CINE2), names(a23))
  
str_subset(names(a23), "length")
str_subset(names(CINE2), "length")


?bind_rows

class(a23$tl)
class(CINE2$tl)

a24 <- bind_rows(a23b, CINE2)

CINE2$tl[CINE2$tl == "."] <- NA
CINE2$tl_se[CINE2$tl_se == "."] <- NA

unique(CINE2$tl_se)

CINE2$tl_se <- as.numeric(CINE2$tl_se)
CINE2$tl <- as.numeric(CINE2$tl)
a24 <- bind_rows(a23b, CINE2)


unique(CINE2$fe_mg)

CINE2$fe_mg[CINE2$fe_mg == "come back"] <- NA
CINE2$fe_mg <- as.numeric(CINE2$fe_mg)
a24 <- bind_rows(a23b, CINE2)

unique(CINE2$se_mcg)
CINE2$se_mcg <- as.numeric(CINE2$se_mcg)

CINE2 <- CINE2 %>% 
  select(-se_mcg)

a24 <- bind_rows(a23, CINE2)
unique(CINE2$vitc_mg)
CINE2$vitc_mg <- as.numeric(CINE2$vitc_mg)
class(CINE2$vitc_mg)
a24 <- bind_rows(a23, CINE2)

glimpse(CINE2)


CINE3 <- CINE2 %>% 
  select(1:27, ca_mg, fe_mg, zn_mg, epa, dha)

a24 <- bind_rows(a23b, CINE3)

### write out the database with the CINE data

write_csv(a24, "data-processed/all_nuts_working24.csv")



# fix the missing subgroup data in the CINE data --------------------------

a24 <- read_csv("data-processed/all_nuts_working24.csv")

missing_subgroup <- a24 %>% 
  filter(is.na(subgroup)) %>% 
  select(subgroup, species_name) %>%
  distinct(species_name)


miss <- inner_join(missing_subgroup, a24) 

miss %>%
  select(species_name, subgroup) %>% 
  ungroup() %>% 
  distinct(species_name, .keep_all = TRUE) %>% View

### go in an fill in the empty subgroup obs for the CINE missing ones

a24$subgroup[a24$species_name == "Coregonus autumnalis"] <- "Finfish"
a24$subgroup[a24$species_name == "Stenodus leucichthys"] <- "Finfish"
a24$subgroup[a24$species_name == "Coregonus nasus"] <- "Finfish"
a24$subgroup[a24$species_name == "Mya spp"] <- "Molluscs"
a24$subgroup[a24$species_name == "Myoxocephalus spp"] <- "Finfish"
a24$subgroup[a24$species_name == "Salvelinus naresi"] <- "Finfish"
a24$subgroup[a24$species_name == "Coregonus spp"] <- "Finfish"
a24$subgroup[a24$species_name == "Oncorhyncus spp"] <- "Finfish"
a24$subgroup[a24$species_name == "Boreogadus saida"] <- "Finfish"
a24$subgroup[a24$species_name == "Coregonus clupeaformis"] <- "Finfish"
a24$subgroup[a24$species_name == "Esox lucius"] <- "Finfish"
a24$subgroup[a24$species_name == "Lota lota"] <- "Finfish"
a24$subgroup[a24$species_name == "Salvelinus namaycush"] <- "Finfish"
a24$subgroup[a24$species_name == "Thymallus arcticus"] <- "Finfish"
a24$subgroup[a24$species_name == "Boreogadus saida"] <- "Finfish"
a24$subgroup[a24$species_name == "Mytilus edulis"] <- "Molluscs"
a24$subgroup[a24$species_name == "Boreogadus  saida"] <- "Finfish"
a24$species_name[a24$species_name == "Boreogadus  saida"] <- "Boreogadus saida"


a25 <- a24  ### after filling in the missing subgroup entries


write_csv(a25, "data-processed/all_nuts_working25.csv")


# more checking -----------------------------------------------------------

a25 <- read_csv("data-processed/all_nuts_working25.csv")
names(a25)

a25$species_name[a25$asfis_scientific_name.x.x == "Strombus gracilior"] <- "Strombus gracilior"


sum(is.na(a25$species_name))

sum(is.na(a25$seanuts_id2))

a25 %>% 
  select(seanuts_id, everything()) %>%
  filter(is.na(seanuts_id)) %>% View

a25$seanuts_id2 <- row.names(a25)

a26 <- a25

write_csv(a26, "data-processed/all_nuts_working26.csv")

### now make a subset file, where we just pull out the relevant columns and get rid of extra clutter

a26 <- read_csv("data-processed/all_nuts_working26.csv")

cols26 <- names(a26)
str_subset(cols26, "Reference")

a27_subset <- a26 %>% 
  select(seanuts_id2, food_item_id_2, database, biblioid, biblioid.x, biblioid.y, biblioid_y, nutrient_ref, species_name, subgroup, food_name_clean, prot_g, protcnt_g, protein_g, fat_g, epa, dha, ca_mg, zn_mg, fe_mg, tl,
         length_from_study, length_source, abs_lat, latitude,
         slmax, slmax_nov28, slmax_source, lwa, lwb,
         country_region, isscaap_cat, isscaap,
         season, asfis_scientific_name_fishbase_swap_in_progress, season, season.x, season.y,
         fapun3, fapun_all_g, reference)



write_csv(a27_subset, "data-processed/all_nuts_working27_subset.csv" )



