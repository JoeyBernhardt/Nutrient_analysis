#### Cleaning up the horrendously messy all_nuts_messy.csv, which contains all the nutrient data from databases to date
#### Last updated November 15 2016



# load libraries ----------------------------------------------------------

library(tidyverse)
library(stringr)


# read in data ------------------------------------------------------------

all <- read_csv("data-processed/nuts_all_messy.csv")
str(all)


# start by pulling out the data we will use for now -----------------------
# namely the minerals, fats and fatty acids

names_all <- names(all)

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
