
library(readr)
library(plyr)
library(dplyr)
#### Data cleaning ####
## Hi Joey, as of Dec 22 2015, ntbl2.csv is the most current data file :) See below for notes on cleaning. 


nut_dec19 <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data/nut_dec19.csv")
### on dec 19, I re-saved a version of nut_dec3.csv as nut_dec19.csv, in the data folder in Nutrient_Analysis.
ntbl <- tbl_df(nut_dec19)
View(ntbl)
ntbl2 <- ntbl %>%
  mutate(HG_mcg = as.numeric(HG_mcg),
         species = as.factor(ASFIS.Scientific.name)) %>% 
  rename(taxon = ISSCAAP_cat,
         max_length = SLMAX,
         FAT = FAT_g,
         PROTEIN = PROTCNT_g) %>% 
  mutate(max_size = (lwA * (max_length^lwB)/1000)) %>% 
  select(Food.Item.ID, species, taxon, max_size, max_length, TL, CA_mg, EPA_g, DHA_g, FE_mg, ZN_mg, HG_mcg, FAT, PROTEIN, lwA, lwB, Habitat, Subgroup, Abs_lat, Latitude, max_length_study)

write_csv(ntbl2, "/Users/Joey/Documents/Nutrient_Analysis/data/ntbl2.csv")

hist(ntbl2$PROTEIN)

#### Fat cleaning and exploring (note, so the result of this is that FAT3 (FAT_g) is the most complete data set.)
ntbl

ntbl.FATS <- ntbl %>%
  mutate(HG_mcg = as.numeric(HG_mcg),
         species = as.factor(ASFIS.Scientific.name)) %>% 
  rename(taxon = ISSCAAP_cat,
         max_length = SLMAX,
         FAT1 = FAT.g.,
         FAT2 = FAT..g.,
         FAT3 = FAT_g,
         FAT4 = FAT_g.1,
         PROTEIN = PROTCNT_g) %>% 
  select(Food.Item.ID, species, taxon, TL, CA_mg, EPA_g, DHA_g, FE_mg, ZN_mg, HG_mcg, FAT1, FAT2, FAT3, FAT4, PROTEIN, lwA, lwB, Habitat, Subgroup, Abs_lat, Latitude, max_length_study)

summary(ntbl.FATS$FAT4)
ntbl$FAT

library(ggplot2)
ggplot(ntbl.FATS)
