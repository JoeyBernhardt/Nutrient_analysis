library(readxl)
library(readr)
library(janitor)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)
library(vegan)
library(ggplot2)


a <- read_excel("~/Desktop/CINE-nutrients/nutrients-6151.xls")
a <- read_csv("~/Desktop/CINE-nutrients/nutrients-6151.csv")


clean_a <- a %>% 
  clean_names()

names(clean_a)
nutrient_files <- c(list.files("/Users/student/Desktop/CINE-csv", full.names = TRUE))


names(nutrient_files) <- nutrient_files %>% 
  gsub(pattern = ".csv$", replacement = "")


#### Step 3: read in all the files!

all_nutrients <- map_df(nutrient_files, read_csv, col_names = FALSE, .id = "file_name")
all_nutrients2 <- all_nutrients %>%
  select(-1)



names(all_nutrients2) <- names(clean_a)

all_nuts <- all_nutrients2 %>% 
  filter(pageid != "PageID")

### let's clean up this dataset

names(all_nuts)

all_nuts_cleaned$dha

all_nuts_cleaned <- all_nuts %>% 
  mutate(ca_mg = as.numeric(str_replace(ca_mg_100g, " (.*)", ""))) %>% 
  mutate(zn_mg = as.numeric(str_replace(zn_mg_100g, " (.*)", ""))) %>%
  mutate(fe_mg = as.numeric(str_replace(fe_mg_100g, " (.*)", ""))) %>% 
  mutate(mn_mg = as.numeric(str_replace(mn_mg_100g, " (.*)", ""))) %>% 
  mutate(mg_mg = as.numeric(str_replace(mg_mg_100g, " (.*)", ""))) %>% 
  mutate(epa = as.numeric(str_replace(omega_3_20_5n3_g_100g, " (.*)", ""))) %>% 
  mutate(dha = as.numeric(str_replace(omega_3_22_6n3_g_100g, ",", "."))) %>% 
  mutate(dha = as.numeric(str_replace(dha, " (.*)", ""))) %>% 
  mutate(protein_g = as.numeric(str_replace(protein_g_100g, " (.*)", ""))) %>%
  mutate(fat_g = as.numeric(str_replace(fat_g_100g, " (.*)", ""))) %>%
  mutate(fapun_all_g = as.numeric(str_replace(total_pufa_g_100g, " (.*)", ""))) %>% 
  mutate(fapun3 = as.numeric(str_replace(total_omega_3_g_100g, " (.*)", ""))) %>% 
  dplyr::select(1:8, ca_mg, zn_mg, fe_mg, protein_g, fat_g, fapun3, fapun_all_g, mn_mg, mg_mg, epa, dha)

## just get the raw portions
nuts_raw <- all_nuts_cleaned %>% 
  filter(preparation == "raw")


nuts_raw_parts <- nuts_raw %>% 
  mutate(part = str_replace(part, "fillet", "muscle")) %>% 
  mutate(part = str_replace(part, "meat", "muscle")) %>%
  mutate(part = str_replace(part, "flesh + skin", "muscle + skin")) %>%
  mutate(part = str_replace(part, "meat + skin", "muscle + skin")) %>%
  mutate(part = str_replace(part, "flesh", "muscle")) %>%
  mutate(part = str_replace(part, "middle cut", "middle")) %>%
  mutate(part = str_replace(part, "roe", "eggs")) %>% 
  mutate(part = str_replace(part, "grease", "oil")) %>% 
  mutate(part = str_replace(part, "tail cut", "muscle")) %>% 
  mutate(part = str_replace(part, "middle", "muscle")) %>%
  mutate(part = str_replace(part, "tail end", "muscle")) %>% 
  mutate(part = str_replace(part, "head end", "muscle"))

unique(nuts_raw_parts$part)
unique(nuts_raw$part)

length(unique(nuts_raw$latin_name))

unique(nuts_raw$part)

hist(log((nuts_raw$epa)))


nuts_mean <- nuts_raw_parts %>% 
  group_by(latin_name) %>% 
  summarise(calcium = mean(ca_mg, na.rm = TRUE),
            zinc = mean(zn_mg, na.rm = TRUE), 
            iron = mean(fe_mg, na.rm = TRUE),
            epa = mean(epa, na.rm = TRUE),
            dha = mean(dha, na.rm = TRUE)) %>% 
  filter(!is.na(calcium), !is.na(iron), !is.na(zinc), !is.na(epa), !is.na(dha))
write_csv(nuts_mean, "data-processed/CINE-inuit-mean-nutrients.csv")

## now make accumulation curves
nuts_mean <- read_csv("data-processed/CINE-inuit-mean-nutrients.csv")

data <- nuts_mean
threshold = 0.1
accumulate <- function(data, threshold) {
  ntbl.RDI.all <- data %>% 
    mutate(RDI.CA = ifelse(calcium > (1200*threshold), 1, 0)) %>% 
    mutate(RDI.FE = ifelse(iron > (18*threshold), 1, 0)) %>% 
    mutate(RDI.ZN = ifelse(zinc > (11*threshold), 1, 0)) %>%
    mutate(RDI.EPA = ifelse(epa > (1*threshold), 1, 0)) %>%
    mutate(RDI.DHA = ifelse(dha > (1*threshold), 1, 0)) %>%
    ungroup() %>% 
    mutate(RDI.micro.tot = rowSums(.[7:11])) %>% 
    filter(!is.na(RDI.micro.tot)) 
  
  all_spa <- ntbl.RDI.all %>%
    dplyr::select(latin_name, 7:11) %>% 
    mutate(subgroup = "all") %>% 
    split( .$subgroup) %>% 
    map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.EPA", "RDI.DHA")) %>%
    map(.f = specaccum, method = "random", permutations = 100000)
  
  
  accumulated_targets <- all_spa %>% 
    map(.f = `[`, "richness") %>% 
    unlist() %>% 
    as.data.frame()
  
  accumulated_targets_sd <- all_spa %>% 
    map(.f = `[`, "sd") %>% 
    unlist() %>% 
    as.data.frame()
  
  accumulated_targets$richness_level = rownames(accumulated_targets)
  colnames(accumulated_targets) <- c("number_of_targets", "richness_level")
  
  accumulated_targets_sd$sd = rownames(accumulated_targets_sd)
  colnames(accumulated_targets_sd) <- c("sd", "number_of_targets")
  
  accumulated_targets_sd <- accumulated_targets_sd %>% 
    separate(number_of_targets, into = c("subgroup", "number_of_species")) %>%
    mutate(number_of_species = str_replace(number_of_species, "sd", "")) %>%
    mutate(number_of_species = as.numeric(number_of_species))
  
  
  accumulated_targets <- accumulated_targets %>% 
    separate(richness_level, into = c("subgroup", "number_of_species")) %>%
    mutate(number_of_species = str_replace(number_of_species, "richness", "")) %>%
    mutate(number_of_species = as.numeric(number_of_species))
  
  accumulated_targets_all <- left_join(accumulated_targets, accumulated_targets_sd)
  accumulated_targets_all <- accumulated_targets_all %>% 
    mutate(se = sd / sqrt(number_of_species))
  return(accumulated_targets_all)
  
}

accumulated_targets_all %>% 
  filter(number_of_species < 11) %>% 
  group_by(subgroup) %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets, group = subgroup, color = subgroup)) + geom_line(size =1.5) +
  geom_ribbon(aes(ymin = number_of_targets - se, ymax = number_of_targets + se, color = subgroup, group = subgroup), alpha = 0.2, size = 0) +
  ylab("number of nutrient requirements fulfilled (10% DRI)") +
  xlab("number of species") + theme(text = element_text(size=14)) + 
  scale_color_grey(start = 0.01, end = 0.7) +
  theme_bw() +
  theme(legend.position = c(0.6, 0.2)) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  theme(legend.title=element_blank())




  
  
  accumulate(data = nuts_mean,  threshold = 0.1) 

