

library(tidyverse)
library(viridis)
library(readxl)
library(stringr)
library(purrr)
library(vegan)


nuts_trad <- read_csv("data-processed/trad-foods-cleaned.csv")
culture_foods <- read_excel("~/Documents/traditional-foods/culture-foods.xlsx")

nuts_mean <- read_csv("data-processed/CINE-inuit-mean-nutrients.csv")



nuts_trad %>% 
  distinct(latin_name) %>% 
  tally

mean_trad <- nuts_trad %>% 
  group_by(latin_name) %>% 
summarise(calcium = mean(ca_mg, na.rm = TRUE),
          zinc = mean(zn_mg, na.rm = TRUE), 
          iron = mean(fe_mg, na.rm = TRUE),
          epa = mean(epa, na.rm = TRUE),
          dha = mean(dha, na.rm = TRUE)) %>% 
  filter(!is.na(calcium), !is.na(iron), !is.na(zinc), !is.na(epa), !is.na(dha)) 


nuts_trad %>% 
  filter(reference != "88") %>% 
  filter(part != "eggs") %>% 
  gather(key = "nutrient", value = "concentration", 9:19) %>% 
  filter(!is.na(concentration)) %>% 
  filter(nutrient == "ca_mg") %>% 
  mutate(latin_name = factor(latin_name)) %>% 
  ggplot(aes(x = reorder(latin_name, concentration), y = concentration)) + geom_point() +
  facet_wrap( ~ nutrient, scales = "free") + theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

cnuts <- left_join(culture_foods, nuts_trad)

write_csv(cnuts, "data-processed/cnuts-trad-foods-culture.csv")
cnuts <- read_csv("data-processed/cnuts-trad-foods-culture.csv")


nuts_mean <- cnuts %>% 
  group_by(culture, latin_name) %>% 
  summarise(calcium = mean(ca_mg, na.rm = TRUE),
            zinc = mean(zn_mg, na.rm = TRUE), 
            iron = mean(fe_mg, na.rm = TRUE),
            epa = mean(epa, na.rm = TRUE),
            dha = mean(dha, na.rm = TRUE)) %>% 
  filter(!is.na(calcium), !is.na(iron), !is.na(zinc), !is.na(epa), !is.na(dha))

write_csv(nuts_mean, "data-processed/trad-foods-mean.csv")

# comparing cine inuit to the new data ------------------------------------


cine_inuit <- read_csv("data-processed/CINE-inuit-mean-nutrients.csv")

cine_inuit2 <- cine_inuit %>% 
  mutate(latin_name = str_replace(latin_name, "Mercenaria mercenaria<ca>", "Mercenaria mercenaria"))

inuit_inu <- nuts_mean %>% 
  filter(culture == "Inuit-Inupiaq") %>% 
  ungroup() %>% 
  select(-culture) 

# mutate(mn_mg = str_replace(mn_mg_100g, " (.*)", "")) %>% 



stringr::str_subset(inuit_inu2$latin_name, "Mercenaria")

name1 <- inuit_inu %>% 
  dplyr::filter(grepl("Mercenaria", latin_name))%>% 
  distinct(latin_name)

name1[[1]]

inuit_inu2 <- inuit_inu %>% 
  mutate(latin_name = ifelse(latin_name == name1[[1]], "Mercenaria mercenaria", latin_name))


all_equal(cine_inuit2, inuit_inu2)

## things that are in inuit_inu but not cine_inuit2
setdiff(inuit_inu2$latin_name, cine_inuit2$latin_name)

inuit_inu3 <- inuit_inu2 %>% 
  filter(!latin_name %in% c(setdiff(inuit_inu2$latin_name, cine_inuit2$latin_name)))

setdiff(inuit_inu3$latin_name, cine_inuit2$latin_name)
setdiff(cine_inuit2$latin_name, inuit_inu3$latin_name)
all_equal(cine_inuit2, inuit_inu3)


cine_inuit3 <- cine_inuit2 %>% 
  filter(!latin_name %in% c(setdiff(cine_inuit2$latin_name, inuit_inu3$latin_name)))

all_equal(cine_inuit3, inuit_inu3)

### ok write out what we've got!

write_csv(nuts_mean, "data-processed/trad-foods-means.csv")

trad_nuts_mean <- read_csv("data-processed/trad-foods-means.csv")
mean_nuts <- read.csv("data-processed/mean_nuts.csv")



# compare species lists
spp <- trad_nuts_mean %>% 
  distinct(latin_name)

spp2 <- mean_trad %>% 
  distinct(latin_name)

spp3 <- mean_nuts %>% 
  select(species_name) %>% 
  rename(latin_name = species_name)
  
intersect(spp$latin_name, spp3$latin_name)
setdiff(spp$latin_name, spp3$latin_name)
setdiff(spp3$latin_name, spp$latin_name)

lj <- inner_join(mean_nuts, mean_trad, by = c("species_name" = "latin_name"))
lj %>% 
  # select(starts_with("calcium"), starts_with("iron"), starts_with("epa")) %>% 
  ggplot(aes(x = iron.x, y = iron.y)) + geom_point() +
  ylim(0,10) +
  geom_abline(slope = 1, intercept = 0)


# accumulation analysis ---------------------------------------------------


threshold = 0.1
data <- cnuts_split[[1]]

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
    dplyr::select(latin_name, 7:12) %>% 
    mutate(subgroup = "all") %>% 
    split( .$subgroup) %>% 
    map(.f = `[`, c("RDI.CA", "RDI.FE", "RDI.ZN", "RDI.EPA", "RDI.DHA")) %>%
    map(.f = specaccum, method = "random", permutations = 1000)
  
  
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


cnuts_split <- nuts_mean %>% 
  split(.$culture)

cnuts_split[[1]]
res <- cnuts_split %>% 
  map_df(accumulate, threshold = 0.1, .id = "culture")

write_csv(res, "data-processed/nut_accumulation_trad_foods.csv")
res <- read_csv("data-processed/nut_accumulation_trad_foods.csv")

res %>% 
  filter(number_of_species < 11) %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets, group = culture, color = culture)) + geom_line(size =1.5) +
  geom_ribbon(aes(ymin = number_of_targets - se, ymax = number_of_targets + se, fill = culture), alpha = 0.2, size = 0) +
  ylab("Number of nutrient requirements fulfilled (10% DRI)") +
  xlab("Number of species") + theme(text = element_text(size=14)) + 
  # scale_color_grey(start = 0.01, end = 0.7) +
  # theme_bw() +
  # theme(legend.position = c(0.6, 0.2)) +
  # scale_x_continuous(breaks = seq(1,10,1)) +
  theme(legend.title=element_blank()) + theme_classic() +
  ylim(0,5) +
  facet_wrap( ~ culture) +
  scale_color_viridis(discrete = TRUE) + scale_fill_viridis(discrete = TRUE)

ggsave("figures/nutrient_accumulation_plots_na_cultures.png", width = 8, height = 6)
