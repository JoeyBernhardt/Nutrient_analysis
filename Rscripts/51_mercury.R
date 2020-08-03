

library(tidyverse)
library(janitor)
library(readxl)
library(cowplot)
theme_set(theme_cowplot())


anf_raw <- read_excel("data/AnFooD1.1.xlsx", sheet = "09 Fish & Shellfish")
ufish_raw <- read_excel("data/uFiSH1.0.xlsx", sheet = "10 Reftbl_RefDatasets") %>% 
  clean_names() %>%
  select(contains("hg")) %>% View

anf2 <- anf_raw %>% 
  clean_names() %>% 
  select(contains("hg")) %>% View



# Read in the Karimi data -------------------------------------------------
library(readxl)
karimi <- read_excel("data/resourceMap_knb_295_2/data/rkarimi.7.1-Seafood_Hg_Database.data.xls", sheet = "Seafood Hg Database") %>% 
  clean_names() %>% 
  rename(ug_hg = mean_hg_concentration_ppm_wet_weight) %>% 
  mutate(ug_hg = ug_hg*100)

karimi2 <- read_excel("data/resourceMap_knb_295_2/data/rkarimi.7.1-Seafood_Hg_Database.data_JB.xls", sheet = "Seafood Hg Database") %>% 
  clean_names() %>% 
  rename(ug_hg = mean_hg_concentration_ppm_wet_weight) %>% 
  mutate(ug_hg = ug_hg*100) %>% View
  group_by(genus_species, subgroup) %>% 
  summarise(mean_hg = mean(ug_hg))

names(karimi)

burger <- read_csv("data/tabula-Burger.csv") %>% 
  rename(species = species_name) %>% 
  mutate(mean_hg = mean_hg_ppm*100) %>% 
  mutate(dataset = "burger")

tox <- read_excel("data/Hall-trace-elements.xlsx") %>% 
  clean_names() %>%
  mutate(species = str_to_lower(species)) %>% 
  mutate(area = str_to_lower(area)) %>% 
  mutate(part = str_to_lower(part)) %>% 
  filter(part != "liver") %>% 
  # select(6:20) %>% 
  filter(cadmium != 2.61) %>% 
  filter(nickel != 1.810) 

str(tox)

names_tox <- names(tox)

tox_sum <- tox %>% 
  group_by(species, subgroup) %>% 
  summarise_at(c(names_tox[6:20]), mean) %>% 
  dplyr::select(species, mercury, subgroup) %>% 
  rename(mean_ug = mercury)

times_hundred <- function(x, na.rm = FALSE) (x*100)
hall_mercury <- tox %>% 
  group_by(species, subgroup) %>% 
  summarise_at(c(names_tox[6:20]), mean) %>% 
  mutate_at(c(names_tox[6:20]), times_hundred) %>% 
  dplyr::select(species, mercury, subgroup) %>% 
  rename(mean_ug = mercury) 

hall_anti <- tox %>% 
  group_by(species) %>% 
  summarise_at(c(names_tox[6:20]), mean) %>% 
  mutate_at(c(names_tox[6:20]), times_hundred) %>% 
  dplyr::select(species, antimony) %>% 
  rename(mean_ug = antimony) 

hall_cad <- tox %>% 
  group_by(species) %>% 
  summarise_at(c(names_tox[6:20]), mean) %>% 
  mutate_at(c(names_tox[6:20]), times_hundred) %>% 
  dplyr::select(species, cadmium) %>% 
  rename(mean_ug = cadmium) 

hall_lead <- tox %>% 
  group_by(species) %>% 
  summarise_at(c(names_tox[6:20]), mean) %>% 
  mutate_at(c(names_tox[6:20]), times_hundred) %>% 
  dplyr::select(species, lead) %>% 
  rename(mean_ug = lead)

hall_arsenic <- tox %>% 
  group_by(species) %>% 
  summarise_at(c(names_tox[6:20]), mean) %>% 
  mutate_at(c(names_tox[6:20]), times_hundred) %>% 
  dplyr::select(species, arsenic) %>% 
  rename(mean_ug = arsenic)

karimi_mean <- tox_sum ### just doing this to run the analyssis on lead


karimi_mean <- karimi %>% 
  group_by(taxon_common_name) %>% 
  summarise(mean_ug = mean(ug_hg)) 

karimi_sum <- karimi_mean %>% 
  summarise_each(funs(mean, median), mean_ug)

karimi2 %>% 
  ggplot(aes(x = mean_hg)) + geom_histogram() +
  # scale_x_log10() +
  geom_vline(xintercept = karimi_sum$mean[1], color = "purple") +
  geom_vline(xintercept = karimi_sum$median[1], color = "cadetblue") +
  xlab("Mercury concentration (ug/100g)")


karimi_mean %>% 
    ggplot(aes(x = mean_ug)) + geom_histogram() +
  geom_vline(xintercept = 7) + 
  # scale_x_log10() +
  geom_vline(xintercept = karimi_sum$mean[1], color = "purple") +
  geom_vline(xintercept = karimi_sum$median[1], color = "cadetblue") +
  xlab("Mercury concentration (ug/100g)")
ggsave("figures/mercury-histogram-karimi.png", width = 6, height = 4)


karimi_mean2 <- karimi2 %>% 
  mutate(dataset = "karimi") %>% 
  rename(species = genus_species) 

hall_mean2 <- hall_mercury %>% 
  mutate(dataset = "hall") %>% 
  mutate(mean_hg = mean_ug) 

adams <- read_csv("data/tabula-Adams-2003.csv") %>% 
  clean_names() %>% 
  dplyr::select(species, mean_hg_ppm, subgroup) %>% 
  rename(mean_ug = mean_hg_ppm) %>% 
  mutate(mean_hg = mean_ug*100) %>% 
  mutate(dataset = "adams")

all_merc <- bind_rows(karimi_mean2, adams, hall_mean2, burger) %>% 
  mutate(species = str_to_lower(species)) %>% 
  mutate(mean_hg = ifelse(subgroup == "mollusc", mean_hg*0.30, mean_hg*0.95))

write_csv(all_merc, "data-processed/mercury-data-compiled.csv")
all_merc <- read_csv("data-processed/mercury-data-compiled.csv")

length(unique(all_merc$species))
#### how many species in CINE are in mercury?

cine <- read_csv("data-processed/CINE-data-all.csv") %>% 
  mutate(species = str_to_lower(latin_name_cleaned)) %>% 
  dplyr::select(species) %>% 
  distinct(.)
length(unique(cine$species))

hall_species <- all_merc %>% 
  filter(dataset == "hall")

overlap <- data.frame(species = intersect(all_merc$species, cine$species)) 



cine2 <- read_csv("data-processed/cine-traits-new-species2.csv") %>% 
  mutate(species = str_to_lower(latin_name)) %>% 
  dplyr::select(species) %>% 
  distinct(.)



length(unique(cine2$latin_name))
cine2 

all_merc %>% 
  group_by(species) %>% 
  summarise(mean_hg = mean(mean_hg)) %>% 
  ggplot(aes(x = mean_hg)) + geom_histogram(bins = 20) + scale_x_log10()

dats <- data.frame(res = rlnorm(n = 1000, mean = 10, sd = 2))

dats %>% 
  ggplot(aes(x = log(res))) + geom_histogram()

# nutrient fishing function -----------------------------------------------

sample_size <- 10

nutrient_fishing_function <- function(sample_size) {
  ntbl_sub1 <- hall_arsenic %>% 
    ungroup() %>% 
    sample_n(size = 40, replace = FALSE) %>%
    sample_n(size = sample_size, replace = FALSE)
  
  sample_list <- NULL
  for (i in 1:nrow(ntbl_sub1) ) {
    output <- combn(nrow(ntbl_sub1), i, FUN=function(x) ntbl_sub1[x,], simplify = FALSE)
    output <- bind_rows(output, .id = "sample_id")
    subsample_size <- rep(i, nrow(output))
    output <- cbind(output, subsample_size)
    sample_list <- rbind(sample_list,output)
  }
  
  sample_list <- split(sample_list, f = sample_list$subsample_size)
  
  new_data_sub1 <- sample_list %>% 
    map_df(`[`, .id = "replicate")
  
  resampling_15 <- new_data_sub1 %>% 
    dplyr::rename(species_number = subsample_size) %>%
    group_by(species_number, sample_id) %>% 
    mutate(hg_total = (mean_ug/species_number)) %>% ## get the amount of calcium each species will contribute
    summarise_each(funs(sum), contains("total")) %>%  ## sum up all of each of the nutrients
    mutate(hg_grams = (hg_total/(150))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no, sample_id) %>% 
    dplyr::select(-contains("total")) %>% 
    gather(key = nutrient, value = concentration, contains("grams")) %>% 
    group_by(species_no, sample_id) %>% 
    summarise(max_percentage = max(concentration) + 0.001) %>%
    mutate(grams_required = 100/max_percentage)
  # %>% 
  #   group_by(species_no, sample_id) %>% 
  #   summarise(max_percentage = max(concentration)) %>%
  #   mutate(grams_required = 100/max_percentage)
}


samples_rep <- rep(10, 100)
samples_rep[1]

global_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run") 

hall_arsenic_acc <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run") %>% 
  mutate(dataset = "GL")

hall_cad_acc <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run") %>% 
  mutate(dataset = "GL")

hall_merc_acc <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run") %>% 
  mutate(dataset = "GL")

hall_lead_acc <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run") %>% 
  mutate(dataset = "GL")

global_10$dataset <- "GL"


write_csv(global_10, "data-processed/global_10_40spp_mercury_efficiency.csv")


# hall mercury ------------------------------------------------------------
library(nlstools)

# hall_merc_med <- hall_merc_acc %>%
#   group_by(dataset, species_no) %>% 
#   summarise(grams_median = median(concentration))

hall_merc_med %>% 
  ggplot(aes(x = species_no, y = grams_median)) + geom_point()

hall_merc_med <- hall_merc_acc %>% 
  # filter(max_percentage > 0.001) %>% 
  group_by(dataset, species_no) %>% 
  summarise(grams_median = median(grams_required))
library(nlstools)
merc_mod <- nls(formula = (grams_median ~ a * species_no^b), data = hall_merc_med,  start = c(a=100, b=-0.5))
merc_boot <- nlsBoot(merc_mod)
merc_boot$bootCI
merc_boot_df <- as_data_frame(merc_boot$coefboot) 
merc_b <- as_data_frame(merc_boot$bootCI) 
merc_b$culture <- "GL"

merc_preds <- merc_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required)) %>% 
  mutate(dataset = "GL")

hall_merc_med %>% 
  ggplot(aes(x = species_no, y = grams_median)) + geom_point() +
  geom_line(aes(x = species_no, y = mean), data = merc_preds) + 
  # geom_hline(yintercept = 7) + 
  # ylim(0, 30) +
  # geom_hline(yintercept =  karimi_sum$median[1]) + 
  geom_line(aes(x = species_no, y = q2.5), data = merc_preds, color = "grey") +
  geom_line(aes(x = species_no, y = q97.5), data = merc_preds, color = "grey") +
  ylab("Maximum portion size \nbefore exceeding PDTI (g)") + xlab("Species richness") +
  ggtitle("b = -0.24")
ggsave("figures/max-portion-size-mercury.png", width = 6, height = 4)


# hall lead ---------------------------------------------------------------

library(nlstools)
hall_lead_med <- hall_lead_acc %>% 
  group_by(dataset, species_no) %>% 
  summarise(grams_median = median(grams_required))
View(hall_lead_med)

hall_lead_med %>% 
  ggplot(aes(x = species_no, y = grams_median, group = species_no)) + geom_point()

lead_mod <- nls(formula = (grams_median ~ a * species_no^b), data = hall_lead_med,  start = c(a=515, b=-0.5))
lead_boot <- nlsBoot(lead_mod)
lead_boot$bootCI
lead_boot_df <- as_data_frame(lead_boot$coefboot) 
lead_b <- as_data_frame(lead_boot$bootCI) 
lead_b$culture <- "GL"

lead_preds <- lead_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required)) %>% 
  mutate(dataset = "GL") 

hall_lead_med %>% 
  ggplot(aes(x = species_no, y = grams_median)) + geom_point() +
  geom_line(aes(x = species_no, y = mean), data = lead_preds) + 
  # geom_hline(yintercept = 7) + 
  # ylim(0, 30) +
  # geom_hline(yintercept =  karimi_sum$median[1]) + 
  geom_line(aes(x = species_no, y = q2.5), data = lead_preds, color = "grey") +
  geom_line(aes(x = species_no, y = q97.5), data = lead_preds, color = "grey") +
  ylab("Lead concentration (ug/100g)") + xlab("Species richness") +
  ggtitle("b = -0.039")



hall_lead_med %>% 
  ggplot(aes(x = species_no, y = grams_median)) + geom_point() +
  geom_point(aes(x = species_no, y = grams_median), data = hall_merc_med) +
  geom_line(aes(x = species_no, y = mean), data = lead_preds) + 
  # geom_hline(yintercept = 7) + 
  # ylim(0, 30) +
  # geom_hline(yintercept =  karimi_sum$median[1]) + 
  geom_line(aes(x = species_no, y = q2.5), data = lead_preds, color = "grey") +
  geom_line(aes(x = species_no, y = q97.5), data = lead_preds, color = "grey") +
  ylab("Lead concentration (ug/100g)") + xlab("Species richness") + scale_y_log10()




# antimony ----------------------------------------------------------------

hall_anti_med <- hall_arsenic_acc %>% 
  group_by(dataset, species_no) %>% 
  summarise(grams_median = median(grams_required))

anti_mod <- nls(formula = (grams_median ~ a * species_no^b), data = hall_anti_med,  start = c(a=150, b=-0.5))
anti_boot <- nlsBoot(anti_mod)
anti_boot$bootCI
anti_boot_df <- as_data_frame(anti_boot$coefboot) 
anti_b <- as_data_frame(anti_boot$bootCI) 
anti_b$culture <- "GL"

anti_preds <- anti_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required)) %>% 
  mutate(dataset = "GL")


hall_anti_med %>% 
  ggplot(aes(x = species_no, y = grams_median)) + geom_point() +
  geom_line(aes(x = species_no, y = mean), data = anti_preds) + 
  # geom_hline(yintercept = 7) + 
  # ylim(0, 30) +
  # geom_hline(yintercept =  karimi_sum$median[1]) + 
  geom_line(aes(x = species_no, y = q2.5), data = anti_preds, color = "grey") +
  geom_line(aes(x = species_no, y = q97.5), data = anti_preds, color = "grey") +
  ylab("Antimony concentration (ug/100g)") + xlab("Species richness") +
  ggtitle("b = 0.041")

# cadmium ----------------------------------------------------------------

hall_cad_med <- hall_cad_acc %>% 
  group_by(dataset, species_no) %>% 
  summarise(grams_median = median(grams_required))

cad_mod <- nls(formula = (grams_median ~ a * species_no^b), data = hall_cad_med,  start = c(a=150, b=-0.5))
cad_boot <- nlsBoot(cad_mod)
cad_boot$bootCI
cad_boot_df <- as_data_frame(cad_boot$coefboot) 
cad_b <- as_data_frame(cad_boot$bootCI) 
cad_b$culture <- "GL"

cad_preds <- cad_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required)) %>% 
  mutate(dataset = "GL")


hall_cad_med %>% 
  ggplot(aes(x = species_no, y = grams_median)) + geom_point() +
  geom_line(aes(x = species_no, y = mean), data = cad_preds) + 
  # geom_hline(yintercept = 7) + 
  # ylim(0, 30) +
  # geom_hline(yintercept =  karimi_sum$median[1]) + 
  geom_line(aes(x = species_no, y = q2.5), data = cad_preds, color = "grey") +
  geom_line(aes(x = species_no, y = q97.5), data = cad_preds, color = "grey") +
  ylab("cadmium concentration (ug/100g)") + xlab("Species richness") +
  ggtitle("b = 0.041")

#### plot

cad_b$metal <- "cadmium"
lead_b$metal <- "lead"
anti_b$metal <- "arsenic"
merc_b$metal <- "methylmercury"
all_pmaxes <- bind_rows(cad_b, lead_b, anti_b, merc_b) %>% 
  filter(Median < 1) %>% 
  clean_names()

all_pmaxes %>% 
  ggplot(aes(x = metal, y = median)) +
  geom_pointrange(aes(x = metal, ymin = x2_5_percent, ymax = x97_5_percent), data = all_pmaxes) +
  ylab("b slope")


lead_preds %>% 
  ggplot(aes(x = species_no, y = mean)) + geom_line()





library(patchwork)
lead_plot + merc_plot
ggsave("figures/lead-merc-bef.png", width = 8, height=4)


lm(log(grams_median) ~ log(species_no), data = global_10_2) %>%
  tidy(., conf.int = TRUE)

global_10_2 %>% 
  ggplot(aes(x = species_no, y = grams_median)) + geom_point() +
  geom_hline(yintercept = 7) + ylim(0, 30)

global_10 %>% 
  ggplot(aes(x = species_no, y = concentration, group = species_no)) + geom_violin() + 
  geom_point(aes(x = species_no, y = grams_median), data = global_10_2) +
  geom_hline(yintercept = 7)

library(nlstools)
library(broom)



global_10_2 %>% 
  ggplot(aes(x = log(species_no), y = log(grams_median))) + geom_point() +
  geom_smooth(method = "lm")


GL_mod <- nls(formula = (grams_median ~ a * species_no^b), data = global_10_2,  start = c(a=1, b=0.5))
GL_boot <- nlsBoot(GL_mod)
GL_boot$bootCI
GL_boot_df <- as_data_frame(GL_boot$coefboot) 
GL_b <- as_data_frame(GL_boot$bootCI) 
GL_b$culture <- "GL"

merc_karimi <- GL_boot
merc_karimi$bootCI
lead_bef <- GL_boot
lead_bef$bootCI

prediction_function <- function(df) {
  pf <-function(x){
    res<-(df$a[[1]]*x^df$b[[1]])
    res
  }
  
  pred <- function(x) {
    y <- pf(x)
  }
  
  x <- seq(1, 10, by = 0.1)
  
  preds <- sapply(x, pred)
  preds <- data.frame(x, preds) %>% 
    rename(species_no = x, 
           grams_required = preds)
}

#15
GL_preds <- GL_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required)) %>% 
  mutate(dataset = "GL")

GL_preds %>% 
  ggplot(aes(x = species_no, y = mean)) + geom_line()

global_10_2 %>% 
  ggplot(aes(x = species_no, y = grams_median)) + geom_point() +
  geom_line(aes(x = species_no, y = mean), data = GL_preds) + 
  # geom_hline(yintercept = 7) + 
  # ylim(0, 30) +
  # geom_hline(yintercept =  karimi_sum$median[1]) + 
  geom_line(aes(x = species_no, y = q2.5), data = GL_preds, color = "grey") +
  geom_line(aes(x = species_no, y = q97.5), data = GL_preds, color = "grey") +
  ylab("Mercury concentration (ug/100g)") + xlab("Species richness")
ggsave("figures/mercury-accumulation.png", width = 6, height= 4)

library(rfishbase)

str(common_to_sci("anchovy"))

eco <- common_to_sci("anchovy") 
eco2 <- ecology(eco$Species)

eco2 %>% 
  filter(!is.na(FoodTroph)) %>% 
  mutate(FoodTroph = as.numeric(FoodTroph)) %>% 
  ggplot(aes(x = FoodTroph)) + geom_histogram()
eco2$FoodTroph


### let's pull in the Adams dataset
library(tidyverse)
library(janitor)
adams <- read_csv("data/tabula-Adams-2003.csv") %>% 
  clean_names()


length(unique(adams$species))


library(readxl)
karimi <- read_excel("data/resourceMap_knb_295_2/data/rkarimi.7.1-Seafood_Hg_Database.data_JB.xls", sheet = "Seafood Hg Database") %>% 
  clean_names() %>% 
  rename(ug_hg = mean_hg_concentration_ppm_wet_weight) %>% 
  mutate(ug_hg = ug_hg*100)

table(karimi$data_source_see_supplemental_material_for_references)

karimi %>% 
  group_by(data_source_see_supplemental_material_for_references) %>% 
  tally() %>% View

adams_species <- unique(adams$species)

library(rfishbase)
eco2 <- ecology(adams_species) %>% 
  mutate(FoodTroph = as.numeric(FoodTroph)) 
spc_adam <- species(adams_species)


eco2 %>% 
  ggplot(aes(x = FoodTroph)) + geom_histogram()

all_adams <- adams %>% 
  left_join(., eco2, by = c("species" = "Species")) %>% 
  left_join(., spc_adam, by = c("species" = "Species")) %>% 
  separate(col = mean_min_length_mm, into = c("mean_length", "max_length"), sep = " ") %>% 
  mutate(mean_length = str_replace(mean_length, "\\*", "")) %>% 
  mutate(mean_length = str_replace(mean_length, ",", "")) %>% 
  mutate(mean_length = as.numeric(mean_length))


all_adams %>% 
  ggplot(aes(x = FoodTroph, y = mean_hg_ppm)) + geom_point() + 
  ylab("Hg ppm") + xlab("Trophic position") 
ggsave("figures/adams-mercury-trophic-level.pdf", width = 8, height = 6)

all_adams %>% 
  ggplot(aes(x = mean_length, y = mean_hg_ppm)) + geom_point() + 
  ylab("Hg ppm") + xlab("Length") 
ggsave("figures/adams-mercury-length.pdf", width = 8, height = 6)


#### get traits for the Adams species

data2 <- adams %>%
  rename(species1 = species)

more_traits2 <- species(data2$species1) ## contains BodyShapeI, DemersPelag, AnaCat, DepthRangeDeep
fec2 <- fecundity(data2$species1) 
ecosys2 <- ecosystem(data2$species1) 
mat2 <- maturity(data2$species1) ### contains age at maturity
stocks12 <- stocks(data2$species1) ###contains EnvTemp
ecology2 <- ecology(data2$species1)


mt3 <- more_traits2 %>% 
  dplyr::select(Species, BodyShapeI, DemersPelag, DepthRangeDeep, Length) %>% 
  group_by(Species, BodyShapeI, DemersPelag) %>% 
  mutate(DepthRangeDeep = as.numeric(DepthRangeDeep)) %>% 
  mutate(Length = as.numeric(Length)) %>% 
  summarise_each(funs(mean), DepthRangeDeep, Length)### 3000 rows

ec3 <- ecology2 %>% 
  dplyr::select(Species, Herbivory2, FoodTroph, FeedingType) %>% 
  group_by(Species, Herbivory2, FeedingType) %>% 
  mutate(FoodTroph = as.numeric(FoodTroph)) %>% 
  summarise(FoodTroph = mean(FoodTroph))

mat3 <- mat2 %>%
  filter(!is.na(AgeMatMin)) %>% 
  dplyr::select(Species, AgeMatMin) %>% 
  mutate(AgeMatMin = as.numeric(AgeMatMin)) %>% 
  group_by(Species) %>% 
  summarise(AgeMatMin = mean(AgeMatMin))

stocks3 <- stocks12 %>% 
  dplyr::select(Species, EnvTemp) %>% 
  dplyr::distinct(Species, EnvTemp)

# all_traits2 <- mat3 %>% 
#   left_join(., ec3)

all_traits3 <- ec3 %>% 
  left_join(., stocks3)

all_traits4 <- all_traits3 %>% 
  left_join(., mt3)

adams_all <- left_join(adams, all_traits4, by = c("species" = "Species")) %>% 
  separate(col = mean_min_length_mm, into = c("mean_length", "max_length"), sep = " ") %>% 
  mutate(mean_length = str_replace(mean_length, "\\*", "")) %>% 
  mutate(mean_length = str_replace(mean_length, ",", "")) %>% 
  mutate(mean_length = as.numeric(mean_length)/10) %>% 
  rename(feeding_mode = FeedingType) %>% 
  rename(feeding_level = Herbivory2)  %>% 
  group_by(species, FoodTroph, Length, feeding_mode, feeding_level, DemersPelag,
           DepthRangeDeep, BodyShapeI) %>% 
  summarise_each(funs(mean), mean_hg_ppm) %>%
  ungroup() %>% 
  filter(complete.cases(.))



adams_all %>% 
  ggplot(aes(x = mean_length, y = Length)) + geom_point() + 
  geom_abline(yintercept = 0, slope = 1)

sum(!is.na(adams_all$feeding_mode))
sum(!is.na(adams_all$feeding_level))
sum(!is.na(adams_all$DemersPelag))
sum(!is.na(adams_all$DepthRangeDeep))


mod1 <- lm(mean_hg_ppm ~ FoodTroph + Length  + feeding_mode + feeding_level +
             DemersPelag + DepthRangeDeep, data = adams_all)
mod2 <- lm(mean_hg_ppm ~ FoodTroph + Length  + feeding_mode + feeding_level +
             DemersPelag, data = adams_all)
mod3 <- lm(mean_hg_ppm ~ FoodTroph + Length  + feeding_mode + feeding_level, data = adams_all)
mod4 <- lm(mean_hg_ppm ~ FoodTroph + Length  + feeding_mode, data = adams_all)
mod5 <- lm(mean_hg_ppm ~ FoodTroph + Length, data = adams_all)
mod6 <- lm(mean_hg_ppm ~ Length, data = adams_all)
model.sel(mod1, mod2, mod3, mod4, mod5, mod6)
model.sel(mod1, mod6)

summary(mod1)
summary(mod6)
summary(mod5)
anova(mod5)

names(adams_all)

adams_all_mod <- left_join(adams, all_traits4, by = c("species" = "Species")) %>% 
  separate(col = mean_min_length_mm, into = c("mean_length", "max_length"), sep = " ") %>% 
  mutate(mean_length = str_replace(mean_length, "\\*", "")) %>% 
  mutate(mean_length = str_replace(mean_length, ",", "")) %>% 
  mutate(mean_length = as.numeric(mean_length)/10) %>% 
  rename(feeding_mode = FeedingType) %>% 
  rename(feeding_level = Herbivory2)  %>% 
  group_by(species, FoodTroph, Length) %>% 
  summarise_each(funs(mean), mean_hg_ppm) %>%
  ungroup() %>% 
  filter(complete.cases(.)) %>% 
  mutate(log_length = log(Length)) %>% 
  mutate(log_mercury  = log(mean_hg_ppm))

mod6 <- lm(mean_hg_ppm ~ Length + FoodTroph, data = adams_all_mod)
mod7 <- lm(log_mercury ~ log_length + FoodTroph, data = adams_all_mod)
summary(mod6)
summary(mod7)

library(modelr)
nuts <- read_csv("data-processed/more_traits-finfish.csv") %>% 
  mutate(Length = exp(log_length)) %>% 
  mutate(FoodTroph = bulk_trophic_level) %>% 
  add_predictions(mod6, var = "predicted_mercury") %>% 
  add_predictions(mod7, var = "pred_log_merc") %>% 
  mutate(mercury = exp(pred_log_merc)*100) %>%
  group_by(species_name) %>% 
  summarise(mercury = mean(mercury))



nuts %>% 
  ggplot(aes(x = mercury)) + geom_histogram() +
  geom_vline(xintercept = 18.28)
  xlab("Mercury concentration (ug/100g)")
ggsave("figures/mercury-histogram.pdf", width = 8, height = 6)


nuts %>% 
  ggplot(aes(x= mercury)) + geom_histogram() + geom_vline(xintercept = 18)

length(unique(nuts$species_name))



# now with fishing for mercury --------------------------------------------

# nutrient fishing function -----------------------------------------------

## new WHO guidelines for mercury are:
## 


sample_size <- 10

nutrient_fishing_function <- function(sample_size) {
  ntbl_sub1 <- nuts %>% 
    sample_n(size = 40, replace = FALSE) %>%
    sample_n(size = sample_size, replace = FALSE)
  
  sample_list <- NULL
  for (i in 1:nrow(ntbl_sub1) ) {
    output <- combn(nrow(ntbl_sub1), i, FUN=function(x) ntbl_sub1[x,], simplify = FALSE)
    output <- bind_rows(output, .id = "sample_id")
    subsample_size <- rep(i, nrow(output))
    output <- cbind(output, subsample_size)
    sample_list <- rbind(sample_list,output)
  }
  
  sample_list <- split(sample_list, f = sample_list$subsample_size)
  
  new_data_sub1 <- sample_list %>% 
    map_df(`[`, .id = "replicate")
  
  resampling_15 <- new_data_sub1 %>% 
    dplyr::rename(species_number = subsample_size) %>%
    group_by(species_number, sample_id) %>% 
    mutate(hg_total = (mercury/species_number)) %>% ## get the amount of calcium each species will contribute
    summarise_each(funs(sum), contains("total")) %>%  ## sum up all of each of the nutrients
    mutate(hg_grams = (hg_total/(1))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no, sample_id) %>% 
    select(-contains("total")) %>% 
    gather(key = nutrient, value = concentration, contains("grams")) 
  # %>%
  #   group_by(species_no, sample_id) %>%
  #   summarise(max_percentage = max(concentration)) %>%
  #   mutate(grams_required = 100/max_percentage)
}


samples_rep <- rep(10, 1000)
samples_rep[1]

global_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run") 

global_10$dataset <- "GL"


# write_csv(global_10, "data-processed/global_10_40spp_mercury_efficiency.csv")


global_10_2 <- global_10 %>% 
  group_by(dataset, species_no) %>% 
  summarise(grams_median = median(concentration))

global_10_2 %>% 
  ggplot(aes(x = species_no, y = grams_median)) + geom_point() 

global_10 %>% 
  ggplot(aes(x = species_no, y = concentration, group = species_no)) + geom_violin() + 
  geom_point(aes(x = species_no, y = grams_median), data = global_10_2) +
  geom_hline(yintercept = 18.28) + ylab("Mercury (ug/100g portion)") +
  xlab("Species richness")
ggsave("figures/mercury-median-violin.pdf", width = 8, height = 6)


GL_mod1 <- nls(formula = (grams_median ~ a * species_no^b), data = global_10_2,  start = c(a=1, b=0.5))
GL_boot1 <- nlsBoot(GL_mod1)
GL_boot1$bootCI
GL_boot_df1 <- as_data_frame(GL_boot1$coefboot) 
GL_b1 <- as_data_frame(GL_boot1$bootCI) 
GL_b1$culture <- "GL"


prediction_function <- function(df) {
  pf <-function(x){
    res<-(df$a[[1]]*x^df$b[[1]])
    res
  }
  
  pred <- function(x) {
    y <- pf(x)
  }
  
  x <- seq(1, 10, by = 0.1)
  
  preds <- sapply(x, pred)
  preds <- data.frame(x, preds) %>% 
    rename(species_no = x, 
           grams_required = preds)
}

#15
GL_preds <- GL_boot_df1 %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required)) %>% 
  mutate(dataset = "GL")

GL_preds %>% 
  ggplot(aes(x = species_no, y = mean)) + geom_line()

global_10_2 %>% 
  ggplot(aes(x = species_no, y = grams_median)) + geom_point() +
  geom_line(aes(x = species_no, y = mean), data = GL_preds) + 
  # geom_hline(yintercept =  karimi_sum$median[1]) + 
  geom_line(aes(x = species_no, y = q2.5), data = GL_preds, color = "grey") +
  geom_line(aes(x = species_no, y = q97.5), data = GL_preds, color = "grey") +
  ylab("Mercury concentration (ug/100g)") + xlab("Species richness") + geom_hline(yintercept = 18.28)
ggsave("figures/mercury-tolerable-limit.pdf", width = 8, height = 6)
