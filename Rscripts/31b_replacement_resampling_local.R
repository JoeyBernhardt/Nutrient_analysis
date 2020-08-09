
library(plotrix)
library(tidyverse)
library(cowplot)
### RDI accumulation for the global and local scales, with replacement design
trad_nuts_mean_raw2 <- read_csv("data-processed/trad-foods-mean-aug2020c.csv") ### ok this one only has fully traceable cine data
trad_nuts_mean_raw2 <- read_csv("data-processed/trad-foods-mean-aug2020d.csv")
trad_nuts_mean_raw1 <- read_csv("data-processed/trad-foods-mean-aug2020.csv")
trad_nuts_mean_raw <- read_csv("data-processed/trad-foods-mean.csv")
species_numbers <- read_csv("data-processed/species_numbers.csv")

trad_nuts_mean <- read_csv("data-processed/tdata.csv") ### update with new extracted trad foods aug 8 2020

mean_nuts <- read_csv("data-processed/mean_nuts.csv") %>% 
  mutate(species_name = ifelse(species_name == "Tenualosa ilisha (juvenile)", "Tenualosa ilisha", species_name)) %>% 
  mutate(species_name = ifelse(species_name == "Pangasianodon hypophthalmus (juvenile)", "Pangasianodon hypophthalmus", species_name)) %>% 
  group_by(species_name, subgroup) %>% 
  summarise_each(funs(mean), calcium, iron, zinc, epa, dha) %>% 
  ungroup()

mean_nuts <- read_csv("data-processed/mean_nuts_aug2020.csv") ## udpate aug 8 2020
  
View(mean_nuts)
mean_nuts$culture <- "global"


View(trad_nuts_mean_raw2)

trad_nuts_mean <- trad_nuts_mean_raw2 %>% 
  filter(grepl(" ", latin_name)) %>% 
  filter(culture %in% species_numbers$culture) %>% 
  dplyr::select(-common_name) 


View(trad_nuts_mean)
trad_nuts_mean %>% 
  mutate(latin_name = str_replace(latin_name, "Octopoda", "Enteroctopus dofleini")) %>% 
  filter(!grepl(" ", latin_name)) %>% 
  dplyr::select(-culture) %>% 
  distinct() %>% View

unique(trad_nuts_mean$latin_name)

nutrient_fishing_function <- function(sample_size) {
  ntbl_sub1 <- mean_nuts %>% 
    sample_n(size = 40, replace = FALSE) %>%
    dplyr:: sample_n(size = sample_size, replace = FALSE)
  
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
    mutate(cal_total = (calcium/species_number)) %>% ## get the amount of calcium each species will contribute
    mutate(zinc_total = (zinc/species_number)) %>% 
    mutate(iron_total = (iron/species_number)) %>% 
    mutate(epa_total = (epa/species_number)) %>%
    mutate(dha_total = (dha/species_number)) %>% 
    summarise_each(funs(sum), contains("total")) %>%   ## sum up all of each of the nutrients, to get total amount of nutrient per sample
    mutate(cal_grams = (cal_total/(1200*threshold))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
    mutate(iron_grams = (iron_total/(18*threshold))) %>%
    mutate(zinc_grams = (zinc_total/(11*threshold))) %>% 
    mutate(epa_grams = (epa_total/(1*threshold))) %>%
    mutate(dha_grams = (dha_total/(1*threshold))) %>%
    mutate(rdi_calcium = ifelse(cal_grams >= 1, 1, 0)) %>% ## if the amount of calcium exceeds 1 (i.e. reaches threshold), give it a value of 1, else 0
    mutate(rdi_iron = ifelse(iron_grams >= 1, 1, 0)) %>% 
    mutate(rdi_zinc = ifelse(zinc_grams >= 1, 1, 0)) %>% 
    mutate(rdi_epa = ifelse(epa_grams >= 1, 1, 0)) %>%
    mutate(rdi_dha = ifelse(dha_grams >= 1, 1, 0)) %>% 
    ungroup() %>% 
    mutate(rdi_micro_tot = rowSums(.[13:17])) %>%  ## add up all the targets reached in one sample
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no, sample_id) %>% 
    dplyr::select(-contains("total")) %>% 
    mutate(threshold_level = threshold)
}


samples_rep <- rep(10, 100)

threshold <- 0.1

global_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

global_10$dataset <- "GL"


# traditional diets -------------------------------------------------------

nutrient_fishing_function <- function(sample_size, culture_name) {
  ntbl_sub1 <- trad_nuts_mean %>% 
    filter(culture == culture_name) %>% 
    dplyr:: sample_n(size = sample_size, replace = FALSE)
  
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
    mutate(cal_total = (calcium/species_number)) %>% ## get the amount of calcium each species will contribute
    mutate(zinc_total = (zinc/species_number)) %>% 
    mutate(iron_total = (iron/species_number)) %>% 
    mutate(epa_total = (epa/species_number)) %>%
    mutate(dha_total = (dha/species_number)) %>% 
    summarise_each(funs(sum), contains("total")) %>%   ## sum up all of each of the nutrients, to get total amount of nutrient per sample
    mutate(cal_grams = (cal_total/(1200*threshold))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
    mutate(iron_grams = (iron_total/(18*threshold))) %>%
    mutate(zinc_grams = (zinc_total/(11*threshold))) %>% 
    mutate(epa_grams = (epa_total/(1*threshold))) %>%
    mutate(dha_grams = (dha_total/(1*threshold))) %>%
    mutate(rdi_calcium = ifelse(cal_grams >= 1, 1, 0)) %>% ## if the amount of calcium exceeds 1 (i.e. reaches threshold), give it a value of 1, else 0
    mutate(rdi_iron = ifelse(iron_grams >= 1, 1, 0)) %>% 
    mutate(rdi_zinc = ifelse(zinc_grams >= 1, 1, 0)) %>% 
    mutate(rdi_epa = ifelse(epa_grams >= 1, 1, 0)) %>%
    mutate(rdi_dha = ifelse(dha_grams >= 1, 1, 0)) %>% 
    ungroup() %>% 
    mutate(rdi_micro_tot = rowSums(.[13:17])) %>%  ## add up all the targets reached in one sample
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no, sample_id) %>% 
    dplyr::select(-contains("total")) %>% 
    mutate(threshold_level = threshold)
}



Abenaki_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Abenaki", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

Abenaki_10$dataset <- "AB"

BellaCoola_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Bella Coola", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

BellaCoola_10$dataset <- "BC"

CentralSalish_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Central Salish", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

CentralSalish_10$dataset <- "CS"

Cree_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Cree", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

Cree_10$dataset <- "CR"

Haida_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Haida", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

Haida_10$dataset <- "HA"

Inuit_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Inuit-Inupiaq", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

Inuit_10$dataset <- "II"

Kwakiutl_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Kwakiutl", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

Kwakiutl_10$dataset <- "KW"

Micmac_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Micmac", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

Micmac_10$dataset <- "MI"

Montagnais_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Montagnais-Naskapi", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

Montagnais_10$dataset <- "MN"

Nootkan_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Nootkan", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

Nootkan_10$dataset <- "NO"

Tlingit_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Tlingit", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

Tlingit_10$dataset <- "TL"

Tsimshian_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Tsimshian", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

Tsimshian_10$dataset <- "TS"

Wampanoag_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Wampanoag", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

Wampanoag_10$dataset <- "WA"

Yupik_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, culture_name = "Yupik", .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

Yupik_10$dataset <- "YU"


all_trad_accum <- bind_rows(Abenaki_10, Yupik_10, Wampanoag_10, Tlingit_10, Tsimshian_10,
                            BellaCoola_10, Haida_10, Nootkan_10, Montagnais_10, Micmac_10, 
                            Kwakiutl_10, Inuit_10, Cree_10, CentralSalish_10, global_10)

write_csv(all_trad_accum, "data-processed/trad_accumulation_replacement.csv")

all_trad_accum <- read_csv("data-processed/trad_accumulation_replacement.csv")
all_trad_accum %>% 
  ggplot(aes(x = species_no, y = rdi_micro_tot_mean, group = run)) + 
  geom_line() + 
facet_wrap( ~ dataset)


all_trad_accum %>% 
  group_by(dataset, species_no) %>% 
  summarise(mean_rdi_count = mean(rdi_micro_tot_mean)) %>% 
  ggplot(aes(x = species_no, y = mean_rdi_count, color = dataset)) + geom_line(size = 2)




# now fit power functions to each culture ---------------------------------

all_trad_accum <- read_csv("data-processed/trad_accumulation_replacement.csv")

unique(all_trad_accum$dataset)

all_trad_accum <- all_trad_accum %>% 
  group_by(dataset, species_no) %>% 
  summarise(rdi_micro_tot_mean = mean(rdi_micro_tot_mean))

all_trad_accum <- all_trad_accum %>% ### update aug 3 2020
  group_by(dataset, species_no) %>% 
  summarise(rdi_micro_tot_mean = mean(mean))
  

library(nlstools)

GL_mod <- nls(formula = (rdi_micro_tot_mean ~ a * species_no^b), data = filter(all_trad_accum, dataset == "GL"),  start = c(a=2, b=0.5))
GL_boot <- nlsBoot(GL_mod)
GL_boot$bootCI
GL_estiboot <- GL_boot$estiboot
GL_boot_df <- as_data_frame(GL_boot$coefboot) 
GL_b <- as_data_frame(GL_boot$bootCI) 
GL_b$culture <- "GL"

AB_mod <- nls(formula = (rdi_micro_tot_mean ~ a * species_no^b), data = filter(all_trad_accum, dataset == "AB"),  start = c(a=2, b=0.5))
AB_boot <- nlsBoot(AB_mod)
AB_boot$bootCI
AB_estiboot <- AB_boot$estiboot
AB_boot_df <- as_data_frame(AB_boot$coefboot)
AB_b <- as_data_frame(AB_boot$bootCI) 
AB_b$culture <- "AB"


YU_mod <- nls(formula = (rdi_micro_tot_mean ~ a * species_no^b), data = filter(all_trad_accum, dataset == "YU"),  start = c(a=2, b=0.5))
YU_boot <- nlsBoot(YU_mod)
YU_boot$bootCI
YU_boot_df <- as_data_frame(YU_boot$coefboot) 
YU_b <- as_data_frame(YU_boot$bootCI) 
YU_b$culture <- "YU"


WA_mod <- nls(formula = (rdi_micro_tot_mean ~ a * species_no^b), data = filter(all_trad_accum, dataset == "WA"),  start = c(a=2, b=0.5))
WA_boot <- nlsBoot(WA_mod)
WA_boot$bootCI
WA_boot_df <- as_data_frame(WA_boot$coefboot) 
WA_b <- as_data_frame(WA_boot$bootCI) 
WA_b$culture <- "WA"

TL_mod <- nls(formula = (rdi_micro_tot_mean ~ a * species_no^b), data = filter(all_trad_accum, dataset == "TL"),  start = c(a=2, b=0.5))
TL_boot <- nlsBoot(TL_mod)
TL_boot$bootCI
TL_boot_df <- as_data_frame(TL_boot$coefboot)
TL_b <- as_data_frame(TL_boot$bootCI) 
TL_b$culture <- "TL"


TS_mod <- nls(formula = (rdi_micro_tot_mean ~ a * species_no^b), data = filter(all_trad_accum, dataset == "TS"),  start = c(a=2, b=0.5))
TS_boot <- nlsBoot(TS_mod)
TS_boot$bootCI
TS_boot_df <- as_data_frame(TS_boot$coefboot)
TS_b <- as_data_frame(TS_boot$bootCI) 
TS_b$culture <- "TS"


BC_mod <- nls(formula = (rdi_micro_tot_mean ~ a * species_no^b), data = filter(all_trad_accum, dataset == "BC"),  start = c(a=2, b=0.5))
BC_boot <- nlsBoot(BC_mod)
BC_boot$bootCI
BC_boot_df <- as_data_frame(BC_boot$coefboot)
BC_b <- as_data_frame(BC_boot$bootCI) 
BC_b$culture <- "BC"

HA_mod <- nls(formula = (rdi_micro_tot_mean ~ a * species_no^b), data = filter(all_trad_accum, dataset == "HA"),  start = c(a=2, b=0.5))
HA_boot <- nlsBoot(HA_mod)
HA_boot$bootCI
HA_boot_df <- as_data_frame(HA_boot$coefboot)
HA_b <- as_data_frame(HA_boot$bootCI) 
HA_b$culture <- "HA"


NO_mod <- nls(formula = (rdi_micro_tot_mean ~ a * species_no^b), data = filter(all_trad_accum, dataset == "NO"),  start = c(a=2, b=0.5))
NO_boot <- nlsBoot(NO_mod)
NO_boot$bootCI
NO_boot_df <- as_data_frame(NO_boot$coefboot)
NO_b <- as_data_frame(NO_boot$bootCI) 
NO_b$culture <- "NO"


MN_mod <- nls(formula = (rdi_micro_tot_mean ~ a * species_no^b), data = filter(all_trad_accum, dataset == "MN"),  start = c(a=2, b=0.5))
MN_boot <- nlsBoot(MN_mod)
MN_boot$bootCI
MN_boot_df <- as_data_frame(MN_boot$coefboot)
MN_b <- as_data_frame(MN_boot$bootCI) 
MN_b$culture <- "MN"

MI_mod <- nls(formula = (rdi_micro_tot_mean ~ a * species_no^b), data = filter(all_trad_accum, dataset == "MI"),  start = c(a=2, b=0.5))
MI_boot <- nlsBoot(MI_mod)
MI_boot$bootCI
MI_boot_df <- as_data_frame(MI_boot$coefboot)
MI_b <- as_data_frame(MI_boot$bootCI) 
MI_b$culture <- "MI"


KW_mod <- nls(formula = (rdi_micro_tot_mean ~ a * species_no^b), data = filter(all_trad_accum, dataset == "KW"),  start = c(a=2, b=0.5))
KW_boot <- nlsBoot(KW_mod)
KW_boot$bootCI
KW_boot_df <- as_data_frame(KW_boot$coefboot)
KW_b <- as_data_frame(KW_boot$bootCI) 
KW_b$culture <- "KW"


II_mod <- nls(formula = (rdi_micro_tot_mean ~ a * species_no^b), data = filter(all_trad_accum, dataset == "II"),  start = c(a=2, b=0.5))
II_boot <- nlsBoot(II_mod)
II_boot$bootCI
II_boot_df <- as_data_frame(II_boot$coefboot)
II_b <- as_data_frame(II_boot$bootCI) 
II_b$culture <- "II"


CR_mod <- nls(formula = (rdi_micro_tot_mean ~ a * species_no^b), data = filter(all_trad_accum, dataset == "CR"),  start = c(a=2, b=0.5))
CR_boot <- nlsBoot(CR_mod)
CR_boot$bootCI
CR_boot_df <- as_data_frame(CR_boot$coefboot)
CR_b <- as_data_frame(CR_boot$bootCI) 
CR_b$culture <- "CR"


CS_mod <- nls(formula = (rdi_micro_tot_mean ~ a * species_no^b), data = filter(all_trad_accum, dataset == "CS"),  start = c(a=2, b=0.5))
CS_boot <- nlsBoot(CS_mod)
CS_boot$bootCI
CS_boot_df <- as_data_frame(CS_boot$coefboot)
CS_b <- as_data_frame(CS_boot$bootCI) 
CS_b$culture <- "CS"


# Merge all params --------------------------------------------------------
 all_b_params <- bind_rows(CS_b, CR_b, II_b, HA_b, MN_b, NO_b, BC_b, GL_b, KW_b, TS_b, TL_b, WA_b, YU_b, AB_b, MI_b) %>% 
  clean_names() %>% 
  rename(lower = x2_5percent,
        upper = x97_5percent) %>% 
filter(median < 1)

all_b_params <- bind_rows(CS_b, CR_b, II_b, HA_b, MN_b, NO_b, BC_b, GL_b, KW_b, TS_b, TL_b, WA_b, YU_b, AB_b, MI_b) %>% 
  clean_names() %>% 
  rename(lower = x2_5_percent,
         upper = x97_5_percent) %>% 
  filter(median < 1)

# View(all_b_params)

ggplot(aes(x = reorder(culture, median), y = median), data = all_b_params) + geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +ylab("b estimate") + xlab("Region")

# all_b_params_mean <- bind_rows(CS_b, CR_b, II_b, HA_b, MN_b, NO_b, BC_b, GL_b, KW_b, TS_b, TL_b, WA_b, YU_b, AB_b, MI_b) %>% 
#   clean_names() %>% 
#   rename(lower = x2_5percent,
#          upper = x97_5percent) %>% 
#   filter(median < 1)

write_csv(all_b_params, "data-processed/replacement_design_accumulation_b_params.csv")
write_csv(all_b_params, "data-processed/replacement_design_accumulation_b_params_aug5.csv")
write_csv(all_b_params, "data-processed/replacement_design_accumulation_b_params_aug8.csv")

accum_b_params <- read_csv("data-processed/replacement_design_accumulation_b_params.csv")
### this is the plot for the appendix
accumulation_b_plot <- ggplot(aes(x = reorder(culture, median), y = median), data = accum_b_params) + geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +ylab("b estimate") + xlab("Region")
ggsave("figures/b_params_accumulation.png", width = 5, height = 5)


accum_b_params %>% 
  filter(culture != "GL") %>% 
  summarise_each(funs(mean, std.error), median) %>% View 

# plot the functions ------------------------------------------------------

prediction_function <- function(df) {
  pf <-function(x){
    res<-(df$a[[1]]*x^df$b[[1]])
    res
  }
  
  pred <- function(x) {
    y <- pf(x)
  }
  
  x <- seq(1, 10, by = 0.5)
  
  preds <- sapply(x, pred)
  preds <- data.frame(x, preds) %>% 
    rename(species_no = x, 
           DRI_targets = preds)
}

#15
CS_preds <- CS_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(DRI_targets, probs=0.025),
            q97.5=quantile(DRI_targets, probs=0.975),
            mean = mean(DRI_targets)) %>% 
  mutate(dataset = "CS")
#14
II_preds <- II_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(DRI_targets, probs=0.025),
            q97.5=quantile(DRI_targets, probs=0.975),
            mean = mean(DRI_targets)) %>% 
  mutate(dataset = "II")
#13
HA_preds <- HA_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(DRI_targets, probs=0.025),
            q97.5=quantile(DRI_targets, probs=0.975),
            mean = mean(DRI_targets)) %>% 
  mutate(dataset = "HA")
#12
KW_preds <- KW_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(DRI_targets, probs=0.025),
            q97.5=quantile(DRI_targets, probs=0.975),
            mean = mean(DRI_targets)) %>% 
  mutate(dataset = "KW")
#11
CR_preds <- CR_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(DRI_targets, probs=0.025),
            q97.5=quantile(DRI_targets, probs=0.975),
            mean = mean(DRI_targets)) %>% 
  mutate(dataset = "CR")
#10
NO_preds <- NO_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(DRI_targets, probs=0.025),
            q97.5=quantile(DRI_targets, probs=0.975),
            mean = mean(DRI_targets)) %>% 
  mutate(dataset = "NO")
#9
MN_preds <- MN_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(DRI_targets, probs=0.025),
            q97.5=quantile(DRI_targets, probs=0.975),
            mean = mean(DRI_targets)) %>% 
  mutate(dataset = "MN")
#8
TL_preds <- TL_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(DRI_targets, probs=0.025),
            q97.5=quantile(DRI_targets, probs=0.975),
            mean = mean(DRI_targets)) %>% 
  mutate(dataset = "TL")
#7
TS_preds <- TS_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(DRI_targets, probs=0.025),
            q97.5=quantile(DRI_targets, probs=0.975),
            mean = mean(DRI_targets)) %>% 
  mutate(dataset = "TS")
#6
GL_preds <- GL_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(DRI_targets, probs=0.025),
            q97.5=quantile(DRI_targets, probs=0.975),
            mean = mean(DRI_targets)) %>% 
  mutate(dataset = "GL")
#5
MI_preds <- MI_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(DRI_targets, probs=0.025),
            q97.5=quantile(DRI_targets, probs=0.975),
            mean = mean(DRI_targets))%>% 
  mutate(dataset = "MI")
#4
BC_preds <- BC_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(DRI_targets, probs=0.025),
            q97.5=quantile(DRI_targets, probs=0.975),
            mean = mean(DRI_targets)) %>% 
  mutate(dataset = "BC")
#3
AB_preds <- AB_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(DRI_targets, probs=0.025),
            q97.5=quantile(DRI_targets, probs=0.975),
            mean = mean(DRI_targets)) %>% 
  mutate(dataset = "AB")

#2
YU_preds <- YU_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(DRI_targets, probs=0.025),
            q97.5=quantile(DRI_targets, probs=0.975),
            mean = mean(DRI_targets)) %>% 
  mutate(dataset = "YU")

#1
WA_preds <- WA_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(DRI_targets, probs=0.025),
            q97.5=quantile(DRI_targets, probs=0.975),
            mean = mean(DRI_targets)) %>% 
  mutate(dataset = "WA")


all_accumulation_lims <- bind_rows(WA_preds, YU_preds, TL_preds, TS_preds, CS_preds, CR_preds, HA_preds, BC_preds, GL_preds,
                            KW_preds, MN_preds, NO_preds, AB_preds, II_preds, MI_preds)

write_csv(all_accumulation_lims, "data-processed/all_accumulation_lims.csv")
write_csv(all_accumulation_lims, "data-processed/all_accumulation_lims-aug3.csv")
write_csv(all_accumulation_lims, "data-processed/all_accumulation_lims-aug5.csv")
write_csv(all_accumulation_lims, "data-processed/all_accumulation_lims-aug8.csv")
all_accumulation_lims <- read_csv("data-processed/all_accumulation_lims.csv")
all_trad_accum_sum %>% 
  ggplot(aes(x = species_no, y = rdi_micro_tot_mean)) + geom_line(size = 1) +
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, x = species_no), data = WA_preds, alpha = 0.7, fill = "pink") 

p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) 
rdi_accum_plot <- p + 
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, x = species_no, group = dataset), data = all_accumulation_lims, alpha = 0.7, fill = "grey") +
  geom_line(aes(x = species_no, y = mean, group = dataset), data = all_accumulation_lims, alpha = 0.7, color = "black") +
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, x = species_no), data = filter(all_accumulation_lims, dataset == "GL"), alpha = 0.7, fill = "cadetblue") +
  geom_line(aes(x = species_no, y = mean), data = filter(all_accumulation_lims, dataset == "GL"), alpha = 0.7, color = "cadetblue") +
  ylab("") + xlab("") +
  scale_x_continuous(breaks = seq(1,10,1)) +
  theme(axis.text = element_text(size=16))
