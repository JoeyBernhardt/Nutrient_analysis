

### get efficiency curve for 40 sampled randomly from the global pool
library(tidyverse)
library(stringr)

# nutrient fishing function -----------------------------------------------
mean_nuts <- read_csv("data-processed/mean_nuts_new.csv")
nutrient_fishing_function <- function(sample_size) {
  ntbl_sub1 <- mean_nuts %>% 
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
    mutate(cal_total = (calcium/species_number)) %>% ## get the amount of calcium each species will contribute
    mutate(zinc_total = (zinc/species_number)) %>% 
    mutate(iron_total = (iron/species_number)) %>% 
    mutate(epa_total = (epa/species_number)) %>% 
    mutate(dha_total = (dha/species_number)) %>%
    summarise_each(funs(sum), contains("total")) %>%  ## sum up all of each of the nutrients
    mutate(cal_grams = (cal_total/(1200))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
    mutate(iron_grams = (iron_total/(18))) %>%
    mutate(zinc_grams = (zinc_total/(11))) %>% 
    mutate(epa_grams = (epa_total/(1))) %>%
    mutate(dha_grams = (dha_total/(1))) %>%
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no, sample_id) %>% 
    select(-contains("total")) %>% 
    gather(key = nutrient, value = concentration, contains("grams")) %>% 
    group_by(species_no, sample_id) %>% 
    summarise(min_percentage = min(concentration)) %>%
    mutate(grams_required = 100/min_percentage)
}


samples_rep <- rep(10, 1000)


global_10 <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run") 

global_10$dataset <- "GL"


# write_csv(global_10, "data-processed/global_10_40spp_efficiency.csv")
write_csv(global_10, "data-processed/global_10_40spp_efficiency_aug2020.csv")

### get the efficiency curves for the local cases
all_trad_eff_raw <- read_csv("data-processed/all_trad_reps-aug2020.csv")
global_10 <- read_csv("data-processed/global_10_40spp_efficiency_aug2020.csv")

unique(all_trad_eff$dataset)




all_trad_eff1 <- all_trad_eff_raw %>%
  filter(!is.infinite(grams_required)) %>% 
  group_by(species_no, culture) %>% 
  mutate(grams_required = grams_required/10) %>% 
  summarise_each(funs(mean, median, std.error), grams_required) %>% 
  rename(dataset = culture) %>% 
  rename(grams_median = median)


global_10_2 <- global_10 %>% 
  group_by(dataset, species_no) %>% 
  summarise(grams_median = median(grams_required)/10)
  

all_trad_effb <- bind_rows(all_trad_eff1, global_10_2)

all_trad_eff <- all_trad_effb %>% 
  ungroup() %>% 
  filter(dataset != "global") %>% 
  filter(!dataset %in% c("25", "25", "29", "57", "40", "20")) %>%
  mutate(dataset = str_replace(dataset, "Inuit-Inupiaq", "II")) %>% 
  mutate(dataset = str_replace(dataset, "Central Salish", "CS")) %>% 
  mutate(dataset = str_replace(dataset, "Wampanoag", "WA")) %>% 
  mutate(dataset = str_replace(dataset, "Cree", "CR")) %>%
  mutate(dataset = str_replace(dataset, "Nootkan", "NO")) %>% 
  mutate(dataset = str_replace(dataset, "Bella Coola", "BC")) %>% 
  mutate(dataset = str_replace(dataset, "Tlingit", "TL")) %>%
  mutate(dataset = str_replace(dataset, "Haida", "HA")) %>%
  mutate(dataset = str_replace(dataset, "Tsimshian", "TS")) %>% 
  mutate(dataset = str_replace(dataset, "Montagnais-Naskapi", "MN")) %>%
  mutate(dataset = str_replace(dataset, "Yupik", "YU")) %>% 
  mutate(dataset = str_replace(dataset, "Abenaki", "AB")) %>%
  mutate(dataset = str_replace(dataset, "Micmac", "MI")) %>%
  mutate(dataset = str_replace(dataset, "Kwakiutl", "KW")) 



GL_mod <- nls(formula = (grams_median ~ a * species_no^b), data = filter(all_trad_eff, dataset == "GL"),  start = c(a=10000, b=-0.5))
GL_boot <- nlsBoot(GL_mod)
GL_boot$bootCI
GL_boot_df <- as_data_frame(GL_boot$coefboot) 
GL_b <- as_data_frame(GL_boot$bootCI) 
GL_b$culture <- "GL"

AB_mod <- nls(formula = (grams_median ~ a * species_no^b), data = filter(all_trad_eff, dataset == "AB"),  start = c(a=10000, b= -0.5))
AB_boot <- nlsBoot(AB_mod)
AB_boot$bootCI
AB_boot_df <- as_data_frame(AB_boot$coefboot)
AB_b <- as_data_frame(AB_boot$bootCI) 
AB_b$culture <- "AB"


YU_mod <- nls(formula = (grams_median ~ a * species_no^b), data = filter(all_trad_eff, dataset == "YU"),  start = c(a=10000, b= -0.5))
YU_boot <- nlsBoot(YU_mod)
YU_boot$bootCI
YU_boot_df <- as_data_frame(YU_boot$coefboot) 
YU_b <- as_data_frame(YU_boot$bootCI) 
YU_b$culture <- "YU"


WA_mod <- nls(formula = (grams_median ~ a * species_no^b), data = filter(all_trad_eff, dataset == "WA"),  start = c(a=10000, b= -0.5))
WA_boot <- nlsBoot(WA_mod)
WA_boot$bootCI
WA_boot_df <- as_data_frame(WA_boot$coefboot) 
WA_b <- as_data_frame(WA_boot$bootCI) 
WA_b$culture <- "WA"

TL_mod <- nls(formula = (grams_median ~ a * species_no^b), data = filter(all_trad_eff, dataset == "TL"),  start = c(a=10000, b= -0.5))
TL_boot <- nlsBoot(TL_mod)
TL_boot$bootCI
TL_boot_df <- as_data_frame(TL_boot$coefboot)
TL_b <- as_data_frame(TL_boot$bootCI) 
TL_b$culture <- "TL"


TS_mod <- nls(formula = (grams_median ~ a * species_no^b), data = filter(all_trad_eff, dataset == "TS"),  start = c(a=10000, b= -0.5))
TS_boot <- nlsBoot(TS_mod)
TS_boot$bootCI
TS_boot_df <- as_data_frame(TS_boot$coefboot)
TS_b <- as_data_frame(TS_boot$bootCI) 
TS_b$culture <- "TS"


BC_mod <- nls(formula = (grams_median ~ a * species_no^b), data = filter(all_trad_eff, dataset == "BC"),  start = c(a=10000, b= -0.5))
BC_boot <- nlsBoot(BC_mod)
BC_boot$bootCI
BC_boot_df <- as_data_frame(BC_boot$coefboot)
BC_b <- as_data_frame(BC_boot$bootCI) 
BC_b$culture <- "BC"

HA_mod <- nls(formula = (grams_median ~ a * species_no^b), data = filter(all_trad_eff, dataset == "HA"),  start = c(a=10000, b= -0.5))
HA_boot <- nlsBoot(HA_mod)
HA_boot$bootCI
HA_boot_df <- as_data_frame(HA_boot$coefboot)
HA_b <- as_data_frame(HA_boot$bootCI) 
HA_b$culture <- "HA"


NO_mod <- nls(formula = (grams_median ~ a * species_no^b), data = filter(all_trad_eff, dataset == "NO"),  start = c(a=10000, b= -0.5))
NO_boot <- nlsBoot(NO_mod)
NO_boot$bootCI
NO_boot_df <- as_data_frame(NO_boot$coefboot)
NO_b <- as_data_frame(NO_boot$bootCI) 
NO_b$culture <- "NO"


MN_mod <- nls(formula = (grams_median ~ a * species_no^b), data = filter(all_trad_eff, dataset == "MN"),  start = c(a=10000, b= -0.5))
MN_boot <- nlsBoot(MN_mod)
MN_boot$bootCI
MN_boot_df <- as_data_frame(MN_boot$coefboot)
MN_b <- as_data_frame(MN_boot$bootCI) 
MN_b$culture <- "MN"

MI_mod <- nls(formula = (grams_median ~ a * species_no^b), data = filter(all_trad_eff, dataset == "MI"),  start = c(a=10000, b= -0.5))
MI_boot <- nlsBoot(MI_mod)
MI_boot$bootCI
MI_boot_df <- as_data_frame(MI_boot$coefboot)
MI_b <- as_data_frame(MI_boot$bootCI) 
MI_b$culture <- "MI"


KW_mod <- nls(formula = (grams_median ~ a * species_no^b), data = filter(all_trad_eff, dataset == "KW"),  start = c(a=10000, b= -0.5))
KW_boot <- nlsBoot(KW_mod)
KW_boot$bootCI
KW_boot_df <- as_data_frame(KW_boot$coefboot)
KW_b <- as_data_frame(KW_boot$bootCI) 
KW_b$culture <- "KW"


II_mod <- nls(formula = (grams_median ~ a * species_no^b), data = filter(all_trad_eff, dataset == "II"),  start = c(a=10000, b= -0.5))
II_boot <- nlsBoot(II_mod)
II_boot$bootCI
II_boot_df <- as_data_frame(II_boot$coefboot)
II_b <- as_data_frame(II_boot$bootCI) 
II_b$culture <- "II"


CR_mod <- nls(formula = (grams_median ~ a * species_no^b), data = filter(all_trad_eff, dataset == "CR"),  start = c(a=10000, b= -0.5))
CR_boot <- nlsBoot(CR_mod)
CR_boot$bootCI
CR_boot_df <- as_data_frame(CR_boot$coefboot)
CR_b <- as_data_frame(CR_boot$bootCI) 
CR_b$culture <- "CR"


CS_mod <- nls(formula = (grams_median ~ a * species_no^b), data = filter(all_trad_eff, dataset == "CS"),  start = c(a=10000, b= -0.5))
CS_boot <- nlsBoot(CS_mod)
CS_boot$bootCI
CS_boot_df <- as_data_frame(CS_boot$coefboot)
CS_b <- as_data_frame(CS_boot$bootCI) 
CS_b$culture <- "CS"


# Merge all params --------------------------------------------------------

all_b_params <- bind_rows(GL_b, CS_b, CR_b, II_b, HA_b, MN_b, NO_b, BC_b, KW_b, TS_b, TL_b, WA_b, YU_b, AB_b, MI_b) %>% 
  clean_names() %>% 
  rename(lower = x2_5_percent,
         upper = x97_5_percent) %>% 
  filter(median < 1)

# write_csv(all_b_params, "data-processed/all_b_params_accumulation.csv")
write_csv(all_b_params, "data-processed/all_b_params_accumulation_aug2020.csv")

# all_b_params <- read_csv("data-processed/all_b_params_accumulation.csv")
all_b_params <- read_csv("data-processed/all_b_params_accumulation_aug2020.csv")

all_b_params %>% 
  filter(culture != "GL") %>% 
  summarise_each(funs(mean, std.error), median) %>% View 

efficiency_b_plot <- ggplot(aes(x = reorder(culture, median), y = median), data = all_b_params) + geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +ylab("b estimate") + xlab("Region")
ggsave("figures/b_params_efficiency.png", width = 5, height = 5)


# plot the functions ------------------------------------------------------

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
CS_preds <- CS_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required)) %>% 
  mutate(dataset = "CS")
#14
II_preds <- II_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required))%>% 
  mutate(dataset = "II")
#13
HA_preds <- HA_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required)) %>% 
  mutate(dataset = "HA")
#12
KW_preds <- KW_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required)) %>% 
  mutate(dataset = "KW")
#11
CR_preds <- CR_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required)) %>% 
  mutate(dataset = "CR")
#10
NO_preds <- NO_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required))%>% 
  mutate(dataset = "NO")
#9
MN_preds <- MN_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required)) %>% 
  mutate(dataset = "MN")
#8
TL_preds <- TL_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required)) %>% 
  mutate(dataset = "TL")
#7
TS_preds <- TS_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required)) %>% 
  mutate(dataset = "TS")
#6
GL_preds <- GL_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required)) %>% 
  mutate(dataset = "GL")
#5
MI_preds <- MI_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required)) %>% 
  mutate(dataset = "MI")
#4
BC_preds <- BC_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required)) %>% 
  mutate(dataset = "BC")
#3
AB_preds <- AB_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required)) %>% 
  mutate(dataset = "AB")

#2
YU_preds <- YU_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required)) %>% 
  mutate(dataset = "YU")

#1
WA_preds <- WA_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(grams_required, probs=0.025),
            q97.5=quantile(grams_required, probs=0.975),
            mean = mean(grams_required)) %>% 
  mutate(dataset = "WA")

all_accum_lims <- bind_rows(WA_preds, YU_preds, TL_preds, TS_preds, CS_preds, CR_preds, HA_preds, BC_preds, GL_preds,
                            KW_preds, MN_preds, NO_preds, AB_preds, II_preds, MI_preds)
all_efficiency_lims <- all_accum_lims

# write_csv(all_efficiency_lims, "data-processed/all_efficiency_lims.csv")
write_csv(all_efficiency_lims, "data-processed/all_efficiency_lims_aug2020.csv")

p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) 
efficiency_local <- p + 
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, x = species_no, group = dataset), data = all_accum_lims, alpha = 0.7, fill = "grey") +
  geom_line(aes(x = species_no, y = mean, group = dataset), data = all_accum_lims, alpha = 0.7, color = "black") +
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, x = species_no), data = filter(all_accum_lims, dataset == "GL"), alpha = 0.7, fill = "cadetblue") +
  geom_line(aes(x = species_no, y = mean), data = filter(all_accum_lims, dataset == "GL"), alpha = 0.7, color = "cadetblue") +
  ylab("") + xlab("") +
  scale_x_continuous(breaks = seq(1,10,1)) +
  theme(axis.text = element_text(size=16))

ggsave("figures/grams_required_local_global_aug2020.png", width = 5, height = 5)
