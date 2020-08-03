



library(readxl)
library(janitor)
tox <- read_excel("data/Hall-trace-elements.xlsx") %>% 
  clean_names() %>%
  mutate(species = str_to_lower(species)) %>% 
  mutate(area = str_to_lower(area)) %>% 
  mutate(part = str_to_lower(part)) %>% 
  filter(part != "liver") %>% 
  # select(6:20) %>% 
  filter(cadmium != 2.61) %>% 
  filter(nickel != 1.810) %>% 
  mutate(mercury = ifelse(subgroup == "mollusc", mercury*0.30, mercury*0.95)) %>% 
  rename(methylmercury = mercury) 

times_hundred <- function(x, na.rm = FALSE) (x*100)
names_tox <- names(tox)
tox_sum <- tox %>% 
  group_by(species) %>% 
  mutate_at(c(names_tox[6:20]), times_hundred) %>% 
  summarise_at(c(names_tox[6:20]), mean) %>% 
  dplyr::select(species, 2:5) %>% 
  gather(key = nutrient, value = concentration, 2:5) %>% 
  mutate(concentration = ifelse(concentration == 0, 0.00001, concentration)) %>%
  spread(key= nutrient, value = concentration) 


nutrient_fishing_function <- function(sample_size) {
  ntbl_sub1 <- tox_sum %>% 
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
    mutate(cadmium_total = (cadmium/species_number)) %>% ## get the amount of calcium each species will contribute
    mutate(mercury_total = (methylmercury/species_number)) %>% 
    mutate(lead_total = (lead/species_number)) %>% 
    mutate(arsenic_total = (arsenic/species_number)) %>%
    summarise_each(funs(sum), contains("total")) %>%   ## sum up all of each of the nutrients, to get total amount of nutrient per sample
    mutate(cadmium_grams = (cadmium_total/(70*threshold))) %>% ## divide that total by the RDI, and into 100 to find out the number of grams required to reach target
    mutate(mercury_grams = (mercury_total/(16*threshold))) %>%
    mutate(lead_grams = (lead_total/(250*threshold))) %>% 
    mutate(arsenic_grams = (arsenic_total/(150*threshold))) %>%
    mutate(ptdi_cad = ifelse(cadmium_grams >= 1, 1, 0)) %>% ## if the amount of calcium exceeds 1 (i.e. reaches threshold), give it a value of 1, else 0
    mutate(ptdi_merc = ifelse(mercury_grams >= 1, 1, 0)) %>% 
    mutate(ptdi_lead = ifelse(lead_grams >= 1, 1, 0)) %>% 
    mutate(ptdi_arsenic = ifelse(arsenic_grams >= 1, 1, 0)) %>%
    ungroup() %>% 
    mutate(rdi_micro_tot = rowSums(.[11:14])) %>%  ## add up all the targets reached in one sample
    dplyr::rename(species_no = species_number) %>% 
    group_by(species_no, sample_id) %>% 
    dplyr::select(-contains("total")) %>% 
    mutate(threshold_level = threshold)
}


samples_rep <- rep(10, 100)

threshold <- 0.1
library(plotrix)
global_10_tox01 <- samples_rep %>% 
  map_df(nutrient_fishing_function, .id = "run") %>% 
  group_by(run, species_no) %>% 
  summarise_each(funs(mean, std.error), rdi_micro_tot)

global_10_tox %>% 
  group_by(species_no) %>% 
  summarise_each(funs(median), mean) %>% 
  ggplot(aes(x = species_no, y = mean )) + geom_point()
write_csv(global_10_tox, "data-processed/global_10_tox.csv")

globe_sum <- global_10_tox1 %>% 
  group_by(species_no) %>% 
  summarise_each(funs(median), mean)

write_csv(global_10_tox01, "data-processed/global_10_tox1.csv")
globe_sum <- global_10_tox05 %>% 
  group_by(species_no) %>% 
  summarise_each(funs(median), mean)
write_csv(global_10_tox05, "data-processed/global_10_tox05.csv")

globe_sum01 <- global_10_tox01 %>% 
  group_by(species_no) %>% 
  summarise_each(funs(median), mean)

globe_sum1 <- global_10_tox%>% 
  group_by(species_no) %>% 
  summarise_each(funs(mean), mean)


lm(log(mean) ~ log(species_no), data = globe_sum1) %>% summary()

global_10_tox %>% 
  ggplot(aes(x = species_no, y = mean )) + geom_jitter()

library(nlstools)
GL_mod <- nls(formula = (mean ~ a * species_no^b), data = global_10_tox,  start = c(a=2, b=0.5))
GL_boot <- nlsBoot(GL_mod)
GL_boot$bootCI
GL_estiboot <- GL_boot$estiboot
GL_boot_df <- as_data_frame(GL_boot$coefboot) 
GL_b <- as_data_frame(GL_boot$bootCI) 
GL_b$culture <- "GL"

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


CS_preds <- GL_boot_df %>% 
  mutate(replicate = rownames(.)) %>% 
  split(.$replicate) %>% 
  map_df(prediction_function, .id = "replicate") %>% 
  group_by(species_no) %>% 
  summarise(q2.5=quantile(DRI_targets, probs=0.025),
            q97.5=quantile(DRI_targets, probs=0.975),
            mean = mean(DRI_targets)) %>% 
  mutate(dataset = "CS")


CS_preds %>% 
  ggplot(aes(x = species_no, y = mean)) + geom_line() +
  geom_ribbon(aes(x = species_no, ymin = q2.5, ymax = q97.5), alpha = 0.1) +
  xlab("Species richness") +
  ylab("Nc (# PDTI)") +
  scale_x_continuous(breaks = seq(1,10,1))+
  scale_y_continuous(breaks = seq(1,2,0.15))
ggsave("figures/tox-replacement-mean.pdf", width = 6, height = 4)


plot2 <- CS_preds %>% 
  ggplot(aes(x = species_no, y = mean)) + geom_line() +
  geom_ribbon(aes(x = species_no, ymin = q2.5, ymax = q97.5), alpha = 0.1) +
  xlab("Species richness") +
  ylab("Nc (# PDTI)") +
  scale_x_continuous(breaks = seq(1,10,1))+
  scale_y_continuous(breaks = seq(1,2,0.15))
