library(tidyverse)
library(vegan)
library(stringr)

trait_data <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data-processed/n.long_lat3.csv")


ntbl.RDI.30 <- trait_data %>% 
  spread(nutrient, concentration) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(mean.CA = mean(ca_mg, na.rm = TRUE),
            mean.ZN = mean(zn_mg, na.rm = TRUE), 
            mean.FE = mean(fe_mg, na.rm = TRUE),
            mean.EPA = mean(epa, na.rm = TRUE),
            mean.DHA = mean(dha, na.rm = TRUE)) %>% 
  mutate(RDI.CA = ifelse(mean.CA > (1200*0.3), 1, 0)) %>% 
  mutate(RDI.FE = ifelse(mean.FE > (18*0.3), 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(mean.ZN > (11*0.3), 1, 0)) %>%
  mutate(RDI.EPA = ifelse(mean.EPA > (1*0.3), 1, 0)) %>% 
  mutate(RDI.DHA = ifelse(mean.DHA > (1*0.3), 1, 0)) %>% 
  ungroup() %>% 
  mutate(RDI.micro.tot = rowSums(.[8:12])) %>% 
  filter(!is.na(RDI.micro.tot)) 

nuts <- ntbl.RDI.30 %>% 
  dplyr::select(species_name, subgroup, contains("RDI")) %>% 
  mutate(ID = rownames(.))

nuts %>% 
  ggplot(aes(x = ID, y = RDI.micro.tot)) + geom_point()


g <- ggplot(nuts, aes(reorder(ID, RDI.micro.tot), RDI.micro.tot))
g + geom_point() 


sample_fish <- function(nuts, sample_number){
  subset_fish <- nuts %>% 
    sample_n(size = sample_number, replace = TRUE)
  write_csv(subset_fish, paste0("./data-processed-samples/richness_", sample_number, ".csv"))
}

sample_fish(8)


for(i in seq(1, 108, 1)){
  sample_fish(nuts, i)
}


richness_files <- list.files("./data-processed-samples", full.names = TRUE)


names(richness_files) <- richness_files %>% 
  gsub(pattern = ".csv$", replacement = "")


#### Step 3: read in all the files!

all_richness <- map_df(richness_files, read_csv, .id = "file_name")


richness <- all_richness %>% 
  mutate(file_name = str_replace(file_name, "./data-processed-samples/richness_", "")) %>%
  mutate(file_name = as.numeric(file_name)) %>% 
  rename(richness_level = file_name)

richness %>% 
  group_by(richness_level) %>% 
  summarise_each(funs(mean), RDI.micro.tot) %>% 
  ggplot(aes(x = richness_level, y = RDI.micro.tot)) + geom_point()


sample_number <- seq(1,30)
sample_fish <- function(nuts, sample_number) {
  y <- vector()
    x <- sample_number
    y <- nuts %>% 
      sample_n(size = sample_number, replace = TRUE) %>% 
      select(RDI.micro.tot) %>% 
      summarise(mean_RDI_targets = mean(RDI.micro.tot)) 
    df <- data.frame(x, y)
    colnames(df) <- c("sample_number","mean_RDI_targets")
    combined_df <- rbind(df)
    return(combined_df)
    }
  

sample_fish(nuts, 8)

mylist <- list()
for(i in 1:30) {
  vec <- numeric(30)
  for(j in 1:30) {
  vec[[j]] <- sample_fish(nuts, sample_number)
  }
  
mylist[[i]] <- vec()
}

df <- do.call("rbind", mylist)



sample_fish <- function(nuts, sample_number) {
  output[i,] <- nuts %>% 
    sample_n(size = sample_number[i], replace = TRUE) %>% 
    select(RDI.micro.tot) %>% 
    summarise(mean_RDI_targets = mean(RDI.micro.tot)) %>% 
    mutate(sample_no = sample_number)
}


sample_n


df <- data.frame(sample_number = double(), 
                 mean_RDI_targets = double())

resultlist <- list() 
for(i in length(sample_number)){
  resultlist[i] <- sample_fish(nuts, sample_number)nuts %>% 
    sample_n(size = sample_number, replace = TRUE) %>% 
    select(RDI.micro.tot) %>% 
    summarise(mean_RDI_targets = mean(RDI.micro.tot))
  
}

resultdfr <- as.data.frame(do.call("rbind", resultlist)) 


sample_fish_df <- function(sample_number) {
  y <- vector()
  x <- sample_number
  y <- nuts %>% 
    sample_n(size = sample_number, replace = TRUE) %>% 
    select(RDI.micro.tot) %>% 
    summarise_each(funs(mean, sum), RDI.micro.tot) 
  df <- data.frame(x, y)
  colnames(df) <- c("sample_number","mean_RDI_targets", "total_RDI_targets")
  combined_df <- rbind(df)
  return(combined_df)
}

sample_fish_df(nuts, 8)


sample_number_rep <- rep(sample_number, 100)
results <- sample_number_rep %>% 
  map_df(sample_fish_df)

results %>% 
  ggplot(aes(x = sample_number, y = mean_RDI_targets)) + geom_point() + theme_bw()



## let's try something different here, with cumsum

sample_fish_df <- function(sample_number) {
  y <- vector()
  x <- sample_number
  y <- nuts %>% 
    sample_n(size = sample_number, replace = TRUE) %>% 
    select(RDI.micro.tot) %>% 
    summarise_each(funs(mean, sum), RDI.micro.tot) 
  df <- data.frame(x, y)
  colnames(df) <- c("sample_number","mean_RDI_targets", "total_RDI_targets")
  combined_df <- rbind(df)
  return(combined_df)
}

sample_fish_targets <- function(sample_number) {
  y <- vector()
  x <- sample_number
y <- nuts %>% 
  # filter(subgroup == "finfish") %>% 
  sample_n(size = sample_number, replace = TRUE) %>%
  summarise_each(funs(sum), RDI.CA, RDI.FE, RDI.ZN, RDI.EPA, RDI.DHA) %>% 
  mutate(sample_size = sample_number) %>% 
  mutate(ca_target = ifelse(RDI.CA != 0, 1, 0)) %>% 
  mutate(fe_target = ifelse(RDI.FE != 0, 1, 0)) %>%
  mutate(zn_target = ifelse(RDI.ZN != 0, 1, 0)) %>%
  mutate(epa_target = ifelse(RDI.EPA != 0, 1, 0)) %>%
  mutate(dha_target = ifelse(RDI.DHA != 0, 1, 0)) %>% 
  mutate(distinct_targets = sum(ca_target, fe_target, zn_target, epa_target, dha_target)) 
df <- data.frame(x, y)
combined_df <- rbind(df)
return(combined_df)
}

  
sample_number_rep <- rep(sample_number, 1000)


results_no_moll <- sample_number_rep %>% 
  map_df(sample_fish_targets)

results <- sample_number_rep %>% 
  map_df(sample_fish_targets)

no_moll_summary <- results_no_moll %>% 
  group_by(sample_size) %>% 
  summarise_each(funs(mean, std.error), distinct_targets) %>% 
  mutate(group = "no molluscs")

write_csv(no_moll_summary, "data-processed/nut_accumulation_no_molluscs.csv")

summary <- results %>% 
  group_by(sample_size) %>% 
  summarise_each(funs(mean, std.error), distinct_targets) %>% 
  mutate(group = "all") 

write_csv(summary, "data-processed/nut_accumulation.csv")


data <- read_csv("data-processed/nut_accumulation.csv")

all_summary <- bind_rows(no_moll_summary, summary)

data %>% 
  ggplot(aes(x = sample_size, y = mean, group = group, color = group)) + geom_line(size = 1) + theme_bw() +
  geom_ribbon(aes(ymin = mean - 1.96*std.error, ymax = mean + 1.96*std.error, fill = group), alpha = 0.5, size = 0)
