


sub <- percentages %>% 
  filter(nutrient == "calcium")


sub$species_name[grepl("Acipenser", sub$species_name)]


references to keep:
  
  cine_refs <- read_csv("data-processed/cine-traits-new-species3.csv") 
  unique(cine_refs$reference)
  cine_keep_ref <- c(1, 2, 3, 4, 5, 6, 8, 11, 13, 17, 19, 20, 24, 25, 27, 28, 34, 37, 38, 39, 40, 41, 43, 44, 46, 49, 50, 52,53, 54,
    65, 66, 72, 90, 91)
  cine_keep_ref2 <- c(1, 2, 3, 4, 5, 6, 8, 11, 17, 19, 20, 24, 25, 27, 28, 34, 37, 38, 39, 40, 41, 43, 44, 49, 52,53, 54,
                    65, 66, 90, 91)
  
  cine_discard_refs <- setdiff(cine_refs$reference, cine_keep_ref2)


cine_traits_old <- read_csv("data-processed/cine-traits-new-species3.csv") %>% 
  filter(part != "not specified") %>% 
  # filter(reference %in% cine_keep_ref) %>% 
  mutate(log_concentration1 = log(concentration)) %>% 
  mutate(log_length1 = log(Length)) %>% 
  mutate(log_concentration = ifelse(is.na(log_concentration), log_concentration1, log_concentration)) %>% 
  mutate(log_length = ifelse(is.na(log_length), log_length1, log_length)) %>% 
  mutate(reference = as.character(reference)) %>% 
  mutate(species1 = ifelse(is.na(species1), latin_name, species1)) %>% 
  mutate(source = "cine")

cine_traits_new <- read_csv("data-processed/cine-traits-new-species3.csv") %>% 
  filter(part != "not specified") %>% 
  filter(reference %in% cine_keep_ref) %>% 
  mutate(log_concentration1 = log(concentration)) %>% 
  mutate(log_length1 = log(Length)) %>% 
  mutate(log_concentration = ifelse(is.na(log_concentration), log_concentration1, log_concentration)) %>% 
  mutate(log_length = ifelse(is.na(log_length), log_length1, log_length)) %>% 
  mutate(reference = as.character(reference)) %>% 
  mutate(species1 = ifelse(is.na(species1), latin_name, species1)) %>% 
  mutate(source = "cine")
