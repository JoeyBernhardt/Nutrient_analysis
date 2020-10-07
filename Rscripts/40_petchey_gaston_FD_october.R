### Petchey and Gaston's FD
library(fundiv)
library(tidyverse)
library(stringr)

#  Calculate.Q returns Rao's quadratic entropy (Q) as a measure of functional diversity for a set of communites, given another set of species traits. 
#  Landuse (communities) and trait files are data frames with species codes as rownames, sorted by the codes. All of the species in the landuse file must be within the trait file, but not necessarily vice versa. 
#  Land use types must be in alphbetical order, left to right (for labeling dendrograms correctly; not necessary if not writing the files out to PDFs).
#  Usage: When the files are in the current working directory (check with getwd() ), read them in with, e.g., Landuse.Best <- read.csv("Landuse.Best.csv") (For data from Best et al.)   
#  Then type Calculate.Q(Landuse.Best, BirdTraits).
#  Writing the files to the working directory by adding Calculate.Q(Landuse.Best, BirdTraits, T).
#  By default, the values of Q and FD are scaled to the maximum value in that set of communities, for comparing across studies. Set scale = FALSE to return raw values.



## ok we want the landuse file to be the species lists for each community, and the trait files

# Calcualting Petchey & Gaston's FD for communities under different land use management.
# See notes in Q.Calculator.

Calculate.FD <- function(landfile, traitfile, writefiles = T, scale = T){
  
  traits <- traitfile
  land <- landfile
  
  community.names = names(land) # get community type names
  
  # Reorder the species and community lists to make a dataframe with two columns: species and community.
  
  lands <- unlist(land) # Vectorize data frames
  dim(land) <- NULL # Vectorize matrices
  
  sppcomm <- data.frame(species = rep(rownames(landfile), length(community.names)), community = rep(community.names, each = 	nrow(landfile)))
  sppcomm <- sppcomm[!is.na(land), ]
  
  library(cluster)  # to get the daisy function -- can use nominal data, unlike dist
  library(stats) # for hclust
  
  distances = daisy(traits, metric = "gower")
  tree = hclust(distances)
  xtree = Xtree(tree)
  length(tree$height)
  
  FD <- tapply(sppcomm$species, sppcomm$community,
               function(x) Getlength(list(xtree[[1]], 
                                          xtree[[2]][!is.na(match(toupper(dimnames(xtree[[2]])[[1]]),toupper(x))),])))
  
  

  
  # Scaling to 1
  if (scale == TRUE){
    FDmax <- max(FD, na.rm = TRUE) 
    if(FDmax != 0) {
      FD <- FD / FDmax 
    }
  }
  
  # Writing the results to files
  if(writefiles==TRUE){
    write.csv(FD, file=paste("FD-", substitute(landfile), ".csv", sep=""))
    invisible()
  }
  else FD
}


#  Landuse (communities) and trait files are data frames with species codes as rownames, sorted by the codes. All of the species in the landuse file must be within the trait file, but not necessarily vice versa. 
#  Land use types must be in alphbetical order, left to right (for labeling dendrograms correctly; not necessary if not writing the files out to PDFs).


## landfiles are dataframes with species codes as rownames, sorted by the codes, columns from left to right
## each community, in alphabetical order

## ok prep data

trad_nuts_mean <-  read_csv("data-processed/traditional_foods_nutrients_cultures_for_analysis.csv") %>% 
  rename(latin_name = genus_species)


global_mean_nuts <- read_csv("data-processed/mean_nuts_oct-4-2020.csv") %>% 
  rename(latin_name = taxize_name)

sum(is.na(trad_nuts_mean$latin_name))

global <- sample_n(distinct(trad_nuts_mean, latin_name, .keep_all = TRUE), size = 40, replace = FALSE) %>% 
  mutate(culture = "global")

all <- bind_rows(trad_nuts_mean, global)


species_numbers <- all %>% 
  group_by(culture) %>% 
  summarise(n_species = n_distinct(latin_name)) %>% 
  filter(n_species >= 23) 

write_csv(species_numbers, "data-processed/species_numbers_october.csv")

trad2 <- all %>% 
  filter(culture %in% species_numbers$culture)


pres_abs <- trad2 %>% 
  spread(key = culture, value = latin_name) %>% 
  mutate(species_name = Micmac) %>% 
  mutate(species_name = ifelse(is.na(species_name), global, species_name)) %>% 
  mutate(species_name = ifelse(is.na(species_name), Nootkan, species_name)) %>%
  mutate(species_name = ifelse(is.na(species_name), `Montagnais-Naskapi`, species_name)) %>% 
  mutate(species_name = ifelse(is.na(species_name), Tlingit, species_name)) %>% 
  dplyr::select(species_name, 12:26) %>% 
  mutate_all(.funs= str_replace, ".*", "1") %>% 
  # mutate_all(.funs= str_replace_na, "0") %>% 
  arrange(species_name)  %>% 
  dplyr::select(-species_name)
  
  
  unique(pres_abs$species_name)

species_names <- trad2 %>% 
  spread(key = culture, value = latin_name) %>% 
  mutate(species_name = Micmac) %>% 
  mutate(species_name = ifelse(is.na(species_name), global, species_name)) %>% 
  mutate(species_name = ifelse(is.na(species_name), Nootkan, species_name)) %>%
  mutate(species_name = ifelse(is.na(species_name), `Montagnais-Naskapi`, species_name)) %>% 
  mutate(species_name = ifelse(is.na(species_name), Tlingit, species_name)) %>% 
  dplyr::select(species_name) %>% 
  arrange(species_name)

length(unique(species_names$species_name))

pres_abs <- as.data.frame(pres_abs)
rownames(pres_abs) <- species_names$species_name

landfile <- pres_abs

traitfile <- trad_nuts_mean %>% 
  distinct(latin_name, .keep_all = TRUE) %>% 
  dplyr::select(-culture) %>% 
  filter(latin_name %in% species_names$species_name) %>% 
  arrange(latin_name) %>% 
  dplyr::select(-latin_name) %>% 
  as.data.frame() %>% 
  dplyr::select(1:5)

rownames(traitfile) <- species_names$species_name


FDs <- Calculate.FD(landfile = landfile, traitfile = traitfile, scale = F, writefiles = F)

fd3 <- data.frame(FD = FDs, region = names(FDs))

fd2 <- left_join(fd, species_numbers, by =c("region" = "culture"))
fd4 <- left_join(fd3, species_numbers, by =c("region" = "culture"))

View(fd2)
View(fd4)

# write_csv(fd4, "data-processed/regional_functional_diversity.csv")
write_csv(fd3, "data-processed/regional_functional_diversity_october2020.csv")


# Do this for the global dataset, mean_nuts -------------------------------

## ok prep data

# # trad_nuts_mean <- read_csv("data-processed/trad-foods-mean.csv") %>% 
#   filter(!is.na(latin_name))

global_mean_nuts <- read_csv("data-processed/mean_nuts_oct-4-2020.csv") %>% 
  rename(latin_name = taxize_name)

# global_mean_nuts <- read_csv("data-processed/mean_nuts.csv") %>% 
#   rename(latin_name = species_name)

sum(is.na(trad_nuts_mean$latin_name))

global_sampling <- function(sample_size) {
  global_sample <- sample_n(distinct(global_mean_nuts, latin_name, .keep_all = TRUE), size = sample_size, replace = FALSE)
}

samples <- rep(40, times = 1000)

global_samples <- samples %>% 
  map_df(global_sampling, .id = "dataset")

# all <- bind_rows(trad_nuts_mean, global)


species_numbers <- global_samples %>% 
  group_by(dataset) %>% 
  summarise(n_species = n_distinct(latin_name)) %>% 
  filter(n_species >= 25) 

trad2 <- global_samples %>% 
  filter(dataset %in% species_numbers$dataset)


pres_abs <- trad2 %>% 
  spread(key = dataset, value = latin_name) %>% 
  mutate(species_name = "dataset") %>% 
  # mutate(species_name = ifelse(is.na(species_name), Wampanoag, species_name)) %>% 
  # mutate(species_name = ifelse(is.na(species_name), Nootkan, species_name)) %>%
  # mutate(species_name = ifelse(is.na(species_name), Cree, species_name)) %>% 
  # mutate(species_name = ifelse(is.na(species_name), `Montagnais-Naskapi`, species_name)) %>% 
  # mutate(species_name = ifelse(is.na(species_name), global, species_name)) %>% 
  dplyr::select(7:1007) %>% 
  mutate_all(.funs= str_replace, ".*", "1") %>% 
  # mutate_all(.funs= str_replace_na, "0") %>% 
  arrange(species_name)  %>% 
  dplyr::select(-species_name)


species_names$species_name <- global_mean_nuts$latin_name

length(unique(species_names$species_name))

pres_abs <- as.data.frame(pres_abs)
rownames(pres_abs) <- global_mean_nuts$latin_name

landfile <- pres_abs

traitfile <- global_mean_nuts %>% 
  distinct(latin_name, .keep_all = TRUE) %>% 
  dplyr::select(-subgroup) %>% 
  filter(latin_name %in% global_mean_nuts$latin_name) %>% 
  arrange(latin_name) %>% 
  dplyr::select(-latin_name) %>% 
  as.data.frame()

rownames(traitfile) <- global_mean_nuts$latin_name


FDs <- Calculate.FD(landfile = landfile, traitfile = traitfile, scale = F, writefiles = F)

global_FDs <- data.frame(FD = FDs, replicate = names(FDs))
write_csv(global_FDs, "data-processed/global_FD.csv")  # this looks wrong! use the version below



# Global FD take 2 --------------------------------------------------------

repeat_fd <- function(sample_size){
  
  global <- sample_n(distinct(trad_nuts_mean2, latin_name, .keep_all = TRUE), size = sample_size, replace = FALSE) %>% 
    mutate(culture = "global")
  
  pres_abs <- global %>% 
    spread(key = culture, value = latin_name) %>% 
    rename(species_name = global) %>% 
    mutate(global = 1) %>% 
    # mutate_all(.funs= str_replace_na, "0") %>% 
    arrange(species_name)  %>% 
    dplyr::select(global)
  
  
  species_names <- global %>% 
    rename(species_name = latin_name) %>% 
    dplyr::select(species_name) %>% 
    arrange(species_name)
  
  
  
  pres_abs <- as.data.frame(pres_abs)
  rownames(pres_abs) <- species_names$species_name
  
  landfile <- pres_abs
  
  traitfile <- global %>% 
    distinct(latin_name, .keep_all = TRUE) %>% 
    dplyr::select(-culture, -subgroup) %>% 
    filter(latin_name %in% species_names$species_name) %>% 
    arrange(latin_name) %>% 
    dplyr::select(-latin_name) %>% 
    as.data.frame()
  
  rownames(traitfile) <- species_names$species_name
  FDs <- Calculate.FD(landfile = landfile, traitfile = traitfile, scale = F, writefiles = F)
  fd3 <- data.frame(FD = FDs, region = names(FDs), sample_size = sample_size)
  return(fd3)
}


sample_sizes <- rep(40, 1000)
sample_sizes

global_fd <- sample_sizes %>% 
  map_df(repeat_fd, .id = "sample")

write_csv(global_fd, "data-processed/global_FD_repeat_october.csv")
