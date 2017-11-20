local-scale-analysis
================

#### Does biodiversity in local diets increase nutritional benefits?

``` r
library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Warning: package 'dplyr' was built under R version 3.4.2

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
library(vegan)
```

    ## Loading required package: permute

    ## Loading required package: lattice

    ## This is vegan 2.4-4

``` r
library(purrr)
library(here)
```

    ## here() starts at /Users/joeybernhardt/Documents/Nutrient_analysis

``` r
library(stringr)

new_global <- read_csv(here("data-processed", "new_global.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   species_name = col_character(),
    ##   subgroup = col_character(),
    ##   calcium = col_double(),
    ##   zinc = col_double(),
    ##   iron = col_double(),
    ##   epa = col_double(),
    ##   dha = col_double()
    ## )

``` r
accumulate_global <- function(data, threshold) {
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
    dplyr::select(species_name, 7:12) %>% 
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
```

``` r
res_global <- accumulate_global(sample_n(new_global, size = 40, replace = FALSE), threshold = 0.1) %>% 
  mutate(culture = "global")
```

    ## Joining, by = c("subgroup", "number_of_species")
