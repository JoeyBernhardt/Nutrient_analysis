local-scale-analysis
================

#### Does biodiversity in local diets increase nutritional benefits?

``` r
library(tidyverse)
library(vegan)
library(purrr)
library(here)
library(stringr)
library(cowplot)
library(broom)

new_global <- read_csv(here("data-processed", "new_global.csv"))
```

Here we create our nutrient accumulation curves. Here I'm showing you the process for the global species pool, but you can imagine that we'd repeat this process for each local diet.

``` r
accumulate_global <- function(data, threshold) {
  ntbl.RDI.all <- data %>% 
    mutate(RDI.CA = ifelse(calcium > (1200*threshold), 1, 0)) %>% ## here we create a matrix of 0s and 1s, corresponding to whether the sample reaches DRI or not
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

Apply the accumulate function to 40 species sampled randomly from the global pool. Here we sample out 40 because this is the average species pool size at the local scale. For the local diets, we sample from the full local species pool.

``` r
res_global <- accumulate_global(sample_n(new_global, size = 40, replace = FALSE), threshold = 0.1) %>% 
  mutate(culture = "global")
```

    ## Joining, by = c("subgroup", "number_of_species")

Bring in the outputs from the resampling process above, but for each local diet.

``` r
res <- read_csv(here("data-processed", "nut_accumulation_trad_foods.csv"))
res_all <- bind_rows(res, res_global)
nuts_mean <- read_csv(here("data-processed", "trad-foods-mean.csv"))

species_numbers <- nuts_mean %>% ## get a list of communities for which we have at least 25 species
  group_by(culture) %>% 
  summarise(n_species = n_distinct(latin_name)) %>% 
  filter(n_species >= 25) 
```

Plot it.

``` r
res_all %>% 
  filter(culture %in% species_numbers$culture | culture == "global") %>% 
  filter(number_of_species < 11) %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets, group = culture)) +
  geom_ribbon(aes(ymin = number_of_targets - se, ymax = number_of_targets + se), alpha = 0.05, size = 0) +
  geom_line(size =.5, alpha = 0.5) +
  geom_line(color = "cadetblue", size =1, data = filter(res_all, culture == "global", number_of_species < 11)) +
  ylab("Number of nutrient requirements fulfilled (10% DRI)") +
  xlab("Number of species") + theme(text = element_text(size=14)) + 
  theme(legend.title=element_blank()) + theme_classic() +
  ylim(0,5) +
  scale_x_continuous(breaks = seq(1,10,1))
```

![](42_local_scale_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

``` r
res_all %>% 
  mutate(culture = ifelse(culture == "global", "Global", culture)) %>% 
  filter(culture %in% species_numbers$culture | culture == "Global") %>% 
  filter(number_of_species < 11) %>% 
  ggplot(aes(x = number_of_species, y = number_of_targets)) + geom_line(size =.5) +
  geom_ribbon(aes(ymin = number_of_targets - se, ymax = number_of_targets + se), alpha = 0.2, size = 0) +
  ylab("Number of nutrient requirements fulfilled (10% DRI)") +
  xlab("Number of species") + theme(text = element_text(size=12)) + 
  theme(legend.title=element_blank()) + theme_classic() +
  ylim(0,5) +
  scale_x_continuous(breaks = seq(1,10,2)) +
  facet_wrap( ~ culture) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
```

![](42_local_scale_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png)

OK now fit power functionsto these curves

``` r
mod <- res_all %>% 
  filter(culture %in% species_numbers$culture | culture == "global") %>% 
  filter(number_of_species < 11) %>% 
  group_by(culture) %>% 
  do(tidy(nls(formula = (number_of_targets ~ a * number_of_species^b),data = .,  start = c(a=1, b=0.3)))) 


mod %>% 
   ggplot(aes(x = reorder(culture, estimate), y = estimate)) + geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Culture") + facet_wrap( ~ term, scales = "free")
```

![](42_local_scale_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png)

Now onto FD. Calculate FD function gives us Petchey and Gaston's FD, code for this comes from Dan McGlynn (hence the 'landfile', and 'traitfile' names, but aiming to keep it consistent here, so I'm not changing them). (code hidden here for the 'Getlength' function and the 'Xtree' function. You can see the code by looking at the raw version of this doc.)

``` r
Calculate.FD <- function(landfile, traitfile, writefiles = T, scale = T){
  
  traits <- traitfile
  land <- landfile
  
  community.names = names(land) # get community type names
  
  # Reorder the species and community lists to make a dataframe with two columns: species and community.
  
  lands <- unlist(land) # Vectorize data frames
  dim(land) <- NULL # Vectorize matrices
  
  sppcomm <- data.frame(species = rep(rownames(landfile), length(community.names)), community = rep(community.names, each =     nrow(landfile)))
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
```

Data prep. Make two input dataframes 1. has the list of species found in each diet (e.g. landfile) 2. has the nutrient concentrations for each species. (e.g. traitfile). I hid the code for this here, b/c it's a bit long and unnecessary, but you can see it by looking at the raw version of this doc.

Calculate FD for each local diet

``` r
FDs <- Calculate.FD(landfile = landfile, traitfile = traitfile, scale = F, writefiles = F)
```

Calculate expected FD, given sample sizes of each diet

``` r
repeat_fd <- function(sample_size){

global <- sample_n(distinct(nuts_mean, latin_name, .keep_all = TRUE), size = sample_size, replace = FALSE) %>% 
  mutate(culture = "global")

pres_abs <- global %>% 
  spread(key = culture, value = latin_name) %>%
 rename(species_name = global) %>% 
  mutate(global = 1) %>% 
  # mutate_all(.funs= str_replace_na, "0") %>% 
  arrange(species_name)  %>% 
  select(global)


species_names <- global %>% 
  rename(species_name = latin_name) %>% 
  select(species_name) %>% 
  arrange(species_name)



pres_abs <- as.data.frame(pres_abs)
rownames(pres_abs) <- species_names$species_name

landfile <- pres_abs

traitfile <- global %>% 
  distinct(latin_name, .keep_all = TRUE) %>% 
  select(-culture, -subgroup) %>% 
  filter(latin_name %in% species_names$species_name) %>% 
  arrange(latin_name) %>% 
  select(-latin_name) %>% 
  as.data.frame()

rownames(traitfile) <- species_names$species_name
FDs <- Calculate.FD(landfile = landfile, traitfile = traitfile, scale = F, writefiles = F)
fd3 <- data.frame(FD = FDs, region = names(FDs), sample_size = sample_size)
return(fd3)
}
```

This is the key step, which repeatedly resamples from the global pool, with sample sizes equivalent to each diet size, and calculates the FD

``` r
sample_sizes <- rep(species_numbers$n_species, 1000)


exp_df <- sample_sizes %>% 
  map_df(repeat_fd, .id = "sample")
```

Since that will take a while, let's just bring in the outputs of that.

``` r
observed_fd <- read_csv(here("data-processed", "regional_functional_diversity.csv"))
exp_df <- read_csv(here("data-processed", "expected_FD_regional.csv"))

expected_fds <- exp_df %>% 
  group_by(sample_size) %>% 
  summarise(exp_df = mean(FD))

all_fds <- left_join(observed_fd, expected_fds, by = c("n_species" = "sample_size"))
```

``` r
all_fds %>% 
  ggplot(aes(x = exp_df, y = FD)) + geom_point(size = 2) +
  geom_abline(slope = 1, intercept = 0) +
  theme_classic() +ylim(2.5, 4) + xlim(2.5, 4) +
  xlab("Expected FD") + ylab("Observed FD")
```

    ## Warning: Removed 3 rows containing missing values (geom_point).

![](42_local_scale_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-15-1.png)

``` r
all_fds %>% 
  ggplot(aes(x = n_species, y = FD)) + geom_point() +
  theme_classic() + geom_smooth(method = "lm") +
  xlab("Species richness") + ylab("FD")
```

![](42_local_scale_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-15-2.png)

``` r
b_terms <- read_csv(here("data-processed", "b_terms_bef.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   culture = col_character(),
    ##   term = col_character(),
    ##   estimate = col_double(),
    ##   std.error = col_double(),
    ##   statistic = col_double(),
    ##   p.value = col_double()
    ## )

``` r
all_terms <- left_join(b_terms, species_numbers)
```

    ## Joining, by = "culture"

``` r
all_fds_b <- left_join(all_fds, all_terms)
```

    ## Joining, by = "n_species"

``` r
all_fds_b %>% 
  mutate(redundancy = exp_df - FD) %>% 
  ggplot(aes(x = redundancy, y = estimate)) + geom_point() +
  geom_smooth(method = "lm") +
  xlab("Functional redundancy") + ylab("Biodiversity effect (b)")
```

![](42_local_scale_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-1.png)

``` r
all_fds_b %>% 
  mutate(redundancy = exp_df - FD) %>% 
  lm(estimate ~ redundancy, data = .) %>% 
  summary()
```

    ## 
    ## Call:
    ## lm(formula = estimate ~ redundancy, data = .)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.071974 -0.012007 -0.008726  0.005999  0.064273 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.30372    0.01753  17.329 4.24e-13 ***
    ## redundancy  -0.03485    0.01593  -2.188   0.0414 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.03452 on 19 degrees of freedom
    ## Multiple R-squared:  0.2012, Adjusted R-squared:  0.1592 
    ## F-statistic: 4.786 on 1 and 19 DF,  p-value: 0.0414