# Nutrient analysis plots and models

Some background on the project: 

*One of the most widely studied and universally important benefits that humans derive from natural ecosystems is food provisioning. Indeed, many coastal human communities rely on wild harvests from local aquatic ecosystems to meet nutritional requirements for macronutrients, such as protein and fats, and micronutrients, such as vitamins and minerals. The value of a fish species in terms of human nutrition benefits can be quantified as the nutrient content in an edible portion relative to Recommended Daily Intake (RDI) values. The RDI is the daily intake level of a nutrient that is considered to be sufficient to meet the requirements of 97–98% of healthy individuals in every demographic. Although fisheries productivity is studied extensively, there has been surprisingly little consideration of the drivers of the nutritional quality of fisheries yields.*

Thus, the main questions I'll be adressing with this data are: 

1. What is the range of nutrient content across species? 
2. Does nutrient content vary with fish species' traits such as body size or trophic level? 

To do this I'll take a model selection approach, wherein I'll compare models which contain different species' traits as parameters to see which models fit the data best, and thus which species traits vary with nutrient content. 

I've collected nutrient and fish trait data from the peer-reviewed literature and databases such as [FishBase](https://en.wikipedia.org/wiki/FishBase). 

#### Loading required packages.

```r
library(ggplot2)
library(plotrix)
library(broom)
library(ggthemes)
suppressPackageStartupMessages(library(dplyr))
library(knitr)
suppressPackageStartupMessages(library(Hmisc))
suppressPackageStartupMessages(library(robustbase))
library(tidyr)

nut <- read.csv("~/Desktop/Nutrient_databases/nut_sept22_lwr_dec3.csv", comment.char="#", stringsAsFactors=TRUE, na.strings=c("",".","NA"))
ntbl <- tbl_df(nut)
```

#### Data cleaning and wrangling

Let's change variable names to more intuitive names.


```r
 ntbl <- ntbl %>%
  mutate(HG_mcg = as.numeric(HG_mcg)) %>% 
   rename(species = ASFIS.Scientific.name,
         taxon = ISSCAAP_cat,
         max_length = SLMAX) 
```

Pull out variables we will use in this analysis. 


```r
ntbl <- ntbl %>%
  select(species, taxon, max_length, TL, CA_mg, EPA_g, DHA_g, FE_mg, ZN_mg, HG_mcg, lwA, lwB, Habitat, Subgroup, Abs_lat)
```

Convert max length to max body size using length-weight conversion (W = a × L^b). For more information about this conversion approach, see this [explanation](http://www.fishbase.ca/manual/FishBaseThe_LENGTH_WEIGHT_Table.htm) on FishBase. 

```r
ntbl <- ntbl %>%
  mutate(max_size = (lwA * (max_length^lwB)/1000))
```

Let's clean up the df to trim out any NA values, because they were giving me some trouble. This just makes fitting the models easier. Here I'm removing any rows that have missing info for any of my variables of interest. Side note: I'd prefer to NOT have to do this kind of thing, and I can run some models without doing this, but not all. Any tips on dealing with lots of NAs would be great. 

```r
ntbl.CA <- ntbl %>%
  filter(!is.na(max_size)) %>% 
  filter(!is.na(CA_mg)) %>% 
  filter(!is.na(taxon))

ntbl.EPA <- ntbl %>%
  filter(!is.na(Abs_lat)) %>% 
  filter(!is.na(EPA_g)) %>% 
  filter(!is.na(taxon))

ntbl.HG <- ntbl %>%
  filter(!is.na(Abs_lat)) %>% 
  filter(!is.na(HG_mcg)) %>% 
  filter(!is.na(taxon))

str(ntbl.HG)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	100 obs. of  16 variables:
##  $ species   : Factor w/ 433 levels "Abramis brama",..: 1 1 9 15 23 31 31 45 53 53 ...
##  $ taxon     : Factor w/ 29 levels " Clams, cockles, arkshells",..: 8 8 6 18 1 8 8 5 8 8 ...
##  $ max_length: num  82 82 62 150 11 120 120 61 64 64 ...
##  $ TL        : num  2.9 2.9 3.19 3.2 2 4.5 4.5 4.4 3.1 3.1 ...
##  $ CA_mg     : num  53 52 14.4 6.8 62.9 70 70 8.8 59 57 ...
##  $ EPA_g     : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ DHA_g     : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ FE_mg     : num  0.6 0.7 0.94 0.15 5.3 0.9 1 0.29 0.9 0.8 ...
##  $ ZN_mg     : num  1 0.9 0.379 0.78 0.84 ...
##  $ HG_mcg    : num  60 24 54 25 8 68 64 14 68 68 ...
##  $ lwA       : num  0.00871 0.00871 0.0065 0.00347 0.02301 ...
##  $ lwB       : num  3.14 3.14 2.96 3.22 2.63 ...
##  $ Habitat   : Factor w/ 3 levels "brackish","freshwater",..: 2 2 2 3 3 2 2 3 2 2 ...
##  $ Subgroup  : Factor w/ 3 levels "Crustacean","Finfish",..: 2 2 2 2 3 2 2 2 2 2 ...
##  $ Abs_lat   : num  41.5 41.5 46 64 64 ...
##  $ max_size  : num  8.9001 8.9001 1.308 35.265 0.0127 ...
```


#### Exploring the range of variability in nutrient content among fish taxa 


First, I'll write out our models. The question I'm asking here is: Does calcium content of fish tissues vary with the body size of the fish? I.e. are smaller fish (such as sardines) better sources of calcium than large fish (such as tuna)? So, I'll fit a model of calcium content as a function of body size. 




Here is a function that will allow me to run a lm for any set of variables:

```r
lm_gen<- function(df, y, x, ...) {
#   lm_formula <-
#     substitute(y ~ x,
#                list(y = substitute(y), x = substitute(x)))
  lm_form_char <- paste0(y, " ~ ", x)
  lm_formula <- as.formula(lm_form_char)
  # browser()
  # eval(lm(lm_formula, data = df, ...))
  lm(lm_formula, data = df, ...)
}

lm_gen(ntbl.CA, "log(max_size)", "log(CA_mg)") #' here I test it with calcium as function of max body size. looks like it works!
```

```
## 
## Call:
## lm(formula = lm_formula, data = df)
## 
## Coefficients:
## (Intercept)   log(CA_mg)  
##      3.4799      -0.6187
```


Here's an even more general function for fitting lms (thanks to Jenny's [post](http://stat545-ubc.github.io/block025_lm-poly.html) for this code). I think this is about as general it's going to get! Nice, it looks like this works. 

```r
lm_general<- function(df, y, x, ...) {
  lm_formula <-
    substitute(y ~ x,
               list(y = substitute(y), x = substitute(x)))
  eval(lm(lm_formula, data = df, ...))
}

lm_general(ntbl.CA, log(max_size), log(CA_mg))
```

```
## 
## Call:
## lm(formula = lm_formula, data = df)
## 
## Coefficients:
## (Intercept)   log(CA_mg)  
##      3.4799      -0.6187
```

```r
size.fits3 <- ntbl.CA %>% group_by(taxon) %>% do(tidy(lm_general(., log(max_size), log(CA_mg))))
(size.fits3)
```

```
## Source: local data frame [28 x 6]
## Groups: taxon [15]
## 
##                                 taxon        term  estimate std.error
##                                (fctr)       (chr)     (dbl)     (dbl)
## 1           Clams, cockles, arkshells (Intercept) -4.368365       NaN
## 2        Miscellaneous coastal fishes (Intercept)  2.938447 1.4589489
## 3        Miscellaneous coastal fishes  log(CA_mg) -0.500871 0.3403379
## 4     Miscellaneous freshwater fishes (Intercept)  3.119484 0.8580236
## 5     Miscellaneous freshwater fishes  log(CA_mg) -0.337518 0.2047178
## 6        Miscellaneous pelagic fishes (Intercept)  7.024622 1.9301798
## 7        Miscellaneous pelagic fishes  log(CA_mg) -1.240157 0.3569467
## 8                               Shads (Intercept) -6.465633 2.5608105
## 9                               Shads  log(CA_mg)  1.890944 0.6541407
## 10 Carps, barbels and other cyprinids (Intercept)  4.351822 0.7231912
## ..                                ...         ...       ...       ...
## Variables not shown: statistic (dbl), p.value (dbl)
```


Now let's apply the CA as function of body size to each of the taxa separately.


```r
size.fitsCA <- ntbl.CA %>% group_by(taxon) %>% do(tidy(lm_gen(., "log(CA_mg)", "log(max_size)"), conf.int = TRUE))

#' And here I plot the slope estimates for each taxon.
ggplot(subset(size.fitsCA, term == "log(max_size)"), aes(estimate, taxon, color = taxon)) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, height = .3)) +
    geom_vline() + theme(legend.position="none") + xlab("body size slope estimate")
```

![](models_plots_files/figure-html/unnamed-chunk-7-1.png) 


#### repeating for the other nutrients

```r
# nuts <- ntbl %>% names %>% .[5:8]
# 
# indep_var <- lapply(nuts, function(nutname) paste0("log(", nutname, ")"))
# 
# names(indep_var) <- nuts
# 
# responses <- indep_var


# for (k in nuts[[1]]){
#   responses[[k]] <- ntbl %>% 
#     group_by(taxon) %>% 
#     do(tidy(lm_gen(.,
#                    y = indep_var[[k]],
#                    x = "log(max_size)"),
#             conf.int = TRUE))
# }


ntbl %>% 
  gather("nutrient", "value", CA_mg:HG_mcg) %>% 
  group_by(taxon, nutrient) %>% 
  mutate(logmass = log(max_size),
         lognutr = log(value)) %>% 
  filter(!is.na(value)) %>% 
  filter(!is.na(logmass)) %>% 
  do(tidy(lm(lognutr ~ logmass, data = .),
          conf.int = TRUE)
     ) %>% 
  filter(term == "logmass") %>% 
  ggplot(aes(estimate, taxon, color = taxon)) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, height = .3)) +
    geom_vline() + theme(legend.position="none") + xlab("body size slope estimate") +
  facet_wrap(~nutrient) + scale_x_continuous(limits=c(-2, 2))
```

![](models_plots_files/figure-html/unnamed-chunk-8-1.png) 

```r
ggsave("slope_estimates.png")
```

```
## Saving 7 x 5 in image
```

```r
#' now for trophic level subset(dftm, C!="Foo")
ntbl %>% 
  filter(taxon!= "Miscellaneous diadromous fishes") %>% 
  gather("nutrient", "value", CA_mg:HG_mcg) %>% 
  group_by(taxon, nutrient) %>% 
  mutate(lognutr = log(value)) %>% 
  filter(!is.na(value)) %>% 
  filter(!is.na(TL)) %>% 
  do(tidy(lm(lognutr ~ TL, data = .),
          conf.int = TRUE)
     ) %>% 
  filter(term == "TL") %>% 
  ggplot(aes(estimate, taxon, color = taxon)) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, height = .3)) +
    geom_vline() + theme(legend.position="none") + xlab("TL slope estimate") +
  facet_wrap(~nutrient) + scale_x_continuous(limits=c(-4, 4))
```

![](models_plots_files/figure-html/unnamed-chunk-8-2.png) 

```r
ggsave("slope_estimates.png")
```

```
## Saving 7 x 5 in image
```



Mercury

```r
ntbl.HG <- ntbl %>%
  filter(!is.na(HG_mcg)) %>% 
  filter(!is.na(taxon)) %>% 
  filter(!is.na(TL))

size.HG <- tidy(lm(log(HG_mcg) ~ TL, data = ntbl.HG), conf.int = TRUE)

summary(size.HG)
```

```
##      term              estimate        std.error        statistic    
##  Length:2           Min.   :0.2455   Min.   :0.1056   Min.   :2.326  
##  Class :character   1st Qu.:0.8587   1st Qu.:0.1776   1st Qu.:3.457  
##  Mode  :character   Median :1.4718   Median :0.2497   Median :4.588  
##                     Mean   :1.4718   Mean   :0.2497   Mean   :4.588  
##                     3rd Qu.:2.0849   3rd Qu.:0.3218   3rd Qu.:5.720  
##                     Max.   :2.6980   Max.   :0.3938   Max.   :6.851  
##     p.value             conf.low         conf.high     
##  Min.   :1.000e-09   Min.   :0.03589   Min.   :0.4552  
##  1st Qu.:5.553e-03   1st Qu.:0.50588   1st Qu.:1.2114  
##  Median :1.111e-02   Median :0.97586   Median :1.9677  
##  Mean   :1.111e-02   Mean   :0.97586   Mean   :1.9677  
##  3rd Qu.:1.666e-02   3rd Qu.:1.44585   3rd Qu.:2.7239  
##  Max.   :2.221e-02   Max.   :1.91583   Max.   :3.4802
```

```r
ggplot(ntbl.HG, aes(TL, log(HG_mcg))) + stat_summary(fun.y= "mean", geom = "point") + stat_smooth(method = "lm")
```

![](models_plots_files/figure-html/unnamed-chunk-9-1.png) 

```r
TL.fitsHG <- ntbl.HG %>% group_by(taxon) %>% do(tidy(lm_general(., log(HG_mcg), TL), conf.int = TRUE))



#' And here I plot the slope estimates for each taxon.
ggplot(subset(TL.fitsHG, term == "TL"), aes(estimate, taxon, color = taxon)) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, height = .3)) +
    geom_vline() + theme(legend.position="none") + xlab("TL slope estimate")
```

![](models_plots_files/figure-html/unnamed-chunk-9-2.png) 



Let's plot those two fits. Neither one looks all that great at this point. 

```r
ntbl.CA %>% ggplot(aes(x = log(max_size), y = log(CA_mg), color=taxon)) + stat_summary(fun.y= "mean", geom = "point") + geom_smooth(method = 'lm')
```

![](models_plots_files/figure-html/unnamed-chunk-10-1.png) 

Residuals, grouped by taxon.

```r
ntbl.CA %>%
  group_by(taxon) %>% 
  do(augment(lm(log(CA_mg) ~ log(max_size), data = .),
          conf.int = TRUE)) %>% 
ggplot(., aes(x= taxon, y=.resid, color = taxon)) + geom_point(size = 3) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position="none")
```

![](models_plots_files/figure-html/unnamed-chunk-11-1.png) 

And now, plot the residuals, by size. Well at least this doesn't look too funnel-shaped or anything too weird. That's somewhat reassuring!

```r
ntbl.CA %>%
  group_by(taxon) %>% 
  do(augment(lm(log(CA_mg) ~ log(max_size), data = .),
          conf.int = TRUE)) %>% 
ggplot(., aes(x= log.max_size., y=.resid, color = taxon)) + geom_point(size = 3) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position="none")
```

![](models_plots_files/figure-html/unnamed-chunk-12-1.png) 

Here's a function for finding the max residuals. This function allows us to figure out which taxon has the highest residual values...an indication of worst fit. Again, here we see that the group 'Miscellaneous freshwater fishes' has the highest residuals, i.e. worst fit to the linear model, which does make sense since it's the most 'grab bag' of the groups, being 'miscellaneous' and all. 

```r
mean_resid <- function(df) {
    size.fit <- lm(log(CA_mg) ~ log(max_size), df)
    x <- mean(abs(resid(size.fit)))
    y <- setNames(data.frame(t(x)), c("mean_residual"))
    y
}

mean_resid(ntbl.CA %>% filter(Habitat == "marine"))
```

```
##   mean_residual
## 1       1.12163
```

```r
resid_1 <- ntbl.CA %>%
group_by(taxon) %>% 
do(mean_resid(.)) %>% 
  unnest(mean_residual) %>% 
  arrange(desc(mean_residual))
(resid_1)
```

```
## Source: local data frame [15 x 2]
## 
##                                 taxon mean_residual
##                                (fctr)         (dbl)
## 1     Miscellaneous freshwater fishes     1.4097088
## 2        Miscellaneous coastal fishes     1.2239459
## 3  Carps, barbels and other cyprinids     0.8418547
## 4          Flounders, halibuts, soles     0.8390861
## 5        Miscellaneous pelagic fishes     0.7570829
## 6          Tunas, bonitos, billfishes     0.5991824
## 7             Salmons, trouts, smelts     0.4584627
## 8       Miscellaneous demersal fishes     0.1958452
## 9                               Shads     0.1942327
## 10        Tilapias and other cichlids     0.1755142
## 11              Cods, hakes, haddocks     0.1451338
## 12      Lobsters, spiny-rock lobsters     0.1437684
## 13          Clams, cockles, arkshells     0.0000000
## 14      Herrings, sardines, anchovies     0.0000000
## 15            Sharks, rays, chimaeras     0.0000000
```

Now let's run the same models, but group by habitat. Tidy as before. Weird! Both the marine and freshwater groups show significant negative slopes and the brackish group shows a positive relationship between calcium content and body size. I can think of no biological explanation for this!

```r
  tidy.fit.hab <- ntbl.CA %>%
  group_by(Habitat) %>% 
  do(tidy(lm(log(CA_mg) ~ log(max_size), data = .),
          conf.int = TRUE)) 

  
  ggplot(subset(tidy.fit.hab, term == "log(max_size)"), aes(estimate, Habitat, color = Habitat)) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, height = .3)) +
    geom_vline() + theme(legend.position="none")
```

![](models_plots_files/figure-html/unnamed-chunk-14-1.png) 


##### Putting nutrient variability in the context of human nutrition

Now, let's explore some of the variability in nutrient content that we saw above, but this time in the context of how this matters to humans: recommended daily intake (RDI) values. Recommended daily intake values may be familiar to you in the context of those numbers printed on food products' nutrition labels (i.e. one serving of milk gives you 30% of your RDI of calcium). These target intake values are typically set by a country's government nutritionists. 

Let's find the % of each taxon that, in one portion, has EPA (an omega-3 fatty acid) values above RDI. 

So, for one taxon, what would this look like?

```r
#' Let's try for one taxon
EPA.RDI <- ntbl.EPA %>%
  filter(taxon == "Salmons, trouts, smelts") %>% 
  mutate(RDI = ifelse(EPA_g > 0.25, 1, 0)) %>% 
  group_by(species) %>% 
 mutate(per.RDI = sum(RDI)/n_distinct(EPA_g)) %>% 
  mutate(mean.per.RDI= mean(per.RDI))    
```
Here's a function for calculating the percentage of species in a taxon that reach 25% of RDI in a single portion (i.e. 100g of fish tissue). 

```r
epa.prop <- function(df) {
  (EPA.RDI <- df %>%
  mutate(RDI = ifelse(EPA_g > 0.25, 1, 0)) %>% 
  group_by(species) %>% 
 mutate(per.RDI = sum(RDI)/n_distinct(EPA_g)) %>% 
  mutate(mean.per.RDI= mean(per.RDI))) 
}
```

Here it is applied to my dataset, grouped by taxon, and unnested, summarised etc. The final output that I want is the percentage of fish species in each taxon that reaches a threshold of 25% of RDI in one portion.

####EPA

```r
epa.prp <- ntbl.EPA %>%
  do(EPA.prp=epa.prop(.)) %>% 
    unnest(EPA.prp) %>%
  group_by(taxon) %>% 
  summarise(meanRDI = mean(mean.per.RDI)) %>% 
  mutate(mean.percent.RDI = meanRDI * 100) %>% 
  arrange(mean.percent.RDI)
```

And this graph shows these percentages ordered by increasing percentage. Yahoo! Success!

```r
ggplot(epa.prp, aes(x = reorder(taxon, mean.percent.RDI), y = mean.percent.RDI, color = taxon)) + geom_point(size = 3) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position="none")
```

![](models_plots_files/figure-html/unnamed-chunk-18-1.png) 

```r
head(epa.prp)
```

```
## Source: local data frame [6 x 3]
## 
##                              taxon meanRDI mean.percent.RDI
##                             (fctr)   (dbl)            (dbl)
## 1        Clams, cockles, arkshells       0                0
## 2  Miscellaneous diadromous fishes       0                0
## 3           Freshwater crustaceans       0                0
## 4       King crabs, squat-lobsters       0                0
## 5    Krill, planktonic crustaceans       0                0
## 6                          Oysters       0                0
```
    
####DHA

```r
ntbl.DHA <- ntbl %>%
  filter(!is.na(Abs_lat)) %>% 
  filter(!is.na(DHA_g)) %>% 
  filter(!is.na(taxon))

ggplot(ntbl, aes(x=taxon, y=DHA_g)) + geom_point(size = 3) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +geom_hline(aes(yintercept=0.25))
```

```
## Warning: Removed 672 rows containing missing values (geom_point).
```

![](models_plots_files/figure-html/DHA-1.png) 

```r
dha.prop <- function(df) {
  (DHA.RDI <- df %>%
  mutate(RDI = ifelse(DHA_g > 0.25, 1, 0)) %>% 
  group_by(species) %>% 
 mutate(per.RDI = sum(RDI)/n_distinct(DHA_g)) %>% 
  mutate(mean.per.RDI= mean(per.RDI))) 
}

dha.prp <- ntbl.DHA%>%
  do(DHA.prp=dha.prop(.)) %>% 
    unnest(DHA.prp) %>%
  group_by(taxon) %>% 
  summarise(meanRDI = mean(mean.per.RDI)) %>% 
  mutate(mean.percent.RDI = meanRDI * 100) %>% 
  arrange(mean.percent.RDI)

ggplot(dha.prp, aes(x = reorder(taxon, mean.percent.RDI), y = mean.percent.RDI, color = taxon)) + geom_point(size = 3) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position="none")
```

![](models_plots_files/figure-html/DHA-2.png) 

#### Calcium

```r
CA.prop <- function(df) {
  (CA.RDI <- df %>%
  mutate(RDI = ifelse(CA_mg > 250, 1, 0)) %>% 
  group_by(species) %>% 
 mutate(per.RDI = sum(RDI)/n_distinct(CA_mg)) %>% 
  mutate(mean.per.RDI= mean(per.RDI))) 
}

CA.prp <- ntbl.CA%>%
  do(CA.prp=CA.prop(.)) %>% 
    unnest(CA.prp) %>%
  group_by(taxon) %>% 
  summarise(meanRDI = mean(mean.per.RDI)) %>% 
  mutate(mean.percent.RDI = meanRDI * 100) %>% 
  arrange(mean.percent.RDI)

ggplot(CA.prp, aes(x = reorder(taxon, mean.percent.RDI), y = mean.percent.RDI, color = taxon)) + geom_point(size = 3) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position="none") + ylab("percent of sp that reach RDI for calcium")
```

![](models_plots_files/figure-html/unnamed-chunk-19-1.png) 

#### Zinc

```r
ntbl.ZN <- ntbl %>%
  filter(!is.na(max_size)) %>% 
  filter(!is.na(ZN_mg)) %>% 
  filter(!is.na(taxon))


ZN.prop <- function(df) {
  (ZN.RDI <- df %>%
  mutate(RDI = ifelse(ZN_mg > (11/4), 1, 0)) %>% 
  group_by(species) %>% 
 mutate(per.RDI = sum(RDI)/n_distinct(ZN_mg)) %>% 
  mutate(mean.per.RDI= mean(per.RDI))) 
}

ZN.prp <- ntbl.ZN%>%
  do(ZN.prp=ZN.prop(.)) %>% 
    unnest(ZN.prp) %>%
  group_by(taxon) %>% 
  summarise(meanRDI = mean(mean.per.RDI)) %>% 
  mutate(mean.percent.RDI = meanRDI * 100) %>% 
  arrange(mean.percent.RDI)

ggplot(ZN.prp, aes(x = reorder(taxon, mean.percent.RDI), y = mean.percent.RDI, color = taxon)) + geom_point(size = 3) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position="none") + ylab("percent of sp that reach RDI for zinc")
```

![](models_plots_files/figure-html/unnamed-chunk-20-1.png) 

#### Iron

```r
ntbl.FE <- ntbl %>%
  filter(!is.na(max_size)) %>% 
  filter(!is.na(FE_mg)) %>% 
  filter(!is.na(taxon))


FE.prop <- function(df) {
  (FE.RDI <- df %>%
  mutate(RDI = ifelse(FE_mg > (18/4), 1, 0)) %>% 
  group_by(species) %>% 
 mutate(per.RDI = sum(RDI)/n_distinct(FE_mg)) %>% 
  mutate(mean.per.RDI= mean(per.RDI))) 
}

FE.prp <- ntbl.FE%>%
  do(FE.prp=FE.prop(.)) %>% 
    unnest(FE.prp) %>%
  group_by(taxon) %>% 
  summarise(meanRDI = mean(mean.per.RDI)) %>% 
  mutate(mean.percent.RDI = meanRDI * 100) %>% 
  arrange(mean.percent.RDI)

ggplot(FE.prp, aes(x = reorder(taxon, mean.percent.RDI), y = mean.percent.RDI, color = taxon)) + geom_point(size = 3) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position="none") + ylab("percent of sp that reach RDI for iron")
```

![](models_plots_files/figure-html/unnamed-chunk-21-1.png) 

Now, let's look at some latitudinal patterns in EPA. Because EPA is required by aquatic species to maintain membrane fluidity at cold water temperatures, I hypothesize that EPA content would be higher in cold water fish species. Here I'm using latitude from the which the fish was caught as a proxy for cold water adaptations. OK, let's look at latitude patterns.


This figure shows how EPA content varies as a function of latitude. 

```r
p <- ggplot(subset(ntbl.EPA, Habitat == "marine"), aes(x=Abs_lat, y=log(EPA_g)))
p + stat_summary(aes(y = log(EPA_g)), fun.y=mean, geom = "point") + geom_hline(aes(yintercept=log(0.5))) + stat_smooth(method = "lm") + theme_pander() + xlab("Absolute latitude") + ylab("log EPA content, g/100g portion") + theme(legend.position="none")
```

![](models_plots_files/figure-html/unnamed-chunk-22-1.png) 


Here I apply my EPA RDI function to the whole dataset and arrange the results by decreasing latitude. 

```r
epa.prp2 <- ntbl.EPA %>%
  do(EPA.prp=epa.prop(.)) %>% 
    unnest(EPA.prp) %>%
arrange(desc(Abs_lat))
```

This figure shows the percentage of each taxon that reaches RDI, as arranged by increasing latitude. This figure probably isn't the best way to visualize this, but one thing that you can see is that many of the taxa that score on the RDI scale are also lower latitude, consistent the cold water adaptation hypothesis. 


```r
ggplot(epa.prp2, aes(x = reorder(taxon, Abs_lat), y = mean.per.RDI, color = taxon)) + stat_summary(fun.y= "mean", geom = "point") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position="none")
```

![](models_plots_files/figure-html/unnamed-chunk-24-1.png) 
   
Here I tried to apply this function to all taxa individually, but couldn't figure out. See below for where I was getting errors.     

```r
#' this doesn't work, because I can't figure out how to pull out the right column to unnest.
#' epa.prop2 <- function(df) {
#'  (EPA.RDI <- df %>%
#'  mutate(RDI = ifelse(EPA_g > 0.25, 1, 0)) %>% 
#'  group_by(species) %>% 
#' mutate(per.RDI = sum(RDI)/n_distinct(EPA_g)) %>% 
#'  mutate(mean.per.RDI= mean(per.RDI))) %>%  
#'  unnest(.[,2]) %>% ##this is where I run into problems. 
#'  group_by(taxon) %>% 
#'  summarise(meanRDI = mean(mean.per.RDI)) %>% 
#'  mutate(mean.percent.RDI = meanRDI * 100)
#'}
#'epa.prop2(ntbl.EPA)
#'head(epa.prp)
```

OK, now we've seen that some fish species' traits (i.e. body size, latitude) are related to nutritional profile. But what if I want to uncover which traits vary with nutritional profile in a more systematic way? I could create a set of models which contain different subsets of the variables I think might influence nutritional profile, and then compare how to models fit the data. Here I write out two such candiate models. 

```r
library(MuMIn)

ntbl.EPA <- ntbl %>%
  filter(!is.na(Abs_lat)) %>% 
  filter(!is.na(EPA_g)) %>% 
  filter(!is.na(taxon)) %>% 
  filter(!is.na(TL)) %>% 
  filter(!is.na(Habitat)) %>% 
  filter(!is.na(max_size))

EPA.1 <- lm(log(EPA_g) ~ log(max_size)*TL + log(max_size)*Abs_lat + log(max_size)*Habitat, data=ntbl.EPA, na.action = "na.fail")

dredge(EPA.1)
```

```
## Fixed term is "(Intercept)"
```

```
## Global model call: lm(formula = log(EPA_g) ~ log(max_size) * TL + log(max_size) * 
##     Abs_lat + log(max_size) * Habitat, data = ntbl.EPA, na.action = "na.fail")
## ---
## Model selection table 
##       (Int) Abs_lat Hbt log(max_siz)       TL Abs_lat:log(max_siz)
## 4   -1.2200 0.01656   +                                           
## 12  -1.0320 0.01693   +              -0.06720                     
## 8   -1.2480 0.01666   +   -0.0070190                              
## 80  -1.2520 0.01706   +    0.2325000  0.03530                     
## 16  -1.0240 0.01693   +    0.0009166 -0.06879                     
## 40  -1.2610 0.01655   +   -0.0119100                              
## 24  -1.2510 0.01674   +   -0.0043450                    -0.0000704
## 112 -1.2730 0.01688   +    0.2452000  0.03743                     
## 96  -1.2750 0.01768   +    0.2622000  0.03848           -0.0005241
## 48  -1.0240 0.01681   +   -0.0042240 -0.07329                     
## 32  -1.0270 0.01705   +    0.0047950 -0.06901           -0.0001014
## 56  -1.2530 0.01606   +   -0.0284300                     0.0003885
## 128 -1.2680 0.01671   +    0.2383000  0.03674            0.0001372
## 64  -1.0130 0.01629   +   -0.0219400 -0.07387            0.0004181
## 3   -0.6291           +                                           
## 11  -0.5859           +              -0.01440                     
## 7   -0.6215           +    0.0022060                              
## 39  -0.6513           +   -0.0064280                              
## 15  -0.5432           +    0.0049070 -0.02299                     
## 79  -0.7529           +    0.2218000  0.07483                     
## 111 -0.7879           +    0.2390000  0.07712                     
## 47  -0.5485           +   -0.0031890 -0.03054                     
## 6   -3.2950 0.02161       -0.0393000                              
## 2   -3.3150 0.02129                                               
## 78  -3.8690 0.02133        0.2223000  0.18290                     
## 14  -3.5080 0.02124       -0.0461000  0.06474                     
## 22  -3.3020 0.02180       -0.0329400                    -0.0001671
## 10  -3.2790 0.02136                  -0.01078                     
## 94  -3.9050 0.02206        0.2575000  0.18640           -0.0006223
## 30  -3.5130 0.02140       -0.0409200  0.06443           -0.0001354
## 1   -2.5430                                                       
## 77  -3.3760                0.2091000  0.26110                     
## 5   -2.5170               -0.0328200                              
## 13  -3.0320               -0.0485500  0.14730                     
## 9   -2.7880                           0.06826                     
##     Hbt:log(max_siz) log(max_siz):TL df   logLik   AICc delta weight
## 4                                     5 -703.054 1416.2  0.00  0.357
## 12                                    6 -702.811 1417.8  1.57  0.162
## 8                                     6 -703.014 1418.2  1.98  0.133
## 80                          -0.06858  8 -701.721 1419.8  3.54  0.061
## 16                                    7 -702.811 1419.9  3.64  0.058
## 40                 +                  7 -702.936 1420.1  3.89  0.051
## 24                                    7 -703.013 1420.3  4.04  0.047
## 112                +        -0.07485  9 -701.450 1421.3  5.08  0.028
## 96                          -0.07144  9 -701.664 1421.8  5.51  0.023
## 48                 +                  8 -702.707 1421.8  5.51  0.023
## 32                                    8 -702.809 1422.0  5.71  0.021
## 56                 +                  8 -702.914 1422.2  5.92  0.018
## 128                +        -0.07451 10 -701.447 1423.4  7.17  0.010
## 64                 +                  9 -702.682 1423.8  7.55  0.008
## 3                                     4 -711.320 1430.7 14.48  0.000
## 11                                    5 -711.309 1432.8 16.51  0.000
## 7                                     5 -711.316 1432.8 16.52  0.000
## 39                 +                  6 -711.087 1434.4 18.13  0.000
## 15                                    6 -711.294 1434.8 18.54  0.000
## 79                          -0.06422  7 -710.375 1435.0 18.77  0.000
## 111                +        -0.07266  8 -709.910 1436.2 19.91  0.000
## 47                 +                  7 -711.049 1436.4 20.12  0.000
## 6                                     4 -719.948 1448.0 31.74  0.000
## 2                                     3 -721.155 1448.4 32.12  0.000
## 78                          -0.07931  6 -718.419 1449.0 32.79  0.000
## 14                                    5 -719.774 1449.7 33.44  0.000
## 22                                    5 -719.943 1450.0 33.78  0.000
## 10                                    4 -721.149 1450.4 34.14  0.000
## 94                          -0.08268  7 -718.344 1451.0 34.71  0.000
## 30                                    6 -719.770 1451.7 35.49  0.000
## 1                                     2 -734.079 1472.2 55.94  0.000
## 77                          -0.07614  5 -731.240 1472.6 56.37  0.000
## 5                                     3 -733.286 1472.6 56.38  0.000
## 13                                    4 -732.417 1472.9 56.68  0.000
## 9                                     3 -733.854 1473.8 57.52  0.000
## Models ranked by AICc(x)
```

```r
EPA.r1 <- lm(log(EPA_g) ~ Abs_lat + Habitat, data=ntbl.EPA, na.action = "na.fail")
EPA.r2 <- lm(log(EPA_g) ~ Abs_lat + Habitat + TL, data=ntbl.EPA, na.action = "na.fail")
EPA.r3 <- lm(log(EPA_g) ~ Abs_lat + Habitat + log(max_size), data=ntbl.EPA, na.action = "na.fail")


AIC.table <- function(mod1, mod2, mod3) {
  model.average <-  model.avg(EPA.r1, EPA.r2, EPA.r3)
  return((msTable <- model.average$msTable))
  }

(AIC.table(EPA.r1, EPA.r2, EPA.r3))
```

```
##     df    logLik     AICc    delta    weight
## 12   5 -703.0535 1416.250 0.000000 0.5472239
## 124  6 -702.8113 1417.823 1.572918 0.2492356
## 123  6 -703.0138 1418.228 1.977986 0.2035405
```

```r
model.average <-  model.avg(EPA.r1, EPA.r2, EPA.r3)
summary(model.average)
```

```
## 
## Call:
## model.avg.default(object = EPA.r1, EPA.r2, EPA.r3)
## 
## Component model call: 
## lm(formula = <3 unique values>, data = ntbl.EPA, na.action = 
##      na.fail)
## 
## Component models: 
##     df  logLik    AICc delta weight
## 12   5 -703.05 1416.25  0.00   0.55
## 124  6 -702.81 1417.82  1.57   0.25
## 123  6 -703.01 1418.23  1.98   0.20
## 
## Term codes: 
##       Abs_lat       Habitat log(max_size)            TL 
##             1             2             3             4 
## 
## Model-averaged coefficients:  
## (full average) 
##                    Estimate Std. Error Adjusted SE z value Pr(>|z|)    
## (Intercept)       -1.179190   1.281083    1.284746   0.918   0.3587    
## Abs_lat            0.016674   0.004069    0.004081   4.086 4.39e-05 ***
## Habitatfreshwater -2.451949   1.268244    1.271886   1.928   0.0539 .  
## Habitatmarine     -1.658787   1.265409    1.269043   1.307   0.1912    
## TL                -0.016748   0.056512    0.056632   0.296   0.7674    
## log(max_size)     -0.001429   0.011645    0.011677   0.122   0.9026    
##  
## (conditional average) 
##                    Estimate Std. Error Adjusted SE z value Pr(>|z|)    
## (Intercept)       -1.179190   1.281083    1.284746   0.918   0.3587    
## Abs_lat            0.016674   0.004069    0.004081   4.086 4.39e-05 ***
## Habitatfreshwater -2.451949   1.268244    1.271886   1.928   0.0539 .  
## Habitatmarine     -1.658787   1.265409    1.269043   1.307   0.1912    
## TL                -0.067197   0.097076    0.097355   0.690   0.4900    
## log(max_size)     -0.007019   0.025041    0.025113   0.280   0.7799    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Relative variable importance: 
##                      Abs_lat Habitat TL   log(max_size)
## Importance:          1.00    1.00    0.25 0.20         
## N containing models:    3       3       1    1
```

####DHA

```r
ntbl.DHA <- ntbl %>%
  filter(!is.na(Abs_lat)) %>% 
  filter(!is.na(DHA_g)) %>% 
  filter(!is.na(taxon)) %>% 
  filter(!is.na(TL)) %>% 
  filter(!is.na(Habitat)) %>% 
  filter(!is.na(max_size))

DHA.1 <- lm(log(DHA_g) ~ log(max_size)*TL + log(max_size)*Abs_lat + log(max_size)*Habitat, data=ntbl.DHA, na.action = "na.fail")

summary(DHA.1)
```

```
## 
## Call:
## lm(formula = log(DHA_g) ~ log(max_size) * TL + log(max_size) * 
##     Abs_lat + log(max_size) * Habitat, data = ntbl.DHA, na.action = "na.fail")
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -3.09536 -0.74552 -0.05655  0.69559  3.13863 
## 
## Coefficients:
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                 -3.2256460  0.4097905  -7.871 3.10e-14 ***
## log(max_size)                0.3284891  0.1677749   1.958   0.0509 .  
## TL                           0.2627060  0.1174500   2.237   0.0258 *  
## Abs_lat                      0.0084467  0.0041943   2.014   0.0447 *  
## Habitatmarine                0.6481359  0.1543682   4.199 3.29e-05 ***
## log(max_size):TL            -0.0920541  0.0426203  -2.160   0.0314 *  
## log(max_size):Abs_lat       -0.0009445  0.0016463  -0.574   0.5665    
## log(max_size):Habitatmarine  0.0599986  0.0666591   0.900   0.3686    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.111 on 414 degrees of freedom
## Multiple R-squared:  0.1273,	Adjusted R-squared:  0.1125 
## F-statistic: 8.626 on 7 and 414 DF,  p-value: 6.853e-10
```

```r
dredge(DHA.1)
```

```
## Fixed term is "(Intercept)"
```

```
## Global model call: lm(formula = log(DHA_g) ~ log(max_size) * TL + log(max_size) * 
##     Abs_lat + log(max_size) * Habitat, data = ntbl.DHA, na.action = "na.fail")
## ---
## Model selection table 
##      (Int)  Abs_lat Hbt log(max_siz)     TL Abs_lat:log(max_siz)
## 80  -3.245 0.007120   +    0.3522000 0.2610                     
## 112 -3.191 0.007250   +    0.3005000 0.2572                     
## 79  -3.065            +    0.3436000 0.2735                     
## 96  -3.255 0.007297   +    0.3608000 0.2621           -0.0001481
## 8   -2.432 0.007188   +    0.0434900                            
## 12  -2.941 0.006969   +              0.1683                     
## 16  -2.794 0.006805   +    0.0304500 0.1139                     
## 128 -3.226 0.008447   +    0.3285000 0.2627           -0.0009445
## 40  -2.370 0.007430   +   -0.0025600                            
## 111 -3.016            +    0.2992000 0.2705                     
## 48  -2.752 0.007041   +   -0.0205800 0.1221                     
## 11  -2.787            +              0.1884                     
## 4   -2.386 0.007811   +                                         
## 7   -2.207            +    0.0473300                            
## 24  -2.414 0.006677   +    0.0268900                   0.0004359
## 15  -2.635            +    0.0321500 0.1305                     
## 32  -2.776 0.006261   +    0.0127600 0.1145            0.0004626
## 56  -2.379 0.008077   +    0.0077270                  -0.0005106
## 39  -2.146            +    0.0071660                            
## 47  -2.593            +   -0.0138300 0.1384                     
## 64  -2.765 0.007759   +   -0.0092410 0.1231           -0.0005699
## 3   -2.135            +                                         
## 78  -3.363 0.010920        0.3487000 0.4065                     
## 94  -3.381 0.011230        0.3640000 0.4085           -0.0002625
## 10  -2.805 0.010660                  0.2266                     
## 14  -2.861 0.010640       -0.0110200 0.2450                     
## 30  -2.845 0.010150       -0.0271500 0.2455            0.0004215
## 2   -2.041 0.012000                                             
## 77  -3.089                 0.3347000 0.4401                     
## 6   -2.048 0.011890        0.0144100                            
## 9   -2.550                           0.2636                     
## 22  -2.033 0.011470        0.0007987                   0.0003571
## 13  -2.611                -0.0119800 0.2835                     
## 1   -1.604                                                      
## 5   -1.618                 0.0178700                            
##     Hbt:log(max_siz) log(max_siz):TL df   logLik   AICc delta weight
## 80                          -0.09526  7 -639.665 1293.6  0.00  0.222
## 112                +        -0.08948  8 -639.415 1295.2  1.58  0.101
## 79                          -0.09218  6 -641.609 1295.4  1.82  0.090
## 96                          -0.09614  8 -639.659 1295.7  2.07  0.079
## 8                                     5 -643.000 1296.1  2.54  0.062
## 12                                    5 -643.078 1296.3  2.70  0.058
## 16                                    6 -642.319 1296.8  3.24  0.044
## 128                +        -0.09205  9 -639.247 1296.9  3.33  0.042
## 40                 +                  6 -642.452 1297.1  3.51  0.039
## 111                +        -0.08718  7 -641.427 1297.1  3.52  0.038
## 48                 +                  7 -641.672 1297.6  4.01  0.030
## 11                                    4 -644.916 1297.9  4.33  0.026
## 4                                     4 -644.934 1298.0  4.36  0.025
## 7                                     4 -644.969 1298.0  4.43  0.024
## 24                                    6 -642.949 1298.1  4.50  0.023
## 15                                    5 -644.076 1298.3  4.70  0.021
## 32                                    7 -642.261 1298.8  5.19  0.017
## 56                 +                  7 -642.404 1299.1  5.48  0.014
## 39                 +                  5 -644.552 1299.2  5.65  0.013
## 47                 +                  6 -643.552 1299.3  5.71  0.013
## 64                 +                  8 -641.611 1299.6  5.97  0.011
## 3                                     3 -647.253 1300.6  6.96  0.007
## 78                          -0.10630  6 -656.494 1325.2 31.59  0.000
## 94                          -0.10780  7 -656.477 1327.2 33.62  0.000
## 10                                    4 -659.646 1327.4 33.79  0.000
## 14                                    5 -659.547 1329.2 35.64  0.000
## 30                                    6 -659.502 1331.2 37.61  0.000
## 2                                     3 -662.790 1331.6 38.04  0.000
## 77                          -0.10240  5 -660.829 1331.8 38.20  0.000
## 6                                     4 -662.587 1333.3 39.67  0.000
## 9                                     3 -663.727 1333.5 39.91  0.000
## 22                                    5 -662.555 1335.3 41.65  0.000
## 13                                    4 -663.611 1335.3 41.72  0.000
## 1                                     2 -667.974 1340.0 46.38  0.000
## 5                                     3 -667.669 1341.4 47.79  0.000
## Models ranked by AICc(x)
```

#####Zinc


```r
library(visreg)
ntbl %>%
  filter(!is.na(Abs_lat)) %>% 
  filter(!is.na(ZN_mg)) %>% 
  filter(!is.na(taxon)) %>% 
  filter(!is.na(TL)) %>% 
  filter(!is.na(Habitat)) %>% 
  filter(!is.na(max_size)) %>% 
  ggplot(., aes(x=log(max_size), y=log(ZN_mg))) + stat_summary(fun.y= "mean", geom = "point") + geom_smooth(method = 'lm')
```

![](models_plots_files/figure-html/unnamed-chunk-28-1.png) 

```r
ntbl.ZN <- ntbl %>%
  filter(!is.na(Abs_lat)) %>% 
  filter(!is.na(ZN_mg)) %>% 
  filter(!is.na(taxon)) %>% 
  filter(!is.na(TL)) %>% 
  filter(!is.na(Habitat)) %>% 
  filter(!is.na(max_size))

ZN.1 <- lm(log(ZN_mg) ~ log(max_size)*TL + log(max_size)*Abs_lat + log(max_size)*Habitat, data=ntbl.ZN, na.action = "na.fail")

dredge(ZN.1) #' best model set includes latitude, size, and TL 
```

```
## Fixed term is "(Intercept)"
```

```
## Global model call: lm(formula = log(ZN_mg) ~ log(max_size) * TL + log(max_size) * 
##     Abs_lat + log(max_size) * Habitat, data = ntbl.ZN, na.action = "na.fail")
## ---
## Model selection table 
##        (Int)  Abs_lat Hbt log(max_siz)       TL Abs_lat:log(max_siz)
## 6    0.84570 -0.02737         -0.05814                              
## 14   0.97720 -0.02709         -0.05377 -0.04188                     
## 22   0.84730 -0.02735         -0.05149                    -1.777e-04
## 8    0.06446 -0.02708   +     -0.06235                              
## 2    0.87300 -0.02973                                               
## 40   0.03356 -0.02864   +     -0.08746                              
## 78   1.07700 -0.02705         -0.13000 -0.07639                     
## 10   1.23600 -0.02845                  -0.11750                     
## 30   0.97530 -0.02709         -0.05090 -0.04106           -7.896e-05
## 16   0.18720 -0.02692   +     -0.05827 -0.03814                     
## 24   0.06422 -0.02703   +     -0.05317                    -2.463e-04
## 56   0.02592 -0.02925   +     -0.13000                     9.548e-04
## 48   0.14440 -0.02848   +     -0.08356 -0.03436                     
## 94   1.08400 -0.02705         -0.13770 -0.07906            1.345e-04
## 80   0.26620 -0.02683   +     -0.14550 -0.07644                     
## 32   0.18100 -0.02690   +     -0.05316 -0.03628           -1.423e-04
## 4    0.37250 -0.02967   +                                           
## 64   0.17880 -0.02914   +     -0.13230 -0.04782            1.127e-03
## 12   0.70630 -0.02859   +              -0.12420                     
## 112  0.19060 -0.02831   +     -0.12870 -0.05539                     
## 96   0.27180 -0.02684   +     -0.15070 -0.07845            9.196e-05
## 128  0.23680 -0.02897   +     -0.19000 -0.07406            1.204e-03
## 5   -0.22810                  -0.09791                              
## 13   0.25490                  -0.08173 -0.14200                     
## 7   -1.03800            +     -0.10150                              
## 15  -0.68830            +     -0.08982 -0.10310                     
## 77   0.37660                  -0.17340 -0.18330                     
## 39  -0.98250            +     -0.08549                              
## 79  -0.58160            +     -0.20340 -0.15270                     
## 47  -0.63300            +     -0.07383 -0.10310                     
## 111 -0.47310            +     -0.21550 -0.16770                     
## 9    0.60240                           -0.26770                     
## 11   0.05312            +              -0.24670                     
## 1   -0.34500                                                        
## 3   -0.68690            +                                           
##     Hbt:log(max_siz) log(max_siz):TL df   logLik  AICc delta weight
## 6                                     4 -170.347 349.0  0.00  0.317
## 14                                    5 -170.251 350.9  1.94  0.120
## 22                                    5 -170.339 351.1  2.12  0.110
## 8                                     6 -169.734 352.0  3.07  0.068
## 2                                     3 -173.201 352.6  3.60  0.052
## 40                 +                  7 -168.970 352.7  3.74  0.049
## 78                           0.02272  6 -170.078 352.7  3.76  0.048
## 10                                    4 -172.348 353.0  4.00  0.043
## 30                                    6 -170.250 353.1  4.10  0.041
## 16                                    7 -169.659 354.1  5.11  0.025
## 24                                    7 -169.720 354.2  5.23  0.023
## 56                 +                  8 -168.813 354.6  5.64  0.019
## 48                 +                  8 -168.909 354.8  5.83  0.017
## 94                           0.02356  7 -170.074 354.9  5.94  0.016
## 80                           0.02592  8 -169.433 355.8  6.88  0.010
## 32                                    8 -169.654 356.3  7.32  0.008
## 4                                     5 -172.954 356.3  7.35  0.008
## 64                 +                  9 -168.699 356.6  7.67  0.007
## 12                                    6 -172.042 356.6  7.69  0.007
## 112                +         0.01402  9 -168.847 356.9  7.96  0.006
## 96                           0.02647  9 -169.431 358.1  9.13  0.003
## 128                +         0.01689 10 -168.610 358.7  9.77  0.002
## 5                                     3 -190.576 387.3 38.35  0.000
## 13                                    4 -189.702 387.7 38.71  0.000
## 7                                     5 -188.784 388.0 39.01  0.000
## 15                                    6 -188.348 389.3 40.30  0.000
## 77                           0.02736  5 -189.505 389.4 40.45  0.000
## 39                 +                  6 -188.561 389.7 40.72  0.000
## 79                           0.03378  7 -188.045 390.8 41.89  0.000
## 47                 +                  7 -188.124 391.0 42.04  0.000
## 111                +         0.04395  8 -187.642 392.3 43.30  0.000
## 9                                     3 -193.540 393.2 44.28  0.000
## 11                                    5 -192.903 396.2 47.25  0.000
## 1                                     2 -197.070 398.2 49.26  0.000
## 3                                     4 -195.734 399.7 50.77  0.000
## Models ranked by AICc(x)
```

```r
ZN.R1 <- lm(log(ZN_mg) ~ Abs_lat + log(max_size), data=ntbl.ZN, na.action = "na.fail")

ZN.R2 <- lm(log(ZN_mg) ~ Abs_lat + log(max_size) + TL, data=ntbl.ZN, na.action = "na.fail")

ZN.avg <- model.avg(ZN.R1, ZN.R2)

summary(ZN.avg)
```

```
## 
## Call:
## model.avg.default(object = ZN.R1, ZN.R2)
## 
## Component model call: 
## lm(formula = <2 unique values>, data = ntbl.ZN, na.action = 
##      na.fail)
## 
## Component models: 
##     df  logLik   AICc delta weight
## 12   4 -170.35 348.96  0.00   0.73
## 123  5 -170.25 350.90  1.94   0.27
## 
## Term codes: 
##       Abs_lat log(max_size)            TL 
##             1             2             3 
## 
## Model-averaged coefficients:  
## (full average) 
##                Estimate Std. Error Adjusted SE z value Pr(>|z|)    
## (Intercept)    0.881844   0.242011    0.243823   3.617 0.000298 ***
## Abs_lat       -0.027294   0.004087    0.004120   6.625  < 2e-16 ***
## log(max_size) -0.056937   0.025010    0.025207   2.259 0.023898 *  
## TL            -0.011499   0.054170    0.054551   0.211 0.833046    
##  
## (conditional average) 
##                Estimate Std. Error Adjusted SE z value Pr(>|z|)    
## (Intercept)    0.881844   0.242011    0.243823   3.617 0.000298 ***
## Abs_lat       -0.027294   0.004087    0.004120   6.625  < 2e-16 ***
## log(max_size) -0.056937   0.025010    0.025207   2.259 0.023898 *  
## TL            -0.041881   0.097031    0.097805   0.428 0.668498    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Relative variable importance: 
##                      Abs_lat log(max_size) TL  
## Importance:          1.00    1.00          0.27
## N containing models:    2       2             1
```

```r
ZN.size <- lm(log(ZN_mg) ~ log(max_size), data=ntbl.ZN)
summary(ZN.1)
```

```
## 
## Call:
## lm(formula = log(ZN_mg) ~ log(max_size) * TL + log(max_size) * 
##     Abs_lat + log(max_size) * Habitat, data = ntbl.ZN, na.action = "na.fail")
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.5238 -0.3381 -0.0940  0.3604  1.7107 
## 
## Coefficients: (1 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                      0.236777   0.835323   0.283    0.777    
## log(max_size)                   -0.189963   0.164406  -1.155    0.250    
## TL                              -0.074063   0.121310  -0.611    0.542    
## Abs_lat                         -0.028970   0.004575  -6.332 2.74e-09 ***
## Habitatfreshwater                0.854533   0.744765   1.147    0.253    
## Habitatmarine                    0.950729   0.754953   1.259    0.210    
## log(max_size):TL                 0.016889   0.041274   0.409    0.683    
## log(max_size):Abs_lat            0.001204   0.001800   0.669    0.505    
## log(max_size):Habitatfreshwater  0.075806   0.060771   1.247    0.214    
## log(max_size):Habitatmarine            NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.7294 on 148 degrees of freedom
## Multiple R-squared:  0.3041,	Adjusted R-squared:  0.2665 
## F-statistic: 8.084 on 8 and 148 DF,  p-value: 5.037e-09
```



This function allows me to average the top models and then prints out a table with the relevant AIC values, and importance weights. One thing I would like it to be able to do, but haven't yet figured out, is to select the top model set and then average them, all in one function. Right now I need to do the model selection process outside the function and plop in the top models into the function. It'd be great if I could automate that process somehow!


```r
AIC.table <- function(mod1, mod2) {
  model.average <-  model.avg(mod1, mod2)
  return((msTable <- model.average$msTable))
  }
```

