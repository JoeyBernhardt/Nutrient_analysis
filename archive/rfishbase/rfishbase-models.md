# rfishbase-models

Load packages

```r
library(devtools)
library(rfishbase)
library(ggplot2)
library(plyr)
suppressPackageStartupMessages(library(dplyr))
library(knitr)
library(tidyr)
library(readr)
library(ggthemes)
library(MuMIn)
library(visreg)
library(broom)
```

Load the data

```r
fb.all <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data/fb.all.csv")

temps.fb <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data/temps.fb.csv")
temps.fb <- select(temps.fb, -SpecCode)
temps.fb <- temps.fb %>% 
  dplyr::rename(species = sciname) %>% 
  mutate(species = as.factor(species))
fb.temps <- inner_join(fb.all, temps.fb, by = "species")
```

```
## Warning in inner_join_impl(x, y, by$x, by$y): joining character vector and
## factor, coercing into character vector
```

```r
ntbl <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data/ntbl.csv")
table(fb.all$Herbivory2)
```

```
## 
##        mainly animals (troph. 2.8 and up) 
##                                       719 
##    mainly plants/detritus (troph. 2-2.19) 
##                                        79 
## plants/detritus+animals (troph. 2.2-2.79) 
##                                        60
```



```r
options(na.action = "na.fail")
fb.all <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data/fb.all.csv")
fb.all <- fb.all %>% filter(!is.na(CA_mg),
                            !is.na(max_size),
                            !is.na(DemersPelag),
                            !is.na(TL),
                            !is.na(Habitat),
                            !is.na(FoodTroph.x),
                            !is.na(Herbivory2),
                            !is.na(taxon),
                            !is.na(Abs_lat))
length(unique(fb.all$species))
```

```
## [1] 61
```

```r
fb.all.ZN <- fb.all %>% 
  filter(!is.na(ZN_mg),
         !is.na(max_size), 
         !is.na(DemersPelag),
         !is.na(TL),
         !is.na(Habitat),
         !is.na(FoodTroph.x),
         !is.na(taxon),
         !is.na(Abs_lat))

### this model contains all the traits I'm considering: body size, ocean zone use (DemersPelag), trophic level, habitat (marine/freshwater), trophic level of diet, herbivory, 
ZN.all <- lm(log(ZN_mg) ~ max_length + DemersPelag + TL + Habitat + FoodTroph.x + Herbivory2, data = fb.all.ZN)
CA.all.lat <- lm(log(CA_mg) ~ max_length + DemersPelag + TL + Habitat + FoodTroph.x + Herbivory2 + Abs_lat,  data = fb.all) 

tidy(CA.all.lat, conf.int = TRUE) %>% kable()
```



term                                                     estimate   std.error     statistic     p.value     conf.low    conf.high
----------------------------------------------------  -----------  ----------  ------------  ----------  -----------  -----------
(Intercept)                                             6.2109613   1.2169489     5.1037158   0.0000011    3.8038835    8.6180390
max_length                                             -0.0009084   0.0012903    -0.7040113   0.4826565   -0.0034606    0.0016438
DemersPelagbenthopelagic                                0.9791905   0.5194810     1.8849401   0.0616198   -0.0483227    2.0067037
DemersPelagdemersal                                     0.8597959   0.5068176     1.6964604   0.0921380   -0.1426696    1.8622615
DemersPelagpelagic                                      1.2331216   0.6411822     1.9232002   0.0565921   -0.0351118    2.5013551
DemersPelagpelagic-neritic                              1.6682198   0.4841814     3.4454437   0.0007633    0.7105277    2.6259118
DemersPelagpelagic-oceanic                              0.0601393   0.5844533     0.1028984   0.9181986   -1.0958866    1.2161653
TL                                                      0.0572072   0.2679636     0.2134888   0.8312726   -0.4728144    0.5872289
Habitatfreshwater                                       1.0910531   0.5185147     2.1041892   0.0372442    0.0654511    2.1166551
Habitatmarine                                           1.3848200   0.4877871     2.8389844   0.0052367    0.4199960    2.3496440
FoodTroph.x                                            -0.4421666   0.3509217    -1.2600150   0.2098703   -1.1362762    0.2519429
Herbivory2mainly plants/detritus (troph. 2-2.19)       -0.8450775   0.5035984    -1.6780782   0.0956801   -1.8411756    0.1510206
Herbivory2plants/detritus+animals (troph. 2.2-2.79)    -0.1427037   0.3111402    -0.4586475   0.6472359   -0.7581270    0.4727196
Abs_lat                                                -0.0785206   0.0061209   -12.8283075   0.0000000   -0.0906274   -0.0664137

```r
CA.all<- lm(log(CA_mg) ~ max_length + DemersPelag + TL + Habitat + FoodTroph.x + Herbivory2,  data = fb.all) 

dd <- dredge(CA.all, m.lim = c(NA, 1), extra = list(
    "R^2", "*" = function(x) {
        s <- summary(x)
        c(Rsq = s$r.squared, adjRsq = s$adj.r.squared,
            F = s$fstatistic[[1]])
    })
)
```

```
## Fixed term is "(Intercept)"
```

```r
dd
```

```
## Global model call: lm(formula = log(CA_mg) ~ max_length + DemersPelag + TL + Habitat + 
##     FoodTroph.x + Herbivory2, data = fb.all)
## ---
## Model selection table 
##    (Int) DmP   FdT.x Hbt Hr2   max_lng      TL     R^2   *.Rsq  *.adjRsq
## 2  2.348   +                                   0.13000 0.13000  0.099150
## 33 5.302                               -0.4764 0.06327 0.06327  0.056810
## 3  5.749     -0.5907                           0.06028 0.06028  0.053800
## 9  3.451                   +                   0.06668 0.06668  0.053720
## 17 3.974                     -0.003663         0.03402 0.03402  0.027350
## 1  3.647                                       0.00000 0.00000  0.000000
## 5  3.987               +                       0.01256 0.01256 -0.001156
##       *.F df   logLik  AICc delta weight
## 2  4.2140  7 -252.165 519.1  0.00  0.561
## 33 9.7930  3 -257.597 521.4  2.23  0.184
## 3  9.3010  3 -257.831 521.8  2.69  0.146
## 9  5.1440  4 -257.329 522.9  3.80  0.084
## 17 5.1060  3 -259.857 525.9  6.75  0.019
## 1          2 -262.401 528.9  9.75  0.004
## 5  0.9157  4 -261.472 531.2 12.09  0.001
## Models ranked by AICc(x)
```

```r
subset(dd, delta < 4)
```

```
## Global model call: lm(formula = log(CA_mg) ~ max_length + DemersPelag + TL + Habitat + 
##     FoodTroph.x + Herbivory2, data = fb.all)
## ---
## Model selection table 
##    (Int) DmP   FdT.x Hr2      TL     R^2   *.Rsq *.adjRsq   *.F df
## 2  2.348   +                     0.13000 0.13000  0.09915 4.214  7
## 33 5.302                 -0.4764 0.06327 0.06327  0.05681 9.793  3
## 3  5.749     -0.5907             0.06028 0.06028  0.05380 9.301  3
## 9  3.451               +         0.06668 0.06668  0.05372 5.144  4
##      logLik  AICc delta weight
## 2  -252.165 519.1  0.00  0.575
## 33 -257.597 521.4  2.23  0.189
## 3  -257.831 521.8  2.69  0.150
## 9  -257.329 522.9  3.80  0.086
## Models ranked by AICc(x)
```

```r
par(mar = c(3,5,6,4))
plot(dd, labAsExpr = TRUE)
```

![](rfishbase-models_files/figure-html/unnamed-chunk-3-1.png) 

```r
model.sel(dd)
```

```
## Global model call: lm(formula = log(CA_mg) ~ max_length + DemersPelag + TL + Habitat + 
##     FoodTroph.x + Herbivory2, data = fb.all)
## ---
## Model selection table 
##    (Int) DmP   FdT.x Hbt Hr2   max_lng      TL     R^2   *.Rsq  *.adjRsq
## 2  2.348   +                                   0.13000 0.13000  0.099150
## 33 5.302                               -0.4764 0.06327 0.06327  0.056810
## 3  5.749     -0.5907                           0.06028 0.06028  0.053800
## 9  3.451                   +                   0.06668 0.06668  0.053720
## 17 3.974                     -0.003663         0.03402 0.03402  0.027350
## 1  3.647                                       0.00000 0.00000  0.000000
## 5  3.987               +                       0.01256 0.01256 -0.001156
##       *.F df   logLik  AICc delta weight
## 2  4.2140  7 -252.165 519.1  0.00  0.561
## 33 9.7930  3 -257.597 521.4  2.23  0.184
## 3  9.3010  3 -257.831 521.8  2.69  0.146
## 9  5.1440  4 -257.329 522.9  3.80  0.084
## 17 5.1060  3 -259.857 525.9  6.75  0.019
## 1          2 -262.401 528.9  9.75  0.004
## 5  0.9157  4 -261.472 531.2 12.09  0.001
## Models ranked by AICc(x)
```

```r
# Model average models with delta AICc < 4
model.avg(dd, subset = delta < 4)
```

```
## 
## Call:
## model.avg.model.selection(object = dd, subset = delta < 4)
## 
## Component models: 
## '1' '4' '2' '3'
## 
## Coefficients: 
##        (Intercept) DemersPelagbenthopelagic DemersPelagdemersal
## full      3.510137                0.8321219           0.4845094
## subset    3.510137                1.4462197           0.8420726
##        DemersPelagpelagic DemersPelagpelagic-neritic
## full            0.8717051                   1.101690
## subset          1.5150150                   1.914726
##        DemersPelagpelagic-oceanic          TL FoodTroph.x
## full                    0.1348625 -0.09007687 -0.08838173
## subset                  0.2343897 -0.47643703 -0.59072122
##        Herbivory2mainly plants/detritus (troph. 2-2.19)
## full                                         0.07147169
## subset                                       0.83162155
##        Herbivory2plants/detritus+animals (troph. 2.2-2.79)
## full                                            0.08378334
## subset                                          0.97487595
```

```r
#or as a 95% confidence set:
model.avg(dd, subset = cumsum(weight) <= .95) # get averaged coefficients
```

```
## 
## Call:
## model.avg.model.selection(object = dd, subset = cumsum(weight) <= 
##     0.95)
## 
## Component models: 
## '1' '3' '2'
## 
## Coefficients: 
##        (Intercept) DemersPelagbenthopelagic DemersPelagdemersal
## full      3.515652                0.9103606           0.5300645
## subset    3.515652                1.4462197           0.8420726
##        DemersPelagpelagic DemersPelagpelagic-neritic
## full            0.9536656                   1.205274
## subset          1.5150150                   1.914726
##        DemersPelagpelagic-oceanic          TL FoodTroph.x
## full                    0.1475427 -0.09854618 -0.09669166
## subset                  0.2343897 -0.47643703 -0.59072122
```

```r
#'Best' model
summary(get.models(dd, 1)[[1]])
```

```
## 
## Call:
## lm(formula = log(CA_mg) ~ DemersPelag + 1, data = fb.all)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.6990 -0.8588 -0.1787  0.4565  3.9428 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                  2.3481     0.6867   3.419 0.000821 ***
## DemersPelagbenthopelagic     1.4462     0.7142   2.025 0.044758 *  
## DemersPelagdemersal          0.8421     0.7179   1.173 0.242815    
## DemersPelagpelagic           1.5150     0.9213   1.644 0.102323    
## DemersPelagpelagic-neritic   1.9147     0.7211   2.655 0.008833 ** 
## DemersPelagpelagic-oceanic   0.2344     0.8608   0.272 0.785805    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.373 on 141 degrees of freedom
## Multiple R-squared:   0.13,	Adjusted R-squared:  0.09915 
## F-statistic: 4.214 on 5 and 141 DF,  p-value: 0.001338
```

```r
#### Calcium models ####
CA_old <- read_csv("/Users/Joey/Desktop/Nutrient_databases/nutmassCA_Feb24.csv")
CA_old2 <- read_csv("/Users/Joey/Desktop/Nutrient_databases/nut_sept22_lwr_dec3.csv")
CA_old3 <- read.csv("/Users/Joey/Desktop/Nutrient_databases/nut_mass_feb10_ff.csv")
ntbl <- read.csv("/Users/Joey/Documents/Nutrient_Analysis/data/ntbl.csv")

options(na.action = "na.omit")
CA.lat <- lm(log(CA_mg) ~ Abs_lat, data = fb.all)
CA.size1 <- lm(log(CA_mg) ~ Length, data = fb.all)
CA.size2 <- lm(log(CA_mg) ~ max_length, data = fb.all)
CA.size3 <- lm(log(CA_mg) ~ log(max_size), data = ntbl)
summary(CA.size1)
```

```
## 
## Call:
## lm(formula = log(CA_mg) ~ Length, data = fb.all)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.6743 -1.1655 -0.0523  0.6104  3.7622 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  3.903643   0.195189  19.999   <2e-16 ***
## Length      -0.002682   0.001621  -1.655      0.1    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.438 on 145 degrees of freedom
## Multiple R-squared:  0.01854,	Adjusted R-squared:  0.01177 
## F-statistic: 2.739 on 1 and 145 DF,  p-value: 0.1001
```

```r
fb.CA.temps <- fb.temps %>%
  group_by(species) %>% 
    summarise(mean.CA = mean(CA_mg, na.rm = TRUE),
              mean.temp = mean(TempMin, na.rm = TRUE))

  
CA.temp <- lm(log(mean.CA) ~ mean.temp, data = fb.CA.temps)
visreg(CA.lat, xvar = "Abs_lat")
```

![](rfishbase-models_files/figure-html/unnamed-chunk-3-2.png) 

```r
visreg(CA.size3, xvar = "max_size", xtrans = log)
```

![](rfishbase-models_files/figure-html/unnamed-chunk-3-3.png) 

```r
visreg(CA.temp, xvar = "mean.temp")
```

![](rfishbase-models_files/figure-html/unnamed-chunk-3-4.png) 

```r
summary(CA.temp)
```

```
## 
## Call:
## lm(formula = log(mean.CA) ~ mean.temp, data = fb.CA.temps)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.6456 -1.0035  0.1231  0.8966  3.0356 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.93415    0.31360   9.356 9.96e-12 ***
## mean.temp    0.12125    0.02591   4.680 3.12e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.397 on 41 degrees of freedom
##   (238 observations deleted due to missingness)
## Multiple R-squared:  0.3482,	Adjusted R-squared:  0.3323 
## F-statistic:  21.9 on 1 and 41 DF,  p-value: 3.124e-05
```

```r
?visreg
length(unique(fb.all$species))
```

```
## [1] 61
```

```r
#### merge FB and SLB basic info ####
intbl.basic <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data/intbl.basic.csv")
fb.length <- fb.all %>% 
  select(species, CA_mg, ZN_mg, FE_mg, EPA_g, DHA_g, Length, max_length, Subgroup, Habitat) %>% filter(Habitat != "brackish")

slb.length <- intbl.basic %>% 
  select(species, CA_mg, ZN_mg, FE_mg, EPA_g, DHA_g, Length, max_length, Subgroup, Habitat)

length.CA <- dplyr::bind_rows(fb.length, slb.length)
CA.length <- lm(log(CA_mg) ~ log(Length), data = length.CA) 
summary(CA.length)
```

```
## 
## Call:
## lm(formula = log(CA_mg) ~ log(Length), data = length.CA)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.7228 -1.0898 -0.2071  0.6099  3.8931 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   5.4346     0.6970   7.797 9.84e-13 ***
## log(Length)  -0.4132     0.1616  -2.557   0.0115 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.417 on 150 degrees of freedom
##   (118 observations deleted due to missingness)
## Multiple R-squared:  0.04178,	Adjusted R-squared:  0.03539 
## F-statistic:  6.54 on 1 and 150 DF,  p-value: 0.01154
```

```r
visreg(CA.length, xvar = "Length", xtrans = log)
```

![](rfishbase-models_files/figure-html/unnamed-chunk-3-5.png) 

```r
ggplot(length.CA, aes(x = Length, y = log(DHA_g), color = Subgroup, shape = Habitat)) + stat_summary(fun.y= "mean", geom = "point") + scale_x_log10() + geom_smooth(method = 'lm')
```

```
## Warning: Removed 189 rows containing missing values (stat_summary).
```

```
## Warning: Removed 5 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 44 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 56 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 49 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 35 rows containing missing values (stat_smooth).
```

![](rfishbase-models_files/figure-html/unnamed-chunk-3-6.png) 

```r
#### inverts ecology ####
intbl.all <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data/intbl.all.csv")

length(unique(intbl.all$species))
```

```
## [1] 43
```

```r
CA.inverts <- lm(log(FE_mg.x) ~ log(Length) + FoodTroph + Subgroup.x, data = intbl.all)

CA.inverts.size <- lm(log(FE_mg.x) ~ log(Length), data = intbl.all)
visreg(CA.inverts.size, xvar = "Length", xtrans = log)
```

![](rfishbase-models_files/figure-html/unnamed-chunk-3-7.png) 

```r
visreg(CA.inverts, xvar = "Length", xtrans = log)
```

![](rfishbase-models_files/figure-html/unnamed-chunk-3-8.png) 

```r
summary(CA.inverts.size)
```

```
## 
## Call:
## lm(formula = log(FE_mg.x) ~ log(Length), data = intbl.all)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.5263 -0.0853  0.0469  0.1946  3.3379 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   0.1139     0.7475   0.152    0.879
## log(Length)   0.1210     0.2553   0.474    0.636
## 
## Residual standard error: 0.6089 on 122 degrees of freedom
##   (675 observations deleted due to missingness)
## Multiple R-squared:  0.001838,	Adjusted R-squared:  -0.006343 
## F-statistic: 0.2247 on 1 and 122 DF,  p-value: 0.6363
```

```r
summary(CA.inverts)
```

```
## 
## Call:
## lm(formula = log(FE_mg.x) ~ log(Length) + FoodTroph + Subgroup.x, 
##     data = intbl.all)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.4658 -0.1347 -0.0220  0.1280  4.3036 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        -10.6667     1.4381  -7.417 9.30e-11 ***
## log(Length)          4.4243     0.5326   8.308 1.58e-12 ***
## FoodTroph           -0.8958     0.1262  -7.099 3.91e-10 ***
## Subgroup.xMolluscs   2.8547     0.3927   7.269 1.82e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5122 on 83 degrees of freedom
##   (712 observations deleted due to missingness)
## Multiple R-squared:  0.5088,	Adjusted R-squared:  0.491 
## F-statistic: 28.66 on 3 and 83 DF,  p-value: 8.15e-13
```
