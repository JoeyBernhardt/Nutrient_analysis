# Homework3



```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)

nut <- read.csv("~/Desktop/Nutrient_databases/nut_sept22_lwr_dec3.csv", comment.char="#", stringsAsFactors=FALSE)

str(nut)
```

```
## 'data.frame':	1188 obs. of  98 variables:
##  $ ASFIS.Scientific.name                : chr  "Abramis brama" "Abramis brama" "Abramis brama" "Abramis brama" ...
##  $ Food.name.in.English                 : chr  "Common bream, wild, skinless fillet, raw" "Bream, wild, dorsal muscle, raw" "Bream, wild, skinless fillet, raw" "Bream, wild, dorsal muscle, raw" ...
##  $ TaxonKey                             : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ lwA                                  : num  0.00871 0.00871 0.00871 0.00871 0.00871 0.00871 0.00871 0.00871 0.00871 0.0055 ...
##  $ lwB                                  : num  3.14 3.14 3.14 3.14 3.14 3.14 3.14 3.14 3.14 3.19 ...
##  $ SLMAX                                : num  82 82 82 82 82 82 82 82 82 59 ...
##  $ SLMAX_nov28                          : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ SLMAX_source                         : chr  "http://www.fishbase.org/summary/Abramis-brama.html" "" "" "" ...
##  $ TL                                   : num  2.9 2.9 2.9 2.9 2.9 2.9 2.9 2.9 2.9 4 ...
##  $ TL_se                                : num  0.4 0.4 0.4 0.4 0.4 0.4 0.4 0.4 0.4 0.66 ...
##  $ TL_nov28                             : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ TLSE_nov28                           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ X                                    : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ sci.name                             : chr  "Abramis abramis" "Abramis brama" "Abramis brama" "Abramis brama" ...
##  $ Food.Item.ID                         : int  900684 900123 900159 900122 900124 900125 900158 900225 902192 900073 ...
##  $ Subgroup                             : chr  "Finfish" "Finfish" "Finfish" "Finfish" ...
##  $ country.region                       : chr  "Germany, Usedom, Baltic Sea" "Poland, Kisajno Lake" "Greece, Evros River" "Poland, Niegocin Lake" ...
##  $ Type                                 : chr  "W" "W" "W" "W" ...
##  $ ISSCAAP                              : int  11 11 11 11 11 11 11 11 11 13 ...
##  $ ISSCAAP_cat                          : chr  "Carps, barbels and other cyprinids" "Carps, barbels and other cyprinids" "Carps, barbels and other cyprinids" "Carps, barbels and other cyprinids" ...
##  $ Habitat                              : chr  "marine" "freshwater" "freshwater" "freshwater" ...
##  $ X3_alpha                             : chr  "FBM" "FBM" "FBM" "FBM" ...
##  $ Food.name.in.own.language            : chr  NA NA NA NA ...
##  $ Processing                           : chr  "r" "r" "r" "r" ...
##  $ ASFIS.English.name                   : chr  "Freshwater bream" "Freshwater bream" "Freshwater bream" "Freshwater bream" ...
##  $ Season                               : chr  "3-Jul" "Jun-97" "Summer 1988" "Jun-97" ...
##  $ Other                                : chr  "fishing areas: North Atlantic, North Sea, Barents Sea, Baltic Sea" "mean length and weight: 33.2cm, 444.1g" "mean length and weight: 30cm, 400g" "mean length and weight: 31.6cm, 361.3g" ...
##  $ Latitude                             : num  53.9 54.1 41.5 54 54.1 ...
##  $ Abs_lat                              : num  53.9 54.1 41.5 54 54.1 ...
##  $ length_from_study                    : num  NA NA NA NA NA NA NA NA NA 39 ...
##  $ length_3                             : num  NA NA NA NA NA ...
##  $ n.x                                  : int  NA NA NA NA NA NA NA NA NA 1 ...
##  $ WATER.g.                             : chr  NA NA NA NA ...
##  $ FAT.g.                               : num  NA NA NA NA NA NA NA 1 NA 0.36 ...
##  $ FATCE.g.                             : chr  NA NA NA NA ...
##  $ FAT..g.                              : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ FASAT.g.                             : num  NA NA NA NA NA ...
##  $ FAMS.g.                              : num  NA NA NA NA NA ...
##  $ FAPU.g.                              : num  NA NA NA NA NA ...
##  $ FAUN.g.                              : chr  NA NA NA NA ...
##  $ FATRN.g.                             : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ FACID.g.                             : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ FAPUN3.g.                            : num  NA NA NA NA NA ...
##  $ FAPUN6.g.                            : num  NA NA NA NA NA ...
##  $ FAPUN9.g.                            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ EPA_g                                : num  NA NA NA NA NA ...
##  $ DHA_g                                : num  NA NA NA NA NA ...
##  $ Food_item_id                         : int  900684 900123 900159 900122 900124 900125 900158 900225 902192 900073 ...
##  $ max_length_study                     : num  NA 33.2 30 31.6 38 50.5 36 NA NA 39 ...
##  $ Comments.on.data.processing.methods.y: chr  NA "Minerals given per DM - conversion to FW" NA "Minerals given per DM - conversion to FW" ...
##  $ Publication.year                     : int  2006 2009 1989 2009 2009 2009 1989 1991 1995 2008 ...
##  $ BiblioID.y                           : chr  "fi105" "fi26" "fi32" "fi26" ...
##  $ Compiler                             : chr  "DR" "DR" "DR" "DR" ...
##  $ EDIBLE                               : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ ENERC_kJ._original                   : num  NA NA NA NA NA ...
##  $ ENERC_kcal._original                 : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ ENERA_kcal                           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ DM_g                                 : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ WATER_g                              : chr  NA "78.76" "78" "80.04" ...
##  $ XN                                   : num  NA NA 6.25 NA NA NA 6.25 NA NA NA ...
##  $ NT_g                                 : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ PROTCNT_g                            : chr  NA NA "19" NA ...
##  $ PROTCNP_g                            : logi  NA NA NA NA NA NA ...
##  $ PROT_g                               : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ NPRO_g                               : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ NNP_mg                               : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ FAT_g                                : num  6.4 NA 1.4 NA NA NA 0.7 1 NA 0.36 ...
##  $ FATCE_g                              : chr  NA NA NA NA ...
##  $ FAT_g.1                              : num  NA NA NA NA NA NA NA NA 4 NA ...
##  $ FASAT_g                              : num  NA NA NA NA NA NA NA 0.24 NA 0.09 ...
##  $ FAMS_g                               : num  NA NA NA NA NA NA NA NA NA 0.07 ...
##  $ FAPU_g                               : num  NA NA NA NA NA NA NA NA NA 0.05 ...
##  $ FAUN_g                               : chr  NA NA NA NA ...
##  $ CA_mg                                : num  NA 11.6 53 20.9 11.5 ...
##  $ FE_mg                                : num  NA 0.17 0.6 0.21 0.18 0.19 0.7 NA NA NA ...
##  $ ID_mcg                               : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ K_mg                                 : num  NA 482 570 448 306 ...
##  $ MG_mg                                : num  NA 20.4 69 19.7 19.2 ...
##  $ MN_mg                                : chr  NA "0.01" "0.09" "0.01" ...
##  $ SE_mcg                               : chr  NA NA NA NA ...
##  $ HG_mcg                               : chr  NA NA "6" NA ...
##  $ PB_mcg                               : chr  NA NA "8" NA ...
##  $ SR_mcg                               : chr  NA NA "nd" NA ...
##  $ RETOL_mcg                            : chr  NA NA NA NA ...
##  $ RETOL13_mcg                          : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ RETOLDH_mcg                          : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ RETOLSUM_mcg                         : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ CARTA_mcg                            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ CARTB_mcg                            : chr  NA NA NA NA ...
##  $ ATX_mcg                              : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ ZEA_mcg                              : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ CARTOID_mcg                          : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ CHOCAL_mcg                           : num  8.6 NA NA NA NA NA NA NA 13.8 NA ...
##  $ TOCPHA_mg                            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ VITB6A_mg                            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ VITB12_mcg                           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ VITC_mg                              : chr  NA NA NA NA ...
##  $ ZN_mg                                : num  NA 0.393 1 0.373 0.373 ...
```

```r
summary(nut)
```

```
##  ASFIS.Scientific.name Food.name.in.English    TaxonKey     
##  Length:1188           Length:1188          Min.   :600024  
##  Class :character      Class :character     1st Qu.:600228  
##  Mode  :character      Mode  :character     Median :600790  
##                                             Mean   :606378  
##                                             3rd Qu.:601368  
##                                             Max.   :690283  
##                                             NA's   :789     
##       lwA               lwB             SLMAX         SLMAX_nov28    
##  Min.   :0.00019   Min.   :0.8935   Min.   :  2.30   Min.   : 15.50  
##  1st Qu.:0.00646   1st Qu.:2.9990   1st Qu.: 27.00   1st Qu.: 60.00  
##  Median :0.00955   Median :3.0500   Median : 60.00   Median : 60.00  
##  Mean   :0.01066   Mean   :3.0182   Mean   : 68.63   Mean   : 79.39  
##  3rd Qu.:0.01270   3rd Qu.:3.1000   3rd Qu.: 90.00   3rd Qu.:100.00  
##  Max.   :0.08800   Max.   :3.8600   Max.   :800.00   Max.   :200.00  
##  NA's   :205       NA's   :205      NA's   :5        NA's   :1124    
##  SLMAX_source             TL            TL_se           TL_nov28    
##  Length:1188        Min.   :2.000   Min.   :0.0000   Min.   :1.600  
##  Class :character   1st Qu.:3.020   1st Qu.:0.3100   1st Qu.:3.000  
##  Mode  :character   Median :3.470   Median :0.4700   Median :3.500  
##                     Mean   :3.461   Mean   :0.4676   Mean   :3.434  
##                     3rd Qu.:4.100   3rd Qu.:0.6550   3rd Qu.:4.000  
##                     Max.   :4.500   Max.   :0.9700   Max.   :4.500  
##                     NA's   :109     NA's   :109      NA's   :832    
##    TLSE_nov28           X            sci.name          Food.Item.ID   
##  Min.   :0.0000   Min.   :   1.0   Length:1188        Min.   :900001  
##  1st Qu.:0.0000   1st Qu.: 297.8   Class :character   1st Qu.:900357  
##  Median :0.3000   Median : 594.5   Mode  :character   Median :900666  
##  Mean   :0.3025   Mean   : 594.5                      Mean   :900764  
##  3rd Qu.:0.5000   3rd Qu.: 891.2                      3rd Qu.:901043  
##  Max.   :0.8600   Max.   :1188.0                      Max.   :902297  
##  NA's   :832                                                          
##    Subgroup         country.region         Type              ISSCAAP    
##  Length:1188        Length:1188        Length:1188        Min.   :11.0  
##  Class :character   Class :character   Class :character   1st Qu.:13.0  
##  Mode  :character   Mode  :character   Mode  :character   Median :33.0  
##                                                           Mean   :30.2  
##                                                           3rd Qu.:37.0  
##                                                           Max.   :81.0  
##                                                           NA's   :14    
##  ISSCAAP_cat          Habitat            X3_alpha        
##  Length:1188        Length:1188        Length:1188       
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##                                                          
##  Food.name.in.own.language  Processing        ASFIS.English.name
##  Length:1188               Length:1188        Length:1188       
##  Class :character          Class :character   Class :character  
##  Mode  :character          Mode  :character   Mode  :character  
##                                                                 
##                                                                 
##                                                                 
##                                                                 
##     Season             Other              Latitude         Abs_lat        
##  Length:1188        Length:1188        Min.   :-82.86   Min.   : 0.03333  
##  Class :character   Class :character   1st Qu.: 18.10   1st Qu.:23.37217  
##  Mode  :character   Mode  :character   Median : 37.37   Median :37.50973  
##                                        Mean   : 30.16   Mean   :35.24923  
##                                        3rd Qu.: 43.07   3rd Qu.:45.86762  
##                                        Max.   : 71.71   Max.   :82.86275  
##                                        NA's   :13       NA's   :13        
##  length_from_study    length_3             n.x           WATER.g.        
##  Min.   :  2.50    Min.   :      16   Min.   : 1.000   Length:1188       
##  1st Qu.: 18.00    1st Qu.:    5832   1st Qu.: 1.000   Class :character  
##  Median : 24.00    Median :   13824   Median : 1.000   Mode  :character  
##  Mean   : 30.35    Mean   :  192069   Mean   : 2.693                     
##  3rd Qu.: 37.00    3rd Qu.:   50653   3rd Qu.: 4.000                     
##  Max.   :251.00    Max.   :15813251   Max.   :36.000                     
##  NA's   :980       NA's   :980        NA's   :950                        
##      FAT.g.         FATCE.g.            FAT..g.           FASAT.g.     
##  Min.   : 0.100   Length:1188        Min.   : 0.1000   Min.   :0.0105  
##  1st Qu.: 0.915   Class :character   1st Qu.: 0.4752   1st Qu.:0.1612  
##  Median : 1.500   Mode  :character   Median : 1.1700   Median :0.3472  
##  Mean   : 3.108                      Mean   : 3.0569   Mean   :0.8373  
##  3rd Qu.: 3.500                      3rd Qu.: 4.3512   3rd Qu.:0.9546  
##  Max.   :26.500                      Max.   :21.6000   Max.   :9.2387  
##  NA's   :805                         NA's   :1135      NA's   :726     
##     FAMS.g.           FAPU.g.         FAUN.g.             FATRN.g.     
##  Min.   : 0.0033   Min.   :0.0140   Length:1188        Min.   :0.0006  
##  1st Qu.: 0.0978   1st Qu.:0.1840   Class :character   1st Qu.:0.0014  
##  Median : 0.2503   Median :0.3715   Mode  :character   Median :0.0036  
##  Mean   : 0.7843   Mean   :0.7305                      Mean   :0.0140  
##  3rd Qu.: 0.8414   3rd Qu.:0.7998                      3rd Qu.:0.0098  
##  Max.   :11.2927   Max.   :6.0127                      Max.   :0.0836  
##  NA's   :737       NA's   :766                         NA's   :1166    
##     FACID.g.        FAPUN3.g.        FAPUN6.g.        FAPUN9.g.     
##  Min.   :0.1900   Min.   :0.0049   Min.   :0.0018   Min.   :0.5291  
##  1st Qu.:0.2800   1st Qu.:0.1404   1st Qu.:0.0377   1st Qu.:0.6018  
##  Median :0.3700   Median :0.3103   Median :0.0675   Median :0.6745  
##  Mean   :0.4756   Mean   :0.6464   Mean   :0.1584   Mean   :0.6745  
##  3rd Qu.:0.4800   3rd Qu.:0.5996   3rd Qu.:0.1623   3rd Qu.:0.7471  
##  Max.   :1.8716   Max.   :5.9032   Max.   :1.6771   Max.   :0.8198  
##  NA's   :1153     NA's   :813      NA's   :824      NA's   :1186    
##      EPA_g            DHA_g         Food_item_id    max_length_study
##  Min.   :0.0004   Min.   :0.0007   Min.   :900001   Min.   :  1.91  
##  1st Qu.:0.0333   1st Qu.:0.0733   1st Qu.:900353   1st Qu.: 18.00  
##  Median :0.0775   Median :0.1660   Median :900660   Median : 24.00  
##  Mean   :0.1723   Mean   :0.3679   Mean   :900746   Mean   : 30.23  
##  3rd Qu.:0.1751   3rd Qu.:0.3712   3rd Qu.:901033   3rd Qu.: 38.60  
##  Max.   :1.9888   Max.   :4.0194   Max.   :902283   Max.   :251.00  
##  NA's   :666      NA's   :672      NA's   :14       NA's   :775     
##  Comments.on.data.processing.methods.y Publication.year  BiblioID.y       
##  Length:1188                           Min.   :1979     Length:1188       
##  Class :character                      1st Qu.:1997     Class :character  
##  Mode  :character                      Median :2006     Mode  :character  
##                                        Mean   :2003                       
##                                        3rd Qu.:2009                       
##                                        Max.   :2012                       
##                                        NA's   :14                         
##    Compiler             EDIBLE       ENERC_kJ._original
##  Length:1188        Min.   :0.2126   Min.   : 294.3    
##  Class :character   1st Qu.:0.4082   1st Qu.: 402.1    
##  Mode  :character   Median :0.5954   Median : 512.0    
##                     Mean   :0.6149   Mean   : 550.9    
##                     3rd Qu.:0.8700   3rd Qu.: 581.0    
##                     Max.   :0.8700   Max.   :1334.0    
##                     NA's   :1064     NA's   :1130      
##  ENERC_kcal._original   ENERA_kcal         DM_g         WATER_g         
##  Min.   : 34.00       Min.   : 95.1   Min.   : 5.10   Length:1188       
##  1st Qu.: 76.00       1st Qu.:105.9   1st Qu.:20.09   Class :character  
##  Median : 85.75       Median :114.2   Median :23.52   Mode  :character  
##  Mean   : 92.05       Mean   :115.7   Mean   :23.65                     
##  3rd Qu.: 99.45       3rd Qu.:123.2   3rd Qu.:25.90                     
##  Max.   :242.00       Max.   :145.3   Max.   :58.70                     
##  NA's   :1146         NA's   :1173    NA's   :1087                      
##        XN            NT_g        PROTCNT_g         PROTCNP_g     
##  Min.   :6.25   Min.   :2.700   Length:1188        Mode:logical  
##  1st Qu.:6.25   1st Qu.:2.900   Class :character   NA's:1188     
##  Median :6.25   Median :3.200   Mode  :character                 
##  Mean   :6.25   Mean   :3.275                                    
##  3rd Qu.:6.25   3rd Qu.:3.500                                    
##  Max.   :6.25   Max.   :4.090                                    
##  NA's   :824    NA's   :1165                                     
##      PROT_g          NPRO_g          NNP_mg          FAT_g       
##  Min.   :10.20   Min.   :2.300   Min.   :174.0   Min.   : 0.100  
##  1st Qu.:14.46   1st Qu.:2.700   1st Qu.:265.2   1st Qu.: 0.910  
##  Median :18.12   Median :2.800   Median :365.0   Median : 1.585  
##  Mean   :17.35   Mean   :2.807   Mean   :382.6   Mean   : 3.413  
##  3rd Qu.:19.80   3rd Qu.:2.900   3rd Qu.:492.0   3rd Qu.: 3.792  
##  Max.   :27.76   Max.   :3.300   Max.   :780.0   Max.   :26.500  
##  NA's   :1132    NA's   :1173    NA's   :1160    NA's   :568     
##    FATCE_g             FAT_g.1          FASAT_g           FAMS_g       
##  Length:1188        Min.   : 0.100   Min.   :0.0100   Min.   : 0.0000  
##  Class :character   1st Qu.: 0.700   1st Qu.:0.1600   1st Qu.: 0.0950  
##  Mode  :character   Median : 1.100   Median :0.3450   Median : 0.2300  
##                     Mean   : 2.358   Mean   :0.8284   Mean   : 0.7724  
##                     3rd Qu.: 3.000   3rd Qu.:0.9400   3rd Qu.: 0.8200  
##                     Max.   :21.600   Max.   :9.2400   Max.   :11.2900  
##                     NA's   :1002     NA's   :718      NA's   :729      
##      FAPU_g          FAUN_g              CA_mg             FE_mg        
##  Min.   :0.0100   Length:1188        Min.   :   2.99   Min.   :  0.010  
##  1st Qu.:0.1800   Class :character   1st Qu.:  13.47   1st Qu.:  0.200  
##  Median :0.3600   Mode  :character   Median :  53.50   Median :  0.600  
##  Mean   :0.7209                      Mean   : 173.24   Mean   :  3.063  
##  3rd Qu.:0.7700                      3rd Qu.:  88.85   3rd Qu.:  1.800  
##  Max.   :6.0100                      Max.   :1502.82   Max.   :102.000  
##  NA's   :758                         NA's   :986       NA's   :987      
##      ID_mcg            K_mg           MG_mg           MN_mg          
##  Min.   :  0.50   Min.   : 22.0   Min.   :  0.05   Length:1188       
##  1st Qu.:  4.05   1st Qu.:223.6   1st Qu.: 22.45   Class :character  
##  Median : 10.65   Median :362.1   Median : 31.65   Mode  :character  
##  Mean   : 26.68   Mean   :329.8   Mean   : 36.28                     
##  3rd Qu.: 32.77   3rd Qu.:424.3   3rd Qu.: 49.25                     
##  Max.   :380.00   Max.   :570.0   Max.   :178.70                     
##  NA's   :1108     NA's   :1028    NA's   :1044                       
##     SE_mcg             HG_mcg             PB_mcg         
##  Length:1188        Length:1188        Length:1188       
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##                                                          
##     SR_mcg           RETOL_mcg          RETOL13_mcg    
##  Length:1188        Length:1188        Min.   :  0.00  
##  Class :character   Class :character   1st Qu.:  2.00  
##  Mode  :character   Mode  :character   Median :  5.00  
##                                        Mean   : 16.11  
##                                        3rd Qu.: 14.50  
##                                        Max.   :101.00  
##                                        NA's   :1142    
##   RETOLDH_mcg         RETOLSUM_mcg       CARTA_mcg      CARTB_mcg        
##  Min.   :     3.00   Min.   :    200   Min.   :0.210   Length:1188       
##  1st Qu.:    82.75   1st Qu.:   7525   1st Qu.:0.420   Class :character  
##  Median :   294.00   Median :  33500   Median :1.280   Mode  :character  
##  Mean   : 14405.03   Mean   : 126950   Mean   :2.604                     
##  3rd Qu.:  5268.25   3rd Qu.:  63150   3rd Qu.:4.960                     
##  Max.   :196800.00   Max.   :1269000   Max.   :6.150                     
##  NA's   :1128        NA's   :1174      NA's   :1183                      
##     ATX_mcg         ZEA_mcg        CARTOID_mcg     CHOCAL_mcg     
##  Min.   :296.4   Min.   :  5.55   Min.   :1040   Min.   : 0.0800  
##  1st Qu.:330.7   1st Qu.: 14.65   1st Qu.:1275   1st Qu.: 0.8425  
##  Median :364.2   Median : 19.24   Median :1590   Median : 2.6000  
##  Mean   :425.0   Mean   : 64.08   Mean   :1492   Mean   : 6.9614  
##  3rd Qu.:458.5   3rd Qu.: 68.67   3rd Qu.:1650   3rd Qu.: 8.4000  
##  Max.   :675.2   Max.   :212.28   Max.   :1810   Max.   :47.7000  
##  NA's   :1184    NA's   :1184     NA's   :1176   NA's   :1094     
##    TOCPHA_mg        VITB6A_mg        VITB12_mcg       VITC_mg         
##  Min.   :0.2200   Min.   :0.1600   Min.   : 0.730   Length:1188       
##  1st Qu.:0.5775   1st Qu.:0.2000   1st Qu.: 2.100   Class :character  
##  Median :0.8050   Median :0.2050   Median : 3.250   Mode  :character  
##  Mean   :0.7792   Mean   :0.2233   Mean   : 7.354                     
##  3rd Qu.:0.9400   3rd Qu.:0.2475   3rd Qu.: 5.862                     
##  Max.   :1.7300   Max.   :0.3100   Max.   :49.400                     
##  NA's   :1162     NA's   :1182     NA's   :1166                       
##      ZN_mg        
##  Min.   : 0.0158  
##  1st Qu.: 0.4150  
##  Median : 0.6650  
##  Mean   : 1.2491  
##  3rd Qu.: 1.3971  
##  Max.   :12.0000  
##  NA's   :992
```

```r
nut$ASFIS.Scientific.name
```

```
##    [1] "Abramis brama"                "Abramis brama"               
##    [3] "Abramis brama"                "Abramis brama"               
##    [5] "Abramis brama"                "Abramis brama"               
##    [7] "Abramis brama"                "Abramis brama"               
##    [9] "Abramis spp"                  "Ageneiosus brevifilis"       
##   [11] "Albatrossia pectoralis"       "Alectis alexandrinus"        
##   [13] "Alepocephalus bairdii"        "Alepocephalus spp"           
##   [15] "Alosa alosa"                  "Alosa sapidissima"           
##   [17] "Aluterus monoceros"           "Ambassidae"                  
##   [19] "Ambassidae"                   "Ambassidae"                  
##   [21] "Ambassidae"                   "Ambassidae"                  
##   [23] "Ambassidae"                   "Amblypharyngodon mola"       
##   [25] "Amblypharyngodon mola"        "Amblypharyngodon mola"       
##   [27] "Amblypharyngodon mola"        "Amblypharyngodon mola"       
##   [29] "Amblypharyngodon mola"        "Amblypharyngodon mola"       
##   [31] "Amblypharyngodon mola"        "Anabas testudineus"          
##   [33] "Anabas testudineus"           "Anabas testudineus"          
##   [35] "Anabas testudineus"           "Anabas testudineus"          
##   [37] "Anadara granosa"              "Anarhichas lupus"            
##   [39] "Anarhichas lupus"             "Anarhichas lupus"            
##   [41] "Anarhichas lupus"             "Anchoa hepsetus"             
##   [43] "Anguilla anguilla"            "Anguilla bicolor"            
##   [45] "Anguilla japonica"            "Anguilla rostrata"           
##   [47] "Anoplopoma fimbria"           "Aphanopus carbo"             
##   [49] "Aphanopus carbo"              "Aphanopus carbo"             
##   [51] "Arctica islandica"            "Ariomma bondi"               
##   [53] "Ariomma bondi"                "Aristaeomorpha foliacea"     
##   [55] "Aristaeomorpha foliacea"      "Aristaeomorpha foliacea"     
##   [57] "Aristaeomorpha foliacea"      "Aristaeomorpha foliacea"     
##   [59] "Aristaeomorpha foliacea"      "Aristaeomorpha foliacea"     
##   [61] "Aristaeomorpha foliacea"      "Aristaeomorpha foliacea"     
##   [63] "Aristaeomorpha foliacea"      "Aristeus antennatus"         
##   [65] "Aristeus antennatus"          "Aristeus antennatus"         
##   [67] "Aristeus antennatus"          "Aristeus antennatus"         
##   [69] "Aristeus antennatus"          "Arius spp"                   
##   [71] "Arius spp"                    "Arius spp"                   
##   [73] "Arius spp"                    "Arius spp"                   
##   [75] "Arnoglossus laterna"          "Arripis trutta"              
##   [77] "Aspitrigla cuculus"           "Aspius aspius"               
##   [79] "Aspius aspius"                "Atheresthes stomias"         
##   [81] "Atherina boyeri"              "Atherinidae"                 
##   [83] "Atherinidae"                  "Atherinidae"                 
##   [85] "Atrobucca nibe"               "Auxis rochei"                
##   [87] "Auxis rochei"                 "Auxis thazard"               
##   [89] "Auxis thazard"                "Auxis thazard"               
##   [91] "Bagrus docmak"                "Bagrus docmak"               
##   [93] "Bagrus docmak"                "Balistidae"                  
##   [95] "Balistidae"                   "Barbonymus goniontus"        
##   [97] "Belone belone"                "Bivalvia"                    
##   [99] "Boops boops"                  "Boops boops"                 
##  [101] "Boops boops"                  "Boops boops"                 
##  [103] "Boops boops"                  "Bothus podus"                
##  [105] "Brama japonica"               "Branchiostegidae"            
##  [107] "Brevoortia aurea"             "Brevoortia spp"              
##  [109] "Brosme brosme"                "Brosme brosme"               
##  [111] "Brycon orbignyanus"           "Callinectes sapidus"         
##  [113] "Callinectes sapidus"          "Caranx crysos"               
##  [115] "Caranx crysos"                "Carassius carassius"         
##  [117] "Carassius carassius"          "Carassius carassius"         
##  [119] "Carcharhinus limbatus"        "Carcinus maenas"             
##  [121] "Catostomus commersoni"        "Centrophorus squamosus"      
##  [123] "Centroscyllium fabricii"      "Centroscymnus coelolepis"    
##  [125] "Cephalopholis taeniops"       "Cephalopholis taeniops"      
##  [127] "Cephalopholis taeniops"       "Cephalopholis taeniops"      
##  [129] "Cephalopholis taeniops"       "Cephalopholis taeniops"      
##  [131] "Cephalopholis taeniops"       "Chaenocephalus aceratus"     
##  [133] "Chaenocephalus aceratus"      "Chamelea gallina"            
##  [135] "Chamelea gallina"             "Chamelea gallina"            
##  [137] "Chamelea gallina"             "Chamelea gallina"            
##  [139] "Chamelea gallina"             "Chamelea gallina"            
##  [141] "Chamelea gallina"             "Chamelea gallina"            
##  [143] "Champsocephalus gunnari"      "Chanda nama"                 
##  [145] "Channa marulius"              "Channa micropeltes"          
##  [147] "Channa punctata"              "Channa punctata"             
##  [149] "Channa punctata"              "Channa punctata"             
##  [151] "Channa striata"               "Channa striata"              
##  [153] "Channa striata"               "Channa striata"              
##  [155] "Channa striata"               "Characidae"                  
##  [157] "Chela cachius"                "Chelidonichthys lucerna"     
##  [159] "Chelidonichthyus lucernus"    "Chelidonichthyus lucernus"   
##  [161] "Chelidonichthyus lucernus"    "Chelon labrosus"             
##  [163] "Chionoecetes opilio"          "Chionoecetes spp"            
##  [165] "Chionoecetes spp"             "Chionoecetes spp"            
##  [167] "Chionoecetes spp"             "Chirocentrus dorab"          
##  [169] "Chondrostoma nasus"           "Chondrostoma nasus"          
##  [171] "Chondrostoma nasus"           "Cipangopaludina chinensis"   
##  [173] "Clarias batrachus"            "Clarias batrachus"           
##  [175] "Clarias batrachus"            "Clarias gariepinus"          
##  [177] "Clarias gariepinus"           "Clarias gariepinus"          
##  [179] "Clarias gariepinus"           "Clarias gariepinus"          
##  [181] "Clarias gariepinus"           "Clarias gariepinus"          
##  [183] "Clarias gariepinus"           "Clarias gariepinus"          
##  [185] "Clarias gariepinus"           "Clarias gariepinus"          
##  [187] "Clarias gariepinus"           "Clarias macrocephalus"       
##  [189] "Clarias macrocephalus"        "Clarias macrocephalus"       
##  [191] "Clarias macrocephalus"        "Clupea harengus"             
##  [193] "Clupea harengus"              "Clupea harengus"             
##  [195] "Clupea harengus"              "Clupea harengus"             
##  [197] "Clupea harengus"              "Clupea harengus"             
##  [199] "Clupea harengus"              "Clupea pallasi"              
##  [201] "Clupeoides borneensis"        "Clupeonella cultriventris"   
##  [203] "Clupeonella cultriventris"    "Colisa fasciatus"            
##  [205] "Colisa lalius"                "Coregonus albula"            
##  [207] "Coregonus albula"             "Coregonus albula"            
##  [209] "Coregonus albula"             "Coregonus albula"            
##  [211] "Coregonus albula"             "Coregonus albula"            
##  [213] "Coregonus albula"             "Coregonus albula"            
##  [215] "Coregonus clupeaformis"       "Coregonus clupeaformis"      
##  [217] "Coregonus clupeaformis"       "Coregonus lavaretus"         
##  [219] "Coregonus lavaretus"          "Coregonus lavaretus"         
##  [221] "Coregonus macrophthalmus"     "Coregonus macrophthalmus"    
##  [223] "Coregonus spp"                "Coregonus spp"               
##  [225] "Coregonus spp"                "Coregonus spp"               
##  [227] "Coregonus spp"                "Coregonus spp"               
##  [229] "Coregonus spp"                "Corica soborna"              
##  [231] "Corica soborna"               "Corica soborna"              
##  [233] "Crangon crangon"              "Crassostrea gigas"           
##  [235] "Crassostrea rhizophorae"      "Crassostrea rhizophorae"     
##  [237] "Crassostrea rhizophorae"      "Crassostrea rhizophorae"     
##  [239] "Crassostrea rhizophorae"      "Crassostrea virginica"       
##  [241] "Crassostrea virginica"        "Cyclocheilichthys apogon"    
##  [243] "Cyclopterus lumpus"           "Cyclopterus lumpus"          
##  [245] "Cyclopterus lumpus"           "Cyclopterus lumpus"          
##  [247] "Cynoglossus arel"             "Cynoscion arenarius"         
##  [249] "Cynoscion jamaicensis"        "Cynoscion nothus"            
##  [251] "Cyprinidae"                   "Cyprinidae"                  
##  [253] "Cyprinidae"                   "Cyprinidae"                  
##  [255] "Cyprinidae"                   "Cyprinidae"                  
##  [257] "Cyprinidae"                   "Cyprinidae"                  
##  [259] "Cyprinidae"                   "Cyprinidae"                  
##  [261] "Cyprinidae"                   "Cyprinidae"                  
##  [263] "Cyprinidae"                   "Cyprinidae"                  
##  [265] "Cyprinus carpio"              "Cyprinus carpio"             
##  [267] "Cyprinus carpio"              "Cyprinus carpio"             
##  [269] "Cyprinus carpio"              "Cyprinus carpio"             
##  [271] "Cyprinus carpio"              "Danio dangila"               
##  [273] "Dasyatis americana"           "Dasyatis americana"          
##  [275] "Dasyatis sabina"              "Dasyatis sabina"             
##  [277] "Deania calceus"               "Decapterus punctatus"        
##  [279] "Decapterus punctatus"         "Dentex dentex"               
##  [281] "Dentex maroccanus"            "Dentex spp"                  
##  [283] "Dentex spp"                   "Dicentrarchus labrax"        
##  [285] "Dicentrarchus labrax"         "Dicentrarchus labrax"        
##  [287] "Dicentrarchus labrax"         "Dicentrarchus labrax"        
##  [289] "Dicentrarchus labrax"         "Diplodus annularis"          
##  [291] "Diplodus puntazzo"            "Diplodus puntazzo"           
##  [293] "Diplodus puntazzo"            "Diplodus sargus"             
##  [295] "Diplodus sargus"              "Diplodus sargus"             
##  [297] "Diplodus sargus"              "Diplodus sargus"             
##  [299] "Diplodus sargus"              "Diplodus sargus"             
##  [301] "Diplodus sargus"              "Diplodus vulgaris"           
##  [303] "Diplodus vulgaris"            "Echelus myrus"               
##  [305] "Eledone moschata"             "Eledone moschata"            
##  [307] "Eledone moschata"             "Eleutheronema tetradactylum" 
##  [309] "Elops saurus"                 "Engraulis encrasicolus"      
##  [311] "Engraulis encrasicolus"       "Engraulis encrasicolus"      
##  [313] "Engraulis encrasicolus"       "Engraulis encrasicolus"      
##  [315] "Engraulis encrasicolus"       "Engraulis encrasicolus"      
##  [317] "Epinephelus aeneus"           "Epinephelus aeneus"          
##  [319] "Epinephelus aeneus"           "Epinephelus aeneus"          
##  [321] "Epinephelus aeneus"           "Epinephelus aeneus"          
##  [323] "Epinephelus coioides"         "Epinephelus sexfasciatus"    
##  [325] "Epinephelus sexfasciatus"     "Eriocheir sinensis"          
##  [327] "Eriocheir sinensis"           "Esomus danricus"             
##  [329] "Esomus danricus"              "Esomus danricus"             
##  [331] "Esomus danricus"              "Esox lucius"                 
##  [333] "Esox lucius"                  "Esox lucius"                 
##  [335] "Esox lucius"                  "Esox lucius"                 
##  [337] "Esox lucius"                  "Esox lucius"                 
##  [339] "Esox lucius"                  "Esox lucius"                 
##  [341] "Esox lucius"                  "Esox lucius"                 
##  [343] "Esox lucius"                  "Etmopterus princeps"         
##  [345] "Etrumeus teres"               "Etrumeus teres"              
##  [347] "Etrumeus teres"               "Etrumeus teres"              
##  [349] "Etrumeus teres"               "Euphausia spp"               
##  [351] "Euphausia spp"                "Euphausia superba"           
##  [353] "Euthynnus alletteratus"       "Euthynnus alletteratus"      
##  [355] "Euthynnus alletteratus"       "Euthynnus alletteratus"      
##  [357] "Euthynnus alletteratus"       "Euthynnus alletteratus"      
##  [359] "Euthynnus alletteratus"       "Euthynnus alletteratus"      
##  [361] "Eutrigla gurnardus"           "Eutrigla gurnardus"          
##  [363] "Eutrigla gurnardus"           "Eutrigla gurnardus"          
##  [365] "Ficus subintermedia"          "Ficus subintermedia"         
##  [367] "Ficus subintermedia"          "Gadus macrocephalus"         
##  [369] "Gadus macrocephalus"          "Gadus macrocephalus"         
##  [371] "Gadus macrocephalus"          "Gadus macrocephalus"         
##  [373] "Gadus morhua"                 "Gadus morhua"                
##  [375] "Gadus morhua"                 "Gadus morhua"                
##  [377] "Gadus morhua"                 "Gadus morhua"                
##  [379] "Gadus morhua"                 "Gadus morhua"                
##  [381] "Gadus morhua"                 "Gadus morhua"                
##  [383] "Gadus morhua"                 "Gadus morhua"                
##  [385] "Gadus morhua"                 "Gasterochisma melampus"      
##  [387] "Gastropoda"                   "Gastropoda"                  
##  [389] "Gudusia chapra"               "Gymnura spp"                 
##  [391] "Helicolenus dacthlopterus"    "Helostoma temminckii"        
##  [393] "Hemiramphidae"                "Hemiramphus spp"             
##  [395] "Hemiramphus spp"              "Hemisorubim platyrhynchos"   
##  [397] "Henicorhynchus siamensis"     "Heteropneustes fossilis"     
##  [399] "Heteropneustes fossilis"      "Hippoglossus hippoglossus "  
##  [401] "Hippoglossus hippoglossus "   "Homarus gammarus"            
##  [403] "Huso huso"                    "Hydrolagus affinis"          
##  [405] "Hyperoglyphe antarctica"      "Hypomesus pretious"          
##  [407] "Hyporhamphus ihi"             "Ictalurus punctatus"         
##  [409] "Ictalurus punctatus"          "Isopsetta isolepis"          
##  [411] "Isurus oxyrinchus"            "Katsuwonus pelamis"          
##  [413] "Katsuwonus pelamis"           "Katsuwonus pelamis"          
##  [415] "Lagocepahlus lagocephalus"    "Lamna nasus"                 
##  [417] "Lampris guttatus"             "Lates angustifrons"          
##  [419] "Lates angustifrons"           "Lates angustifrons"          
##  [421] "Lates angustifrons"           "Lates angustifrons"          
##  [423] "Lates angustifrons"           "Lates angustifrons"          
##  [425] "Lates angustifrons"           "Lates angustifrons"          
##  [427] "Lates angustifrons"           "Lates angustifrons"          
##  [429] "Lates angustifrons"           "Lates calcarifer"            
##  [431] "Lates calcarifer"             "Lates calcarifer"            
##  [433] "Lates niloticus"              "Lates niloticus"             
##  [435] "Lates niloticus"              "Lates niloticus"             
##  [437] "Lepidopsetta bilineata"       "Lepidorhombus whiffiagonis"  
##  [439] "Lepidorhombus whiffiagonis"   "Leptomelanosoma indicum"     
##  [441] "Leuciscus cephalus"           "Leuciscus cephalus"          
##  [443] "Leuciscus cephalus"           "Leuciscus idus"              
##  [445] "Leuciscus idus"               "Leuciscus idus"              
##  [447] "Limanda aspera"               "Limnothrissa miodon"         
##  [449] "Limnothrissa miodon"          "Lithognathus mormyrus"       
##  [451] "Lithognathus mormyrus"        "Lithognathus mormyrus"       
##  [453] "Liza aurata"                  "Liza aurata"                 
##  [455] "Liza aurata"                  "Liza ramada"                 
##  [457] "Liza ramada"                  "Liza ramada"                 
##  [459] "Liza ramada"                  "Liza saliens"                
##  [461] "Liza spp"                     "Loligo pealeii"              
##  [463] "Loligo plei"                  "Loligo vulgaris"             
##  [465] "Loligo vulgaris"              "Loligo vulgaris"             
##  [467] "Loligo vulgaris"              "Loligo vulgaris"             
##  [469] "Loligo vulgaris"              "Lophius budegassa"           
##  [471] "Lophius piscatorius"          "Lophius piscatorius"         
##  [473] "Lota lota"                    "Lota lota"                   
##  [475] "Lutjanus argentimaculatus"    "Lutjanus johnii"             
##  [477] "Lutjanus malabaricus"         "Macrobrachium nipponense"    
##  [479] "Macrobrachium spp."           "Macrodon ancylodon"          
##  [481] "Macrognathus aculeatus"       "Macrognathus aculeatus"      
##  [483] "Macrognathus aculeatus"       "Macrourus berglax"           
##  [485] "Mactra violacea"              "Mactra violacea"             
##  [487] "Mactra violacea"              "Mactra violacea"             
##  [489] "Mactra violacea"              "Mactra violacea"             
##  [491] "Mactra violacea"              "Mactra violacea"             
##  [493] "Mactra violacea"              "Mactra violacea"             
##  [495] "Mallotus villosus"            "Megalaspis cordyla"          
##  [497] "Melanogrammus aeglefinus"     "Melanogrammus aeglefinus"    
##  [499] "Melanogrammus aeglefinus"     "Melanogrammus aeglefinus"    
##  [501] "Melanogrammus aeglefinus"     "Melanogrammus aeglefinus"    
##  [503] "Melanogrammus aeglefinus"     "Menticirrhus americanus"     
##  [505] "Merlangius merlangus"         "Merlangius merlangus"        
##  [507] "Merlangius merlangus"         "Merlangius merlangus"        
##  [509] "Merluccius hubbsi"            "Merluccius hubbsi"           
##  [511] "Merluccius hubbsi"            "Merluccius hubbsi"           
##  [513] "Merluccius hubbsi"            "Merluccius merluccius"       
##  [515] "Merluccius merluccius"        "Merluccius merluccius"       
##  [517] "Merluccius merluccius"        "Merluccius merluccius"       
##  [519] "Merluccius merluccius"        "Merluccius merluccius"       
##  [521] "Merluccius merluccius"        "Merluccius productus"        
##  [523] "Merluccius productus"         "Merluccius productus"        
##  [525] "Merluccius productus"         "Metapenaeus affinis"         
##  [527] "Metapenaeus dobsoni"          "Metapenaeus monoceros"       
##  [529] "Metapenaeus monoceros"        "Metapenaeus monoceros"       
##  [531] "Metapenaeus monoceros"        "Metapenaeus monoceros"       
##  [533] "Metapenaeus monoceros"        "Metapenaeus monoceros"       
##  [535] "Metapenaeus monoceros"        "Micorpogonias furnieri"      
##  [537] "Micromesistius poutassou"     "Micromesistius poutassou"    
##  [539] "Microstomus kitt"             "Microstomus kitt"            
##  [541] "Microstomus kitt"             "Microstomus kitt"            
##  [543] "Microstomus pacificus"        "Microstomus pacificus"       
##  [545] "Microstomus pacificus"        "Microstomus pacificus"       
##  [547] "Microstomus pacificus"        "Molva dypterygia"            
##  [549] "Molva dypterygia"             "Molva molva"                 
##  [551] "Molva molva"                  "Monopterus albus"            
##  [553] "Monopterus albus"             "Mora moro"                   
##  [555] "Mormyrus rume"                "Morone saxatilis"            
##  [557] "Morone saxatilis"             "Morone saxatilis"            
##  [559] "Morone saxatilis"             "Morone saxatilis"            
##  [561] "Morone saxatilis"             "Morone saxatilis"            
##  [563] "Moroteuthis ingens"           "Moroteuthis ingens"          
##  [565] "Mugil cephalus"               "Mugil cephalus"              
##  [567] "Mugil cephalus"               "Mugil cephalus"              
##  [569] "Mugil cephalus"               "Mugil cephalus"              
##  [571] "Mugil liza"                   "Mullus barbatus"             
##  [573] "Mullus barbatus"              "Mullus barbatus"             
##  [575] "Mullus barbatus"              "Mullus barbatus"             
##  [577] "Mullus barbatus"              "Mullus barbatus"             
##  [579] "Mullus barbatus"              "Mullus barbatus"             
##  [581] "Mullus surmuletus"            "Mustelus lenticulatus"       
##  [583] "Mycteroperca acutirostris"    "Mycteroperca rubra"          
##  [585] "Mystus spp"                   "Mystus vittatus"             
##  [587] "Mytilus edulis"               "Mytilus galloprovincialis"   
##  [589] "Mytilus galloprovincialis"    "Mytilus galloprovincialis"   
##  [591] "Mytilus galloprovincialis"    "Mytilus galloprovincialis"   
##  [593] "Mytilus spp"                  "Mastacembelus armatus"       
##  [595] "Mastacembelus pancalus"       "Lepidocephalus guntea"       
##  [597] "Velesunio ambiguus"           "Nandus nandus"               
##  [599] "Nemipterus japonicus"         "Nephrops norvegicus"         
##  [601] "Nephrops norvegicus"          "Nephrops norvegicus"         
##  [603] "Nephrops norvegicus"          "Nephrops norvegicus"         
##  [605] "Nephrops norvegicus"          "Nephrops norvegicus"         
##  [607] "Nephrops norvegicus"          "Nephrops norvegicus"         
##  [609] "Nephrops norvegicus"          "Nephrops norvegicus"         
##  [611] "Nephrops norvegicus"          "Nephrops norvegicus"         
##  [613] "Notopterus notopterus"        "Notopterus spp"              
##  [615] "Notothenia gibberifrons"      "Notothenia neglecta"         
##  [617] "Notothenia neglecta"          "Oblada melanura"             
##  [619] "Octopus vulgaris"             "Octopus vulgaris"            
##  [621] "Octopus vulgaris"             "Octopus vulgaris"            
##  [623] "Octopus vulgaris"             "Octopus vulgaris"            
##  [625] "Octopus vulgaris"             "Odax pullus"                 
##  [627] "Oncorhynchus gorbusha"        "Oncorhynchus gorbusha"       
##  [629] "Oncorhynchus kisutch"         "Oncorhynchus kisutch"        
##  [631] "Oncorhynchus kisutch"         "Oncorhynchus kisutch"        
##  [633] "Oncorhynchus kisutch"         "Oncorhynchus mykiss"         
##  [635] "Oncorhynchus mykiss"          "Oncorhynchus mykiss"         
##  [637] "Oncorhynchus tshawytscha"     "Oncorhynchus tshawytscha"    
##  [639] "Oncorhynchus\x86keta"         "Oncorhynchus\x86mykiss"      
##  [641] "Oncorhynchus\x86nerka\x86"    "Oncorhynchus\x86tshawytscha" 
##  [643] "Ophiodon elongatus"           "Opisthonema oglinum"         
##  [645] "Orconectes limosus"           "Orconectes limosus"          
##  [647] "Oreochromis _=Tilapia spp"    "Oreochromis _=Tilapia spp"   
##  [649] "Oreochromis macrochir"        "Oreochromis macrochir"       
##  [651] "Oreochromis macrochir"        "Oreochromis niloticus"       
##  [653] "Oreochromis niloticus"        "Oreochromis niloticus"       
##  [655] "Oreochromis niloticus"        "Oreochromis niloticus"       
##  [657] "Oreochromis niloticus"        "Oreochromis niloticus"       
##  [659] "Oreochromis niloticus"        "Oreochromis niloticus"       
##  [661] "Oreochromis niloticus"        "Oreochromis niloticus"       
##  [663] "Oreochromis niloticus"        "Oreochromis niloticus"       
##  [665] "Oreochromis niloticus"        "Oreochromis niloticus"       
##  [667] "Oreochromis niloticus"        "Oreochromis niloticus"       
##  [669] "Oreochromis niloticus"        "Oreochromis niloticus"       
##  [671] "Oreochromis niloticus"        "Oreochromis niloticus"       
##  [673] "Oreochromis niloticus"        "Osteochilus hasselti"        
##  [675] "Ostrea edulis"                "Pagelius erythrinus"         
##  [677] "Pagellus bogaraveo"           "Pagellus bogaraveo"          
##  [679] "Pagellus bogaraveo"           "Pagellus bogaraveo"          
##  [681] "Pagellus erythrinus"          "Pagrus auratus"              
##  [683] "Pagrus caeruleocinctus"       "Pagrus caeruleocinctus"      
##  [685] "Pagrus pagrus"                "Pagrus pagrus"               
##  [687] "Palaemon serratus"            "Palaemonidae"                
##  [689] "Palinurus elephas"            "Palinurus elephas"           
##  [691] "Palinurus elephas"            "Palinurus elephas"           
##  [693] "Pampus argenteus"             "Pampus argenteus"            
##  [695] "Pampus spp"                   "Panaeus brasiliensis"        
##  [697] "Panaeus brasiliensis"         "Panaeus brasiliensis"        
##  [699] "Pandalus borealis"            "Pandalus jordani"            
##  [701] "Pandalus jordani"             "Pandalus jordani"            
##  [703] "Pangasius hypophthalamus"     "Panopea abrupta"             
##  [705] "Paralithodes camtschaticus"   "Paralithodes platypus"       
##  [707] "Parambassis\x86wolffii"       "Parapenaeopsis atlantica"    
##  [709] "Parapenaeopsis stylifera"     "Parapenaeus longirostris"    
##  [711] "Parapenaeus longirostris"     "Parastromateus niger"        
##  [713] "Parastromateus niger"         "Parona signata"              
##  [715] "Patagonotothen ramsayi"       "Patagonotothen ramsayi"      
##  [717] "Patagonotothen ramsayi"       "Patagonotothen ramsayi"      
##  [719] "Patagonotothen ramsayi"       "Penaeus indicus"             
##  [721] "Penaeus kerathurus"           "Penaeus kerathurus"          
##  [723] "Penaeus monodon"              "Penaeus notialis"            
##  [725] "Penaeus schmitti"             "Penaeus schmitti"            
##  [727] "Penaeus semisulcatus"         "Penaeus semisulcatus"        
##  [729] "Penaeus semisulcatus"         "Penaeus semisulcatus"        
##  [731] "Penaeus semisulcatus"         "Penaeus semisulcatus"        
##  [733] "Penaeus semisulcatus"         "Penaeus semisulcatus"        
##  [735] "Perca flavescens"             "Perca flavescens"            
##  [737] "Perca fluviatilis"            "Perca fluviatilis"           
##  [739] "Perca fluviatilis"            "Perca fluviatilis"           
##  [741] "Perca fluviatilis"            "Perca fluviatilis"           
##  [743] "Perca fluviatilis"            "Perca fluviatilis"           
##  [745] "Perca fluviatilis"            "Perca fluviatilis"           
##  [747] "Perca fluviatilis"            "Perca fluviatilis"           
##  [749] "Perca fluviatilis"            "Perca fluviatilis"           
##  [751] "Perca fluviatilis"            "Perca fluviatilis"           
##  [753] "Perca Fluviatilis"            "Perca Fluviatilis"           
##  [755] "Perca Fluviatilis"            "Perca Fluviatilis"           
##  [757] "Perca Fluviatilis"            "Perca Fluviatilis"           
##  [759] "Perca Fluviatilis"            "Perca Fluviatilis"           
##  [761] "Perca Fluviatilis"            "Perca Fluviatilis"           
##  [763] "Perna viridis"                "Perna viridis"               
##  [765] "Perna viridis"                "Perna viridis"               
##  [767] "Phycis blennoides"            "Phycis blennoides"           
##  [769] "Phycis phycis"                "Piaractus mesopotamicus"     
##  [771] "Pimelodus argenteus"          "Pimelodus maculatus"         
##  [773] "Pinctada radiata"             "Pinirampus\x86pirinampu\x86" 
##  [775] "Platichthys flesus"           "Platichthys flesus"          
##  [777] "Platichthys stellatus"        "Platichthys\x86stellatus"    
##  [779] "Pleurogrammus monopterygius"  "Pleuronectes platessa"       
##  [781] "Pleuronectes platessa"        "Pleuronectes platessa"       
##  [783] "Pleuronectes platessa"        "Pleuronectes vetulus"        
##  [785] "Plotosus canius"              "Pogonias cromis"             
##  [787] "Pollachius pollachius"        "Pollachius pollachius"       
##  [789] "Pollachius virens"            "Pollachius virens"           
##  [791] "Pollachius virens"            "Pollachius virens"           
##  [793] "Polyprion oxygeneios"         "Pomatomus saltatrix"         
##  [795] "Pomatomus saltatrix"          "Pomoxis nigromaculatus"      
##  [797] "Portunus pelagicus"           "Portunus pelagicus"          
##  [799] "Portunus pelagicus"           "Portunus pelagicus"          
##  [801] "Portunus pelagicus"           "Portunus pelagicus"          
##  [803] "Procambarus acutus"           "Procambarus acutus"          
##  [805] "Procambarus clarkii"          "Procambarus clarkii"         
##  [807] "Prochilodus reticulatus"      "Protopterus aethiopicus"     
##  [809] "Protopterus aethiopicus"      "Protopterus aethiopicus"     
##  [811] "Protopterus spp"              "Psetta maxima"               
##  [813] "Psetta maxima"                "Psetta maxima"               
##  [815] "Psetta maxima"                "Pseudoplatystoma coruscans"  
##  [817] "Pseudoplatystoma fasciatum"   "Pseudoplatystoma fasciatum"  
##  [819] "Pseudoplatystoma fasciatum"   "Puntioplites proctozystron"  
##  [821] "Puntius sophore"              "Puntius sophore"             
##  [823] "Puntius sophore"              "Puntius spp"                 
##  [825] "Puntius spp"                  "Puntius ticto"               
##  [827] "Rapana spp"                   "Rapana spp"                  
##  [829] "Rasbora tornieri"             "Rasbora tornieri"            
##  [831] "Rasbora tornieri"             "Rastrelliger brachysoma"     
##  [833] "Rastrelliger kanagurta"       "Rastrineobola argentea"      
##  [835] "Rastrineobola argentea"       "Rastrineobola argentea"      
##  [837] "Reinhardtius hippoglossoides" "Reinhardtius hippoglossoides"
##  [839] "Reinhardtius hippoglossoides" "Rhizoprionodon terraenovae"  
##  [841] "Rutilus frisii"               "Rutilus frisii"              
##  [843] "Rutilus frisii"               "Rutilus frisii"              
##  [845] "Rutilus rutilus"              "Rutilus rutilus"             
##  [847] "Rutilus rutilus"              "Rutilus rutilus"             
##  [849] "Rutilus rutilus"              "Rutilus rutilus"             
##  [851] "Salmo salar"                  "Salmo salar"                 
##  [853] "Salmo salar"                  "Salvelinus fontinalis"       
##  [855] "Salvelinus fontinalis"        "Salvelinus namaycush"        
##  [857] "Sander lucioperca"            "Sander lucioperca"           
##  [859] "Sander lucioperca"            "Sander lucioperca"           
##  [861] "Sander lucioperca"            "Sander lucioperca"           
##  [863] "Sander lucioperca"            "Sander lucioperca"           
##  [865] "Sander lucioperca"            "Sander lucioperca"           
##  [867] "Sander lucioperca"            "Sander lucioperca"           
##  [869] "Sander lucioperca"            "Sander vitreus"              
##  [871] "Sarda orientalis"             "Sarda orientalis"            
##  [873] "Sarda orientalis"             "Sarda sarda"                 
##  [875] "Sarda sarda"                  "Sarda sarda"                 
##  [877] "Sarda sarda"                  "Sarda sarda"                 
##  [879] "Sarda sarda"                  "Sarda sarda"                 
##  [881] "Sarda sarda"                  "Sarda sarda"                 
##  [883] "Sarda sarda"                  "Sarda sarda"                 
##  [885] "Sarda sarda"                  "Sarda sarda"                 
##  [887] "Sarda sarda"                  "Sarda sarda"                 
##  [889] "Sarda sarda"                  "Sarda sarda"                 
##  [891] "Sardina pilchardus"           "Sardina pilchardus"          
##  [893] "Sardina pilchardus"           "Sardina pilchardus"          
##  [895] "Sardina pilchardus"           "Sardina pilchardus"          
##  [897] "Sardina pilchardus"           "Sardina pilchardus"          
##  [899] "Sardina pilchardus"           "Sardina pilchardus"          
##  [901] "Sardina pilchardus"           "Sardinella aurita"           
##  [903] "Sardinella aurita"            "Sardinella aurita"           
##  [905] "Sardinella aurita"            "Sardinella aurita"           
##  [907] "Sardinella aurita"            "Sardinella aurita"           
##  [909] "Sardinella aurita"            "Sardinella aurita"           
##  [911] "Sardinella aurita"            "Sardinella aurita"           
##  [913] "Sardinella brasiliensis"      "Sardinella fimbriata"        
##  [915] "Sardinella maderensis"        "Sardinella maderensis"       
##  [917] "Sardinella spp"               "Sardinella spp"              
##  [919] "Sardinops sagax"              "Sarpa salpa"                 
##  [921] "Sarpa salpa"                  "Sarpa salpa"                 
##  [923] "Sarpa salpa"                  "Saurida undosquamis"         
##  [925] "Saxidomus gigantea"           "Scardinius erythrophthalmus" 
##  [927] "Scardinius erythrophthalmus"  "Sciaena umbra"               
##  [929] "Scomber japonicus"            "Scomber japonicus"           
##  [931] "Scomber japonicus"            "Scomber japonicus"           
##  [933] "Scomber japonicus"            "Scomber japonicus"           
##  [935] "Scomber japonicus"            "Scomber scombrus"            
##  [937] "Scomber scombrus"             "Scomber scombrus"            
##  [939] "Scomber scombrus"             "Scomber scombrus"            
##  [941] "Scomber scombrus"             "Scomber spp"                 
##  [943] "Scomberomorus commerson"      "Scomberomorus guttatus"      
##  [945] "Scomper spp"                  "Scophthalmidae"              
##  [947] "Scophthalmus rhombus"         "Scophthalmus rhombus"        
##  [949] "Scorpaena porcus "            "Scorpaena porcus "           
##  [951] "Scorpaena porcus "            "Scorpaena scrofa"            
##  [953] "Scorpaena scrofa"             "Scorpaena scrofa"            
##  [955] "Scorpaena scrofa"             "Scorpaena scrofa"            
##  [957] "Sebastes flavidus"            "Sebastes marinus"            
##  [959] "Sebastes marinus"             "Sebastes marinus"            
##  [961] "Sebastes marinus"             "Sebastes marinus"            
##  [963] "Sebastes marinus"             "Sebastes marinus"            
##  [965] "Sebastes marinus"             "Sebastes marinus"            
##  [967] "Sebastes melanops"            "Sebastes mentella"           
##  [969] "Sebastes mentella"            "Sebastes mentella"           
##  [971] "Sebastes pinniger"            "Sebastes pinniger"           
##  [973] "Sebastes ruberrimus"          "Sebastes viviparus"          
##  [975] "Sebastes viviparus"           "Selaroides leptolepis"       
##  [977] "Selene vomer"                 "Sepia officinalis"           
##  [979] "Sepia officinalis"            "Sepia officinalis"           
##  [981] "Sepia officinalis"            "Sepia officinalis"           
##  [983] "Sepia officinalis"            "Sepia officinalis"           
##  [985] "Sepia officinalis"            "Sepia officinalis"           
##  [987] "Sepia officinalis"            "Sepia officinalis"           
##  [989] "Sepia officinalis"            "Seriolina nigrofasciata"     
##  [991] "Serranus scriba"              "Serranus scriba"             
##  [993] "Serranus scriba"              "Serranus scriba"             
##  [995] "Serranus scriba"              "Serranus scriba"             
##  [997] "Siganus rivulatus"            "Siganus rivulatus"           
##  [999] "Siganus rivulatus"            "Siganus rivulatus"           
## [1001] "Siganus rivulatus"            "Siganus rivulatus"           
## [1003] "Siganus rivulatus"            "Siganus rivulatus"           
## [1005] "Sillago sihama"               "Silurus glanis"              
## [1007] "Silurus glanis"               "Silurus glanis"              
## [1009] "Silurus glanis"               "Silurus glanis"              
## [1011] "Solea lascaris"               "Solea solea"                 
## [1013] "Solea solea"                  "Solea solea"                 
## [1015] "Solea solea"                  "Soleidae"                    
## [1017] "Sorubim lima"                 "Sparisoma cretense"          
## [1019] "Sparus aurata"                "Sparus aurata"               
## [1021] "Sphyraena chrysotaenia"       "Sphyraena sphyraena"         
## [1023] "Spicara maena"                "Spicara smaris"              
## [1025] "Spicara spp"                  "Spicara spp"                 
## [1027] "Spicara spp"                  "Spicara spp"                 
## [1029] "Spondyliosoma cantharus"      "Spondyliosoma cantharus"     
## [1031] "Sprattus sprattus"            "Sprattus sprattus"           
## [1033] "Sprattus sprattus"            "Squalus acanthias"           
## [1035] "Stolothrissa tanganicae"      "Stolothrissa tanganicae"     
## [1037] "Strombus gracilior"           "Strombus gracilior"          
## [1039] "Strombus gracilior"           "Strombus gracilior"          
## [1041] "Syacium gunteri"              "Synodontidae"                
## [1043] "Synodontis spp"               "Synodontis spp"              
## [1045] "Synodontis spp"               "Tellina palatam"             
## [1047] "Tellina palatam"              "Tellina palatam"             
## [1049] "Tellina palatam"              "Tenualosa ilisha"            
## [1051] "Tenualosa ilisha"             "Tenualosa ilisha"            
## [1053] "Tenualosa ilisha"             "Tenualosa ilisha"            
## [1055] "Tenualosa ilisha"             "Tenualosa ilisha"            
## [1057] "Tenualosa ilisha"             "Tenualosa ilisha"            
## [1059] "Tenualosa ilisha"             "Tenualosa ilisha"            
## [1061] "Tenualosa ilisha"             "Tenualosa macrura"           
## [1063] "Thaleichthys\x86pacificus"    "Theragra chalcogramma"       
## [1065] "Theragra chalcogramma"        "Theragra chalcogramma"       
## [1067] "Theragra chalcogramma"        "Theragra chalcogramma"       
## [1069] "Theragra chalcogramma"        "Thunnus alalunga"            
## [1071] "Thunnus alalunga"             "Thunnus alalunga"            
## [1073] "Thunnus orientalis"           "Thunnus thynnus"             
## [1075] "Thymallus arcticus"           "Thymallus arcticus"          
## [1077] "Thymallus arcticus"           "Thymallus arcticus"          
## [1079] "Thymallus arcticus"           "Thymallus arcticus"          
## [1081] "Thymallus arcticus"           "Thymallus arcticus"          
## [1083] "Thymallus arcticus"           "Thymallus arcticus"          
## [1085] "Thymallus arcticus"           "Thymallus arcticus"          
## [1087] "Thymallus arcticus"           "Thymallus arcticus"          
## [1089] "Thymallus arcticus"           "Thymallus arcticus"          
## [1091] "Thymallus arcticus"           "Thymallus arcticus"          
## [1093] "Thymallus arcticus"           "Thymallus arcticus"          
## [1095] "Thymallus arcticus"           "Thymallus arcticus"          
## [1097] "Thymallus arcticus"           "Thymallus arcticus"          
## [1099] "Thymallus arcticus"           "Thynnichthys thynnoides"     
## [1101] "Thynnichthys thynnoides"      "Thynnichthys thynnoides"     
## [1103] "Tilapia rendalli"             "Tilapia rendalli"            
## [1105] "Tilapia rendalli"             "Tilapia\x86zillii\x86\x86"   
## [1107] "Tilapia\x86zillii\x86\x86"    "Tilapia\x86zillii\x86\x86"   
## [1109] "Tilapia\x86zillii\x86\x86"    "Tilapia\x86zillii\x86\x86"   
## [1111] "Tilapia\x86zillii\x86\x86"    "Tinca tinca"                 
## [1113] "Tinca tinca"                  "Tinca tinca"                 
## [1115] "Todarodes sagittatus"         "Trachinotus blochii"         
## [1117] "Trachinotus ovatus"           "Trachinus draco"             
## [1119] "Trachurus lathami"            "Trachurus lathami"           
## [1121] "Trachurus lathami"            "Trachurus lathami"           
## [1123] "Trachurus lathami"            "Trachurus mediterraneus"     
## [1125] "Trachurus mediterraneus"      "Trachurus mediterraneus"     
## [1127] "Trachurus mediterraneus"      "Trachurus mediterraneus"     
## [1129] "Trachurus mediterraneus"      "Trachurus mediterraneus"     
## [1131] "Trachurus mediterraneus"      "Trachurus mediterraneus"     
## [1133] "Trachurus mediterraneus"      "Trachurus mediterraneus"     
## [1135] "Trachurus mediterraneus"      "Trachurus mediterraneus"     
## [1137] "Trachurus mediterraneus"      "Trachurus trachurus"         
## [1139] "Trachurus trachurus"          "Trachurus trachurus"         
## [1141] "Trachurus trachurus"          "Trachurus trachurus"         
## [1143] "Trachurus trachurus"          "Trachurus trachurus"         
## [1145] "Trachurus trachurus"          "Trachurus trachurus"         
## [1147] "Trachurus trachurus"          "Trachurus trachurus"         
## [1149] "Trachurus trachurus"          "Trachurus trachurus"         
## [1151] "Trachurus trachurus"          "Trachurus trachurus"         
## [1153] "Trachurus trachurus"          "Trachurus trachurus"         
## [1155] "Trachurus trachurus"          "Trachurus trachurus"         
## [1157] "Trachurus trachurus"          "Trachurus trachurus"         
## [1159] "Trachurus trachurus"          "Trachurus trachurus"         
## [1161] "Trachurus trachurus"          "Trachurus trachurus"         
## [1163] "Trachurus trachurus"          "Trachurus trachurus"         
## [1165] "Trachurus trachurus"          "Trachurus trachurus"         
## [1167] "Trachurus trachurus"          "Trachurus trachurus"         
## [1169] "Trachurus trachurus"          "Trachurus trachurus"         
## [1171] "Trachurus trachurus"          "Trachypenaeus curvirostris"  
## [1173] "Triakidae"                    "Trichiurus lepturus"         
## [1175] "Trichiurus lepturus"          "Trichogaster spp"            
## [1177] "Trigla lyra"                  "Trisopterus minutus"         
## [1179] "Umbrina cirrosa"              "Upeneus moluccensis"         
## [1181] "Upeneus parvus"               "Xenentodon cancila"          
## [1183] "Xiphias gladius"              "Xiphopenaeus kroyeri"        
## [1185] "Zeus faber"                   "Zungaro\x86zungaro"          
## [1187] "Metacarcinus magister"        "Haliotis cracherodii"
```

```r
ntbl <- tbl_df(nut)

glimpse(ntbl)
```

```
## Observations: 1,188
## Variables: 98
## $ ASFIS.Scientific.name                 (chr) "Abramis brama", "Abrami...
## $ Food.name.in.English                  (chr) "Common bream, wild, ski...
## $ TaxonKey                              (int) NA, NA, NA, NA, NA, NA, ...
## $ lwA                                   (dbl) 0.00871, 0.00871, 0.0087...
## $ lwB                                   (dbl) 3.140, 3.140, 3.140, 3.1...
## $ SLMAX                                 (dbl) 82.0, 82.0, 82.0, 82.0, ...
## $ SLMAX_nov28                           (dbl) NA, NA, NA, NA, NA, NA, ...
## $ SLMAX_source                          (chr) "http://www.fishbase.org...
## $ TL                                    (dbl) 2.90, 2.90, 2.90, 2.90, ...
## $ TL_se                                 (dbl) 0.40, 0.40, 0.40, 0.40, ...
## $ TL_nov28                              (dbl) NA, NA, NA, NA, NA, NA, ...
## $ TLSE_nov28                            (dbl) NA, NA, NA, NA, NA, NA, ...
## $ X                                     (int) 1, 2, 3, 4, 5, 6, 7, 8, ...
## $ sci.name                              (chr) "Abramis abramis", "Abra...
## $ Food.Item.ID                          (int) 900684, 900123, 900159, ...
## $ Subgroup                              (chr) "Finfish", "Finfish", "F...
## $ country.region                        (chr) "Germany, Usedom, Baltic...
## $ Type                                  (chr) "W", "W", "W", "W", "W",...
## $ ISSCAAP                               (int) 11, 11, 11, 11, 11, 11, ...
## $ ISSCAAP_cat                           (chr) "Carps, barbels and othe...
## $ Habitat                               (chr) "marine", "freshwater", ...
## $ X3_alpha                              (chr) "FBM", "FBM", "FBM", "FB...
## $ Food.name.in.own.language             (chr) NA, NA, NA, NA, NA, NA, ...
## $ Processing                            (chr) "r", "r", "r", "r", "r",...
## $ ASFIS.English.name                    (chr) "Freshwater bream", "Fre...
## $ Season                                (chr) "3-Jul", "Jun-97", "Summ...
## $ Other                                 (chr) "fishing areas: North At...
## $ Latitude                              (dbl) 53.87537, 54.08424, 41.5...
## $ Abs_lat                               (dbl) 53.87537, 54.08424, 41.5...
## $ length_from_study                     (dbl) NA, NA, NA, NA, NA, NA, ...
## $ length_3                              (dbl) NA, NA, NA, NA, NA, NA, ...
## $ n.x                                   (int) NA, NA, NA, NA, NA, NA, ...
## $ WATER.g.                              (chr) NA, NA, NA, NA, NA, NA, ...
## $ FAT.g.                                (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FATCE.g.                              (chr) NA, NA, NA, NA, NA, NA, ...
## $ FAT..g.                               (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FASAT.g.                              (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FAMS.g.                               (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FAPU.g.                               (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FAUN.g.                               (chr) NA, NA, NA, NA, NA, NA, ...
## $ FATRN.g.                              (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FACID.g.                              (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FAPUN3.g.                             (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FAPUN6.g.                             (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FAPUN9.g.                             (dbl) NA, NA, NA, NA, NA, NA, ...
## $ EPA_g                                 (dbl) NA, NA, NA, NA, NA, NA, ...
## $ DHA_g                                 (dbl) NA, NA, NA, NA, NA, NA, ...
## $ Food_item_id                          (int) 900684, 900123, 900159, ...
## $ max_length_study                      (dbl) NA, 33.2, 30.0, 31.6, 38...
## $ Comments.on.data.processing.methods.y (chr) NA, "Minerals given per ...
## $ Publication.year                      (int) 2006, 2009, 1989, 2009, ...
## $ BiblioID.y                            (chr) "fi105", "fi26", "fi32",...
## $ Compiler                              (chr) "DR", "DR", "DR", "DR", ...
## $ EDIBLE                                (dbl) NA, NA, NA, NA, NA, NA, ...
## $ ENERC_kJ._original                    (dbl) NA, NA, NA, NA, NA, NA, ...
## $ ENERC_kcal._original                  (dbl) NA, NA, NA, NA, NA, NA, ...
## $ ENERA_kcal                            (dbl) NA, NA, NA, NA, NA, NA, ...
## $ DM_g                                  (dbl) NA, NA, NA, NA, NA, NA, ...
## $ WATER_g                               (chr) NA, "78.76", "78", "80.0...
## $ XN                                    (dbl) NA, NA, 6.25, NA, NA, NA...
## $ NT_g                                  (dbl) NA, NA, NA, NA, NA, NA, ...
## $ PROTCNT_g                             (chr) NA, NA, "19", NA, NA, NA...
## $ PROTCNP_g                             (lgl) NA, NA, NA, NA, NA, NA, ...
## $ PROT_g                                (dbl) NA, NA, NA, NA, NA, NA, ...
## $ NPRO_g                                (dbl) NA, NA, NA, NA, NA, NA, ...
## $ NNP_mg                                (int) NA, NA, NA, NA, NA, NA, ...
## $ FAT_g                                 (dbl) 6.40, NA, 1.40, NA, NA, ...
## $ FATCE_g                               (chr) NA, NA, NA, NA, NA, NA, ...
## $ FAT_g.1                               (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FASAT_g                               (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FAMS_g                                (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FAPU_g                                (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FAUN_g                                (chr) NA, NA, NA, NA, NA, NA, ...
## $ CA_mg                                 (dbl) NA, 11.60, 53.00, 20.92,...
## $ FE_mg                                 (dbl) NA, 0.17, 0.60, 0.21, 0....
## $ ID_mcg                                (dbl) NA, NA, NA, NA, NA, NA, ...
## $ K_mg                                  (dbl) NA, 481.79, 570.00, 448....
## $ MG_mg                                 (dbl) NA, 20.41, 69.00, 19.74,...
## $ MN_mg                                 (chr) NA, "0.01", "0.09", "0.0...
## $ SE_mcg                                (chr) NA, NA, NA, NA, NA, NA, ...
## $ HG_mcg                                (chr) NA, NA, "6", NA, NA, NA,...
## $ PB_mcg                                (chr) NA, NA, "8", NA, NA, NA,...
## $ SR_mcg                                (chr) NA, NA, "nd", NA, NA, NA...
## $ RETOL_mcg                             (chr) NA, NA, NA, NA, NA, NA, ...
## $ RETOL13_mcg                           (int) NA, NA, NA, NA, NA, NA, ...
## $ RETOLDH_mcg                           (int) NA, NA, NA, NA, NA, NA, ...
## $ RETOLSUM_mcg                          (int) NA, NA, NA, NA, NA, NA, ...
## $ CARTA_mcg                             (dbl) NA, NA, NA, NA, NA, NA, ...
## $ CARTB_mcg                             (chr) NA, NA, NA, NA, NA, NA, ...
## $ ATX_mcg                               (dbl) NA, NA, NA, NA, NA, NA, ...
## $ ZEA_mcg                               (dbl) NA, NA, NA, NA, NA, NA, ...
## $ CARTOID_mcg                           (int) NA, NA, NA, NA, NA, NA, ...
## $ CHOCAL_mcg                            (dbl) 8.6, NA, NA, NA, NA, NA,...
## $ TOCPHA_mg                             (dbl) NA, NA, NA, NA, NA, NA, ...
## $ VITB6A_mg                             (dbl) NA, NA, NA, NA, NA, NA, ...
## $ VITB12_mcg                            (dbl) NA, NA, NA, NA, NA, NA, ...
## $ VITC_mg                               (chr) NA, NA, NA, NA, NA, NA, ...
## $ ZN_mg                                 (dbl) NA, 0.392940, 1.000000, ...
```

```r
(snippet <- subset(nut, ASFIS.Scientific.name == "Abramis brama"))
```

```
##   ASFIS.Scientific.name                        Food.name.in.English
## 1         Abramis brama    Common bream, wild, skinless fillet, raw
## 2         Abramis brama             Bream, wild, dorsal muscle, raw
## 3         Abramis brama           Bream, wild, skinless fillet, raw
## 4         Abramis brama             Bream, wild, dorsal muscle, raw
## 5         Abramis brama             Bream, wild, dorsal muscle, raw
## 6         Abramis brama             Bream, wild, dorsal muscle, raw
## 7         Abramis brama           Bream, wild, skinless fillet, raw
## 8         Abramis brama Bream, wild, skinless, boneless fillet, raw
##   TaxonKey     lwA  lwB SLMAX SLMAX_nov28
## 1       NA 0.00871 3.14    82          NA
## 2       NA 0.00871 3.14    82          NA
## 3       NA 0.00871 3.14    82          NA
## 4       NA 0.00871 3.14    82          NA
## 5       NA 0.00871 3.14    82          NA
## 6       NA 0.00871 3.14    82          NA
## 7       NA 0.00871 3.14    82          NA
## 8       NA 0.00871 3.14    82          NA
##                                         SLMAX_source  TL TL_se TL_nov28
## 1 http://www.fishbase.org/summary/Abramis-brama.html 2.9   0.4       NA
## 2                                                    2.9   0.4       NA
## 3                                                    2.9   0.4       NA
## 4                                                    2.9   0.4       NA
## 5                                                    2.9   0.4       NA
## 6                                                    2.9   0.4       NA
## 7                                                    2.9   0.4       NA
## 8                                                    2.9   0.4       NA
##   TLSE_nov28 X        sci.name Food.Item.ID Subgroup
## 1         NA 1 Abramis abramis       900684  Finfish
## 2         NA 2   Abramis brama       900123  Finfish
## 3         NA 3   Abramis brama       900159  Finfish
## 4         NA 4   Abramis brama       900122  Finfish
## 5         NA 5   Abramis brama       900124  Finfish
## 6         NA 6   Abramis brama       900125  Finfish
## 7         NA 7   Abramis brama       900158  Finfish
## 8         NA 8   Abramis brama       900225  Finfish
##                country.region Type ISSCAAP
## 1 Germany, Usedom, Baltic Sea    W      11
## 2        Poland, Kisajno Lake    W      11
## 3         Greece, Evros River    W      11
## 4       Poland, Niegocin Lake    W      11
## 5         Poland, Dargin Lake    W      11
## 6         Poland, Dargin Lake    W      11
## 7         Greece, Evros River    W      11
## 8         Greece, Evros River    W      11
##                          ISSCAAP_cat    Habitat X3_alpha
## 1 Carps, barbels and other cyprinids     marine      FBM
## 2 Carps, barbels and other cyprinids freshwater      FBM
## 3 Carps, barbels and other cyprinids freshwater      FBM
## 4 Carps, barbels and other cyprinids freshwater      FBM
## 5 Carps, barbels and other cyprinids freshwater      FBM
## 6 Carps, barbels and other cyprinids freshwater      FBM
## 7 Carps, barbels and other cyprinids freshwater      FBM
## 8 Carps, barbels and other cyprinids freshwater      FBM
##   Food.name.in.own.language Processing ASFIS.English.name       Season
## 1                      <NA>          r   Freshwater bream        3-Jul
## 2                      <NA>          r   Freshwater bream       Jun-97
## 3                      <NA>          r   Freshwater bream  Summer 1988
## 4                      <NA>          r   Freshwater bream       Jun-97
## 5                      <NA>          r   Freshwater bream       Jun-97
## 6                      <NA>          r   Freshwater bream       Jun-97
## 7                      <NA>          r   Freshwater bream  Summer 1987
## 8                      <NA>          r   Freshwater bream Jul-Aug 1987
##                                                               Other
## 1 fishing areas: North Atlantic, North Sea, Barents Sea, Baltic Sea
## 2                            mean length and weight: 33.2cm, 444.1g
## 3                                mean length and weight: 30cm, 400g
## 4                            mean length and weight: 31.6cm, 361.3g
## 5                            mean length and weight: 38.0cm, 607.6g
## 6                           mean length and weight: 50.5cm, 1557.9g
## 7                                mean length and weight: 36cm, 500g
## 8                                         mean weight of fish: 380g
##   Latitude  Abs_lat length_from_study length_3 n.x WATER.g. FAT.g.
## 1 53.87537 53.87537                NA       NA  NA     <NA>     NA
## 2 54.08424 54.08424                NA       NA  NA     <NA>     NA
## 3 41.53000 41.53000                NA       NA  NA     <NA>     NA
## 4 54.00394 54.00394                NA       NA  NA     <NA>     NA
## 5 54.11710 54.11710                NA       NA  NA     <NA>     NA
## 6 54.11710 54.11710                NA       NA  NA     <NA>     NA
## 7 41.53000 41.53000                NA       NA  NA     <NA>     NA
## 8 41.53000 41.53000                NA       NA  NA       78      1
##   FATCE.g. FAT..g. FASAT.g. FAMS.g. FAPU.g. FAUN.g. FATRN.g. FACID.g.
## 1     <NA>      NA       NA      NA      NA    <NA>       NA       NA
## 2     <NA>      NA       NA      NA      NA    <NA>       NA       NA
## 3     <NA>      NA       NA      NA      NA    <NA>       NA       NA
## 4     <NA>      NA       NA      NA      NA    <NA>       NA       NA
## 5     <NA>      NA       NA      NA      NA    <NA>       NA       NA
## 6     <NA>      NA       NA      NA      NA    <NA>       NA       NA
## 7     <NA>      NA       NA      NA      NA    <NA>       NA       NA
## 8     <NA>      NA  0.24016      NA      NA    <NA>       NA       NA
##   FAPUN3.g. FAPUN6.g. FAPUN9.g.   EPA_g   DHA_g Food_item_id
## 1        NA        NA        NA      NA      NA       900684
## 2        NA        NA        NA      NA      NA       900123
## 3        NA        NA        NA      NA      NA       900159
## 4        NA        NA        NA      NA      NA       900122
## 5        NA        NA        NA      NA      NA       900124
## 6        NA        NA        NA      NA      NA       900125
## 7        NA        NA        NA      NA      NA       900158
## 8   0.25122   0.08453        NA 0.09322 0.12087       900225
##   max_length_study    Comments.on.data.processing.methods.y
## 1               NA                                     <NA>
## 2             33.2 Minerals given per DM - conversion to FW
## 3             30.0                                     <NA>
## 4             31.6 Minerals given per DM - conversion to FW
## 5             38.0 Minerals given per DM - conversion to FW
## 6             50.5 Minerals given per DM - conversion to FW
## 7             36.0                                     <NA>
## 8               NA                   FA converted using XFA
##   Publication.year BiblioID.y Compiler EDIBLE ENERC_kJ._original
## 1             2006      fi105       DR     NA                 NA
## 2             2009       fi26       DR     NA                 NA
## 3             1989       fi32       DR     NA                 NA
## 4             2009       fi26       DR     NA                 NA
## 5             2009       fi26       DR     NA                 NA
## 6             2009       fi26       DR     NA                 NA
## 7             1989       fi32       DR     NA                 NA
## 8             1991       fi47       DR     NA                 NA
##   ENERC_kcal._original ENERA_kcal DM_g WATER_g   XN NT_g PROTCNT_g
## 1                   NA         NA   NA    <NA>   NA   NA      <NA>
## 2                   NA         NA   NA   78.76   NA   NA      <NA>
## 3                   NA         NA   NA      78 6.25   NA        19
## 4                   NA         NA   NA   80.04   NA   NA      <NA>
## 5                   NA         NA   NA   78.58   NA   NA      <NA>
## 6                   NA         NA   NA   78.46   NA   NA      <NA>
## 7                   NA         NA   NA      78 6.25   NA        20
## 8                   NA         NA   NA      78   NA   NA      <NA>
##   PROTCNP_g PROT_g NPRO_g NNP_mg FAT_g FATCE_g FAT_g.1 FASAT_g FAMS_g
## 1        NA     NA     NA     NA   6.4    <NA>      NA      NA     NA
## 2        NA     NA     NA     NA    NA    <NA>      NA      NA     NA
## 3        NA     NA     NA     NA   1.4    <NA>      NA      NA     NA
## 4        NA     NA     NA     NA    NA    <NA>      NA      NA     NA
## 5        NA     NA     NA     NA    NA    <NA>      NA      NA     NA
## 6        NA     NA     NA     NA    NA    <NA>      NA      NA     NA
## 7        NA     NA     NA     NA   0.7    <NA>      NA      NA     NA
## 8        NA     NA     NA     NA   1.0    <NA>      NA    0.24     NA
##   FAPU_g FAUN_g CA_mg FE_mg ID_mcg   K_mg MG_mg MN_mg SE_mcg HG_mcg PB_mcg
## 1     NA   <NA>    NA    NA     NA     NA    NA  <NA>   <NA>   <NA>   <NA>
## 2     NA   <NA> 11.60  0.17     NA 481.79 20.41  0.01   <NA>   <NA>   <NA>
## 3     NA   <NA> 53.00  0.60     NA 570.00 69.00  0.09   <NA>      6      8
## 4     NA   <NA> 20.92  0.21     NA 448.20 19.74  0.01   <NA>   <NA>   <NA>
## 5     NA   <NA> 11.46  0.18     NA 306.26 19.17  0.01   <NA>   <NA>   <NA>
## 6     NA   <NA> 12.11  0.19     NA 448.55 18.29  0.01   <NA>   <NA>   <NA>
## 7     NA   <NA> 52.00  0.70     NA 540.00 62.00  0.11   <NA>     14     13
## 8     NA   <NA>    NA    NA     NA     NA    NA  <NA>   <NA>   <NA>   <NA>
##   SR_mcg RETOL_mcg RETOL13_mcg RETOLDH_mcg RETOLSUM_mcg CARTA_mcg
## 1   <NA>      <NA>          NA          NA           NA        NA
## 2   <NA>      <NA>          NA          NA           NA        NA
## 3     nd      <NA>          NA          NA           NA        NA
## 4   <NA>      <NA>          NA          NA           NA        NA
## 5   <NA>      <NA>          NA          NA           NA        NA
## 6   <NA>      <NA>          NA          NA           NA        NA
## 7     nd      <NA>          NA          NA           NA        NA
## 8   <NA>      <NA>          NA          NA           NA        NA
##   CARTB_mcg ATX_mcg ZEA_mcg CARTOID_mcg CHOCAL_mcg TOCPHA_mg VITB6A_mg
## 1      <NA>      NA      NA          NA        8.6        NA        NA
## 2      <NA>      NA      NA          NA         NA        NA        NA
## 3      <NA>      NA      NA          NA         NA        NA        NA
## 4      <NA>      NA      NA          NA         NA        NA        NA
## 5      <NA>      NA      NA          NA         NA        NA        NA
## 6      <NA>      NA      NA          NA         NA        NA        NA
## 7      <NA>      NA      NA          NA         NA        NA        NA
## 8      <NA>      NA      NA          NA         NA        NA        NA
##   VITB12_mcg VITC_mg    ZN_mg
## 1         NA    <NA>       NA
## 2         NA    <NA> 0.392940
## 3         NA    <NA> 1.000000
## 4         NA    <NA> 0.373252
## 5         NA    <NA> 0.372708
## 6         NA    <NA> 0.366180
## 7         NA    <NA> 0.900000
## 8         NA    <NA>       NA
```

```r
filter(ntbl, TL < 3)
```

```
## Source: local data frame [213 x 98]
## 
##    ASFIS.Scientific.name                        Food.name.in.English
##                    (chr)                                       (chr)
## 1          Abramis brama    Common bream, wild, skinless fillet, raw
## 2          Abramis brama             Bream, wild, dorsal muscle, raw
## 3          Abramis brama           Bream, wild, skinless fillet, raw
## 4          Abramis brama             Bream, wild, dorsal muscle, raw
## 5          Abramis brama             Bream, wild, dorsal muscle, raw
## 6          Abramis brama             Bream, wild, dorsal muscle, raw
## 7          Abramis brama           Bream, wild, skinless fillet, raw
## 8          Abramis brama Bream, wild, skinless, boneless fillet, raw
## 9            Abramis spp           Bream, wild, skinless fillet, raw
## 10 Amblypharyngodon mola      Mola, edible parts _eyes included, raw
## ..                   ...                                         ...
## Variables not shown: TaxonKey (int), lwA (dbl), lwB (dbl), SLMAX (dbl),
##   SLMAX_nov28 (dbl), SLMAX_source (chr), TL (dbl), TL_se (dbl), TL_nov28
##   (dbl), TLSE_nov28 (dbl), X (int), sci.name (chr), Food.Item.ID (int),
##   Subgroup (chr), country.region (chr), Type (chr), ISSCAAP (int),
##   ISSCAAP_cat (chr), Habitat (chr), X3_alpha (chr),
##   Food.name.in.own.language (chr), Processing (chr), ASFIS.English.name
##   (chr), Season (chr), Other (chr), Latitude (dbl), Abs_lat (dbl),
##   length_from_study (dbl), length_3 (dbl), n.x (int), WATER.g. (chr),
##   FAT.g. (dbl), FATCE.g. (chr), FAT..g. (dbl), FASAT.g. (dbl), FAMS.g.
##   (dbl), FAPU.g. (dbl), FAUN.g. (chr), FATRN.g. (dbl), FACID.g. (dbl),
##   FAPUN3.g. (dbl), FAPUN6.g. (dbl), FAPUN9.g. (dbl), EPA_g (dbl), DHA_g
##   (dbl), Food_item_id (int), max_length_study (dbl),
##   Comments.on.data.processing.methods.y (chr), Publication.year (int),
##   BiblioID.y (chr), Compiler (chr), EDIBLE (dbl), ENERC_kJ._original
##   (dbl), ENERC_kcal._original (dbl), ENERA_kcal (dbl), DM_g (dbl), WATER_g
##   (chr), XN (dbl), NT_g (dbl), PROTCNT_g (chr), PROTCNP_g (lgl), PROT_g
##   (dbl), NPRO_g (dbl), NNP_mg (int), FAT_g (dbl), FATCE_g (chr), FAT_g.1
##   (dbl), FASAT_g (dbl), FAMS_g (dbl), FAPU_g (dbl), FAUN_g (chr), CA_mg
##   (dbl), FE_mg (dbl), ID_mcg (dbl), K_mg (dbl), MG_mg (dbl), MN_mg (chr),
##   SE_mcg (chr), HG_mcg (chr), PB_mcg (chr), SR_mcg (chr), RETOL_mcg (chr),
##   RETOL13_mcg (int), RETOLDH_mcg (int), RETOLSUM_mcg (int), CARTA_mcg
##   (dbl), CARTB_mcg (chr), ATX_mcg (dbl), ZEA_mcg (dbl), CARTOID_mcg (int),
##   CHOCAL_mcg (dbl), TOCPHA_mg (dbl), VITB6A_mg (dbl), VITB12_mcg (dbl),
##   VITC_mg (chr), ZN_mg (dbl)
```

```r
filter(ntbl, ASFIS.Scientific.name == "Abramis brama")
```

```
## Source: local data frame [8 x 98]
## 
##   ASFIS.Scientific.name                        Food.name.in.English
##                   (chr)                                       (chr)
## 1         Abramis brama    Common bream, wild, skinless fillet, raw
## 2         Abramis brama             Bream, wild, dorsal muscle, raw
## 3         Abramis brama           Bream, wild, skinless fillet, raw
## 4         Abramis brama             Bream, wild, dorsal muscle, raw
## 5         Abramis brama             Bream, wild, dorsal muscle, raw
## 6         Abramis brama             Bream, wild, dorsal muscle, raw
## 7         Abramis brama           Bream, wild, skinless fillet, raw
## 8         Abramis brama Bream, wild, skinless, boneless fillet, raw
## Variables not shown: TaxonKey (int), lwA (dbl), lwB (dbl), SLMAX (dbl),
##   SLMAX_nov28 (dbl), SLMAX_source (chr), TL (dbl), TL_se (dbl), TL_nov28
##   (dbl), TLSE_nov28 (dbl), X (int), sci.name (chr), Food.Item.ID (int),
##   Subgroup (chr), country.region (chr), Type (chr), ISSCAAP (int),
##   ISSCAAP_cat (chr), Habitat (chr), X3_alpha (chr),
##   Food.name.in.own.language (chr), Processing (chr), ASFIS.English.name
##   (chr), Season (chr), Other (chr), Latitude (dbl), Abs_lat (dbl),
##   length_from_study (dbl), length_3 (dbl), n.x (int), WATER.g. (chr),
##   FAT.g. (dbl), FATCE.g. (chr), FAT..g. (dbl), FASAT.g. (dbl), FAMS.g.
##   (dbl), FAPU.g. (dbl), FAUN.g. (chr), FATRN.g. (dbl), FACID.g. (dbl),
##   FAPUN3.g. (dbl), FAPUN6.g. (dbl), FAPUN9.g. (dbl), EPA_g (dbl), DHA_g
##   (dbl), Food_item_id (int), max_length_study (dbl),
##   Comments.on.data.processing.methods.y (chr), Publication.year (int),
##   BiblioID.y (chr), Compiler (chr), EDIBLE (dbl), ENERC_kJ._original
##   (dbl), ENERC_kcal._original (dbl), ENERA_kcal (dbl), DM_g (dbl), WATER_g
##   (chr), XN (dbl), NT_g (dbl), PROTCNT_g (chr), PROTCNP_g (lgl), PROT_g
##   (dbl), NPRO_g (dbl), NNP_mg (int), FAT_g (dbl), FATCE_g (chr), FAT_g.1
##   (dbl), FASAT_g (dbl), FAMS_g (dbl), FAPU_g (dbl), FAUN_g (chr), CA_mg
##   (dbl), FE_mg (dbl), ID_mcg (dbl), K_mg (dbl), MG_mg (dbl), MN_mg (chr),
##   SE_mcg (chr), HG_mcg (chr), PB_mcg (chr), SR_mcg (chr), RETOL_mcg (chr),
##   RETOL13_mcg (int), RETOLDH_mcg (int), RETOLSUM_mcg (int), CARTA_mcg
##   (dbl), CARTB_mcg (chr), ATX_mcg (dbl), ZEA_mcg (dbl), CARTOID_mcg (int),
##   CHOCAL_mcg (dbl), TOCPHA_mg (dbl), VITB6A_mg (dbl), VITB12_mcg (dbl),
##   VITC_mg (chr), ZN_mg (dbl)
```

```r
filter(ntbl, ASFIS.Scientific.name %in% c("Abramis brama", "Thymallus arcticus"))
```

```
## Source: local data frame [33 x 98]
## 
##    ASFIS.Scientific.name
##                    (chr)
## 1          Abramis brama
## 2          Abramis brama
## 3          Abramis brama
## 4          Abramis brama
## 5          Abramis brama
## 6          Abramis brama
## 7          Abramis brama
## 8          Abramis brama
## 9     Thymallus arcticus
## 10    Thymallus arcticus
## ..                   ...
## Variables not shown: Food.name.in.English (chr), TaxonKey (int), lwA
##   (dbl), lwB (dbl), SLMAX (dbl), SLMAX_nov28 (dbl), SLMAX_source (chr), TL
##   (dbl), TL_se (dbl), TL_nov28 (dbl), TLSE_nov28 (dbl), X (int), sci.name
##   (chr), Food.Item.ID (int), Subgroup (chr), country.region (chr), Type
##   (chr), ISSCAAP (int), ISSCAAP_cat (chr), Habitat (chr), X3_alpha (chr),
##   Food.name.in.own.language (chr), Processing (chr), ASFIS.English.name
##   (chr), Season (chr), Other (chr), Latitude (dbl), Abs_lat (dbl),
##   length_from_study (dbl), length_3 (dbl), n.x (int), WATER.g. (chr),
##   FAT.g. (dbl), FATCE.g. (chr), FAT..g. (dbl), FASAT.g. (dbl), FAMS.g.
##   (dbl), FAPU.g. (dbl), FAUN.g. (chr), FATRN.g. (dbl), FACID.g. (dbl),
##   FAPUN3.g. (dbl), FAPUN6.g. (dbl), FAPUN9.g. (dbl), EPA_g (dbl), DHA_g
##   (dbl), Food_item_id (int), max_length_study (dbl),
##   Comments.on.data.processing.methods.y (chr), Publication.year (int),
##   BiblioID.y (chr), Compiler (chr), EDIBLE (dbl), ENERC_kJ._original
##   (dbl), ENERC_kcal._original (dbl), ENERA_kcal (dbl), DM_g (dbl), WATER_g
##   (chr), XN (dbl), NT_g (dbl), PROTCNT_g (chr), PROTCNP_g (lgl), PROT_g
##   (dbl), NPRO_g (dbl), NNP_mg (int), FAT_g (dbl), FATCE_g (chr), FAT_g.1
##   (dbl), FASAT_g (dbl), FAMS_g (dbl), FAPU_g (dbl), FAUN_g (chr), CA_mg
##   (dbl), FE_mg (dbl), ID_mcg (dbl), K_mg (dbl), MG_mg (dbl), MN_mg (chr),
##   SE_mcg (chr), HG_mcg (chr), PB_mcg (chr), SR_mcg (chr), RETOL_mcg (chr),
##   RETOL13_mcg (int), RETOLDH_mcg (int), RETOLSUM_mcg (int), CARTA_mcg
##   (dbl), CARTB_mcg (chr), ATX_mcg (dbl), ZEA_mcg (dbl), CARTOID_mcg (int),
##   CHOCAL_mcg (dbl), TOCPHA_mg (dbl), VITB6A_mg (dbl), VITB12_mcg (dbl),
##   VITC_mg (chr), ZN_mg (dbl)
```

```r
nut %>% head
```

```
##   ASFIS.Scientific.name                     Food.name.in.English TaxonKey
## 1         Abramis brama Common bream, wild, skinless fillet, raw       NA
## 2         Abramis brama          Bream, wild, dorsal muscle, raw       NA
## 3         Abramis brama        Bream, wild, skinless fillet, raw       NA
## 4         Abramis brama          Bream, wild, dorsal muscle, raw       NA
## 5         Abramis brama          Bream, wild, dorsal muscle, raw       NA
## 6         Abramis brama          Bream, wild, dorsal muscle, raw       NA
##       lwA  lwB SLMAX SLMAX_nov28
## 1 0.00871 3.14    82          NA
## 2 0.00871 3.14    82          NA
## 3 0.00871 3.14    82          NA
## 4 0.00871 3.14    82          NA
## 5 0.00871 3.14    82          NA
## 6 0.00871 3.14    82          NA
##                                         SLMAX_source  TL TL_se TL_nov28
## 1 http://www.fishbase.org/summary/Abramis-brama.html 2.9   0.4       NA
## 2                                                    2.9   0.4       NA
## 3                                                    2.9   0.4       NA
## 4                                                    2.9   0.4       NA
## 5                                                    2.9   0.4       NA
## 6                                                    2.9   0.4       NA
##   TLSE_nov28 X        sci.name Food.Item.ID Subgroup
## 1         NA 1 Abramis abramis       900684  Finfish
## 2         NA 2   Abramis brama       900123  Finfish
## 3         NA 3   Abramis brama       900159  Finfish
## 4         NA 4   Abramis brama       900122  Finfish
## 5         NA 5   Abramis brama       900124  Finfish
## 6         NA 6   Abramis brama       900125  Finfish
##                country.region Type ISSCAAP
## 1 Germany, Usedom, Baltic Sea    W      11
## 2        Poland, Kisajno Lake    W      11
## 3         Greece, Evros River    W      11
## 4       Poland, Niegocin Lake    W      11
## 5         Poland, Dargin Lake    W      11
## 6         Poland, Dargin Lake    W      11
##                          ISSCAAP_cat    Habitat X3_alpha
## 1 Carps, barbels and other cyprinids     marine      FBM
## 2 Carps, barbels and other cyprinids freshwater      FBM
## 3 Carps, barbels and other cyprinids freshwater      FBM
## 4 Carps, barbels and other cyprinids freshwater      FBM
## 5 Carps, barbels and other cyprinids freshwater      FBM
## 6 Carps, barbels and other cyprinids freshwater      FBM
##   Food.name.in.own.language Processing ASFIS.English.name      Season
## 1                      <NA>          r   Freshwater bream       3-Jul
## 2                      <NA>          r   Freshwater bream      Jun-97
## 3                      <NA>          r   Freshwater bream Summer 1988
## 4                      <NA>          r   Freshwater bream      Jun-97
## 5                      <NA>          r   Freshwater bream      Jun-97
## 6                      <NA>          r   Freshwater bream      Jun-97
##                                                               Other
## 1 fishing areas: North Atlantic, North Sea, Barents Sea, Baltic Sea
## 2                            mean length and weight: 33.2cm, 444.1g
## 3                                mean length and weight: 30cm, 400g
## 4                            mean length and weight: 31.6cm, 361.3g
## 5                            mean length and weight: 38.0cm, 607.6g
## 6                           mean length and weight: 50.5cm, 1557.9g
##   Latitude  Abs_lat length_from_study length_3 n.x WATER.g. FAT.g.
## 1 53.87537 53.87537                NA       NA  NA     <NA>     NA
## 2 54.08424 54.08424                NA       NA  NA     <NA>     NA
## 3 41.53000 41.53000                NA       NA  NA     <NA>     NA
## 4 54.00394 54.00394                NA       NA  NA     <NA>     NA
## 5 54.11710 54.11710                NA       NA  NA     <NA>     NA
## 6 54.11710 54.11710                NA       NA  NA     <NA>     NA
##   FATCE.g. FAT..g. FASAT.g. FAMS.g. FAPU.g. FAUN.g. FATRN.g. FACID.g.
## 1     <NA>      NA       NA      NA      NA    <NA>       NA       NA
## 2     <NA>      NA       NA      NA      NA    <NA>       NA       NA
## 3     <NA>      NA       NA      NA      NA    <NA>       NA       NA
## 4     <NA>      NA       NA      NA      NA    <NA>       NA       NA
## 5     <NA>      NA       NA      NA      NA    <NA>       NA       NA
## 6     <NA>      NA       NA      NA      NA    <NA>       NA       NA
##   FAPUN3.g. FAPUN6.g. FAPUN9.g. EPA_g DHA_g Food_item_id max_length_study
## 1        NA        NA        NA    NA    NA       900684               NA
## 2        NA        NA        NA    NA    NA       900123             33.2
## 3        NA        NA        NA    NA    NA       900159             30.0
## 4        NA        NA        NA    NA    NA       900122             31.6
## 5        NA        NA        NA    NA    NA       900124             38.0
## 6        NA        NA        NA    NA    NA       900125             50.5
##      Comments.on.data.processing.methods.y Publication.year BiblioID.y
## 1                                     <NA>             2006      fi105
## 2 Minerals given per DM - conversion to FW             2009       fi26
## 3                                     <NA>             1989       fi32
## 4 Minerals given per DM - conversion to FW             2009       fi26
## 5 Minerals given per DM - conversion to FW             2009       fi26
## 6 Minerals given per DM - conversion to FW             2009       fi26
##   Compiler EDIBLE ENERC_kJ._original ENERC_kcal._original ENERA_kcal DM_g
## 1       DR     NA                 NA                   NA         NA   NA
## 2       DR     NA                 NA                   NA         NA   NA
## 3       DR     NA                 NA                   NA         NA   NA
## 4       DR     NA                 NA                   NA         NA   NA
## 5       DR     NA                 NA                   NA         NA   NA
## 6       DR     NA                 NA                   NA         NA   NA
##   WATER_g   XN NT_g PROTCNT_g PROTCNP_g PROT_g NPRO_g NNP_mg FAT_g FATCE_g
## 1    <NA>   NA   NA      <NA>        NA     NA     NA     NA   6.4    <NA>
## 2   78.76   NA   NA      <NA>        NA     NA     NA     NA    NA    <NA>
## 3      78 6.25   NA        19        NA     NA     NA     NA   1.4    <NA>
## 4   80.04   NA   NA      <NA>        NA     NA     NA     NA    NA    <NA>
## 5   78.58   NA   NA      <NA>        NA     NA     NA     NA    NA    <NA>
## 6   78.46   NA   NA      <NA>        NA     NA     NA     NA    NA    <NA>
##   FAT_g.1 FASAT_g FAMS_g FAPU_g FAUN_g CA_mg FE_mg ID_mcg   K_mg MG_mg
## 1      NA      NA     NA     NA   <NA>    NA    NA     NA     NA    NA
## 2      NA      NA     NA     NA   <NA> 11.60  0.17     NA 481.79 20.41
## 3      NA      NA     NA     NA   <NA> 53.00  0.60     NA 570.00 69.00
## 4      NA      NA     NA     NA   <NA> 20.92  0.21     NA 448.20 19.74
## 5      NA      NA     NA     NA   <NA> 11.46  0.18     NA 306.26 19.17
## 6      NA      NA     NA     NA   <NA> 12.11  0.19     NA 448.55 18.29
##   MN_mg SE_mcg HG_mcg PB_mcg SR_mcg RETOL_mcg RETOL13_mcg RETOLDH_mcg
## 1  <NA>   <NA>   <NA>   <NA>   <NA>      <NA>          NA          NA
## 2  0.01   <NA>   <NA>   <NA>   <NA>      <NA>          NA          NA
## 3  0.09   <NA>      6      8     nd      <NA>          NA          NA
## 4  0.01   <NA>   <NA>   <NA>   <NA>      <NA>          NA          NA
## 5  0.01   <NA>   <NA>   <NA>   <NA>      <NA>          NA          NA
## 6  0.01   <NA>   <NA>   <NA>   <NA>      <NA>          NA          NA
##   RETOLSUM_mcg CARTA_mcg CARTB_mcg ATX_mcg ZEA_mcg CARTOID_mcg CHOCAL_mcg
## 1           NA        NA      <NA>      NA      NA          NA        8.6
## 2           NA        NA      <NA>      NA      NA          NA         NA
## 3           NA        NA      <NA>      NA      NA          NA         NA
## 4           NA        NA      <NA>      NA      NA          NA         NA
## 5           NA        NA      <NA>      NA      NA          NA         NA
## 6           NA        NA      <NA>      NA      NA          NA         NA
##   TOCPHA_mg VITB6A_mg VITB12_mcg VITC_mg    ZN_mg
## 1        NA        NA         NA    <NA>       NA
## 2        NA        NA         NA    <NA> 0.392940
## 3        NA        NA         NA    <NA> 1.000000
## 4        NA        NA         NA    <NA> 0.373252
## 5        NA        NA         NA    <NA> 0.372708
## 6        NA        NA         NA    <NA> 0.366180
```

```r
nut %>% head(3)
```

```
##   ASFIS.Scientific.name                     Food.name.in.English TaxonKey
## 1         Abramis brama Common bream, wild, skinless fillet, raw       NA
## 2         Abramis brama          Bream, wild, dorsal muscle, raw       NA
## 3         Abramis brama        Bream, wild, skinless fillet, raw       NA
##       lwA  lwB SLMAX SLMAX_nov28
## 1 0.00871 3.14    82          NA
## 2 0.00871 3.14    82          NA
## 3 0.00871 3.14    82          NA
##                                         SLMAX_source  TL TL_se TL_nov28
## 1 http://www.fishbase.org/summary/Abramis-brama.html 2.9   0.4       NA
## 2                                                    2.9   0.4       NA
## 3                                                    2.9   0.4       NA
##   TLSE_nov28 X        sci.name Food.Item.ID Subgroup
## 1         NA 1 Abramis abramis       900684  Finfish
## 2         NA 2   Abramis brama       900123  Finfish
## 3         NA 3   Abramis brama       900159  Finfish
##                country.region Type ISSCAAP
## 1 Germany, Usedom, Baltic Sea    W      11
## 2        Poland, Kisajno Lake    W      11
## 3         Greece, Evros River    W      11
##                          ISSCAAP_cat    Habitat X3_alpha
## 1 Carps, barbels and other cyprinids     marine      FBM
## 2 Carps, barbels and other cyprinids freshwater      FBM
## 3 Carps, barbels and other cyprinids freshwater      FBM
##   Food.name.in.own.language Processing ASFIS.English.name      Season
## 1                      <NA>          r   Freshwater bream       3-Jul
## 2                      <NA>          r   Freshwater bream      Jun-97
## 3                      <NA>          r   Freshwater bream Summer 1988
##                                                               Other
## 1 fishing areas: North Atlantic, North Sea, Barents Sea, Baltic Sea
## 2                            mean length and weight: 33.2cm, 444.1g
## 3                                mean length and weight: 30cm, 400g
##   Latitude  Abs_lat length_from_study length_3 n.x WATER.g. FAT.g.
## 1 53.87537 53.87537                NA       NA  NA     <NA>     NA
## 2 54.08424 54.08424                NA       NA  NA     <NA>     NA
## 3 41.53000 41.53000                NA       NA  NA     <NA>     NA
##   FATCE.g. FAT..g. FASAT.g. FAMS.g. FAPU.g. FAUN.g. FATRN.g. FACID.g.
## 1     <NA>      NA       NA      NA      NA    <NA>       NA       NA
## 2     <NA>      NA       NA      NA      NA    <NA>       NA       NA
## 3     <NA>      NA       NA      NA      NA    <NA>       NA       NA
##   FAPUN3.g. FAPUN6.g. FAPUN9.g. EPA_g DHA_g Food_item_id max_length_study
## 1        NA        NA        NA    NA    NA       900684               NA
## 2        NA        NA        NA    NA    NA       900123             33.2
## 3        NA        NA        NA    NA    NA       900159             30.0
##      Comments.on.data.processing.methods.y Publication.year BiblioID.y
## 1                                     <NA>             2006      fi105
## 2 Minerals given per DM - conversion to FW             2009       fi26
## 3                                     <NA>             1989       fi32
##   Compiler EDIBLE ENERC_kJ._original ENERC_kcal._original ENERA_kcal DM_g
## 1       DR     NA                 NA                   NA         NA   NA
## 2       DR     NA                 NA                   NA         NA   NA
## 3       DR     NA                 NA                   NA         NA   NA
##   WATER_g   XN NT_g PROTCNT_g PROTCNP_g PROT_g NPRO_g NNP_mg FAT_g FATCE_g
## 1    <NA>   NA   NA      <NA>        NA     NA     NA     NA   6.4    <NA>
## 2   78.76   NA   NA      <NA>        NA     NA     NA     NA    NA    <NA>
## 3      78 6.25   NA        19        NA     NA     NA     NA   1.4    <NA>
##   FAT_g.1 FASAT_g FAMS_g FAPU_g FAUN_g CA_mg FE_mg ID_mcg   K_mg MG_mg
## 1      NA      NA     NA     NA   <NA>    NA    NA     NA     NA    NA
## 2      NA      NA     NA     NA   <NA>  11.6  0.17     NA 481.79 20.41
## 3      NA      NA     NA     NA   <NA>  53.0  0.60     NA 570.00 69.00
##   MN_mg SE_mcg HG_mcg PB_mcg SR_mcg RETOL_mcg RETOL13_mcg RETOLDH_mcg
## 1  <NA>   <NA>   <NA>   <NA>   <NA>      <NA>          NA          NA
## 2  0.01   <NA>   <NA>   <NA>   <NA>      <NA>          NA          NA
## 3  0.09   <NA>      6      8     nd      <NA>          NA          NA
##   RETOLSUM_mcg CARTA_mcg CARTB_mcg ATX_mcg ZEA_mcg CARTOID_mcg CHOCAL_mcg
## 1           NA        NA      <NA>      NA      NA          NA        8.6
## 2           NA        NA      <NA>      NA      NA          NA         NA
## 3           NA        NA      <NA>      NA      NA          NA         NA
##   TOCPHA_mg VITB6A_mg VITB12_mcg VITC_mg   ZN_mg
## 1        NA        NA         NA    <NA>      NA
## 2        NA        NA         NA    <NA> 0.39294
## 3        NA        NA         NA    <NA> 1.00000
```

Change variable names to more intuitive names

```r
 ntbl <- ntbl %>%
  rename(species = ASFIS.Scientific.name,
         taxon = ISSCAAP_cat,
         body_size = SLMAX)
ntbl <- ntbl %>%
  rename(max_length = body_size)
```

Pull out variables we will use in this analysis

```r
ntbl %>%
  select(species, taxon, max_length, TL, CA_mg, EPA_g, DHA_g, ZN_mg, HG_mcg, lwA, lwB) %>%
  head(4)
```

```
## Source: local data frame [4 x 11]
## 
##         species                              taxon max_length    TL CA_mg
##           (chr)                              (chr)      (dbl) (dbl) (dbl)
## 1 Abramis brama Carps, barbels and other cyprinids         82   2.9    NA
## 2 Abramis brama Carps, barbels and other cyprinids         82   2.9 11.60
## 3 Abramis brama Carps, barbels and other cyprinids         82   2.9 53.00
## 4 Abramis brama Carps, barbels and other cyprinids         82   2.9 20.92
## Variables not shown: EPA_g (dbl), DHA_g (dbl), ZN_mg (dbl), HG_mcg (chr),
##   lwA (dbl), lwB (dbl)
```

Convert max length to max body size using length-weight conversion (W = a × L^b)

```r
ntbl <- ntbl %>%
  mutate(max_size = lwA * (max_length^lwB))

ntbl %>%
  glimpse()
```

```
## Observations: 1,188
## Variables: 99
## $ species                               (chr) "Abramis brama", "Abrami...
## $ Food.name.in.English                  (chr) "Common bream, wild, ski...
## $ TaxonKey                              (int) NA, NA, NA, NA, NA, NA, ...
## $ lwA                                   (dbl) 0.00871, 0.00871, 0.0087...
## $ lwB                                   (dbl) 3.140, 3.140, 3.140, 3.1...
## $ max_length                            (dbl) 82.0, 82.0, 82.0, 82.0, ...
## $ SLMAX_nov28                           (dbl) NA, NA, NA, NA, NA, NA, ...
## $ SLMAX_source                          (chr) "http://www.fishbase.org...
## $ TL                                    (dbl) 2.90, 2.90, 2.90, 2.90, ...
## $ TL_se                                 (dbl) 0.40, 0.40, 0.40, 0.40, ...
## $ TL_nov28                              (dbl) NA, NA, NA, NA, NA, NA, ...
## $ TLSE_nov28                            (dbl) NA, NA, NA, NA, NA, NA, ...
## $ X                                     (int) 1, 2, 3, 4, 5, 6, 7, 8, ...
## $ sci.name                              (chr) "Abramis abramis", "Abra...
## $ Food.Item.ID                          (int) 900684, 900123, 900159, ...
## $ Subgroup                              (chr) "Finfish", "Finfish", "F...
## $ country.region                        (chr) "Germany, Usedom, Baltic...
## $ Type                                  (chr) "W", "W", "W", "W", "W",...
## $ ISSCAAP                               (int) 11, 11, 11, 11, 11, 11, ...
## $ taxon                                 (chr) "Carps, barbels and othe...
## $ Habitat                               (chr) "marine", "freshwater", ...
## $ X3_alpha                              (chr) "FBM", "FBM", "FBM", "FB...
## $ Food.name.in.own.language             (chr) NA, NA, NA, NA, NA, NA, ...
## $ Processing                            (chr) "r", "r", "r", "r", "r",...
## $ ASFIS.English.name                    (chr) "Freshwater bream", "Fre...
## $ Season                                (chr) "3-Jul", "Jun-97", "Summ...
## $ Other                                 (chr) "fishing areas: North At...
## $ Latitude                              (dbl) 53.87537, 54.08424, 41.5...
## $ Abs_lat                               (dbl) 53.87537, 54.08424, 41.5...
## $ length_from_study                     (dbl) NA, NA, NA, NA, NA, NA, ...
## $ length_3                              (dbl) NA, NA, NA, NA, NA, NA, ...
## $ n.x                                   (int) NA, NA, NA, NA, NA, NA, ...
## $ WATER.g.                              (chr) NA, NA, NA, NA, NA, NA, ...
## $ FAT.g.                                (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FATCE.g.                              (chr) NA, NA, NA, NA, NA, NA, ...
## $ FAT..g.                               (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FASAT.g.                              (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FAMS.g.                               (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FAPU.g.                               (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FAUN.g.                               (chr) NA, NA, NA, NA, NA, NA, ...
## $ FATRN.g.                              (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FACID.g.                              (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FAPUN3.g.                             (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FAPUN6.g.                             (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FAPUN9.g.                             (dbl) NA, NA, NA, NA, NA, NA, ...
## $ EPA_g                                 (dbl) NA, NA, NA, NA, NA, NA, ...
## $ DHA_g                                 (dbl) NA, NA, NA, NA, NA, NA, ...
## $ Food_item_id                          (int) 900684, 900123, 900159, ...
## $ max_length_study                      (dbl) NA, 33.2, 30.0, 31.6, 38...
## $ Comments.on.data.processing.methods.y (chr) NA, "Minerals given per ...
## $ Publication.year                      (int) 2006, 2009, 1989, 2009, ...
## $ BiblioID.y                            (chr) "fi105", "fi26", "fi32",...
## $ Compiler                              (chr) "DR", "DR", "DR", "DR", ...
## $ EDIBLE                                (dbl) NA, NA, NA, NA, NA, NA, ...
## $ ENERC_kJ._original                    (dbl) NA, NA, NA, NA, NA, NA, ...
## $ ENERC_kcal._original                  (dbl) NA, NA, NA, NA, NA, NA, ...
## $ ENERA_kcal                            (dbl) NA, NA, NA, NA, NA, NA, ...
## $ DM_g                                  (dbl) NA, NA, NA, NA, NA, NA, ...
## $ WATER_g                               (chr) NA, "78.76", "78", "80.0...
## $ XN                                    (dbl) NA, NA, 6.25, NA, NA, NA...
## $ NT_g                                  (dbl) NA, NA, NA, NA, NA, NA, ...
## $ PROTCNT_g                             (chr) NA, NA, "19", NA, NA, NA...
## $ PROTCNP_g                             (lgl) NA, NA, NA, NA, NA, NA, ...
## $ PROT_g                                (dbl) NA, NA, NA, NA, NA, NA, ...
## $ NPRO_g                                (dbl) NA, NA, NA, NA, NA, NA, ...
## $ NNP_mg                                (int) NA, NA, NA, NA, NA, NA, ...
## $ FAT_g                                 (dbl) 6.40, NA, 1.40, NA, NA, ...
## $ FATCE_g                               (chr) NA, NA, NA, NA, NA, NA, ...
## $ FAT_g.1                               (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FASAT_g                               (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FAMS_g                                (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FAPU_g                                (dbl) NA, NA, NA, NA, NA, NA, ...
## $ FAUN_g                                (chr) NA, NA, NA, NA, NA, NA, ...
## $ CA_mg                                 (dbl) NA, 11.60, 53.00, 20.92,...
## $ FE_mg                                 (dbl) NA, 0.17, 0.60, 0.21, 0....
## $ ID_mcg                                (dbl) NA, NA, NA, NA, NA, NA, ...
## $ K_mg                                  (dbl) NA, 481.79, 570.00, 448....
## $ MG_mg                                 (dbl) NA, 20.41, 69.00, 19.74,...
## $ MN_mg                                 (chr) NA, "0.01", "0.09", "0.0...
## $ SE_mcg                                (chr) NA, NA, NA, NA, NA, NA, ...
## $ HG_mcg                                (chr) NA, NA, "6", NA, NA, NA,...
## $ PB_mcg                                (chr) NA, NA, "8", NA, NA, NA,...
## $ SR_mcg                                (chr) NA, NA, "nd", NA, NA, NA...
## $ RETOL_mcg                             (chr) NA, NA, NA, NA, NA, NA, ...
## $ RETOL13_mcg                           (int) NA, NA, NA, NA, NA, NA, ...
## $ RETOLDH_mcg                           (int) NA, NA, NA, NA, NA, NA, ...
## $ RETOLSUM_mcg                          (int) NA, NA, NA, NA, NA, NA, ...
## $ CARTA_mcg                             (dbl) NA, NA, NA, NA, NA, NA, ...
## $ CARTB_mcg                             (chr) NA, NA, NA, NA, NA, NA, ...
## $ ATX_mcg                               (dbl) NA, NA, NA, NA, NA, NA, ...
## $ ZEA_mcg                               (dbl) NA, NA, NA, NA, NA, NA, ...
## $ CARTOID_mcg                           (int) NA, NA, NA, NA, NA, NA, ...
## $ CHOCAL_mcg                            (dbl) 8.6, NA, NA, NA, NA, NA,...
## $ TOCPHA_mg                             (dbl) NA, NA, NA, NA, NA, NA, ...
## $ VITB6A_mg                             (dbl) NA, NA, NA, NA, NA, NA, ...
## $ VITB12_mcg                            (dbl) NA, NA, NA, NA, NA, NA, ...
## $ VITC_mg                               (chr) NA, NA, NA, NA, NA, NA, ...
## $ ZN_mg                                 (dbl) NA, 0.392940, 1.000000, ...
## $ max_size                              (dbl) 8900.07469, 8900.07469, ...
```
