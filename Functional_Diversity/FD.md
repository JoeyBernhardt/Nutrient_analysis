# FD

Hi Mary!

Load packages

```r
library(ggplot2)
library(broom)
library(plyr)
suppressPackageStartupMessages(library(dplyr))
library(knitr)
library(tidyr)
library(readr)
```

Import and clean data


```r
nut_dec3 <- read.csv("~/Desktop/Nutrient_databases/nut_dec3.csv", comment.char="#")
ntbl <- tbl_df(nut_dec3)
ntbl <- ntbl %>%
  mutate(HG_mcg = as.numeric(HG_mcg)) %>% 
  mutate(PROTCNT_g = as.numeric(PROTCNT_g)) %>% 
   rename(species = ASFIS.Scientific.name,
         taxon = ISSCAAP_cat,
         max_length = SLMAX,
         FAT = FAT.g.) %>% 
   mutate(max_size = (lwA * (max_length^lwB)/1000)) %>% 
            mutate(species = revalue(species,
                               c("Oreochromis _=Tilapia spp" = "Tilapia spp",
                                 "Tilapia\x86zillii\x86\x86" = "Tilapia zillii",
                                 "Thaleichthys\x86pacificus" = "Thaleichthys pacificus", 
                                 "Zungaro\x86zungaro" = "Zungaro zungaro",
                                 "Pinirampus\x86pirinampu\x86" = "Pinirampus pirinampu",
                                 "Platichthys\x86stellatus" = "Platichthys stellatus",
                                 "Parambassis\x86wolffii" = "Parambassis wolffii",
                                 "Oncorhynchus\x86mykiss" = "Oncorhynchus mykiss",
                                 "Oncorhynchus\x86tshawytscha" = "Oncorhynchus tshawytscha",
                                 "Oncorhynchus\x86keta" = "Oncorhynchus keta",
                                 "Oncorhynchus\x86nerka\x86" = "Oncorhynchus nerka"))) %>%
  select(species, taxon, max_size, max_length, TL, CA_mg, EPA_g, DHA_g, FE_mg, ZN_mg, HG_mcg, FAT, PROTCNT_g, lwA, lwB, Habitat, Subgroup, Abs_lat)
```


Out of the species for which we have the full suite of micronutrient data (i.e. 5 micronutrients), how many RDI targets do they reach?

```r
ntbl.RDI.mac <- ntbl%>% 
  group_by(species) %>% 
    summarise(mean.FAT = mean(FAT, na.rm = TRUE),
              mean.PRO = mean(PROTCNT_g, na.rm = TRUE)) %>% 
  mutate(RDI.FAT = (mean.FAT > 7)) %>% 
  mutate(RDI.PRO = (mean.PRO > 5)) %>% 
  mutate(RDI.macro.tot = rowSums(.[4:5])) %>% 
  filter(!is.na(RDI.macro.tot)) %>% 
ggplot(., aes(x = reorder(species, RDI.macro.tot), y = RDI.macro.tot, na.rm = TRUE, color = species)) + geom_point(size = 3) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position="none") + ylim(0,2) + ylab("number of macronutrient RDI targets reached") + xlab("species")
ggsave("RDI.tot.mac.png")
```

```
## Saving 7 x 5 in image
```

```r
ntbl.RDI.mic <- ntbl %>% 
  group_by(species) %>% 
    summarise(mean.CA = mean(CA_mg, na.rm = TRUE),
            mean.EPA = mean(EPA_g, na.rm = TRUE), 
            mean.DHA = mean(DHA_g, na.rm = TRUE), 
            mean.ZN = mean(ZN_mg, na.rm = TRUE), 
            mean.FE = mean(FE_mg, na.rm = TRUE)) %>% 
  mutate(RDI.CA = (mean.CA > 300)) %>% 
  mutate(RDI.FE = (mean.FE > 4.5)) %>% 
  mutate(RDI.ZN = (mean.ZN > 2.75)) %>% 
  mutate(RDI.EPA = (mean.EPA > 0.25)) %>% 
  mutate(RDI.DHA = (mean.DHA > 0.25)) %>% 
  mutate(RDI.micro.tot = rowSums(.[7:11])) %>% 
  filter(!is.na(RDI.micro.tot)) %>%
    ggplot(., aes(x = reorder(species, RDI.micro.tot), y = RDI.micro.tot, na.rm = TRUE, color = species)) + geom_point(size = 3) + theme(axis.text.x = element_text(angle = 75, hjust = 1)) + theme(legend.position="none") + ylim(0,5) + ylab("number of micronutrient RDI targets reached") + xlab("species")
ggsave("RDI.tot.mic.png")
```

```
## Saving 7 x 5 in image
```

![](RDI.tot.mic.png)
![](RDI.tot.mac.png)


How many species reach RDI targets for macro vs. micronutrients?

```r
macro.RDI <- ntbl%>% 
  group_by(species) %>% 
    summarise(mean.FAT = mean(FAT, na.rm = TRUE),
              mean.PRO = mean(PROTCNT_g, na.rm = TRUE)) %>% 
  mutate(RDI.FAT = (mean.FAT > 7)) %>% 
  mutate(RDI.PRO = (mean.PRO > 5)) %>% 
  mutate(RDI.macro.tot = rowSums(.[4:5])) %>% 
  filter(!is.na(RDI.macro.tot)) %>% 
  arrange(desc(RDI.macro.tot)) 
  knitr::kable(macro.RDI)
```



species                         mean.FAT    mean.PRO  RDI.FAT   RDI.PRO    RDI.macro.tot
---------------------------  -----------  ----------  --------  --------  --------------
Alosa alosa                   15.9100000   249.00000  TRUE      TRUE                   2
Ariomma bondi                  7.3500000   230.00000  TRUE      TRUE                   2
Belone belone                 13.4200000   182.00000  TRUE      TRUE                   2
Brevoortia spp                13.6000000   153.00000  TRUE      TRUE                   2
Clupeonella cultriventris     10.2300000   114.00000  TRUE      TRUE                   2
Cyclopterus lumpus            22.8333333    11.00000  TRUE      TRUE                   2
Engraulis encrasicolus         9.5866667   112.00000  TRUE      TRUE                   2
Merlangius merlangus          18.3000000    79.00000  TRUE      TRUE                   2
Pinirampus pirinampu           7.3000000    50.00000  TRUE      TRUE                   2
Sarda sarda                   10.3850000   202.73333  TRUE      TRUE                   2
Sorubim lima                  11.1900000    42.00000  TRUE      TRUE                   2
Spicara smaris                17.4900000   104.00000  TRUE      TRUE                   2
Sprattus sprattus              9.4200000   253.00000  TRUE      TRUE                   2
Xiphias gladius               12.0000000    91.00000  TRUE      TRUE                   2
Zungaro zungaro               10.4400000    66.00000  TRUE      TRUE                   2
Abramis brama                  1.0000000   159.00000  FALSE     TRUE                   1
Ageneiosus brevifilis          0.3600000    92.00000  FALSE     TRUE                   1
Anarhichas lupus               1.6583333    48.00000  FALSE     TRUE                   1
Atherina boyeri                6.2000000   154.00000  FALSE     TRUE                   1
Atherinidae                    5.7366667   127.00000  FALSE     TRUE                   1
Balistidae                     0.5300000    69.00000  FALSE     TRUE                   1
Boops boops                    3.6400000   116.33333  FALSE     TRUE                   1
Carassius carassius            1.5000000   101.00000  FALSE     TRUE                   1
Carcharhinus limbatus          0.4598000   270.00000  FALSE     TRUE                   1
Carcinus maenas                0.5000000    60.00000  FALSE     TRUE                   1
Centrophorus squamosus         0.9900000   180.00000  FALSE     TRUE                   1
Centroscyllium fabricii        0.7000000    69.00000  FALSE     TRUE                   1
Centroscymnus coelolepis       0.9500000   214.00000  FALSE     TRUE                   1
Chaenocephalus aceratus        3.0600000    75.00000  FALSE     TRUE                   1
Chamelea gallina               1.1740000   100.44444  FALSE     TRUE                   1
Channa striata                 0.9900000   149.00000  FALSE     TRUE                   1
Chelidonichthyus lucernus      1.2466667   156.33333  FALSE     TRUE                   1
Chondrostoma nasus             1.3000000   101.00000  FALSE     TRUE                   1
Clarias gariepinus             3.2100000   129.00000  FALSE     TRUE                   1
Clarias macrocephalus          1.0300000    94.00000  FALSE     TRUE                   1
Coregonus lavaretus            2.6666667   146.66667  FALSE     TRUE                   1
Crassostrea rhizophorae        1.6500000   232.80000  FALSE     TRUE                   1
Cyprinus carpio                1.9633333   135.00000  FALSE     TRUE                   1
Dasyatis americana             0.6013000   270.50000  FALSE     TRUE                   1
Dasyatis sabina                0.4550000   253.50000  FALSE     TRUE                   1
Decapterus punctatus           1.7500000   240.50000  FALSE     TRUE                   1
Dentex dentex                  1.6100000   238.00000  FALSE     TRUE                   1
Dicentrarchus labrax           3.3400000   182.50000  FALSE     TRUE                   1
Diplodus puntazzo              2.2366667   146.00000  FALSE     TRUE                   1
Eriocheir sinensis             1.8000000   165.00000  FALSE     TRUE                   1
Etrumeus teres                 1.3040000   219.20000  FALSE     TRUE                   1
Gadus morhua                   0.6233333    84.00000  FALSE     TRUE                   1
Gasterochisma melampus         2.0000000   264.00000  FALSE     TRUE                   1
Hemisorubim platyrhynchos      6.3500000   103.00000  FALSE     TRUE                   1
Isurus oxyrinchus              1.2000000   220.00000  FALSE     TRUE                   1
Lamna nasus                    1.3000000   138.00000  FALSE     TRUE                   1
Lampris guttatus               1.0000000   259.00000  FALSE     TRUE                   1
Lates calcarifer               0.6700000   202.50000  FALSE     TRUE                   1
Leuciscus cephalus             1.3000000   159.00000  FALSE     TRUE                   1
Leuciscus idus                 1.6000000   138.00000  FALSE     TRUE                   1
Lithognathus mormyrus          1.2100000   117.00000  FALSE     TRUE                   1
Liza aurata                    3.6100000   226.00000  FALSE     TRUE                   1
Liza ramada                    1.3400000    63.66667  FALSE     TRUE                   1
Loligo vulgaris                1.8025000    96.50000  FALSE     TRUE                   1
Macrodon ancylodon             0.5600000   145.00000  FALSE     TRUE                   1
Macrourus berglax              0.6100000    58.00000  FALSE     TRUE                   1
Melanogrammus aeglefinus       0.5533333    91.00000  FALSE     TRUE                   1
Menticirrhus americanus        6.7000000   169.00000  FALSE     TRUE                   1
Merluccius merluccius          0.7600000    82.50000  FALSE     TRUE                   1
Merluccius productus           0.7300000    63.00000  FALSE     TRUE                   1
Micorpogonias furnieri         1.2900000   211.00000  FALSE     TRUE                   1
Microstomus pacificus          1.0000000    61.00000  FALSE     TRUE                   1
Mora moro                      0.4100000    85.00000  FALSE     TRUE                   1
Morone saxatilis               2.4885714   147.57143  FALSE     TRUE                   1
Mugil cephalus                 2.1100000   167.00000  FALSE     TRUE                   1
Mullus barbatus                6.1225000   117.12500  FALSE     TRUE                   1
Mytilus galloprovincialis      2.2700000   150.25000  FALSE     TRUE                   1
Nephrops norvegicus            0.6450000    89.75000  FALSE     TRUE                   1
Notothenia neglecta            3.1900000   141.00000  FALSE     TRUE                   1
Octopus vulgaris               0.8200000    16.00000  FALSE     TRUE                   1
Oncorhynchus tshawytscha       7.0000000    94.50000  FALSE     TRUE                   1
Oreochromis niloticus          0.9666667   109.50000  FALSE     TRUE                   1
Pagellus bogaraveo             0.5633333   220.00000  FALSE     TRUE                   1
Pagrus caeruleocinctus         1.0200000   105.00000  FALSE     TRUE                   1
Pagrus pagrus                  0.8600000   229.00000  FALSE     TRUE                   1
Palinurus elephas              1.6066667   104.00000  FALSE     TRUE                   1
Parona signata                 5.5000000   211.00000  FALSE     TRUE                   1
Penaeus kerathurus             1.7000000    30.00000  FALSE     TRUE                   1
Perca fluviatilis              0.9166667   105.33333  FALSE     TRUE                   1
Pimelodus argenteus            0.3500000    59.00000  FALSE     TRUE                   1
Pimelodus maculatus            5.5500000    91.00000  FALSE     TRUE                   1
Pleuronectes platessa          1.7000000    37.00000  FALSE     TRUE                   1
Pogonias cromis                1.2000000   153.00000  FALSE     TRUE                   1
Pollachius virens              0.8500000   120.00000  FALSE     TRUE                   1
Portunus pelagicus             0.9150000   165.75000  FALSE     TRUE                   1
Rhizoprionodon terraenovae     0.5200000   262.00000  FALSE     TRUE                   1
Rutilus frisii                 4.1200000   228.00000  FALSE     TRUE                   1
Sander lucioperca              0.6360000   147.71429  FALSE     TRUE                   1
Sardina pilchardus             2.8316667   168.42857  FALSE     TRUE                   1
Sardinella aurita              5.6566667   174.12500  FALSE     TRUE                   1
Sarpa salpa                    0.9500000   101.00000  FALSE     TRUE                   1
Sciaena umbra                  1.4700000   213.00000  FALSE     TRUE                   1
Scomber japonicus              3.2933333   233.71429  FALSE     TRUE                   1
Scorpaena porcus               0.8000000    93.00000  FALSE     TRUE                   1
Scorpaena scrofa               0.7433333   145.00000  FALSE     TRUE                   1
Sebastes marinus               3.0430000    77.00000  FALSE     TRUE                   1
Sebastes pinniger              3.3100000   131.00000  FALSE     TRUE                   1
Sepia officinalis              1.3355556    65.00000  FALSE     TRUE                   1
Silurus glanis                 2.0200000    91.00000  FALSE     TRUE                   1
Solea solea                    0.7400000    70.50000  FALSE     TRUE                   1
Spondyliosoma cantharus        0.7370000   138.00000  FALSE     TRUE                   1
Theragra chalcogramma          0.7900000    14.00000  FALSE     TRUE                   1
Thunnus alalunga               5.0000000   268.00000  FALSE     TRUE                   1
Tinca tinca                    0.6100000   138.00000  FALSE     TRUE                   1
Trachurus lathami              1.5000000   174.40000  FALSE     TRUE                   1
Trachurus mediterraneus        4.4300000   201.15385  FALSE     TRUE                   1
Trachurus trachurus            3.5500000   130.64706  FALSE     TRUE                   1
Trichiurus lepturus            2.7500000   164.00000  FALSE     TRUE                   1
  
15 out of 113 hit 2/2 RDI targets for protein and fat, and 98 out of 113 hit the protein RDI target.
macro.species <- macro.100$species %>% droplevels()
intersect(macro.100$species, ntbl$species)


```r
ntbl.macro <- ntbl %>% 
  filter(species %in% c("Alosa alosa", "Ariomma bondi", "Belone belone", "Brevoortia spp", "Clupeonella cultriventris", "Cyclopterus lumpus", "Engraulis encrasicolus", "Merlangius merlangus", "Pinirampus pirinampu", "Sarda sarda", "Sorubim lima", "Spicara smaris", "Sprattus sprattus", "Xiphias gladius", "Zungaro zungaro"))

# 
# summary(ntbl.macro$max_size)
# summary(ntbl$max_size)
# 
# ntbl$species <- macro.species[ntbl$species]


# inner_join(ntbl,macro.species)
# macro.100per <- ntbl %>%
#     filter(grepl(macro.species, ntbl$species))
# 
# subset(ntbl, species == c("macro.species"))



ntbl.micro <- ntbl %>% 
  group_by(species) %>% 
    summarise(mean.CA = mean(CA_mg, na.rm = TRUE),
            mean.EPA = mean(EPA_g, na.rm = TRUE), 
            mean.DHA = mean(DHA_g, na.rm = TRUE), 
            mean.ZN = mean(ZN_mg, na.rm = TRUE), 
            mean.FE = mean(FE_mg, na.rm = TRUE)) %>% 
  mutate(RDI.CA = (mean.CA > 300)) %>% 
  mutate(RDI.FE = (mean.FE > 4.5)) %>% 
  mutate(RDI.ZN = (mean.ZN > 2.75)) %>% 
  mutate(RDI.EPA = (mean.EPA > 0.25)) %>% 
  mutate(RDI.DHA = (mean.DHA > 0.25)) %>% 
  mutate(RDI.micro.tot = rowSums(.[7:11])) %>% 
  filter(!is.na(RDI.micro.tot)) %>% 
  arrange(desc(RDI.micro.tot))
  knitr::kable(ntbl.micro)
```



species                          mean.CA    mean.EPA    mean.DHA      mean.ZN      mean.FE  RDI.CA   RDI.FE   RDI.ZN   RDI.EPA   RDI.DHA    RDI.micro.tot
--------------------------  ------------  ----------  ----------  -----------  -----------  -------  -------  -------  --------  --------  --------------
Rapana spp                    628.000000   0.0006685   0.0007103    9.4500000   75.0000000  TRUE     TRUE     TRUE     FALSE     FALSE                  3
Trachurus trachurus           272.036667   0.2782049   0.5692811    2.2530583    7.2991667  FALSE    TRUE     FALSE    TRUE      TRUE                   3
Cipangopaludina chinensis    1200.000000   0.0033425   0.0036767    8.1000000    4.3000000  TRUE     FALSE    TRUE     FALSE     FALSE                  2
Clarias gariepinus            832.000000   0.0763066   0.2219787    8.0000000    3.0000000  TRUE     FALSE    TRUE     FALSE     FALSE                  2
Cyclopterus lumpus              7.900000   1.6074610   1.6662046    0.6100000    0.4600000  FALSE    FALSE    FALSE    TRUE      TRUE                   2
Oncorhynchus tshawytscha       26.000000   0.4400000   0.5500000    0.4400000    0.3000000  FALSE    FALSE    FALSE    TRUE      TRUE                   2
Oreochromis niloticus         883.000000   0.0360158   0.1443437    7.0000000    3.0000000  TRUE     FALSE    TRUE     FALSE     FALSE                  2
Sarda sarda                    47.815833   0.3209793   0.1521186    0.9820833    6.1841667  FALSE    TRUE     FALSE    TRUE      FALSE                  2
Thunnus alalunga               18.000000   0.2939300   0.8591800    0.5000000    0.6000000  FALSE    FALSE    FALSE    TRUE      TRUE                   2
Chamelea gallina              218.700000   0.1436040   0.1291647    1.2560000    7.6760000  FALSE    TRUE     FALSE    FALSE     FALSE                  1
Coregonus clupeaformis          5.666667   0.0766667   0.3000000    0.4666667    0.2433333  FALSE    FALSE    FALSE    FALSE     TRUE                   1
Crassostrea rhizophorae        13.140000   0.1128395   0.0619544   11.8710630    2.3900000  FALSE    FALSE    TRUE     FALSE     FALSE                  1
Cyprinidae                    753.335000   0.0150339   0.0662677    2.5424678    3.3475000  TRUE     FALSE    FALSE    FALSE     FALSE                  1
Dentex spp                    249.290000   0.1397982   0.4284139    0.6116600    0.7100000  FALSE    FALSE    FALSE    FALSE     TRUE                   1
Gasterochisma melampus         11.000000   0.0878730   0.3273700    0.5000000    0.9000000  FALSE    FALSE    FALSE    FALSE     TRUE                   1
Lamna nasus                    10.000000   0.0310271   0.2888730    0.4000000    1.8000000  FALSE    FALSE    FALSE    FALSE     TRUE                   1
Liza aurata                     6.900000   0.2674729   0.0659872    0.1930320    0.1600000  FALSE    FALSE    FALSE    TRUE      FALSE                  1
Tilapia spp                   621.600000   0.0391667   0.1331668    1.3209000    0.8300000  TRUE     FALSE    FALSE    FALSE     FALSE                  1
Patagonotothen ramsayi          8.635000   0.0971383   0.2635418    0.6825000    0.3265000  FALSE    FALSE    FALSE    FALSE     TRUE                   1
Pollachius virens              10.000000   0.0718836   0.2541632    0.3800000    0.2500000  FALSE    FALSE    FALSE    FALSE     TRUE                   1
Salvelinus fontinalis           7.900000   0.1600000   0.4150000    0.8000000    0.2550000  FALSE    FALSE    FALSE    FALSE     TRUE                   1
Salvelinus namaycush            3.000000   0.1400000   0.3000000    0.5000000    0.2000000  FALSE    FALSE    FALSE    FALSE     TRUE                   1
Sardinella spp                398.520000   0.0040562   0.0326811    1.5670200    1.8300000  TRUE     FALSE    FALSE    FALSE     FALSE                  1
Sebastes marinus               13.000000   0.1911752   0.3138082    0.3100000    0.2000000  FALSE    FALSE    FALSE    FALSE     TRUE                   1
Xiphias gladius                39.000000   0.2431660   1.3263600    1.3000000    1.2000000  FALSE    FALSE    FALSE    FALSE     TRUE                   1
Abramis brama                  26.848333   0.0932200   0.1208700    0.5675133    0.3416667  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Anarhichas lupus                6.800000   0.1610413   0.1485772    0.7800000    0.1500000  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Aphanopus carbo                12.950000   0.0451881   0.1316034    0.3850000    0.1000000  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Carassius carassius            58.000000   0.0753900   0.0502600    0.5500000    0.8500000  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Channa striata                 71.000000   0.0056208   0.0841974    0.7000000    0.3000000  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Chondrostoma nasus             55.000000   0.0641940   0.0962910    0.5000000    0.9500000  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Clarias batrachus             299.000000   0.0531200   0.0398400    0.9000000    0.6000000  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Coregonus lavaretus            33.520000   0.1861477   0.1785328    0.6088333    0.3633333  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Cyprinus carpio                47.483333   0.0742065   0.0569135    0.8037920    0.4933333  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Esox lucius                     9.600000   0.0495096   0.2119175    0.8992122    0.1000000  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Gadus macrocephalus            11.800000   0.0546799   0.1440613    0.3839333    0.1800000  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Gadus morhua                    8.700000   0.0510784   0.1285803    0.3400000    0.1200000  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Isurus oxyrinchus              12.000000   0.0253916   0.2050860    0.4000000    1.2000000  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Lampris guttatus                6.000000   0.0300200   0.1659000    0.3000000    1.0000000  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Leuciscus cephalus             61.500000   0.0748930   0.0641940    0.5000000    0.6000000  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Leuciscus idus                 60.000000   0.1282310   0.0674900    0.5500000    0.5500000  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Melanogrammus aeglefinus        7.800000   0.0541585   0.1236772    0.2900000    0.1100000  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Merluccius productus            9.800000   0.0694674   0.1188103    0.3510500    0.2500000  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Microstomus pacificus          14.533333   0.0800000   0.0900000    0.3887667    0.2133333  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Nephrops norvegicus            90.666667   0.0372172   0.0446645    1.5100000    2.2166667  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Pagellus bogaraveo             57.333333   0.0180000   0.2173333    0.2916667    0.1600000  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Perca flavescens                2.990000   0.0003849   0.0565033    0.1353880    0.0900000  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Perca fluviatilis              32.470000   0.0411597   0.1432096    0.5200358    0.1425000  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Pleuronectes platessa          28.000000   0.1740541   0.1581016    0.4800000    0.1900000  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Portunus pelagicus            113.500000   0.0891160   0.0739906    2.4500000    0.3750000  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Rutilus frisii                  7.590000   0.2273698   0.1271335    0.1069980    0.3400000  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Sander lucioperca              55.520000   0.0231288   0.0483198    0.7144137    0.3940000  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Silurus glanis                 25.500000   0.1299152   0.2142431    0.7033333    0.6000000  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Theragra chalcogramma          12.366667   0.0491900   0.1064163    0.3887000    0.1933333  FALSE    FALSE    FALSE    FALSE     FALSE                  0
Tinca tinca                    63.000000   0.0371159   0.0715898    1.1000000    0.8500000  FALSE    FALSE    FALSE    FALSE     FALSE                  0

```r
ntbl.micro.2 <- ntbl %>% filter(species %in% c("Cipangopaludina chinensis", "Clarias gariepinus", "Cyclopterus lumpus", "Oncorhynchus tshawytscha", "Oreochromis niloticus", "Sarda sarda", "Thunnus alalunga"))

summary(ntbl.micro.2$max_size)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   3.658   3.658   8.357  13.880  25.610  57.850       1
```

```r
summary(ntbl$max_size, subset = species == c("Rapana spp", "Trachurus trachurus"))
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
##    0.000    0.680    2.997   30.280    9.862 6077.000      205
```

```r
# mean size of species that reach 3/5 nutrients' RDI is 30.28, mean size of those that reach 2 of RDI is 13.88

#' 2 hit 3/5 of RDI micronutrient targets, 7 hit 2/5 targets, 16 hit 1/5 targets  #' (out of 55)
```


```r
# ?FD
# library(FD)
# ntbl.RDI.mic
# ntbl.matrix.mic <- data.matrix(ntbl.RDI.mic[, 2:6])
# rownames(ntbl.matrix.mic) <- ntbl.RDI.mic$species
# 
# FD.mic <- dbFD(ntbl.matrix.mic)
# FD.mic
# 
# 
# ntbl.mac.matrix <- data.matrix(ntbl.RDI.mac[, 2:3])
# rownames(ntbl.mac.matrix) <- ntbl.RDI.mac$species
# 
# FD.mac <- dbFD(ntbl.mac.matrix)
# (as.data.frame(FD.mac))
# (as.data.frame(FD.mic))
```


```r
# ntbl.mic.matrix <- data.matrix(ntbl.RDI.mic[, 2:6])
# rownames(ntbl.mic.matrix) <- ntbl.RDI.mic$species
# 
# ntbl.mic.matrix
# 
# mydist <- function(x) dist(x, method = "euclidian")
# myhclust <- function(x) hclust(x, method = "average")
# 
# tree <- myhclust(mydist(ntbl.mic.matrix))
# plot(tree)
# rect.hclust(tree, k = 7)
```

