
#### Load packages
library(FD)
library(ggplot2)
library(broom)
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(readr)

#### Step 1. Import the data

ntbl <- read_csv("data/ntbl.csv")
str(ntbl)


#### Step 2. Create RDI profiles
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
  filter(!is.na(RDI.micro.tot)) 

#### Step 3. Turn RDI df into a matrix
ntbl.matrix.mic <- data.matrix(ntbl.RDI.mic[, 2:6])
rownames(ntbl.matrix.mic) <- ntbl.RDI.mic$species


#### Step 4. Sample 29 species from the RDI matrix randomly
randsp.data <- ntbl.matrix.mic[sample(1:length(row.names(ntbl.matrix.mic)), 29, replace = FALSE),]

### Step 5. Run the functional diversity function on the random subset
rand.fd <- dbFD(randsp.data)
rand.fd$FDiv

#### repeat this subsampling process many times...to generate a mean expected FD value.

fdiv.exp <- vector()
for (i in 1:5000) {
  randsp.data<- ntbl.matrix.mic[sample(1:length(row.names(ntbl.matrix.mic)), 29, replace = FALSE),]
  fdiv.exp[i] <- dbFD(randsp.data)$FDiv
}

hist(fdiv.exp, xlim = c(0.4,0.8))
abline(v = 0.4502677, col = "red") #this is the value of FDiv I calculated for the US species
quantile(fdiv.exp, probs = c(0.025, 0.975))

fdiv_expected <- data.frame(fdiv_expected = fdiv.exp)
write_csv(fdiv_expected, "data-processed/fdiv_expected.csv")



FEve.exp <- vector()
for (i in 1:1000) {
  randsp.data<- ntbl.matrix.mic[sample(1:length(row.names(ntbl.matrix.mic)), 29, replace = FALSE),]
  FEve.exp[i] <- dbFD(randsp.data)$FEve
}

hist(fdiv.exp, xlim = c(0.4,0.8))
abline(v = 0.4502677, col = "red") #this is the value of FDiv I calculated for the US species
quantile(fdiv.exp, probs = c(0.025, 0.975))

FEve_expected <- data.frame(FEve_expected = FEve.exp)
write_csv(FEve_expected, "data-processed/FEve_expected.csv")



mean(fdiv.exp)



#### Step 6. 
# Take the mean FDiv value, and compare my observed FDiv value to mean expected value.
