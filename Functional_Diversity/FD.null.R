
#### Load packages
library(FD)
library(ggplot2)
library(broom)
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(readr)

#### Step 1. Import the data

ntbl <- read_csv("ntbl.csv")
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
for (i in 1:1000) {
  randsp.data<- ntbl.matrix.mic[sample(1:length(row.names(ntbl.matrix.mic)), 29, replace = FALSE),]
  fdiv.obs[i] <- dbFD(randsp.data)$FDiv
}

hist(fdiv.exp)


#### Step 6. 
# Take the mean FDiv value, and compare my observed FDiv value to mean expected value.
