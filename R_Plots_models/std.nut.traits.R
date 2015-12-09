###March 10 2015
library(MuMIn)
library(arm)
library(visreg)

##EPA models
EPA.1 <- lm(log(EPA_g) ~ logmass*TL + logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmass_EPA_Feb27)
EPA.2 <- lm(log(EPA_g) ~ logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmass_EPA_Feb27)
EPA.3 <- lm(log(EPA_g) ~ logmass*TL + logmass*Habitat, na.action="na.fail", data=nutmass_EPA_Feb27)
EPA.4 <- lm(log(EPA_g) ~ logmass*TL + logmass*Abs_lat, na.action="na.fail", data=nutmass_EPA_Feb27)
EPA.5 <- lm(log(EPA_g) ~ logmass*TL, na.action="na.fail", data=nutmass_EPA_Feb27)
EPA.6 <- lm(log(EPA_g) ~ logmass*Abs_lat, na.action="na.fail", data=nutmass_EPA_Feb27)
EPA.7 <- lm(log(EPA_g) ~ logmass*Habitat, na.action="na.fail", data=nutmass_EPA_Feb27)
EPA.8 <- lm(log(EPA_g) ~ logmass + Abs_lat + Habitat, na.action="na.fail", data=nutmass_EPA_Feb27)
EPA.8b <- lm(log(EPA_g) ~ TL + logmass + Abs_lat + Habitat, na.action="na.fail", data=nutmass_EPA_Feb27)

EPA.9 <- lm(log(EPA_g) ~ Abs_lat + Habitat, na.action="na.fail", data=nutmass_EPA_Feb27)
EPA.10 <- lm(log(EPA_g) ~ logmass + Habitat, na.action="na.fail", data=nutmass_EPA_Feb27)
EPA.11 <- lm(log(EPA_g) ~ logmass + Abs_lat, na.action="na.fail", data=nutmass_EPA_Feb27)
EPA.12 <- lm(log(EPA_g) ~ logmass, na.action="na.fail", data=nutmass_EPA_Feb27)
EPA.13 <- lm(log(EPA_g) ~ Abs_lat, na.action="na.fail", data=nutmass_EPA_Feb27)
EPA.14 <- lm(log(EPA_g) ~ Habitat, na.action="na.fail", data=nutmass_EPA_Feb27)

model.sel(EPA.1, EPA.2, EPA.3, EPA.4, EPA.5, EPA.6, EPA.7, EPA.8, EPA.8b, EPA.9, EPA.10, EPA.11, EPA.12, EPA.13, EPA.14)
coef.EPA.avg <- coef(model.avg(EPA.1, EPA.2))
confint.EPA.avg <- confint(model.avg(EPA.1, EPA.2))
coef.table.EPA <- cbind(coef.EPA.avg, confint.EPA.avg)
coef.table.EPA
summary(model.avg(EPA.1, EPA.2))

EPA.lat <- lm(log(EPA_g) ~ Abs_lat, data=nutmass_EPA_Feb27)
EPA.mass <- lm(log(EPA_g) ~ logmass, data=nutmass_EPA_Feb27)

visreg(EPA.lat)
visreg(EPA.mass)
summary(EPA.mass)

###standardized models
EPA.1 <- standardize(lm(log(EPA_g) ~ logmass*TL + logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmass_EPA_Feb27))
EPA.2 <- standardize(lm(log(EPA_g) ~ logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmass_EPA_Feb27))
EPA.3 <- standardize(lm(log(EPA_g) ~ logmass*TL + logmass*Habitat, na.action="na.fail", data=nutmass_EPA_Feb27))
EPA.4 <- standardize(lm(log(EPA_g) ~ logmass*TL + logmass*Abs_lat, na.action="na.fail", data=nutmass_EPA_Feb27))
EPA.5 <- standardize(lm(log(EPA_g) ~ logmass*TL, na.action="na.fail", data=nutmass_EPA_Feb27))
EPA.6 <- standardize(lm(log(EPA_g) ~ logmass*Abs_lat, na.action="na.fail", data=nutmass_EPA_Feb27))
EPA.7 <- standardize(lm(log(EPA_g) ~ logmass*Habitat, na.action="na.fail", data=nutmass_EPA_Feb27))
EPA.8 <- standardize(lm(log(EPA_g) ~ logmass + Abs_lat + Habitat, na.action="na.fail", data=nutmass_EPA_Feb27))
EPA.8b <- standardize(lm(log(EPA_g) ~ TL + logmass + Abs_lat + Habitat, na.action="na.fail", data=nutmass_EPA_Feb27))
EPA.9 <- standardize(lm(log(EPA_g) ~ Abs_lat + Habitat, na.action="na.fail", data=nutmass_EPA_Feb27))
EPA.10 <- standardize(lm(log(EPA_g) ~ logmass + Habitat, na.action="na.fail", data=nutmass_EPA_Feb27))
EPA.11 <- standardize(lm(log(EPA_g) ~ logmass + Abs_lat, na.action="na.fail", data=nutmass_EPA_Feb27))
EPA.12 <- standardize(lm(log(EPA_g) ~ logmass, na.action="na.fail", data=nutmass_EPA_Feb27))
EPA.13 <- standardize(lm(log(EPA_g) ~ Abs_lat, na.action="na.fail", data=nutmass_EPA_Feb27))
EPA.14 <- standardize(lm(log(EPA_g) ~ Habitat, na.action="na.fail", data=nutmass_EPA_Feb27))

model.sel(EPA.1, EPA.2, EPA.3, EPA.4, EPA.5, EPA.6, EPA.7, EPA.8, EPA.8b, EPA.9, EPA.10, EPA.11, EPA.12, EPA.13)


##EPA standardize reg coefs
EPA.1.s <- standardize(lm(log(EPA_g) ~ logmass*TL + logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmass_EPA_Feb27))
EPA.2.s <- standardize(lm(log(EPA_g) ~ logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmass_EPA_Feb27))

coef.EPA.avg.s <- coef(model.avg(EPA.1.s, EPA.2.s))
confint.EPA.avg.s <- confint(model.avg(EPA.1.s, EPA.2.s))
coef.table.EPA.s <- cbind(coef.EPA.avg.s, confint.EPA.avg.s)
coef.table.EPA.s

coefplot(EPA.2.s)
coefplot(EPA.1.s, main="EPA coefficients", cex.pts=1.5)
summary(model.avg(EPA.1.s, EPA.2.s))
summary(EPA.2.s)


##DHA models
DHA.1 <- lm(log(DHA_g) ~ logmass*TL + logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassDHA_Feb27)
DHA.2 <- lm(log(DHA_g) ~ logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassDHA_Feb27)
DHA.3 <- lm(log(DHA_g) ~ logmass*TL + logmass*Habitat, na.action="na.fail", data=nutmassDHA_Feb27)
DHA.4 <- lm(log(DHA_g) ~ logmass*TL + logmass*Abs_lat, na.action="na.fail", data=nutmassDHA_Feb27)
DHA.5 <- lm(log(DHA_g) ~ logmass*TL, na.action="na.fail", data=nutmassDHA_Feb27)
DHA.6 <- lm(log(DHA_g) ~ logmass*Abs_lat, na.action="na.fail", data=nutmassDHA_Feb27)
DHA.7 <- lm(log(DHA_g) ~ logmass*Habitat, na.action="na.fail", data=nutmassDHA_Feb27)
DHA.8 <- lm(log(DHA_g) ~ logmass + Abs_lat + Habitat, na.action="na.fail", data=nutmassDHA_Feb27)
DHA.8b <- lm(log(DHA_g) ~ TL + logmass + Abs_lat + Habitat, na.action="na.fail", data=nutmassDHA_Feb27)
DHA.9 <- lm(log(DHA_g) ~ Abs_lat + Habitat, na.action="na.fail", data=nutmassDHA_Feb27)
DHA.10 <- lm(log(DHA_g) ~ logmass + Habitat, na.action="na.fail", data=nutmassDHA_Feb27)
DHA.11 <- lm(log(DHA_g) ~ logmass + Abs_lat, na.action="na.fail", data=nutmassDHA_Feb27)
DHA.12 <- lm(log(DHA_g) ~ logmass, na.action="na.fail", data=nutmassDHA_Feb27)
DHA.13 <- lm(log(DHA_g) ~ Abs_lat, na.action="na.fail", data=nutmassDHA_Feb27)
DHA.14 <- lm(log(DHA_g) ~ Habitat, na.action="na.fail", data=nutmassDHA_Feb27)

model.sel(DHA.1, DHA.2, DHA.3, DHA.4, DHA.5, DHA.6, DHA.7, DHA.8, DHA.8b, DHA.9, DHA.10, DHA.11, DHA.12, DHA.13, DHA.14)
coef.DHA <- coef(DHA.1)
confint.DHA <- confint(DHA.1)
coef.table.DHA <- cbind(coef.DHA, confint.DHA)
coef.table.DHA
summary(DHA.1)

##standardized DHA models
DHA.1 <- standardize(lm(log(DHA_g) ~ logmass*TL + logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassDHA_Feb27))
DHA.2 <- standardize(lm(log(DHA_g) ~ logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassDHA_Feb27))
DHA.3 <- standardize(lm(log(DHA_g) ~ logmass*TL + logmass*Habitat, na.action="na.fail", data=nutmassDHA_Feb27))
DHA.4 <- standardize(lm(log(DHA_g) ~ logmass*TL + logmass*Abs_lat, na.action="na.fail", data=nutmassDHA_Feb27))
DHA.5 <- standardize(lm(log(DHA_g) ~ logmass*TL, na.action="na.fail", data=nutmassDHA_Feb27))
DHA.6 <- standardize(lm(log(DHA_g) ~ logmass*Abs_lat, na.action="na.fail", data=nutmassDHA_Feb27))
DHA.7 <- standardize(lm(log(DHA_g) ~ logmass*Habitat, na.action="na.fail", data=nutmassDHA_Feb27))
DHA.8 <- standardize(lm(log(DHA_g) ~ logmass + Abs_lat + Habitat, na.action="na.fail", data=nutmassDHA_Feb27))
DHA.8b <- standardize(lm(log(DHA_g) ~ TL + logmass + Abs_lat + Habitat, na.action="na.fail", data=nutmassDHA_Feb27))
DHA.9 <- standardize(lm(log(DHA_g) ~ Abs_lat + Habitat, na.action="na.fail", data=nutmassDHA_Feb27))
DHA.10 <- standardize(lm(log(DHA_g) ~ logmass + Habitat, na.action="na.fail", data=nutmassDHA_Feb27))
DHA.11 <- standardize(lm(log(DHA_g) ~ logmass + Abs_lat, na.action="na.fail", data=nutmassDHA_Feb27))
DHA.12 <- standardize(lm(log(DHA_g) ~ logmass, na.action="na.fail", data=nutmassDHA_Feb27))
DHA.13 <- standardize(lm(log(DHA_g) ~ Abs_lat, na.action="na.fail", data=nutmassDHA_Feb27))
DHA.14 <- standardize(lm(log(DHA_g) ~ Habitat, na.action="na.fail", data=nutmassDHA_Feb27))

model.sel(DHA.1, DHA.2, DHA.3, DHA.4, DHA.5, DHA.6, DHA.7, DHA.8, DHA.8b, DHA.9, DHA.10, DHA.11, DHA.12, DHA.13, DHA.14)
coef.DHA <- coef(DHA.1)
confint.DHA <- confint(DHA.1)
coef.table.DHA <- cbind(coef.DHA, confint.DHA)
coef.table.DHA
summary(DHA.1)
coefplot(DHA.1, main="DHA coefficients", cex.pts=1.5)
summary(DHA.1)

###CA models
nutmassCA_Feb24 <- read.csv("/Users/Joey/Desktop/Nutrient_databases/nutmassCA_Feb24.csv")
CA.1 <- lm(log(CA_mg) ~ logmass*TL + logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassCA_Feb24)
CA.2 <- lm(log(CA_mg) ~ logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassCA_Feb24)
CA.3 <- lm(log(CA_mg) ~ logmass*TL + logmass*Habitat, na.action="na.fail", data=nutmassCA_Feb24)
CA.4 <- lm(log(CA_mg) ~ logmass*TL + logmass*Abs_lat, na.action="na.fail", data=nutmassCA_Feb24)
CA.5 <- lm(log(CA_mg) ~ logmass*TL, na.action="na.fail", data=nutmassCA_Feb24)
CA.6 <- lm(log(CA_mg) ~ logmass*Abs_lat, na.action="na.fail", data=nutmassCA_Feb24)
CA.7 <- lm(log(CA_mg) ~ logmass*Habitat, na.action="na.fail", data=nutmassCA_Feb24)
CA.8 <- lm(log(CA_mg) ~ logmass + Abs_lat + Habitat, na.action="na.fail", data=nutmassCA_Feb24)
CA.8b <- lm(log(CA_mg) ~ TL + logmass + Abs_lat + Habitat, na.action="na.fail", data=nutmassCA_Feb24)
CA.9 <- lm(log(CA_mg) ~ Abs_lat + Habitat, na.action="na.fail", data=nutmassCA_Feb24)
CA.10 <- lm(log(CA_mg) ~ logmass + Habitat, na.action="na.fail", data=nutmassCA_Feb24)
CA.11 <- lm(log(CA_mg) ~ logmass + Abs_lat, na.action="na.fail", data=nutmassCA_Feb24)
CA.12 <- lm(log(CA_mg) ~ logmass, na.action="na.fail", data=nutmassCA_Feb24)
CA.13 <- lm(log(CA_mg) ~ Abs_lat, na.action="na.fail", data=nutmassCA_Feb24)
CA.14 <- lm(log(CA_mg) ~ Habitat, na.action="na.fail", data=nutmassCA_Feb24)

model.sel(CA.1, CA.2, CA.3, CA.4, CA.5, CA.6, CA.7, CA.8, CA.8b, CA.9, CA.10, CA.11, CA.12, CA.13, CA.14)
coef.CA <- coef(model.avg(CA.8, CA.8b))
confint.CA <- confint(model.avg(CA.8, CA.8b))
coef.table.CA <- cbind(coef.CA, confint.CA)
coef.table.CA
summary(CA.8)
1.10^(-0.151034)
exp(-0.075334)


###standardized CA models
CA.1 <- standardize(lm(log(CA_mg) ~ logmass*TL + logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassCA_Feb24))
CA.2 <- standardize(lm(log(CA_mg) ~ logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassCA_Feb24))
CA.3 <- standardize(lm(log(CA_mg) ~ logmass*TL + logmass*Habitat, na.action="na.fail", data=nutmassCA_Feb24))
CA.4 <- standardize(lm(log(CA_mg) ~ logmass*TL + logmass*Abs_lat, na.action="na.fail", data=nutmassCA_Feb24))
CA.5 <- standardize(lm(log(CA_mg) ~ logmass*TL, na.action="na.fail", data=nutmassCA_Feb24))
CA.6 <- standardize(lm(log(CA_mg) ~ logmass*Abs_lat, na.action="na.fail", data=nutmassCA_Feb24))
CA.7 <- standardize(lm(log(CA_mg) ~ logmass*Habitat, na.action="na.fail", data=nutmassCA_Feb24))
CA.8 <- standardize(lm(log(CA_mg) ~ logmass + Abs_lat + Habitat, na.action="na.fail", data=nutmassCA_Feb24))
CA.8b <- standardize(lm(log(CA_mg) ~ TL + logmass + Abs_lat + Habitat, na.action="na.fail", data=nutmassCA_Feb24))
CA.9 <- standardize(lm(log(CA_mg) ~ Abs_lat + Habitat, na.action="na.fail", data=nutmassCA_Feb24))
CA.10 <- standardize(lm(log(CA_mg) ~ logmass + Habitat, na.action="na.fail", data=nutmassCA_Feb24))
CA.11 <- standardize(lm(log(CA_mg) ~ logmass + Abs_lat, na.action="na.fail", data=nutmassCA_Feb24))
CA.12 <- standardize(lm(log(CA_mg) ~ logmass, na.action="na.fail", data=nutmassCA_Feb24))
CA.13 <- standardize(lm(log(CA_mg) ~ Abs_lat, na.action="na.fail", data=nutmassCA_Feb24))
CA.14 <- standardize(lm(log(CA_mg) ~ Habitat, na.action="na.fail", data=nutmassCA_Feb24))

model.sel(CA.1, CA.2, CA.3, CA.4, CA.5, CA.6, CA.7, CA.8, CA.8b, CA.9, CA.10, CA.11, CA.12, CA.13)
coef.CA <- coef(model.avg(CA.8, CA.8b))
confint.CA <- confint(model.avg(CA.8, CA.8b))
coef.table.CA <- cbind(coef.CA, confint.CA)
coef.table.CA
summary(model.avg(CA.8, CA.8b))
coefplot(CA.8)
coefplot(CA.8b, main="CA coefficients", cex.pts=1.5)
summary(CA.8b)
summary(CA.8)
nut.CA <- nutmassCA_Feb24
summary(nut.CA$logmass)

##ZN models

ZN.1 <- lm(log(ZN_mg) ~ logmass*TL + logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassZN_Feb27)
ZN.2 <- lm(log(ZN_mg) ~ logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassZN_Feb27)
ZN.3 <- lm(log(ZN_mg) ~ logmass*TL + logmass*Habitat, na.action="na.fail", data=nutmassZN_Feb27)
ZN.4 <- lm(log(ZN_mg) ~ logmass*TL + logmass*Abs_lat, na.action="na.fail", data=nutmassZN_Feb27)
ZN.5 <- lm(log(ZN_mg) ~ logmass*TL, na.action="na.fail", data=nutmassZN_Feb27)
ZN.6 <- lm(log(ZN_mg) ~ logmass*Abs_lat, na.action="na.fail", data=nutmassZN_Feb27)
ZN.7 <- lm(log(ZN_mg) ~ logmass*Habitat, na.action="na.fail", data=nutmassZN_Feb27)
ZN.8 <- lm(log(ZN_mg) ~ logmass + Abs_lat + Habitat, na.action="na.fail", data=nutmassZN_Feb27)
ZN.8b <- lm(log(ZN_mg) ~ TL + logmass + Abs_lat + Habitat, na.action="na.fail", data=nutmassZN_Feb27)
ZN.9 <- lm(log(ZN_mg) ~ Abs_lat + Habitat, na.action="na.fail", data=nutmassZN_Feb27)
ZN.10 <- lm(log(ZN_mg) ~ logmass + Habitat, na.action="na.fail", data=nutmassZN_Feb27)
ZN.11 <- lm(log(ZN_mg) ~ logmass + Abs_lat, na.action="na.fail", data=nutmassZN_Feb27)
ZN.12 <- lm(log(ZN_mg) ~ logmass, na.action="na.fail", data=nutmassZN_Feb27)
ZN.13 <- lm(log(ZN_mg) ~ Abs_lat, na.action="na.fail", data=nutmassZN_Feb27)
ZN.14 <- lm(log(ZN_mg) ~ Habitat, na.action="na.fail", data=nutmassZN_Feb27)

model.sel(ZN.1, ZN.2, ZN.3, ZN.4, ZN.5, ZN.6, ZN.7, ZN.8, ZN.8b, ZN.9, ZN.10, ZN.11, ZN.12, ZN.13, ZN.14)
summary(model.avg(ZN.13, ZN.11, ZN.6))

coef.ZN.avg <- coef(model.avg(ZN.13, ZN.11, ZN.6))
confint.ZN.avg <- confint(model.avg(ZN.13, ZN.11, ZN.6))
coef.table.ZN <- cbind(coef.ZN.avg, confint.ZN.avg)
coef.table.ZN
summary(ZN.13)
exp(-0.029316)

###standardized ZN models
ZN.1 <- standardize(lm(log(ZN_mg) ~ logmass*TL + logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassZN_Feb27))
ZN.2 <- standardize(lm(log(ZN_mg) ~ logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassZN_Feb27))
ZN.3 <- standardize(lm(log(ZN_mg) ~ logmass*TL + logmass*Habitat, na.action="na.fail", data=nutmassZN_Feb27))
ZN.4 <- standardize(lm(log(ZN_mg) ~ logmass*TL + logmass*Abs_lat, na.action="na.fail", data=nutmassZN_Feb27))
ZN.5 <- standardize(lm(log(ZN_mg) ~ logmass*TL, na.action="na.fail", data=nutmassZN_Feb27))
ZN.6 <- standardize(lm(log(ZN_mg) ~ logmass*Abs_lat, na.action="na.fail", data=nutmassZN_Feb27))
ZN.7 <- standardize(lm(log(ZN_mg) ~ logmass*Habitat, na.action="na.fail", data=nutmassZN_Feb27))
ZN.8 <- standardize(lm(log(ZN_mg) ~ logmass + Abs_lat + Habitat, na.action="na.fail", data=nutmassZN_Feb27))
ZN.8b <- standardize(lm(log(ZN_mg) ~ TL + logmass + Abs_lat + Habitat, na.action="na.fail", data=nutmassZN_Feb27))
ZN.9 <- standardize(lm(log(ZN_mg) ~ Abs_lat + Habitat, na.action="na.fail", data=nutmassZN_Feb27))
ZN.10 <- standardize(lm(log(ZN_mg) ~ logmass + Habitat, na.action="na.fail", data=nutmassZN_Feb27))
ZN.11 <- standardize(lm(log(ZN_mg) ~ logmass + Abs_lat, na.action="na.fail", data=nutmassZN_Feb27))
ZN.12 <- standardize(lm(log(ZN_mg) ~ logmass, na.action="na.fail", data=nutmassZN_Feb27))
ZN.13 <- standardize(lm(log(ZN_mg) ~ Abs_lat, na.action="na.fail", data=nutmassZN_Feb27))
ZN.14 <- standardize(lm(log(ZN_mg) ~ Habitat, na.action="na.fail", data=nutmassZN_Feb27))

model.sel(ZN.1, ZN.2, ZN.3, ZN.4, ZN.5, ZN.6, ZN.7, ZN.8, ZN.8b, ZN.9, ZN.10, ZN.11, ZN.12, ZN.13, ZN.14)

coef.ZN.avg <- coef(model.avg(ZN.13, ZN.11, ZN.6))
confint.ZN.avg <- confint(model.avg(ZN.13, ZN.11, ZN.6))
coef.table.ZN <- cbind(coef.ZN.avg, confint.ZN.avg)
coef.table.ZN
coefplot(ZN.6, main="ZN coefficients", cex.pts=1.5)
coefplot(ZN.13)
coefplot(ZN.11)
summary(ZN.6)
summary(ZN.11)
summary(ZN.13)

##FE models

FE.1 <- lm(log(FE_mg) ~ logmass*TL + logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassFE_feb27)
FE.2 <- lm(log(FE_mg) ~ logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassFE_feb27)
FE.3 <- lm(log(FE_mg) ~ logmass*TL + logmass*Habitat, na.action="na.fail", data=nutmassFE_feb27)
FE.4 <- lm(log(FE_mg) ~ logmass*TL + logmass*Abs_lat, na.action="na.fail", data=nutmassFE_feb27)
FE.5 <- lm(log(FE_mg) ~ logmass*TL, na.action="na.fail", data=nutmassFE_feb27)
FE.6 <- lm(log(FE_mg) ~ logmass*Abs_lat, na.action="na.fail", data=nutmassFE_feb27)
FE.7 <- lm(log(FE_mg) ~ logmass*Habitat, na.action="na.fail", data=nutmassFE_feb27)
FE.8 <- lm(log(FE_mg) ~ logmass + Abs_lat + Habitat, na.action="na.fail", data=nutmassFE_feb27)
FE.8b <- lm(log(FE_mg) ~ logmass + Abs_lat + Habitat + TL, na.action="na.fail", data=nutmassFE_feb27)
FE.9 <- lm(log(FE_mg) ~ Abs_lat + Habitat, na.action="na.fail", data=nutmassFE_feb27)
FE.10 <- lm(log(FE_mg) ~ logmass + Habitat, na.action="na.fail", data=nutmassFE_feb27)
FE.11 <- lm(log(FE_mg) ~ logmass + Abs_lat, na.action="na.fail", data=nutmassFE_feb27)
FE.12 <- lm(log(FE_mg) ~ logmass, na.action="na.fail", data=nutmassFE_feb27)
FE.13 <- lm(log(FE_mg) ~ Abs_lat, na.action="na.fail", data=nutmassFE_feb27)
FE.14 <- lm(log(FE_mg) ~ Habitat, na.action="na.fail", data=nutmassFE_feb27)

model.sel(FE.1, FE.2, FE.3, FE.4, FE.5, FE.6, FE.7, FE.8, FE.8b, FE.9, FE.10, FE.11, FE.12, FE.13, FE.14)
model.avg(FE.8, FE.9)
coef.FE.avg <- coef(model.avg(FE.8, FE.9))
confint.FE.avg <- confint(model.avg(FE.8, FE.9))
coef.table.FE <- cbind(coef.FE.avg, confint.FE.avg)
coef.table.FE
summary(FE.8)
summary(FE.9)
exp(-0.049477)

###standardized FE models


FE.1 <- standardize(lm(log(FE_mg) ~ logmass*TL + logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassFE_feb27))
FE.2 <- standardize(lm(log(FE_mg) ~ logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassFE_feb27))
FE.3 <- standardize(lm(log(FE_mg) ~ logmass*TL + logmass*Habitat, na.action="na.fail", data=nutmassFE_feb27))
FE.4 <- standardize(lm(log(FE_mg) ~ logmass*TL + logmass*Abs_lat, na.action="na.fail", data=nutmassFE_feb27))
FE.5 <- standardize(lm(log(FE_mg) ~ logmass*TL, na.action="na.fail", data=nutmassFE_feb27))
FE.6 <- standardize(lm(log(FE_mg) ~ logmass*Abs_lat, na.action="na.fail", data=nutmassFE_feb27))
FE.7 <- standardize(lm(log(FE_mg) ~ logmass*Habitat, na.action="na.fail", data=nutmassFE_feb27))
FE.8 <- standardize(lm(log(FE_mg) ~ logmass + Abs_lat + Habitat, na.action="na.fail", data=nutmassFE_feb27))
FE.8b <- standardize(lm(log(FE_mg) ~ logmass + Abs_lat + Habitat + TL, na.action="na.fail", data=nutmassFE_feb27))
FE.9 <- standardize(lm(log(FE_mg) ~ Abs_lat + Habitat, na.action="na.fail", data=nutmassFE_feb27))
FE.10 <- standardize(lm(log(FE_mg) ~ logmass + Habitat, na.action="na.fail", data=nutmassFE_feb27))
FE.11 <- standardize(lm(log(FE_mg) ~ logmass + Abs_lat, na.action="na.fail", data=nutmassFE_feb27))
FE.12 <- standardize(lm(log(FE_mg) ~ logmass, na.action="na.fail", data=nutmassFE_feb27))
FE.13 <- standardize(lm(log(FE_mg) ~ Abs_lat, na.action="na.fail", data=nutmassFE_feb27))
FE.14 <- standardize(lm(log(FE_mg) ~ Habitat, na.action="na.fail", data=nutmassFE_feb27))

model.sel(FE.1, FE.2, FE.3, FE.4, FE.5, FE.6, FE.7, FE.8, FE.8b, FE.9, FE.10, FE.11, FE.12, FE.13, FE.14)
model.avg(FE.8, FE.9)
coef.FE.avg <- coef(model.avg(FE.8, FE.9))
confint.FE.avg <- confint(model.avg(FE.8, FE.9))
coef.table.FE <- cbind(coef.FE.avg, confint.FE.avg)
coef.table.FE
coefplot(FE.8, main="FE coefficients", cex.pts=1.5)
coefplot(FE.9, main="FE coefficients", cex.pts=1.5)
summary(FE.8)
summary(FE.9)

##HG models
HG.1 <- lm(log(HG_mcg) ~ logmass*TL + logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassHG_Feb27)
HG.2 <- lm(log(HG_mcg) ~ logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassHG_Feb27)
HG.3 <- lm(log(HG_mcg) ~ logmass*TL + logmass*Habitat, na.action="na.fail", data=nutmassHG_Feb27)
HG.4 <- lm(log(HG_mcg) ~ logmass*TL + logmass*Abs_lat, na.action="na.fail", data=nutmassHG_Feb27)
HG.5 <- lm(log(HG_mcg) ~ logmass*TL, na.action="na.fail", data=nutmassHG_Feb27)
HG.6 <- lm(log(HG_mcg) ~ logmass*Abs_lat, na.action="na.fail", data=nutmassHG_Feb27)
HG.7 <- lm(log(HG_mcg) ~ logmass*Habitat, na.action="na.fail", data=nutmassHG_Feb27)
HG.8 <- lm(log(HG_mcg) ~ logmass + Abs_lat + Habitat, na.action="na.fail", data=nutmassHG_Feb27)
HG.8b <- lm(log(HG_mcg) ~ logmass + Abs_lat + Habitat + TL, na.action="na.fail", data=nutmassHG_Feb27)
HG.9 <- lm(log(HG_mcg) ~ Abs_lat + Habitat, na.action="na.fail", data=nutmassHG_Feb27)
HG.10 <- lm(log(HG_mcg) ~ logmass + Habitat, na.action="na.fail", data=nutmassHG_Feb27)
HG.11 <- lm(log(HG_mcg) ~ logmass + Abs_lat, na.action="na.fail", data=nutmassHG_Feb27)
HG.12 <- lm(log(HG_mcg) ~ logmass, na.action="na.fail", data=nutmassHG_Feb27)
HG.13 <- lm(log(HG_mcg) ~ Abs_lat, na.action="na.fail", data=nutmassHG_Feb27)
HG.14 <- lm(log(HG_mcg) ~ Habitat, na.action="na.fail", data=nutmassHG_Feb27)

model.sel(HG.1, HG.2, HG.3, HG.4, HG.5, HG.6, HG.7, HG.8, HG.8b, HG.9, HG.10, HG.11, HG.12, HG.13, HG.14)

summary(HG.8)
coef.HG <- coef(model.avg(HG.8, HG.8b))
confint.HG <- confint(model.avg(HG.8, HG.8b))
coef.table.HG <- cbind(coef.HG, confint.HG)
coef.table.HG
summary(HG.8)
summary(HG.8b)
1.10^0.24488 
exp(-0.04236606)
exp(0.42093897)

##standardized HG models
HG.1 <- standardize(lm(log(HG_mcg) ~ logmass*TL + logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassHG_Feb27))
HG.2 <- standardize(lm(log(HG_mcg) ~ logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassHG_Feb27))
HG.3 <- standardize(lm(log(HG_mcg) ~ logmass*TL + logmass*Habitat, na.action="na.fail", data=nutmassHG_Feb27))
HG.4 <- standardize(lm(log(HG_mcg) ~ logmass*TL + logmass*Abs_lat, na.action="na.fail", data=nutmassHG_Feb27))
HG.5 <- standardize(lm(log(HG_mcg) ~ logmass*TL, na.action="na.fail", data=nutmassHG_Feb27))
HG.6 <- standardize(lm(log(HG_mcg) ~ logmass*Abs_lat, na.action="na.fail", data=nutmassHG_Feb27))
HG.7 <- standardize(lm(log(HG_mcg) ~ logmass*Habitat, na.action="na.fail", data=nutmassHG_Feb27))
HG.8 <- standardize(lm(log(HG_mcg) ~ logmass + Abs_lat + Habitat, na.action="na.fail", data=nutmassHG_Feb27))
HG.8b <- standardize(lm(log(HG_mcg) ~ logmass + Abs_lat + Habitat + TL, na.action="na.fail", data=nutmassHG_Feb27))
HG.9 <- standardize(lm(log(HG_mcg) ~ Abs_lat + Habitat, na.action="na.fail", data=nutmassHG_Feb27))
HG.10 <- standardize(lm(log(HG_mcg) ~ logmass + Habitat, na.action="na.fail", data=nutmassHG_Feb27))
HG.11 <- standardize(lm(log(HG_mcg) ~ logmass + Abs_lat, na.action="na.fail", data=nutmassHG_Feb27))
HG.12 <- standardize(lm(log(HG_mcg) ~ logmass, na.action="na.fail", data=nutmassHG_Feb27))
HG.13 <- standardize(lm(log(HG_mcg) ~ Abs_lat, na.action="na.fail", data=nutmassHG_Feb27))
HG.14 <- standardize(lm(log(HG_mcg) ~ Habitat, na.action="na.fail", data=nutmassHG_Feb27))

model.sel(HG.1, HG.2, HG.3, HG.4, HG.5, HG.6, HG.7, HG.8, HG.8b, HG.9, HG.10, HG.11, HG.12, HG.13, HG.14)

summary(HG.8)
coef.HG <- coef(model.avg(HG.8, HG.8b))
confint.HG <- confint(model.avg(HG.8, HG.8b))
coef.table.HG <- cbind(coef.HG, confint.HG)
coef.table.HG
summary(HG.8b)
coefplot(HG.8b, main="HG coefficients", cex.pts=1.5)
summary(HG.8)

###Scatterplots
EPA.lat <- lm(log(EPA_g) ~ Abs_lat, data=nutmass_EPA_Feb27)
EPA.mass <- lm(log(EPA_g) ~ logmass, data=nutmass_EPA_Feb27)

visreg(EPA.lat)
visreg(EPA.mass)
summary(EPA.mass)
summary(EPA.lat)

100-100*(1.10^-0.1268)
100*(exp(0.024596))

DHA.lat <- lm(log(DHA_g) ~ Abs_lat, data=nutmassDHA_Feb27)
DHA.size <- lm(log(DHA_g) ~ logmass, data=nutmassDHA_Feb27)
visreg(DHA.lat)
visreg(DHA.size)
summary(DHA.lat)
summary(DHA.size)
100*(10*exp(0.015331))
100*(1-(1.10^-0.09145))


HG.lat <- lm(log(HG_mcg) ~ Abs_lat, data=nutmassHG_Feb27)
HG.size <- lm(log(HG_mcg) ~ logmass, data=nutmassHG_Feb27)
visreg(HG.lat)
visreg(HG.size)
summary(HG.size)
100*(1.10^0.27158)

FE.lat <- lm(log(FE_mg) ~ Abs_lat, data=nutmassFE_feb27)
FE.size <- lm(log(FE_mg) ~ logmass, data=nutmassFE_feb27)
visreg(FE.lat)
visreg(FE.size)

ZN.lat <- lm(log(ZN_mg) ~ Abs_lat, data=nutmassZN_Feb27)
ZN.size <- lm(log(ZN_mg) ~ logmass, data=nutmassZN_Feb27)
visreg(ZN.lat)
visreg(ZN.size)

CA.lat <- lm(log(CA_mg) ~ Abs_lat, data=nutmassCA_Feb24)
CA.size <- lm(log(CA_mg) ~ logmass, data=nutmassCA_Feb24)
visreg(CA.lat)
visreg(CA.size)
summary(CA.size)
summary(CA.lat)

(1-(1.10^(-0.36185)))*100
100-(100*(exp(-0.075209)))

plot(nutmassCA_Feb24$logmass ~ nutmassCA_Feb24$Abs_lat)
cor(nutmassCA_Feb24$logmass, nutmassCA_Feb24$Abs_lat)
