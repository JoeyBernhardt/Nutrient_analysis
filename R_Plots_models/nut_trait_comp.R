###March 2 2015
library(MuMIn)
library(arm)
library(visreg)
library(lm.beta)

CA_marine <- (lm(log(CA_mg) ~ logmass*TL + logmass*Abs_lat, na.action="na.fail", data=nutmassCA_Feb24, subset=Habitat=="marine"))
mod1fish <- (lm(log(CA_mg) ~ logmass*TL + logmass*Abs_lat, na.action="na.fail", data=nutmassCA_Feb24, subset=Habitat=="freshwater"))

CA_marine <- dredge(CA_marine, beta=TRUE)
dd1marine <- dredge(mod1marine, beta=TRUE)
coefplot(get.models(dd1f, 1)[[1]], cex.pts=2, vertical=TRUE, main="freshwater fish regression coefficients")
summary((get.models(dd1fish, 1)[[1]]))
summary((get.models(dd1marine, 1)[[1]]))
summary(model.avg(dd1fish, subset = delta < 2))
summary(model.avg(dd1marine, subset = delta < 2))

mod.avg <- model.avg(dd1fish, subset = delta < 2)
summary(mod.avg)

get.models(dredge(mod1fish), subset= cumsum(weight) <= .95)
get.models(dredge(mod1fish), subset= delta <2)

model.avg(dd1fish,  subset= cumsum(weight) <= .95)

delta2fresh <- lm(formula = log(CA_mg) ~ Abs_lat + logmass + 1, data = nutmassCA_Feb24, 
                  subset = Habitat == "freshwater", na.action = "na.fail")

coefplot(delta2fresh, )
summary(delta2fresh)
model.avg(dd1fish,  subset= delta<2)
coefplot(delta2fresh, cex.pts=2, main="Calcium freshwater fish regression coefficients")

best.marine.CA <- lm(formula = log(CA_mg) ~ Abs_lat + logmass + 1, data = nutmassCA_Feb24, 
                     subset = Habitat == "marine", na.action = "na.fail")
coefplot(best.marine.CA)
confint(best.marine.CA)
confint(model.avg(dd1marine, subset = delta < 2))
confint((get.models(dd1marine, 1)[[1]]))


visreg(mod1marine, xvar="logmass", by="ISSCAAP_cat")
CA.marine.size <- lm(log(CA_mg) ~ logmass, na.action="na.fail", data=nutmassCA_Feb24, subset=Habitat=="marine")
visreg(CA.marine.size)

###EPA
EPA.marine <- lm(log(EPA_g) ~ logmass*TL + logmass*Abs_lat, na.action="na.fail", data=nutmass_EPA_Feb27, subset=Habitat=="marine")
dd.EPA.marine <- dredge(EPA.marine)
summary(model.avg(dd.EPA.marine, subset = delta < 2))
confint(model.avg(dd.EPA.marine, subset = delta < 2))
confint((get.models(dd.EPA.marine, 1)[[1]]))
summary((get.models(dd.EPA.marine, 1)[[1]]))

EPA.fresh <- lm(log(EPA_g) ~ logmass*TL + logmass*Abs_lat + logmass*ISSCAAP_cat, na.action="na.fail", data=nutmass_EPA_Feb27, subset=Habitat=="freshwater")
dd.EPA.fresh <- dredge(EPA.fresh)
summary(model.avg(dd.EPA.fresh, subset = delta < 2))
confint(model.avg(dd.EPA.fresh, subset = delta < 2))
confint((get.models(dd.EPA.fresh, 1)[[1]]))
summary((get.models(dd.EPA.fresh, 1)[[1]]))

EPA.all <- lm(log(EPA_g) ~ logmass*TL+ logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmass_EPA_Feb27, subset=Habitat=="!brackish")
dd.EPA.all <- dredge(EPA.all)
summary(model.avg(dd.EPA.all, subset = delta < 2))
coefs <- coef(model.avg(dd.EPA.all, subset = delta < 2))
confint <- confint(model.avg(dd.EPA.all, subset = delta < 2))
coef.table.EPA <- cbind(coefs, confint)
coef.table.EPA
summary(nutmass_EPA_Feb27$Habitat)

confint((get.models(dd.EPA.all, 1)[[1]]))
summary((get.models(dd.EPA.all, 1)[[1]]))
summary(nutmass_EPA_Feb27$Habitat)

coef(model.avg(dd.EPA.all, subset = delta < 2))

##EPA model 1
summary(model.avg(dd.EPA.all, subset = delta < 2))
summary(get.models(dd.EPA.all, 1)[[1]])
confint.EPA.1 <-confint((get.models(dd.EPA.all, 1)[[1]]))
coef.EPA.1 <-coef((get.models(dd.EPA.all, 1)[[1]]))
coef.table.EPA.1 <- cbind(coef.EPA.1, confint.EPA.1)
coef.table.EPA.1

##EPA model 2
confint.EPA.2 <-confint((get.models(dd.EPA.all, 2)[[1]]))
coef.EPA.2 <-coef((get.models(dd.EPA.all, 2)[[1]]))
coef.table.EPA.2 <- cbind(coef.EPA.2, confint.EPA.2)
coef.table.EPA.2

##EPA model 3
confint.EPA.3 <-confint((get.models(dd.EPA.all, 3)[[1]]))
coef.EPA.3 <-coef((get.models(dd.EPA.all, 3)[[1]]))
coef.table.EPA.3 <- cbind(coef.EPA.3, confint.EPA.3)
coef.table.EPA.3

##EPA model 4

summary(get.models(dd.EPA.all, 4)[[1]])
confint.EPA.4 <-confint((get.models(dd.EPA.all, 4)[[1]]))
coef.EPA.4 <-coef((get.models(dd.EPA.all, 4)[[1]]))
coef.table.EPA.4 <- cbind(coef.EPA.4, confint.EPA.4)
coef.table.EPA.4

##EPA model 5
summary(get.models(dd.EPA.all, 5)[[1]])
confint.EPA.5 <-confint((get.models(dd.EPA.all, 5)[[1]]))
coef.EPA.5 <-coef((get.models(dd.EPA.all, 5)[[1]]))
coef.table.EPA.5 <- cbind(coef.EPA.5, confint.EPA.5)
coef.table.EPA.5

##EPA model 6
summary(get.models(dd.EPA.all, 6)[[1]])
confint.EPA.6 <-confint((get.models(dd.EPA.all, 6)[[1]]))
coef.EPA.6 <-coef((get.models(dd.EPA.all, 6)[[1]]))
coef.table.EPA.6 <- cbind(coef.EPA.6, confint.EPA.6)
coef.table.EPA.6



###DHA
summary(nutmassDHA_Feb27)

DHA.marine <- lm(log(DHA_g) ~ logmass*TL + logmass*Abs_lat +logmass*ISSCAAP_cat, na.action="na.fail", data=nutmassDHA_Feb27, subset=Habitat=="marine")
dd.DHA.marine <- dredge(DHA.marine)
summary(model.avg(dd.DHA.marine, subset = delta < 2))
confint(model.avg(dd.DHA.marine, subset = delta < 2))
confint((get.models(dd.DHA.marine, 1)[[1]]))
summary((get.models(dd.DHA.marine, 1)[[1]]))

DHA.fresh <- lm(log(DHA_g) ~ logmass*TL + logmass*Abs_lat + logmass*ISSCAAP_cat, na.action="na.fail", data=nutmassDHA_Feb27, subset=Habitat=="freshwater")
dd.DHA.fresh <- dredge(DHA.fresh)
summary(model.avg(dd.DHA.fresh, subset = delta < 2))
confint(model.avg(dd.DHA.fresh, subset = delta < 2))
confint((get.models(dd.DHA.fresh, 1)[[1]]))
summary((get.models(dd.DHA.fresh, 1)[[1]]))

DHA.all <- lm(log(DHA_g) ~ logmass*TL + logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassDHA_Feb27)
dd.DHA.all <- dredge(DHA.all)
summary(model.avg(dd.DHA.all, subset = delta < 2))
confint.DHA <- confint(model.avg(dd.DHA.all, subset = delta < 2))
coefs.DHA <- coef(model.avg(dd.DHA.all, subset = delta < 2))

coef.table.DHA <- cbind(coefs.DHA, confintDHA)
summary(get.models(dd.DHA.all, 1)[[1]])

confint.DHA <- confint((get.models(dd.DHA.all, 1)[[1]]))
coefs.DHA <- coef((get.models(dd.DHA.all, 1)[[1]]))
coef.table.DHA <- cbind(coefs.DHA, confint.DHA)
coef.table.DHA

##Fe
summary(nutmassFE_feb27)

FE.marine <- lm(log(FE_mg) ~ logmass*TL + logmass*Abs_lat + logmass*ISSCAAP_cat, na.action="na.fail", data=nutmassFE_feb27, subset=Habitat=="marine")
dd.FE.marine <- dredge(FE.marine)
summary(model.avg(dd.FE.marine, subset = delta < 2))
confint(model.avg(dd.FE.marine, subset = delta < 2))
confint((get.models(dd.FE.marine, 1)[[1]]))
summary((get.models(dd.FE.marine, 1)[[1]]))

FE.fresh <- lm(log(FE_mg) ~ logmass*TL + logmass*Abs_lat + logmass*ISSCAAP_cat, na.action="na.fail", data=nutmassFE_feb27, subset=Habitat=="freshwater")
dd.FE.fresh <- dredge(FE.fresh)
summary(model.avg(dd.FE.fresh, subset = delta < 2))
confint(model.avg(dd.FE.fresh, subset = delta < 2))
confint((get.models(dd.FE.fresh, 1)[[1]]))
summary((get.models(dd.FE.fresh, 1)[[1]]))


FE.all <- lm(log(FE_mg) ~ logmass*TL + logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassFE_feb27)
dd.FE.all <- dredge(FE.all)

summary(model.avg(dd.FE.all, subset = delta < 2))

coefs.fe <- coef(model.avg(dd.FE.all, subset = delta < 2))
confint.fe <- confint(model.avg(dd.FE.all, subset = delta < 2))
coef.table.fe <- cbind(coefs.fe, confint.fe)
coef.table.fe

summary((get.models(dd.FE.all, 1)[[1]]))
coef.FE.1 <- coef(get.models(dd.FE.all, 1)[[1]])
confint.FE.1 <- confint(get.models(dd.FE.all, 1)[[1]])
coef.table.FE.1 <- cbind(coef.FE.1, confint.FE.1)
coef.table.FE.1

confint((get.models(dd.FE.all, 2)[[1]]))
summary((get.models(dd.FE.all, 2)[[1]]))
coef.FE.2 <- coef(get.models(dd.FE.all, 2)[[1]])
confint.FE.2 <- confint(get.models(dd.FE.all, 2)[[1]])
coef.table.FE.2 <- cbind(coef.FE.2, confint.FE.2)
coef.table.FE.2

summary((get.models(dd.FE.all, 3)[[1]]))
coef.FE.3 <- coef(get.models(dd.FE.all, 3)[[1]])
confint.FE.3 <- confint(get.models(dd.FE.all, 3)[[1]])
coef.table.FE.3 <- cbind(coef.FE.3, confint.FE.3)
coef.table.FE.3

coef.table.FE.all <- cbind(coef.table.FE.1, coef.table.FE.2, coef.table.FE.3)

###HG  data=nutmassHG_Feb27
summary(nutmassHG_Feb27)


HG.marine <- lm(log(HG_mcg) ~ logmass*TL + logmass*Abs_lat, na.action="na.fail", data=nutmassHG_Feb27, subset=Habitat=="marine")
dd.HG.marine <- dredge(HG.marine)
summary(model.avg(dd.HG.marine, subset = delta < 2))
confint(model.avg(dd.HG.marine, subset = delta < 2))
confint((get.models(dd.HG.marine, 1)[[1]]))
summary((get.models(dd.HG.marine, 1)[[1]]))

HG.fresh <- lm(log(HG_mcg) ~ logmass*TL + logmass*Abs_lat, na.action="na.fail", data=nutmassHG_Feb27, subset=Habitat=="freshwater")
dd.HG.fresh <- dredge(HG.fresh)
summary(model.avg(dd.HG.fresh, subset = delta < 2))
confint(model.avg(dd.HG.fresh, subset = delta < 2))
confint((get.models(dd.HG.fresh, 1)[[1]]))
summary((get.models(dd.HG.fresh, 1)[[1]]))


HG.all <- lm(log(HG_mcg) ~ logmass*TL + logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassHG_Feb27)
dd.HG.all <- dredge(HG.all)
summary(model.avg(dd.HG.all, subset = delta < 2))
coef.HG <- coef(model.avg(dd.HG.all, subset = delta < 2))
confint.HG <- confint(model.avg(dd.HG.all, subset = delta < 2))
coef.table.HG <- cbind(coef.HG, confint.HG)
coef.table.HG

confint((get.models(dd.HG.all, 1)[[1]]))
summary((get.models(dd.HG.all, 1)[[1]]))

##HG model 1
summary(model.avg(dd.HG.all, subset = delta < 2))
summary(get.models(dd.HG.all, 2)[[1]])
confint.HG.1 <-confint((get.models(dd.HG.all, 1)[[1]]))
coef.HG.1 <-coef((get.models(dd.HG.all, 1)[[1]]))
coef.table.HG.1 <- cbind(coef.HG.1, confint.HG.1)
coef.table.HG.1

confint.HG.2 <-confint((get.models(dd.HG.all, 2)[[1]]))
coef.HG.2 <-coef((get.models(dd.HG.all, 2)[[1]]))
coef.table.HG.2 <- cbind(coef.HG.2, confint.HG.2)
coef.table.HG.2

###ZN data=nutmassZN_Feb27

summary(nutmassZN_Feb27)

ZN.marine <- lm(log(ZN_mg) ~ logmass*TL + logmass*Abs_lat + logmass*ISSCAAP_cat, na.action="na.fail", data=nutmassZN_Feb27, subset=Habitat=="marine")
dd.ZN.marine <- dredge(ZN.marine)
summary(model.avg(dd.ZN.marine, subset = delta < 2))
confint(model.avg(dd.ZN.marine, subset = delta < 2))
confint((get.models(dd.ZN.marine, 1)[[1]]))
summary((get.models(dd.ZN.marine, 1)[[1]]))

ZN.fresh <- lm(log(ZN_mg) ~ logmass*TL + logmass*Abs_lat, na.action="na.fail", data=nutmassZN_Feb27, subset=Habitat=="freshwater")
dd.ZN.fresh <- dredge(ZN.fresh)
summary(model.avg(dd.ZN.fresh, subset = delta < 2))
confint(model.avg(dd.ZN.fresh, subset = delta < 2))
confint((get.models(dd.ZN.fresh, 1)[[1]]))
summary((get.models(dd.ZN.fresh, 1)[[1]]))

ZN.all <- lm(log(ZN_mg) ~ logmass*TL + logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassZN_Feb27)
dd.ZN.all <- dredge(ZN.all)
summary(model.avg(dd.ZN.all, subset = delta < 2))
coef.ZN <-coef(model.avg(dd.ZN.all, subset = delta < 2))
confint.ZN <- confint(model.avg(dd.ZN.all, subset = delta < 2))
coef.table.ZN <- cbind(coef.ZN, confint.ZN)
coef.table.ZN

confint((get.models(dd.ZN.all, 1)[[1]]))
summary((get.models(dd.ZN.all, 1)[[1]]))

##ZN model 1
summary(model.avg(dd.ZN.all, subset = delta < 2))
summary(get.models(dd.ZN.all, 4)[[1]])
confint.ZN.1 <-confint((get.models(dd.ZN.all, 1)[[1]]))
coef.ZN.1 <-coef((get.models(dd.ZN.all, 1)[[1]]))
coef.table.ZN.1 <- cbind(coef.ZN.1, confint.ZN.1)
coef.table.ZN.1

confint.ZN.2 <-confint((get.models(dd.ZN.all, 2)[[1]]))
coef.ZN.2 <-coef((get.models(dd.ZN.all, 2)[[1]]))
coef.table.ZN.2 <- cbind(coef.ZN.2, confint.ZN.2)
coef.table.ZN.2

confint.ZN.3 <-confint((get.models(dd.ZN.all, 3)[[1]]))
coef.ZN.3 <-coef((get.models(dd.ZN.all, 3)[[1]]))
coef.table.ZN.3 <- cbind(coef.ZN.3, confint.ZN.3)
coef.table.ZN.3

confint.ZN.4 <-confint((get.models(dd.ZN.all, 4)[[1]]))
coef.ZN.4 <-coef((get.models(dd.ZN.all, 4)[[1]]))
coef.table.ZN.4 <- cbind(coef.ZN.4, confint.ZN.4)
coef.table.ZN.4



##CA
CA.marine <- lm(log(CA_mg) ~ logmass*TL + logmass*Abs_lat, na.action="na.fail", data=nutmassCA_Feb24, subset=Habitat=="marine")
dd.CA.marine <- dredge(CA.marine)
summary(model.avg(dd.CA.marine, subset = delta < 2))
confint(model.avg(dd.CA.marine, subset = delta < 2))
confint((get.models(dd.CA.marine, 1)[[1]]))
summary((get.models(dd.CA.marine, 1)[[1]]))
visreg(CA.marine, xvar="logmass", by="Abs_lat")

CA.fresh <- lm(log(CA_mg) ~ logmass*TL + logmass*Abs_lat, na.action="na.fail", data=nutmassCA_Feb24, subset=Habitat=="freshwater")
dd.CA.fresh <- dredge(CA.fresh)
summary(model.avg(dd.CA.fresh, subset = delta < 2))
confint(model.avg(dd.CA.fresh, subset = delta < 2))
confint((get.models(dd.CA.fresh, 1)[[1]]))
summary((get.models(dd.CA.fresh, 1)[[1]]))

CA.all <- lm(log(CA_mg) ~ logmass*TL + logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassCA_Feb24)
dd.CA.all <- dredge(CA.all)
summary(model.avg(dd.CA.all, subset = delta < 2))
confint(model.avg(dd.CA.all, subset = delta < 2))

##Ca model 1
summary(get.models(dd.CA.all, 3)[[1]])
confint.CA.1 <-confint((get.models(dd.CA.all, 1)[[1]]))
coef.CA.1 <-coef((get.models(dd.CA.all, 1)[[1]]))
coef.table.CA.1 <- cbind(coef.CA.1, confint.CA.1)
coef.table.CA.1

confint.CA.2 <-confint((get.models(dd.CA.all, 2)[[1]]))
coef.CA.2 <-coef((get.models(dd.CA.all, 2)[[1]]))
coef.table.CA.2 <- cbind(coef.CA.2, confint.CA.2)
coef.table.CA.2

confint.CA.3 <-confint((get.models(dd.CA.all, 3)[[1]]))
coef.CA.3 <-coef((get.models(dd.CA.all, 3)[[1]]))
coef.table.CA.3 <- cbind(coef.CA.3, confint.CA.3)
coef.table.CA.3


CA.all.rescale <- lm(log(CA_mg) ~ rescale(logmass)*rescale(TL) + rescale(logmass)*rescale(Abs_lat) + rescale(logmass)*Habitat, na.action="na.fail", data=nutmassCA_Feb24)








summary(nutmassCA_Feb24)
CA.1 <- lm(log(CA_mg) ~ logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmassCA_Feb24)
CA.1b <- lm(log(CA_mg) ~ Abs_lat + Habitat + logmass, na.action="na.fail", data=nutmassCA_Feb24)
CA.2 <- lm(log(CA_mg) ~ logmass*TL + logmass*Habitat, na.action="na.fail", data=nutmassCA_Feb24)
CA.3 <- lm(log(CA_mg) ~ logmass*TL + logmass*Abs_lat, na.action="na.fail", data=nutmassCA_Feb24)
CA.4 <- lm(log(CA_mg) ~ logmass*TL, na.action="na.fail", data=nutmassCA_Feb24)
CA.5 <- lm(log(CA_mg) ~ logmass*Abs_lat, na.action="na.fail", data=nutmassCA_Feb24)
CA.6 <- lm(log(CA_mg) ~ logmass*Habitat, na.action="na.fail", data=nutmassCA_Feb24)

model.sel(CA.all, CA.1, CA.1b, CA.2, CA.3, CA.4, CA.5, CA.6)

?MuMIn

dd.CA.all <- dredge(CA.all, m.min=2)
CA.avg <-(model.avg(dd.CA.all, subset = delta < 2))
summary(CA.avg)

dd.CA.all.rescale <- dredge(CA.all.rescale, m.min=2)
CA.rescale.avg <-(model.avg(dd.CA.all.rescale, subset = delta < 2))
confint.rescale.avg <- confint(CA.rescale.avg)
coefs.rescale <- coef(CA.rescale.avg)
coeftable.rescale <- cbind(coefs.rescale, confint.rescale.avg)
coeftable.rescale
summary(CA.rescale.avg)



summary(model.avg(dd.CA.all))

confint(model.avg(dd.CA.all, subset = delta < 2))
confint((get.models(dd.CA.all, 3)[[1]]))
summary((get.models(dd.CA.all, 2)[[1]]))

##Figures
CA.size <- lm(log(CA_mg) ~ logmass, data=nutmassCA_Feb24)
visreg(CA.size, xvar="logmass", mar=c(2, 6, 2, 2), xlab="log(body mass), g", ylab="log(calcium content, mg/100 edible portion)")

CA.all <- standardize(lm(log(CA_mg) ~ logmass*TL + logmass*Abs_lat +logmass*Habitat, na.action="na.fail", data=nutmassCA_Feb24))
CA.all.scale <- lm(log(CA_mg) ~ rescale(logmass)*rescale(TL) + rescale(logmass)*rescale(Abs_lat) + rescale(logmass)*Habitat, na.action="na.fail", data=nutmassCA_Feb24.fm)
dd.CA.all<- dredge(CA.all)
mod.avg <-model.avg(dd.CA.all, subset = delta < 2)
conf.int.avg <- confint(model.avg(dd.CA.all, subset = delta < 2))
coefs.avg <- coef(mod.avg)
coef.table.avg <- cbind(coefs.avg, conf.int.avg)
coef.table.avg
summary(mod.avg)
coefplot(mod.avg)


coefplot(CA.all)
model.names(dd.CA.all)
confint(model.avg(dd.CA.all, subset = delta < 2))
confint((get.models(dd.CA.all, 1)[[1]]))
summary((get.models(dd.CA.all, 1)[[1]]))



CA.best <- lm(log(CA_mg) ~ Abs_lat +Habitat+logmass+TL+Abs_lat*logmass, na.action="na.fail", data=nutmassCA_Feb24)
coefplot(standardize(CA.best), cex.pts=2, varnames=longnames, mar=c(1, 0, 3, 1), vertical=TRUE, h.axis=TRUE, v.axis=TRUE, main="")
longnames<-c("intercept", "Latitude", "Habitat (freshwater)", "Habitat (marine)", "Body size", "Trophic position", "Latitude*Size")
?coefplot
CA.best.beta <- lm.beta(CA.best)
summary(CA.best.beta)
coefplot(CA.best.beta)
coefplot(CA.best)
summary(nutmassCA_Feb24$Habitat)

CA.best$coefficients
conf.int<-confint(CA.best)
par <-c("Abs_lat", "logmass")

coef.table <- cbind(conf.int, CA.best$coefficients)
coef.table

nutmassCA_Feb24.fm <- subset(nutmassCA_Feb24, Habitat!="brackish")

?scale
?standardize
confint(standardize(CA.best))
coefplot(standardize(CA.best))
summary(standardize(CA.best))

?rescale

visreg(CA.best)

?standardize
standardize(CA.best)
