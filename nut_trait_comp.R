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

EPA.all <- lm(log(EPA_g) ~ logmass*TL+ logmass*Abs_lat + logmass*Habitat, na.action="na.fail", data=nutmass_EPA_Feb27)
dd.EPA.all <- dredge(EPA.all)
summary(model.avg(dd.EPA.all, subset = delta < 2))
confint(model.avg(dd.EPA.all, subset = delta < 2))
confint((get.models(dd.EPA.all, 1)[[1]]))
summary((get.models(dd.EPA.all, 1)[[1]]))
summary(nutmass_EPA_Feb27$Habitat)

coef(model.avg(dd.EPA.all, subset = delta < 2))


###DHA

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
confint(model.avg(dd.DHA.all, subset = delta < 2))
confint((get.models(dd.DHA.all, 1)[[1]]))
summary((get.models(dd.DHA.all, 1)[[1]]))


##Fe

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
confint(model.avg(dd.FE.all, subset = delta < 2))
confint((get.models(dd.FE.all, 1)[[1]]))
summary((get.models(dd.FE.all, 1)[[1]]))


###HG  data=nutmassHG_Feb27

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
confint(model.avg(dd.HG.all, subset = delta < 2))
confint((get.models(dd.HG.all, 1)[[1]]))
summary((get.models(dd.HG.all, 1)[[1]]))

HG.best <- lm(formula = log(HG_mcg) ~ Abs_lat + logmass + Abs_lat:logmass + 
                1, data = nutmassHG_Feb27, subset = Habitat == "freshwater", 
              na.action = "na.fail")
visreg(HG.best, xvar="logmass")

###ZN data=nutmassZN_Feb27

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
confint(model.avg(dd.ZN.all, subset = delta < 2))
confint((get.models(dd.ZN.all, 1)[[1]]))
summary((get.models(dd.ZN.all, 1)[[1]]))

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
summary(model.avg(dd.CA.all, subset = delta < 2))
summary(model.avg(dd.CA.all))

confint(model.avg(dd.CA.all, subset = delta < 2))
confint((get.models(dd.CA.all, 3)[[1]]))
summary((get.models(dd.CA.all, 2)[[1]]))

##Figures
CA.size <- lm(log(CA_mg) ~ logmass, data=nutmassCA_Feb24)
visreg(CA.size, xvar="logmass", mar=c(2, 6, 2, 2), xlab="log(body mass), g", ylab="log(calcium content, mg/100 edible portion)")

CA.all <- lm(log(CA_mg) ~ logmass*TL + logmass*Abs_lat +logmass*Habitat, na.action="na.fail", data=nutmassCA_Feb24)
CA.all.scale <- lm(log(CA_mg) ~ rescale(logmass)*rescale(TL) + rescale(logmass)*rescale(Abs_lat) + rescale(logmass)*Habitat, na.action="na.fail", data=nutmassCA_Feb24.fm)
dd.CA.all.scale <- dredge(CA.all.scale)
summary(model.avg(dd.CA.all.scale, subset = delta < 2))
coefplot(CA.all)
model.names(dd.CA.all)
confint(model.avg(dd.CA.all.scale, subset = delta < 2))
confint((get.models(dd.CA.all, 1)[[1]]))
summary((get.models(dd.CA.all, 1)[[1]]))

ca.best.beta <- lm.beta(model.avg(dd.CA.all, subset = delta < 2))
print(lm.D9.beta)
summary(lm.D9.beta)
coef(lm.D9.beta)


CA.best <- lm(log(CA_mg) ~ Abs_lat +Habitat+logmass+TL+Abs_lat*logmass, na.action="na.fail", data=nutmassCA_Feb24.fm)
coefplot(standardize(CA.best), cex.pts=2, varnames=longnames, mar=c(1, 0, 3, 1), vertical=TRUE, h.axis=TRUE, v.axis=TRUE, main="")
longnames<-c("intercept", "Latitude", "Habitat (freshwater)", "Habitat (marine)", "Body size", "Trophic position", "Latitude*Size")
?coefplot
CA.best.beta <- lm.beta(CA.best)
summary(CA.best.beta)
coefplot(CA.best.beta)
coefplot(CA.best)
summary(nutmassCA_Feb24$Habitat)

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
