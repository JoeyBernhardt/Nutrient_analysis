
##Calcium RDI is 1000mg/day

hist(nutmassCA_Feb24$CA_mg)

View(nutmassCA_Feb24)
boxplot(nutmassCA_Feb24$CA_mg ~ nutmassCA_Feb24$ISSCAAP_cat)
abline(h=1000)
summary(subset(nutmassCA_Feb24$CA_mg > 1000))

CA.RDI <- subset(nutmassCA_Feb24, CA_mg >325)
exp(mean(CA.RDI$logmass))
exp(min(CA.RDI$logmass))
exp(max(CA.RDI$logmass))/1000

0.1*60




length(unique(CA.RDI$ASFIS.Scientific.name))
length(unique(nutmassCA_Feb24$ASFIS.Scientific.name))

CA.lessthanRDI <- subset(nutmassCA_Feb24, CA_mg <325)
length(unique(CA.lessthanRDI$ASFIS.Scientific.name))
summary(CA.lessthanRDI)
exp(mean(CA.lessthanRDI$logmass))/1000
exp(min(CA.lessthanRDI$logmass))
exp(max(CA.lessthanRDI$logmass))/1000

(exp(6.206))

1300/4
1503/3

##amount of variation in calcium content

min(nutmassCA_Feb24$CA_mg)
max(nutmassCA_Feb24$CA_mg)

summary(nutmassCA_Feb24$CA_mg)
hist(nutmassCA_Feb24$CA_mg, breaks=1000)
hist(CA.lessthanRDI$CA_mg, breaks=1000)
abline(h=1)
?hist

####Mercury

summary(nutmassHG_Feb27$HG_mcg)
length(unique(nutmassHG_Feb27$ASFIS.Scientific.name))
belowrfd <-subset(nutmassHG_Feb27, HG_mcg < 6.00)
summary(belowrfd)
length(unique(belowrfd$ASFIS.Scientific.name))


exp(mean(belowrfd$logmass))/1000
exp(min(belowrfd$logmass))/1000
exp(max(belowrfd$logmass))/1000

exp(mean(aboverfd$logmass))/1000
exp(min(aboverfd$logmass))/1000
exp(max(aboverfd$logmass))/1000



aboverfd <-subset(nutmassHG_Feb27, HG_mcg > 6.00)
summary(aboverfd)
length(unique(aboverfd$ASFIS.Scientific.name))

###variation in HG content
summary(nutmassHG_Feb27)


summary(nutmass_EPA_Feb27$EPA_g)
max(nutmass_EPA_Feb27$EPA_g)/min(nutmass_EPA_Feb27$EPA_g)

?summary
0.1816000/0.0336100

22.82/6.60
71.02/12.51


