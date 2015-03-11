
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



