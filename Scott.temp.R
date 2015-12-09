### Scott's temperature data analysis 

sdata <- read.csv("/Users/Joey/Documents/Nutrient_Analysis/data/Book1.csv")
 
 # install.packages("ggplot2")
 library(ggplot2)
 library(visreg)
 library(dplyr)
 library(tidyr)
 library(plyr)
 
 hist(sdata$Temp)
 
 sdata <- sdata %>% 
   mutate(treatment = revalue(treatment, 
                              c("G2" = "alfred",
                                "B2" = "evol28Acl16",
                                "E2" = "evol16Acl28",
                                "D2" = "evol28Acl28")))
 
 temp.fit <- function(df) {
   model <- lm(dO2 ~ Temp, data = df)
   y   = coef(model)[2]
   ylo = confint(model)[2]
   yhi = confint(model)[4]
   setNames(data.frame(t(c(y, ylo, yhi))), c("beta", "ylo", "yhi"))
 }
 
 temp.fit(sdata)
 
 test<- sdata %>%  
   group_by(treatment) %>% 
   do(temp.fit(.)) %>% 
   ungroup() %>% 
   arrange(desc(beta))
 
 str(test)
 
 test$treatment <- factor(test$treatment, levels=unique(test$treatment))
 ggplot(test, aes(x=treatment, y=beta, ymin=ylo, ymax=yhi)) +
   geom_pointrange() + 
   # coord_flip() + 
   geom_hline(aes(x=0), lty=2) +
   xlab('Temp') +
   ylab('Regression Coefficient') + theme(legend.position="none")
 ggsave("figure2.png")
 
 
 
?revalue
 
 View(sdata)
 ggplot(sdata, aes(x= Temp, y = dO2, color = treatment)) + geom_point()
 ggsave()
 
 mod1 <- lm(dO2 ~ Temp*treatment, data = sdata)
 visreg(mod1, xvar = "Temp", by = "treatment", overlay = "FALSE")
 ?visreg
 