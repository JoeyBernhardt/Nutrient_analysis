
#### Load packages ####
library(readr)
library(plyr)
library(dplyr)

ntbl.RDI.tot <- minerals %>% 
  group_by(species) %>% 
  summarise(mean.CA = mean(CA_mg, na.rm = TRUE),
            mean.ZN = mean(ZN_mg, na.rm = TRUE), 
            mean.FE = mean(FE_mg, na.rm = TRUE)) %>% 
  mutate(RDI.CA = ifelse(mean.CA > 120, 1, 0)) %>% 
  mutate(RDI.FE = ifelse(mean.FE > 1.8, 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(mean.ZN > 2.75, 1, 0)) %>%
  mutate(RDI.micro.tot = rowSums(.[5:7])) %>% 
  filter(!is.na(RDI.micro.tot)) %>% 
  arrange(., RDI.micro.tot)


n.long <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data/ntbl.long.csv")
intbl.all <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data/intbl.all.csv")
ntbl.inv <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data/ntbl.inv.csv")

table(n.long$Subgroup)
names(intbl.all)
intbl.all$Food.Item.ID

n.long <- n.long %>% 
  mutate(RDI.25per = (concentration/(RDI/4)),
         RDI.per = (concentration/RDI),
         RDI.20per = (concentration/(RDI/5)),
         RDI.15per = (concentration/(RDI/6)),
         RDI.target = (concentration > (RDI/10)))

n.long.min <- n.long %>% 
  filter(nutrient %in% c("CA_mg", "ZN_mg", "FE_mg")) %>% 
  group_by(species) %>% 
  summarise(mean.RDI = mean(RDI.per, na.rm = TRUE),
            mean.size = mean(max_size, na.rm = TRUE),
            mean.lat = mean(Abs_lat, na.rm = TRUE),
            mean.TL = mean(TL, na.rm = TRUE))
summary(n.long.min$mean.RDI)

RDI.prop <- lm(log(mean.RDI) ~ log(mean.size) + mean.TL + mean.lat, data = n.long.min)
RDI.size <- lm(log(mean.RDI) ~ log(mean.size), data = n.long.min)
summary(RDI.prop)
confint(RDI.prop)
visreg::visreg(RDI.size, xvar = "mean.size", xtrans = log)
visreg::visreg(RDI.prop, xvar = "mean.lat")
coefplot(RDI.prop, intercept = FALSE)

hist(log(n.long.min$mean.RDI))

RDI.per <- lm(log(RDI.per) ~ log(max_size) + TL + Habitat, data = n.long)
summary(RDI.per)
confint(RDI.per)

n.long.all <- inner_join(n.long.min, n.long, by = "species")

class(n.long.all$Subgroup)

n.long.all <- n.long.all %>% 
  mutate(Subgroup = as.factor(Subgroup),
         Habitat = as.factor(Habitat),
         Subgroup = droplevels(Subgroup),
         Habitat = droplevels(Habitat))

ntbl.long.drop <- n.long.all %>% 
  mutate(Subgroup = droplevels(Subgroup),
         Habitat = droplevels(Habitat),
         DemersPelag = droplevels(DemersPelag),
         taxon = droplevels(taxon))

RDI.props <- standardize(lm(log(mean.RDI) ~ log(mean.size) + mean.TL + mean.lat + Habitat, data = n.long.all, na.action = na.omit))
summary(RDI.props)
coefplot(RDI.props, intercept = FALSE)
confint(RDI.props)

mylogit <- glm(RDI.target ~ log(max_size) + Abs_lat + TL, data = n.long, family = "binomial", na.action = na.omit)
summary(mylogit)
mylogit
confint(mylogit)
coefplot(mylogit, intercept = FALSE)
visreg::visreg(mylogit, xvar = "max_size", xtrans = log)
plot(mylogit)

anova(mylogit, test = "Chisq")
3109.3-3054.7
1 - pchisq(54.6, df=4)

plot(RDI.target ~ log(max_size), data=n.long.all)
lines(log(n.long.all$max_size), mylogit$fitted, type="l", col="red")

library(ggplot2)


n.long$RDI.target <- as.numeric(n.long$RDI.target)

n.long.all <- n.long.all %>% 
  mutate(bones.body = as.numeric(max_size < 0.1 | Abs_lat == 12.265754 | Abs_lat == 11.066667))

logr_bones <- glm(RDI.target ~ bones.body, data = n.long.all, family = binomial)
summary(logr_bones)
confint(logr_bones)

n.long$RDI.target <- as.numeric(n.long$RDI.target)

n.long %>%
  group_by(species) %>% 
filter(nutrient %in% c("CA_mg", "ZN_mg", "FE_mg")) %>% 
ggplot(., aes(x=log(max_size), y=RDI.target)) + geom_point(aes(colour = factor(Subgroup)), shape=16, position=position_jitter(width=.7 ,height=.03)) + 
  stat_smooth(method="glm", family="binomial", se=FALSE)

ggsave("/Users/Joey/Documents/Nutrient_Analysis/figures/logistic-reg-RDItargets.png")

table(n.long.all$RDI.target)
