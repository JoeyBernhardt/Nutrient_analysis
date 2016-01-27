## result 5 trait analysis
aq.wide <- read.csv("/Users/Joey/Documents/Nutrient_Analysis/data/aq.wide.csv")

fb.all <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data/fb.all.csv")
fb.length <- fb.all %>% 
  dplyr::select(species, CA_mg, ZN_mg, FE_mg, EPA_g, DHA_g, Length, max_length, Subgroup, Habitat, TL, FoodTroph.x, Herbivory2, Abs_lat, DemersPelag, taxon, max_size) %>% 
  rename(FoodTroph = FoodTroph.x) %>% 
  mutate(species = as.factor(species))

table(fb.length$Subgroup)

ZN.all <- standardize(lm(log(ZN_mg) ~ log(Length) + TL + Habitat + Abs_lat, data = fb.length, na.action = "na.omit"))
test.ZN.all <- tidy(ZN.all, conf.int =TRUE)
summary(ZN.all)


test.ZN.all$term <- factor(test.ZN.all$term, levels=unique(test.ZN.all$term))
ggplot(test.ZN.all, aes(x=term, y=estimate, ymin=conf.low, ymax=conf.high)) +
  geom_pointrange() + 
  coord_flip() + 
  geom_hline(aes(x=0), lty=2) +
  xlab('term ZN') +
  ylab('Regression Coefficient') + theme(legend.position="none")

CA.all <- standardize(lm(log(CA_mg) ~ log(max_size) + TL + Habitat + Abs_lat, data = aq.wide, subset = Subgroup == "Finfish", na.action = "na.omit"))
CA.all <- (lm(log(CA_mg) ~ log(max_size) + TL + Habitat + Abs_lat, data = aq.wide, subset = Subgroup == "Finfish", na.action = "na.omit"))

test.CA.all <- tidy(CA.all, conf.int =TRUE) %>% View

test.CA.all$term <- factor(test.CA.all$term, levels=unique(test.CA.all$term))
ggplot(test.CA.all, aes(x=term, y=estimate, ymin=conf.low, ymax=conf.high)) +
  geom_pointrange() + 
  coord_flip() + 
  geom_hline(aes(x=0), lty=2) +
  xlab('term CA') +
  ylab('Regression Coefficient') + theme(legend.position="none")
coefplot(CA.all, innerCI = 2, intercept = FALSE)

ZN.all <- standardize(lm(log(ZN_mg) ~ log(Length) + TL + Habitat + Abs_lat, data = fb.length, na.action = "na.omit"))
test.ZN.all <- tidy(ZN.all, conf.int =TRUE) 

CA.all <- standardize(lm(log(CA_mg) ~ log(Length) + TL + Habitat + Abs_lat, data = fb.length, na.action = "na.omit"))
test.CA.all <- tidy(CA.all, conf.int =TRUE) %>% View

FE.all <- standardize(lm(log(FE_mg) ~ log(Length) + TL + Habitat + Abs_lat, data = fb.length, na.action = "na.omit"))
test.FE.all <- tidy(FE.all, conf.int =TRUE) 

EPA.all <- standardize(lm(log(EPA_g) ~ log(Length) + TL + Habitat + Abs_lat, data = fb.length, na.action = "na.omit"))
test.EPA.all <- tidy(EPA.all, conf.int =TRUE)

DHA.all <- standardize(lm(log(DHA_g) ~ log(Length) + TL + Habitat + Abs_lat, data = fb.length, na.action = "na.omit"))
test.DHA.all <- tidy(DHA.all, conf.int =TRUE) 

multiplot(ZN.all, CA.all, FE.all, EPA.all, DHA.all, innerCI = 2, intercept = FALSE)
ggsave("/Users/Joey/Documents/Nutrient_Analysis/figures/coefplot.png")


intbl <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data/intbl.all.csv")
intbl <- intbl %>% 
  mutate(Subgroup = as.factor(Subgroup),
         DemersPelag = as.factor(DemersPelag),
         taxon = as.factor(taxon),
         Habitat = as.factor(Habitat))

table(intbl$DemersPelag)
hist(intbl$Weight)

class(intbl$Weight)
summary(intbl$FoodTroph)

intbl$Weight <- as.numeric(intbl$Weight)

inv.length <- lm(log(CA_mg) ~ log(Length) + FoodTroph, data = intbl)
inv.size <- lm(log(CA_mg) ~ log(Length), data = intbl)
summary(inv.length)

visreg::visreg(inv.length, xvar = "Length", xtrans = log)


summary(inv.size)
confint(inv.size)
coefplot::coefplot(inv.length, innerCI = 2, intercept = FALSE)
length(!is.na(intbl$taxon))

### merge the inverts and verts
fb <- fb.all %>% 
  dplyr::select(species, CA_mg, ZN_mg, FE_mg, EPA_g, DHA_g, Length, max_length, Subgroup, Habitat, TL, FoodTroph.x, Herbivory2, Abs_lat, DemersPelag, taxon, max_size, Weight) %>% 
  rename(FoodTroph = FoodTroph.x) %>% 
  mutate(species = as.factor(species))
sb <- intbl %>% 
  dplyr::select(species, CA_mg, ZN_mg, FE_mg, EPA_g, DHA_g, Length, max_length, Subgroup, Habitat, TL, FoodTroph, Herbivory2, Abs_lat, DemersPelag, taxon, max_size, Weight) %>% 
  mutate(species = as.factor(species))

ntbl <- bind_rows(fb, sb)
write.csv(ntbl, "/Users/Joey/Documents/Nutrient_Analysis/data/ntbl.int.csv")

class(ntbl$FoodTroph)
ntbl <- ntbl %>% 
  mutate(Subgroup = as.factor(Subgroup),
         DemersPelag = as.factor(DemersPelag),
         taxon = as.factor(taxon),
         Habitat = as.factor(Habitat))

ntbl.CA <- ntbl %>% 
  filter(!is.na(CA_mg))
#          !is.na(max_size),
#          # !is.na(Abs_lat),
#          !is.na(Subgroup),
#          # !is.na(taxon),
#          # !is.na(Habitat),
#          # !is.na(DemersPelag))

ntbl.CA$Subgroup <- droplevels(ntbl.CA$Subgroup)

library(gdata)

ntbl.CA <- ntbl.CA %>% 
  mutate(Subgroup = droplevels(Subgroup),
         Habitat = droplevels(Habitat),
         DemersPelag = droplevels(DemersPelag),
         taxon = droplevels(taxon))
str(ntbl.CA)

ntbl.CA <- drop.levels(ntbl.CA)
table(ntbl.CA$Subgroup)

mod1 <- lm(log(CA_mg) ~ log(max_size) + FoodTroph + Subgroup + taxon + Habitat + Abs_lat, data = ntbl)
summary(mod1)
confint(mod1)

mod1s <- standardize(lm(log(CA_mg) ~ log(max_size) + FoodTroph + taxon + Habitat + Abs_lat, na.action = na.omit, data = ntbl))
coefplot(mod1s, innerCI = 2, intercept = FALSE)
summary(mod1)
confint(mod1s)
summary(ntbl.CA$Abs_lat)



mod2 <- lm(log(CA_mg) ~ log(Length) + FoodTroph + Habitat + Abs_lat + Subgroup, data = ntbl)
# mod2 <- standardize(lm(log(CA_mg) ~ log(max_size) + FoodTroph + Habitat + Abs_lat + Subgroup, data = ntbl))
# mod1 <- standardize(lm(log(CA_mg) ~ log(max_size) + FoodTroph + Subgroup + DemersPelag + taxon + Habitat + Abs_lat, data = ntbl.CA))
# mod1.tidy <- tidy(mod1, conf.int = TRUE)
summary(mod2)
coefs <- buildModelCI(mod1) 
confint(mod2)

mod2 <- lm(log(CA_mg) ~ log(Weight), data = ntbl)
coefplot(mod2, innerCI = 2, intercept = FALSE)
ggsave("/Users/Joey/Documents/Nutrient_Analysis/figures/CA_length_coefplot.png")

modZN <- lm(log(ZN_mg) ~ log(max_size) + TL + Habitat + Abs_lat, data = aq.wide)


summary(modZN)
confint(modZN)
coefplot::coefplot(modZN, intercept = FALSE)

modFE <- lm(log(FE_mg) ~ log(max_size) + FoodTroph + Habitat + Abs_lat, data = ntbl)
summary(modFE)
confint(modFE)


length(unique(ntbl$Weight))
visreg::visreg(mod2, xtrans = log)
hist(ntbl$Weight)
