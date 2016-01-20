

modCs <- standardize(lm(log(CA_mg) ~ log(max_size) + FoodTroph + taxon + Habitat + Abs_lat, data = ntbl))
modZs <- standardize(lm(log(ZN_mg) ~ log(max_size) + FoodTroph + taxon + Habitat + Abs_lat, data = ntbl))
modFs <- standardize(lm(log(FE_mg) ~ log(max_size) + FoodTroph + taxon + Habitat + Abs_lat, data = ntbl))
modEs <- standardize(lm(log(EPA_g) ~ log(max_size) + FoodTroph + taxon + Habitat + Abs_lat, data = ntbl))
modDs <- standardize(lm(log(DHA_g) ~ log(max_size) + FoodTroph + taxon + Habitat + Abs_lat, data = ntbl))

coefplot(modDs, innerCI = 2, intercept = FALSE)
confint(modEs)
summary(modDs)
