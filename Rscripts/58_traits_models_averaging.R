

m1 <- gls(log_concentration ~ bulk_trophic_level + log_length + feeding_mode + DemersPelag
             + DepthRangeDeep + AgeMatMin + BodyShapeI + realm + EnvTemp, correlation = corPagel(value = 0, phy = tree, fixed = TRUE),
             data = calg2, method = "ML")


m2 <- gls(log_concentration ~ log_length, correlation = corPagel(value = 0, phy = tree, fixed = TRUE),
          data = calg2, method = "ML")

m3 <- gls(log_concentration ~ bulk_trophic_level, correlation = corPagel(value = 0, phy = tree, fixed = TRUE),
          data = calg2, method = "ML")

m4 <- gls(log_concentration ~ feeding_mode, correlation = corPagel(value = 0, phy = tree, fixed = TRUE),
          data = calg2, method = "ML")

m5 <- gls(log_concentration ~ DemersPelag, correlation = corPagel(value = 0, phy = tree, fixed = TRUE),
          data = calg2, method = "ML")

m6 <- gls(log_concentration ~ DepthRangeDeep, correlation = corPagel(value = 0, phy = tree, fixed = TRUE),
          data = calg2, method = "ML")

m7 <- gls(log_concentration ~ AgeMatMin, correlation = corPagel(value = 0, phy = tree, fixed = TRUE),
          data = calg2, method = "ML")
m8 <- gls(log_concentration ~ realm, correlation = corPagel(value = 0, phy = tree, fixed = TRUE),
          data = calg2, method = "ML")
m9 <- gls(log_concentration ~ 1, correlation = corPagel(value = 0, phy = tree, fixed = TRUE),
            data = calg2, method = "ML")

m10 <- gls(log_concentration ~ BodyShapeI, correlation = corPagel(value = 0, phy = tree, fixed = TRUE),
          data = calg2, method = "ML")


### diet model
moda <- gls(log_concentration ~ bulk_trophic_level + feeding_mode, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")

### life history model
modb <- gls(log_concentration ~ log_length + AgeMatMin + BodyShapeI, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")

### habitat model
modc <- gls(log_concentration ~  DemersPelag + DepthRangeDeep + realm, correlation = corPagel(value = 0, phy = tree, fixed = TRUE), data = calg2, method = "ML")


model.sel(m1, m2, m3, m4, m5, m6, m7, m8, m9,m10, moda, modb, modc, rank = "AIC", extra = "rsquared") %>% View

res <- dredge(m1, extra = "rsquared", rank = "AIC")
summary(model.avg(res,  subset= cumsum(weight) <= .95))
confint(model.avg(res,  subset= cumsum(weight) <= .95))
