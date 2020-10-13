library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())


confints_dha <- read_csv("data-processed/dha-traits-confints.csv")
confints_epa <- read_csv("data-processed/epa-traits-confints.csv")
confints_calcium <- read_csv("data-processed/calcium-traits-confints.csv")
confints_iron <- read_csv("data-processed/iron-traits-confints.csv")
confints_zinc <- read_csv("data-processed/zinc-traits-confints.csv")

### all traits plots


confints_epa2 <- confints_epa %>% 
  mutate(nutrient = "EPA")

confints_dha2 <- confints_dha %>% 
  mutate(nutrient = "DHA")

confints_calcium2 <- confints_calcium %>% 
  mutate(nutrient = "Calcium")

confints_iron2 <- confints_iron %>% 
  mutate(nutrient = "Iron")

confints_zinc2 <- confints_zinc %>% 
  mutate(nutrient = "Zinc")

all_confints <- bind_rows(confints_epa2, confints_calcium2, confints_dha2, confints_iron2, confints_zinc2) %>% 
  mutate(conf_int_overlap_0 = ifelse(upper < 0 & estimate < 0 | lower > 0 & estimate > 0, "yes", "no")) 

all_confints %>% 
  mutate(nutrient = factor(nutrient, levels = c("Calcium", "Iron", "Zinc", "EPA", "DHA"))) %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(x = term, y = estimate, ymin = lower, ymax = upper), fill = "transparent") +
  geom_point(aes(x = term, y = estimate, shape = conf_int_overlap_0, color = conf_int_overlap_0)) +
  scale_color_manual(values = c("black", "white")) +
  scale_shape_manual(values = c(1, 19)) +
  coord_flip() +
  geom_hline(yintercept = 0) + facet_wrap( ~ nutrient, nrow = 1, ncol = 5, scales = "free_x") +
	theme(legend.position = "none")
ggsave("figures/coef-plots.pdf", width = 12, height = 6)
ggsave("figures/coef-plots.png", width = 12, height = 6)




# now try with the dredge model -------------------------------------------

mod1_iron <- phylolm(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm, phy = iron_tree, data = irong2, model = "lambda")
dd_iron <- dredge(mod1_iron, extra = "rsquared", rank = "AICc") %>% 
  mutate(cumsum = cumsum(weight))
View(dd)
summary(model.avg(dd_iron,  subset= cumsum(weight) <= .95))
confint(model.avg(dd_iron,  subset= cumsum(weight) <= .95))

iron_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(dd_iron, subset = cumsum(weight) <= .95))), var = "term")
iron_slopes_average <- enframe(coef(model.avg(dd_iron, subset = cumsum(weight) <= .95)), name = "term", value = "slope")



iron_mod_out <- left_join(iron_CI_average, iron_slopes_average) %>% 
  rename(conf.low = `2.5 %`,
         conf.high = `97.5 %`) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(nutrient = "Iron")

mod1_calcium <- phylolm(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm, phy = calcium_tree, data = calciumg2, model = "lambda")
dd_calcium <- dredge(mod1_calcium, extra = "rsquared", rank = "AICc") %>% 
  mutate(cumsum = cumsum(weight))
View(dd)
# summary(model.avg(dd_calcium,  subset= cumsum(weight) <= .95))
# confint(model.avg(dd_calcium,  subset= cumsum(weight) <= .95))

calcium_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(dd_calcium, subset = cumsum(weight) <= .95))), var = "term")
calcium_slopes_average <- enframe(coef(model.avg(dd_calcium, subset = cumsum(weight) <= .95)), name = "term", value = "slope")



calcium_mod_out <- left_join(calcium_CI_average, calcium_slopes_average) %>% 
  rename(conf.low = `2.5 %`,
         conf.high = `97.5 %`) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(nutrient = "Calcium")

mod1_zinc <- phylolm(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm, phy = zinc_tree, data = zincg2, model = "lambda")
dd_zinc <- dredge(mod1_zinc, extra = "rsquared", rank = "AICc") %>% 
  mutate(cumsum = cumsum(weight))

summary(model.avg(dd_zinc,  subset= cumsum(weight) <= .95))
confint(model.avg(dd_zinc,  subset= cumsum(weight) <= .95))

zinc_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(dd_zinc, subset = cumsum(weight) <= .95))), var = "term")
zinc_slopes_average <- enframe(coef(model.avg(dd_zinc, subset = cumsum(weight) <= .95)), name = "term", value = "slope")



zinc_mod_out <- left_join(zinc_CI_average, zinc_slopes_average) %>% 
  rename(conf.low = `2.5 %`,
         conf.high = `97.5 %`) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(nutrient = "Zinc")

mod1_EPA <- phylolm(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm, phy = epa_tree, data = epag2, model = "lambda")
dd_EPA <- dredge(mod1_EPA, extra = "rsquared", rank = "AICc") %>% 
  mutate(cumsum = cumsum(weight))
# View(dd)
# summary(model.avg(dd_EPA,  subset= cumsum(weight) <= .95))
# confint(model.avg(dd_EPA,  subset= cumsum(weight) <= .95))

EPA_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(dd_EPA, subset = cumsum(weight) <= .95))), var = "term")
EPA_slopes_average <- enframe(coef(model.avg(dd_EPA, subset = cumsum(weight) <= .95)), name = "term", value = "slope")



EPA_mod_out <- left_join(EPA_CI_average, EPA_slopes_average) %>% 
  rename(conf.low = `2.5 %`,
         conf.high = `97.5 %`) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(nutrient = "EPA")

mod1_DHA <- phylolm(log_concentration ~  feeding_mode + log_length + DemersPelag + bulk_trophic_level + realm, phy = dha_tree, data = dhag2, model = "lambda")
dd_DHA <- dredge(mod1_DHA, extra = "rsquared", rank = "AICc") %>% 
  mutate(cumsum = cumsum(weight))
View(dd)
summary(model.avg(dd_DHA,  subset= cumsum(weight) <= .95))
confint(model.avg(dd_DHA,  subset= cumsum(weight) <= .95))

DHA_CI_average <- rownames_to_column(as.data.frame(confint(model.avg(dd_DHA, subset = cumsum(weight) <= .95))), var = "term")
DHA_slopes_average <- enframe(coef(model.avg(dd_DHA, subset = cumsum(weight) <= .95)), name = "term", value = "slope")



DHA_mod_out <- left_join(DHA_CI_average, DHA_slopes_average) %>% 
  rename(conf.low = `2.5 %`,
         conf.high = `97.5 %`) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(nutrient = "DHA")

all_coefs <- bind_rows(calcium_mod_out, iron_mod_out, zinc_mod_out, EPA_mod_out, DHA_mod_out) 
View(all_coefs)

all_coefs %>% 
  rename(lower = conf.low,
         upper = conf.high) %>% 
  mutate(nutrient = factor(nutrient, levels = c("Calcium", "Iron", "Zinc", "EPA", "DHA"))) %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(x = term, y = slope, ymin = lower, ymax = upper)) +
  coord_flip() +
  geom_hline(yintercept = 0) + facet_wrap( ~ nutrient, nrow = 1, ncol = 5, scales = "free_x")
ggsave("figures/all-coefs-traits-dredge.pdf", width = 12, height = 6)
