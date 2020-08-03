
cine2 <- cine_traits_new2 %>% 
  filter(part != "not specified") %>% 
  filter(part != "unknown") %>% 
  filter(!is.na(concentration)) %>% 
  filter(nutrient == "ca_mg") %>% 
  mutate(log_concentration = log(concentration)) %>% 
  mutate(log_length = log(Length)) %>%
  ungroup() %>% 
  group_by(latin_name, feeding_mode, feeding_level, BodyShapeI, DemersPelag, part, EnvTemp) %>% 
  summarise_each(funs(mean), AgeMatMin, log_concentration, log_length, Length, bulk_trophic_level, log_length, DepthRangeDeep) %>% 
  ungroup() %>% 
  filter(complete.cases(.))

cine2 <- traits1b %>% 
  filter(part != "not specified") %>% 
  filter(part != "unknown") %>% 
  filter(!is.na(concentration)) %>% 
  filter(nutrient == "ca_mg") %>% 
  ungroup() %>% 
  group_by(species1, feeding_mode, feeding_level, BodyShapeI, DemersPelag, part, EnvTemp) %>% 
  summarise_each(funs(mean), AgeMatMin, log_concentration, log_length, bulk_trophic_level, DepthRangeDeep) %>% 
  ungroup() %>% 
  filter(complete.cases(.))

library(plotrix)
traits1b %>% 
  filter(!is.na(concentration)) %>% 
  filter(!is.na(part)) %>% 
  filter(nutrient %in% c("ca_mg", "fe_mg", "zn_mg")) %>% 
  filter(nutrient %in% c("fe_mg")) %>% View
  group_by(nutrient, part) %>% 
  summarise_each(funs(mean, std.error), concentration) %>% 
  ggplot(aes(x = part, y = mean)) + geom_point() +
  geom_errorbar(aes(x = part, ymin = mean - std.error, ymax = mean + std.error)) +
  facet_wrap( ~ nutrient, scales = "free") +
  theme(axis.text.x = element_text(angle = 90))


mod1 <- lm(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + feeding_level +
             DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + part, data = cine2, na.action=na.exclude)
stargazer(mod1, title = "", type = "html", out="tables/cine-models-expanded-cal.htm",ci=TRUE, ci.level=0.95, digits = 2, single.row = TRUE)

mod1 <- gls(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + feeding_level +
      DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + part, data = cine2, method = "ML")
summary(mod1)
plot1 <- visreg(mod1, "DemersPelag", gg = TRUE, size = 4) +
  ylab("log(calcium) mg/100g ") + xlab("Habitat association") +
  theme(axis.text.x = element_text(angle = 90))
plot2 <- visreg(mod1, "log_length", gg = TRUE, size = 4) +
  ylab("log(calcium) mg/100g ") + xlab("Log length (cm)")  +
  theme(axis.text.x = element_text(angle = 90))
plot3 <- visreg(mod1, "AgeMatMin", gg = TRUE, size = 4) +
  ylab("log(calcium) mg/100g ") + xlab("Age at Maturity (years)") +
  theme(axis.text.x = element_text(angle = 90))
plot4 <- visreg(mod1, "DepthRangeDeep", gg = TRUE, size = 4) +
  ylab("log(calcium) mg/100g ") + xlab("Depth (m)") +
  theme(axis.text.x = element_text(angle = 90))
plot5 <- visreg(mod1, "bulk_trophic_level", gg = TRUE, size = 4) +
  ylab("log(calcium) mg/100g ") + xlab("Trophic position") +
  theme(axis.text.x = element_text(angle = 90))
plot6 <- visreg(mod1, "part", gg = TRUE, size = 4) +
  ylab("log(calcium) mg/100g ") + xlab("Body part") +
  theme(axis.text.x = element_text(angle = 90))
plot7 <- visreg(mod1, "BodyShapeI", gg = TRUE, size = 4) +
  ylab("log(calcium) mg/100g ") + xlab("Body shape") +
  theme(axis.text.x = element_text(angle = 90))
plot8 <- visreg(mod1, "feeding_mode", gg = TRUE, size = 4) +
  ylab("log(calcium) mg/100g ") + xlab("Feeding mode") +
  theme(axis.text.x = element_text(angle = 90))
plot9 <- visreg(mod1, "feeding_level", gg = TRUE, size = 4) +
  ylab("log(calcium) mg/100g ") + xlab("Feeding level") +
  theme(axis.text.x = element_text(angle = 90))

library(patchwork)
plot_all <- plot1 + plot2 + plot3 + plot4 + plot5 + plot7 + plot8 + plot6 +
  plot_annotation(tag_levels = 'A') + plot_layout(ncol = 4)
ggsave("figures/calcium-partial-regressions.pdf", plot = plot_all, width = 14, height = 10)
ggsave("figures/calcium-partial-regressions-mac.png", plot = plot_all, width = 14, height = 10)



# iron --------------------------------------------------------------------

cine2 <- cine_traits_new2 %>% 
  filter(part != "not specified") %>% 
  filter(part != "unknown") %>% 
  filter(!is.na(concentration)) %>% 
  filter(nutrient == "fe_mg") %>% 
  mutate(log_concentration = log(concentration)) %>% 
  mutate(log_length = log(Length)) %>%
  ungroup() %>% 
  group_by(latin_name, feeding_mode, feeding_level, BodyShapeI, DemersPelag, part, EnvTemp) %>% 
  summarise_each(funs(mean), AgeMatMin, log_concentration, log_length, Length, bulk_trophic_level, log_length, DepthRangeDeep) %>% 
  ungroup() %>% 
  filter(complete.cases(.))

cine2 <- traits1b %>% 
  filter(part != "not specified") %>% 
  filter(part != "unknown") %>% 
  filter(!is.na(concentration)) %>% 
  filter(nutrient == "fe_mg") %>% 
  ungroup() %>% 
  group_by(species1, feeding_mode, feeding_level, BodyShapeI, DemersPelag, part, EnvTemp) %>% 
  summarise_each(funs(mean), AgeMatMin, log_concentration, log_length, bulk_trophic_level, DepthRangeDeep) %>% 
  ungroup() %>% 
  filter(complete.cases(.))

mod1 <- lm(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + feeding_level +
             DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + part, data = cine2, na.action=na.exclude)
stargazer(mod1, title = "", type = "html", out="tables/cine-models-expanded-iron.htm",ci=TRUE, ci.level=0.95, digits = 2, single.row = TRUE)


summary(mod1)
plot1 <- visreg(mod1, "DemersPelag", gg = TRUE, size = 4) +
  ylab("log(iron) mg/100g ") + xlab("Habitat association") +
  theme(axis.text.x = element_text(angle = 90))
plot2 <- visreg(mod1, "log_length", gg = TRUE, size = 4) +
  ylab("log(iron) mg/100g ") + xlab("Log length (cm)")  +
  theme(axis.text.x = element_text(angle = 90))
plot3 <- visreg(mod1, "AgeMatMin", gg = TRUE, size = 4) +
  ylab("log(iron) mg/100g ") + xlab("Age at Maturity (years)") +
  theme(axis.text.x = element_text(angle = 90))
plot4 <- visreg(mod1, "DepthRangeDeep", gg = TRUE, size = 4) +
  ylab("log(iron) mg/100g ") + xlab("Depth (m)") +
  theme(axis.text.x = element_text(angle = 90))
plot5 <- visreg(mod1, "bulk_trophic_level", gg = TRUE, size = 4) +
  ylab("log(iron) mg/100g ") + xlab("Trophic position") +
  theme(axis.text.x = element_text(angle = 90))
plot6 <- visreg(mod1, "part", gg = TRUE, size = 4) +
  ylab("log(calcium) mg/100g ") + xlab("Body part") +
  theme(axis.text.x = element_text(angle = 90))
plot7 <- visreg(mod1, "BodyShapeI", gg = TRUE, size = 4) +
  ylab("log(iron) mg/100g ") + xlab("Body shape") +
  theme(axis.text.x = element_text(angle = 90))
plot8 <- visreg(mod1, "feeding_mode", gg = TRUE, size = 4) +
  ylab("log(iron) mg/100g ") + xlab("Feeding mode") +
  theme(axis.text.x = element_text(angle = 90))
plot9 <- visreg(mod1, "feeding_level", gg = TRUE, size = 4) +
  ylab("log(iron) mg/100g ") + xlab("Feeding level") +
  theme(axis.text.x = element_text(angle = 90))

library(patchwork)
plot_all <- plot1 + plot2 + plot3 + plot4 + plot5 + plot6 + plot7 + plot8 +
  plot_annotation(tag_levels = 'A') + plot_layout(ncol = 4)
ggsave("figures/iron-partial-regressions.pdf", plot = plot_all, width = 14, height = 10)
ggsave("figures/iron-partial-regressions-mac.png", plot = plot_all, width = 14, height = 10)


# zinc --------------------------------------------------------------------

cine2 <- cine_traits_new2 %>% 
  filter(part != "not specified") %>% 
  filter(part != "unknown") %>% 
  filter(!is.na(concentration)) %>% 
  filter(nutrient == "zn_mg") %>% 
  mutate(log_concentration = log(concentration)) %>% 
  mutate(log_length = log(Length)) %>%
  ungroup() %>% 
  group_by(latin_name, feeding_mode, feeding_level, BodyShapeI, DemersPelag, part, EnvTemp) %>% 
  summarise_each(funs(mean), AgeMatMin, log_concentration, log_length, Length, bulk_trophic_level, log_length, DepthRangeDeep) %>% 
  ungroup() %>% 
  filter(complete.cases(.))

cine2 <- traits1b %>% 
  filter(part != "not specified") %>% 
  filter(part != "unknown") %>% 
  filter(!is.na(concentration)) %>% 
  filter(nutrient == "zn_mg") %>% 
  ungroup() %>% 
  group_by(species1, feeding_mode, feeding_level, BodyShapeI, DemersPelag, part, EnvTemp) %>% 
  summarise_each(funs(mean), AgeMatMin, log_concentration, log_length, bulk_trophic_level, DepthRangeDeep) %>% 
  ungroup() %>% 
  filter(complete.cases(.))

mod1 <- lm(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + feeding_level +
             DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + part, data = cine2, na.action=na.exclude)
stargazer(mod1, title = "", type = "html", out="tables/cine-models-expanded-zinc.htm",ci=TRUE, ci.level=0.95, digits = 2, single.row = TRUE)

summary(mod1)
plot1 <- visreg(mod1, "DemersPelag", gg = TRUE, size = 4) +
  ylab("log(zinc) mg/100g ") + xlab("Habitat association") +
  theme(axis.text.x = element_text(angle = 90))
plot2 <- visreg(mod1, "log_length", gg = TRUE, size = 4) +
  ylab("log(zinc) mg/100g ") + xlab("Log length (cm)")  +
  theme(axis.text.x = element_text(angle = 90))
plot3 <- visreg(mod1, "AgeMatMin", gg = TRUE, size = 4) +
  ylab("log(zinc) mg/100g ") + xlab("Age at Maturity (years)") +
  theme(axis.text.x = element_text(angle = 90))
plot4 <- visreg(mod1, "DepthRangeDeep", gg = TRUE, size = 4) +
  ylab("log(zinc) mg/100g ") + xlab("Depth (m)") +
  theme(axis.text.x = element_text(angle = 90))
plot5 <- visreg(mod1, "bulk_trophic_level", gg = TRUE, size = 4) +
  ylab("log(zinc) mg/100g ") + xlab("Trophic position") +
  theme(axis.text.x = element_text(angle = 90))
plot6 <- visreg(mod1, "part", gg = TRUE, size = 4) +
  ylab("log(zinc) mg/100g ") + xlab("Body part") +
  theme(axis.text.x = element_text(angle = 90))
plot7 <- visreg(mod1, "BodyShapeI", gg = TRUE, size = 4) +
  ylab("log(zinc) mg/100g ") + xlab("Body shape") +
  theme(axis.text.x = element_text(angle = 90))
plot8 <- visreg(mod1, "feeding_mode", gg = TRUE, size = 4) +
  ylab("log(zinc) mg/100g ") + xlab("Feeding mode") +
  theme(axis.text.x = element_text(angle = 90))
plot9 <- visreg(mod1, "feeding_level", gg = TRUE, size = 4) +
  ylab("log(zinc) mg/100g ") + xlab("Feeding level") +
  theme(axis.text.x = element_text(angle = 90))


plot_all <- plot1 + plot2 + plot3 + plot4 + plot5 + plot6 + plot7 + plot8 +
  plot_annotation(tag_levels = 'A') + plot_layout(ncol = 4)
ggsave("figures/zinc-partial-regressions.pdf", plot = plot_all, width = 14, height = 10)
ggsave("figures/zinc-partial-regressions-mac.png", plot = plot_all, width = 14, height = 10)
 

# epa ---------------------------------------------------------------------



cine2 <- cine_traits_new2 %>% 
  filter(part != "not specified") %>% 
  filter(part != "unknown") %>% 
  filter(!is.na(concentration)) %>% 
  filter(nutrient == "epa") %>% 
  mutate(log_concentration = log(concentration)) %>% 
  mutate(log_length = log(Length)) %>%
  ungroup() %>% 
  group_by(latin_name, feeding_mode, feeding_level, BodyShapeI, DemersPelag, part, EnvTemp) %>% 
  summarise_each(funs(mean), AgeMatMin, log_concentration, log_length, Length, bulk_trophic_level, log_length, DepthRangeDeep) %>% 
  ungroup() %>% 
  filter(complete.cases(.))

cine2 <- traits1b %>% 
  filter(part != "not specified") %>% 
  filter(part != "unknown") %>% 
  filter(!is.na(concentration)) %>% 
  filter(nutrient == "epa") %>% 
  ungroup() %>% 
  group_by(species1, feeding_mode, feeding_level, BodyShapeI, DemersPelag, part, EnvTemp) %>% 
  summarise_each(funs(mean), AgeMatMin, log_concentration, log_length, bulk_trophic_level, DepthRangeDeep) %>% 
  ungroup() %>% 
  filter(complete.cases(.))

mod1 <- lm(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + feeding_level +
             DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI + part, data = cine2, na.action=na.exclude)
stargazer(mod1, title = "", type = "html", out="tables/cine-models-expanded-epa.htm",ci=TRUE, ci.level=0.95, digits = 2, single.row = TRUE)

summary(mod1)
plot1 <- visreg(mod1, "DemersPelag", gg = TRUE, size = 4) +
  ylab("log(EPA) g/100g ") + xlab("Habitat association") +
  theme(axis.text.x = element_text(angle = 90))
plot2 <- visreg(mod1, "log_length", gg = TRUE, size = 4) +
  ylab("log(EPA) g/100g ") + xlab("Log length (cm)")  +
  theme(axis.text.x = element_text(angle = 90))
plot3 <- visreg(mod1, "AgeMatMin", gg = TRUE, size = 4) +
  ylab("log(EPA) g/100g ") + xlab("Age at Maturity (years)") +
  theme(axis.text.x = element_text(angle = 90))
plot4 <- visreg(mod1, "DepthRangeDeep", gg = TRUE, size = 4) +
  ylab("log(EPA) g/100g ") + xlab("Depth (m)") +
  theme(axis.text.x = element_text(angle = 90))
plot5 <- visreg(mod1, "bulk_trophic_level", gg = TRUE, size = 4) +
  ylab("log(EPA) g/100g ") + xlab("Trophic position") +
  theme(axis.text.x = element_text(angle = 90))
plot6 <- visreg(mod1, "part", gg = TRUE, size = 4) +
  ylab("log(EPA) g/100g ") + xlab("Body part") +
  theme(axis.text.x = element_text(angle = 90))
plot7 <- visreg(mod1, "BodyShapeI", gg = TRUE, size = 4) +
  ylab("log(EPA) g/100g ") + xlab("Body shape") +
  theme(axis.text.x = element_text(angle = 90))
plot8 <- visreg(mod1, "feeding_mode", gg = TRUE, size = 4) +
  ylab("log(EPA) g/100g ") + xlab("Feeding mode") +
  theme(axis.text.x = element_text(angle = 90))
plot9 <- visreg(mod1, "feeding_level", gg = TRUE, size = 4) +
  ylab("log(EPA) g/100g ") + xlab("Feeding level") +
  theme(axis.text.x = element_text(angle = 90))


plot_all <- plot1 + plot2 + plot3 + plot4 + plot5 + plot6 + plot7 + plot8 +
  plot_annotation(tag_levels = 'A') + plot_layout(ncol = 4)
ggsave("figures/EPA-partial-regressions.pdf", plot = plot_all, width = 14, height = 10)
ggsave("figures/EPA-partial-regressions-mac.png", plot = plot_all, width = 14, height = 10)



# dha ---------------------------------------------------------------------



cine2 <- cine_traits_new2 %>% 
  filter(part != "not specified") %>% 
  filter(part != "unknown") %>% 
  filter(!is.na(concentration)) %>% 
  filter(nutrient == "dha") %>% 
  mutate(log_concentration = log(concentration)) %>% 
  mutate(log_length = log(Length)) %>%
  ungroup() %>% 
  group_by(latin_name, feeding_mode, feeding_level, BodyShapeI, DemersPelag, part, EnvTemp) %>% 
  summarise_each(funs(mean), AgeMatMin, log_concentration, log_length, Length, bulk_trophic_level, log_length, DepthRangeDeep) %>% 
  ungroup() %>% 
  filter(complete.cases(.))

cine2 <- traits1b %>% 
  filter(part != "not specified") %>% 
  filter(part != "unknown") %>% 
  filter(!is.na(concentration)) %>% 
  filter(nutrient == "dha") %>% 
  ungroup() %>% 
  group_by(species1, feeding_mode, feeding_level, BodyShapeI, DemersPelag, part, EnvTemp) %>% 
  summarise_each(funs(mean), AgeMatMin, log_concentration, log_length, bulk_trophic_level, DepthRangeDeep) %>% 
  ungroup() %>% 
  filter(complete.cases(.))

mod1 <- lm(log_concentration ~ bulk_trophic_level + log_length  + feeding_mode + feeding_level +
             DemersPelag + DepthRangeDeep + AgeMatMin + BodyShapeI  + part, data = cine2, na.action=na.exclude)
stargazer(mod1, title = "", type = "html", out="tables/cine-models-expanded-dha.htm",ci=TRUE, ci.level=0.95, digits = 2, single.row = TRUE)

summary(mod1)
plot1 <- visreg(mod1, "DemersPelag", gg = TRUE, size = 4) +
  ylab("log(DHA) g/100g ") + xlab("Habitat association") +
  theme(axis.text.x = element_text(angle = 90))
plot2 <- visreg(mod1, "log_length", gg = TRUE, size = 4) +
  ylab("log(DHA) g/100g ") + xlab("Log length (cm)")  +
  theme(axis.text.x = element_text(angle = 90))
plot3 <- visreg(mod1, "AgeMatMin", gg = TRUE, size = 4) +
  ylab("log(DHA) g/100g ") + xlab("Age at Maturity (years)") +
  theme(axis.text.x = element_text(angle = 90))
plot4 <- visreg(mod1, "DepthRangeDeep", gg = TRUE, size = 4) +
  ylab("log(DHA) g/100g ") + xlab("Depth (m)") +
  theme(axis.text.x = element_text(angle = 90))
plot5 <- visreg(mod1, "bulk_trophic_level", gg = TRUE, size = 4) +
  ylab("log(DHA) g/100g ") + xlab("Trophic position") +
  theme(axis.text.x = element_text(angle = 90))
plot6 <- visreg(mod1, "part", gg = TRUE, size = 4) +
  ylab("log(DHA) g/100g ") + xlab("Body part") +
  theme(axis.text.x = element_text(angle = 90))
plot7 <- visreg(mod1, "BodyShapeI", gg = TRUE, size = 4) +
  ylab("log(DHA) g/100g ") + xlab("Body shape") +
  theme(axis.text.x = element_text(angle = 90))
plot8 <- visreg(mod1, "feeding_mode", gg = TRUE, size = 4) +
  ylab("log(DHA) g/100g ") + xlab("Feeding mode") +
  theme(axis.text.x = element_text(angle = 90))

plot9 <- visreg(mod1, "feeding_level", gg = TRUE, size = 4) +
  ylab("log(DHA) g/100g ") + xlab("Feeding level") +
  theme(axis.text.x = element_text(angle = 90))


plot_all <- plot1 + plot2 + plot3 + plot4 + plot5 + plot6 + plot7 + plot8 +
  plot_annotation(tag_levels = 'A') + plot_layout(ncol = 4)
ggsave("figures/DHA-partial-regressions.pdf", plot = plot_all, width = 14, height = 10)
ggsave("figures/DHA-partial-regressions-mac.png", plot = plot_all, width = 14, height = 10)
