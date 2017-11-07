library(broom)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(plotrix)


inuit_resampling <- read_csv("data-processed/grams-required-10-spp-100reps-inuit.csv")
reps100 <- read_csv("data-processed/grams-required-10-spp-1000reps.csv")
bang_resampling <- read_csv("data-processed/grams-required-10-spp-1000reps-bangladesh.csv")
yupik_resampling <- read_csv("data-processed/grams-required-10-spp-100reps-yupik.csv")

summary <- inuit_resampling %>%
  filter(!is.na(grams_required)) %>% 
  mutate(grams_for_25_percent = grams_required/10) %>% 
  group_by(species_no) %>%
  summarise_each(funs(mean, min, max, median, std.error), grams_required, grams_for_25_percent)


summary %>% 
  select(grams_for_25_percent_median, species_no) %>% 
  ggplot(aes(x = species_no, y = grams_for_25_percent_median)) + geom_line()


inuit <- inuit_resampling %>% 
 filter(!is.infinite(grams_required))

yupik <- yupik_resampling %>% 
  filter(!is.infinite(grams_required))

# inuit %>% 
#   ggplot(aes(x = species_no, y = grams_required)) + geom_point()
# 
# yupik %>% 
#   ggplot(aes(x = species_no, y = grams_required)) + geom_point()


inuit.fit <- nls(formula=(grams_required ~ a * species_no^b), data=inuit, start = c(a=10000, b=-0.7))
yupik.fit <- nls(formula=(grams_required ~ a * species_no^b), data=yupik, start = c(a=10000, b=-0.7))
full.fit <- nls(formula=(grams_required ~ a * species_no^b), data=reps100, start = c(a=10000, b=-0.7))


summary(inuit.fit)
summary(yupik.fit)
summary(full.fit)

bind_rows(tidy(inuit.fit, conf.int = TRUE), tidy(full.fit, conf.int = TRUE)) 
bind_rows(tidy(inuit.fit, conf.int = TRUE), tidy(yupik.fit, conf.int = TRUE)) 

full <- tidy(full.fit, conf.int = TRUE) %>% 
  mutate(dataset = "full")
inuit_params <- tidy(inuit.fit, conf.int = TRUE) %>% 
  mutate(dataset = "inuit")
yupik_params <- tidy(yupik.fit, conf.int = TRUE) %>% 
  mutate(dataset = "yupik")


bind_rows(full, inuit_params) %>% 
  ggplot(aes(x = dataset, y = estimate)) + geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, width = 0.1)) +
  facet_wrap( ~ term, scales = "free")

## now let's try a power function

## ok take out the super high numbers, those look like outliers and might be affecting the fit
reps <- reps100 %>% 
  filter(grams_required < 100000) 


full.fit <- nls(formula=(grams_required ~ a * species_no^b), data=reps, start = c(a=10000, b=-0.7))
full_power <- nls(grams_required ~a*exp(-b*species_no), data = reps, start = list(a=10000, b=0.7))
plot(x=reps$species_no,y=reps$grams_required)

a.est <- coef(full.fit)[1]
b.est <- coef(full.fit)[2]
curve(a.est * x^b.est , add=T)

AIC(full_power) - AIC(full.fit)
AIC(full.fit) - AIC(full_power)

plot(x=reps100$species_no,y=reps100$grams_required)
a.est_exp <- coef(full_power)[1]
b.est_exp <- coef(full_power)[2]

curve(a.est_exp *exp(x*-b.est_exp) , add=T)
summary(full_power)
cor(reps$grams_required,predict(full.fit))
cor(inuit$grams_required,predict(inuit.fit))

## ok now let's try with just the means

reps_summary <- reps %>% 
  group_by(species_no) %>% 
  summarise_each(funs(mean, median), grams_required) 

reps_summary %>% 
  ggplot(aes(x = species_no, y = mean)) + geom_point()



### ok here we fit a power function to the median grams required for the full dataset
sum_power <- nls(formula=(median ~ a * species_no^b), data=reps_summary, start = c(a=10000, b=-0.7))
summary(sum_power)
cor(reps_summary$median, predict(sum_power))

a.est <- coef(sum_power)[1]
b.est <- coef(sum_power)[2]

power_function <- function(x) a.est*x^b.est
ggplot(aes(x = species_no, y = median), data = reps_summary) + geom_point(size = 3) +
  stat_function(fun = power_function, color = "grey") + theme_bw() + ylab("median grams required to reach 5 DRI targets") + xlab("species richness") +
  annotate("text", x=7, y=4000, label= "power function fit: a = 4876.64, b = -0.52") + scale_x_continuous(breaks = 1:10)


power_linear <- lm(log(median) ~ log(species_no), data = reps_summary)
summary(power_linear)

tidy(sum_power, conf.int = TRUE)
tidy(power_linear, conf.int = TRUE)

## now let's do this for the bangladesh dataset
bang_sum <- bang_resampling %>% 
  filter(!is.infinite(grams_required)) %>%
  mutate(grams_required = grams_required/10) %>% 
  group_by(species_no) %>% 
  summarise_each(funs(mean, median), grams_required) %>% 
  mutate(dataset = "bangladesh")

inuit_sum <- inuit_resampling %>% 
  filter(!is.infinite(grams_required)) %>% 
  mutate(grams_required = grams_required/10) %>% 
  group_by(species_no) %>% 
  summarise_each(funs(mean, median), grams_required) %>% 
  mutate(dataset = "inuit")

yupik_sum <- yupik_resampling %>% 
  filter(!is.infinite(grams_required)) %>% 
  mutate(grams_required = grams_required/10) %>% 
  group_by(species_no) %>% 
  summarise_each(funs(mean, median), grams_required) %>% 
  mutate(dataset = "yupik") %>% 
  rename(mean = grams_required_mean,
         median = grams_required_median)

reps_sum <- reps100 %>% 
  filter(!is.infinite(grams_required)) %>%
  mutate(grams_required = grams_required/10) %>% 
  group_by(species_no) %>% 
  summarise_each(funs(mean, median), grams_required) %>% 
  mutate(dataset = "global")

### let's plot them all together
all <- bind_rows(bang_sum, inuit_sum, reps_sum)
write_csv(all, "data-processed/summary_resampling_global_bang_inuit.csv")

all <- read_csv("data-processed/summary_resampling_global_bang_inuit.csv") %>% 
  bind_rows(yupik_sum)

all %>% 
  ggplot(aes(x = species_no, y = median, color = dataset)) + geom_line() +
  facet_wrap( ~ dataset) + theme_classic()

bang_power <- nls(formula=(grams_required_median ~ a * species_no^b), data=bang_sum, start = c(a=10000, b=-0.7))
a.bang <- coef(bang_power)[1]
b.bang <- coef(bang_power)[2]

global_power <- nls(formula=(grams_required_median ~ a * species_no^b), data=reps_sum, start = c(a=10000, b=-0.7))
a.global <- coef(global_power)[1]
b.global <- coef(global_power)[2]

inuit_power <- nls(formula=(grams_required_median ~ a * species_no^b), data=inuit_sum, start = c(a=10000, b=-0.7))
a.inuit <- coef(inuit_power)[1]
b.inuit <- coef(inuit_power)[2]

cor(inuit_sum$median, predict(inuit_power))
summary(inuit_power)

bang_power_function <- function(x) a.bang*x^b.bang
global_power_function <- function(x) a.global*x^b.global
inuit_power_function <- function(x) a.inuit*x^b.inuit


grams_plots <- ggplot(aes(x = species_no, y = median, color = dataset), data = all) + geom_line(size = 1) +
  # stat_function(fun = bang_power_function, color = "grey") +
  # stat_function(fun = inuit_power_function, color = "grey") +
  # stat_function(fun = global_power_function, color = "grey") +
  theme_classic() + ylab("Median grams required to reach 5 DRI targets") + xlab("Species richness") +
  scale_x_continuous(breaks = 1:10) +
  scale_color_viridis(discrete = TRUE)
  # annotate("text", x=8, y=330, label= "inuit: a = 408.24, b = -0.18") +
  # annotate("text", x=8, y=210, label= "global: a = 487.66, b = -0.52") +
  # annotate("text", x=8, y=110, label= "bangladesh: a = 207.02, b = -0.48") 

ggsave("figures/bef-curves-color.png", width = 5, height = 5)  

inuit_params <- tidy(inuit_power, conf.int = TRUE) %>% 
  mutate(dataset = "inuit")
bang_params <- tidy(bang_power, conf.int = TRUE) %>% 
  mutate(dataset = "bangladesh")
global_params <- tidy(global_power, conf.int = TRUE) %>% 
  mutate(dataset = "global")

all_params <- bind_rows(inuit_params, bang_params, global_params)
write_csv(all_params, "data-processed/power_function_parameters.csv")
all_params <- read_csv("data-processed/power_function_parameters.csv")

params_plot <- all_params %>% 
  ggplot(aes(x = dataset, y = estimate, color = dataset)) + geom_point(size = 3) +
  facet_wrap( ~ term, scales = "free") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, width = 0.1)) + 
  theme_bw()

grid.arrange(grams_plots, params_plot, ncol = 2)
  

