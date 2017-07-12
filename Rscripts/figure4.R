

data <- read_csv("data-processed/n.long_lat3.csv") %>% 
  filter(!grepl("^Mohanty, B. P.,", ref_info))

n.long <- read_csv("data-processed/n.long_jan18.csv")

mod_all <- n.long %>% 
  mutate(log_length = log(bulk_max_length),
         log_concentration = log(concentration)) %>% 
  filter(!grepl("^Mohanty, B. P.,", ref_info))


n.long %>% 
  # filter(nutrient == "ca_mg") %>% 
  filter(concentration > 0) %>%
  filter(nutrient %in% c("ca_mg", "zn_mg", "fe_mg")) %>% 
  mutate(nutrient = ifelse(nutrient == "ca_mg", "calcium", nutrient)) %>% 
  mutate(nutrient = ifelse(nutrient == "fe_mg", "iron", nutrient)) %>% 
  mutate(nutrient = ifelse(nutrient == "zn_mg", "zinc", nutrient)) %>% 
  ggplot(aes( x= log(bulk_mean_length), y = log(concentration))) +
  geom_point(size = 3, alpha = 0.5, ) + geom_smooth(method = "lm", color = "black") +
  theme_bw() + ylab("ln nutrient concentration (mg/100g)") + xlab("ln body length") + facet_wrap( ~ nutrient, scales = "free") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text=element_text(family="Helvetica", size=16)) +
  theme(strip.background = element_blank()) +
  theme(legend.title=element_blank()) +
  theme(strip.text = element_text(),
  strip.text.x = element_text(size=16, face="plain")) +
  theme(strip.text.y = element_text(size = 16))

ggsave("figures/figure4.pdf", width = 10, height = 3.6)
