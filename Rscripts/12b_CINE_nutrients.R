library(tidyverse)
library(stringr)
library(janitor)
library(plotrix)
library(gridExtra)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(broom)
library(cowplot)


CINE_raw_data <- read_csv("data/CINE-raw-fish.csv")
refs <- read_csv("data/CINE-nutrients-fish-references.csv")
trait_data <- read_csv("data-processed/n.long_lat3.csv")
rdis <- read_csv("data-processed/RDIs.csv")

rdis <- rdis %>% 
  mutate(nutrient = str_replace(nutrient, "FE_mg", "fe_mg"),
         nutrient = str_replace(nutrient, "CA_mg", "ca_mg"),
         nutrient = str_replace(nutrient, "ZN_mg", "zn_mg"),
         nutrient = str_replace(nutrient, "FAT", "fat_g"),
         nutrient = str_replace(nutrient, "PROTEIN", "protein_g"))






CINE <- CINE_raw_data %>% 
  clean_names() 


length(unique(CINE$latin_name))
unique(CINE$part)
names(CINE)

CINE <- CINE %>% 
  select(-contains("sample_size"))

str(CINE)

unique(CINE$ca_mg_100g)

omega_3_20_5n3_g_100g
omega_3_22_6n3_g_100g

CINE_rename <- CINE %>% 
  mutate(ca_mg = as.numeric(str_replace(ca_mg_100g, " (.*)", ""))) %>% 
  mutate(zn_mg = as.numeric(str_replace(zn_mg_100g, " (.*)", ""))) %>%
  mutate(fe_mg = as.numeric(str_replace(fe_mg_100g, " (.*)", ""))) %>% 
  mutate(mn_mg = as.numeric(str_replace(mn_mg_100g, " (.*)", ""))) %>% 
  mutate(mg_mg = as.numeric(str_replace(mg_mg_100g, " (.*)", ""))) %>% 
  mutate(epa = as.numeric(str_replace(omega_3_20_5n3_g_100g, " (.*)", ""))) %>% 
  mutate(dha = as.numeric(str_replace(omega_3_22_6n3_g_100g, " (.*)", ""))) %>% 
  mutate(protein_g = as.numeric(str_replace(protein_g_100g, " (.*)", ""))) %>%
  mutate(fat_g = as.numeric(str_replace(fat_g_100g, " (.*)", ""))) %>%
  mutate(fapun_all_g = as.numeric(str_replace(total_pufa_g_100g, " (.*)", ""))) %>% 
  mutate(fapun3 = as.numeric(str_replace(total_omega_3_g_100g, " (.*)", ""))) %>% 
  dplyr::select(1:8, ca_mg, zn_mg, fe_mg, protein_g, fat_g, fapun3, fapun_all_g, mn_mg, mg_mg, epa, dha) 
  
  
  
write_csv(CINE_rename, "data-processed/CINE-fish-nutrients-processed.csv")
### update april 2020 to include epa and dha
write_csv(CINE_rename, "data-processed/CINE-fish-nutrients-processed.csv")

# make figures! -----------------------------------------------------------

CINE_rename <- read_csv("data-processed/CINE-fish-nutrients-processed.csv")

CINE_rename %>% 
  gather(key = nutrient, value = concentration, 9:16) %>% 
  filter(!is.na(concentration)) %>% 
  filter(nutrient == "ca_mg") %>% 
  ggplot(aes(x = part, y = concentration)) + geom_boxplot() + 
  facet_wrap( ~ nutrient, scales = "free")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


calcium <- CINE_rename %>% 
  gather(key = nutrient, value = concentration, 9:16) %>% 
  filter(!is.na(concentration)) %>% 
  filter(nutrient == "ca_mg") 



CINE_long <- CINE_rename %>% 
  gather(key = nutrient, value = concentration, 9:19) %>% 
  filter(!is.na(concentration))
unique(CINE_long$part)

CINE_merge <- CINE_long %>% 
  mutate(part = str_replace(part, "fillet", "muscle")) %>% 
  mutate(part = str_replace(part, "meat", "muscle")) %>%
  mutate(part = str_replace(part, "flesh + skin", "muscle + skin")) %>%
  mutate(part = str_replace(part, "meat + skin", "muscle + skin")) %>%
  mutate(part = str_replace(part, "flesh", "muscle")) %>%
  mutate(part = str_replace(part, "middle cut", "middle")) %>%
  mutate(part = str_replace(part, "roe", "eggs")) %>% 
  mutate(part = str_replace(part, "grease", "oil")) %>% 
  mutate(part = str_replace(part, "tail cut", "muscle")) %>% 
  mutate(part = str_replace(part, "middle", "muscle")) %>%
  mutate(part = str_replace(part, "tail end", "muscle")) %>% 
  mutate(part = str_replace(part, "head end", "muscle")) %>% 
  mutate(part = str_replace(part, "muscle, dark meat", "muscle")) %>% 
  mutate(part = str_replace(part, "muscle, light meat", "muscle")) %>% 
  mutate(part = str_replace(part, "light meat", "muscle")) %>% 
  mutate(part = str_replace(part, "white meat", "muscle")) %>% 
  mutate(part = str_replace(part, "white muscle", "muscle")) %>% 
  mutate(part = str_replace(part, "muscle, dark muscle", "muscle")) %>% 
  mutate(part = str_replace(part, "muscle, light muscle", "muscle")) %>% 
  mutate(part = str_replace(part, "dark muscle", "muscle")) %>% 
  mutate(part = str_replace(part, "muscle, cheeks", "muscle"))

unique(CINE_long$part)

write_csv(CINE_merge, "data-processed/CINE-body-parts-2020.csv")
write_csv(CINE_merge, "data-processed/CINE-body-parts.csv")

CINE_merge <- read_csv("data-processed/CINE-body-parts.csv")

CINE_merge %>% 
  ggplot(aes(x = part, y = concentration)) + geom_boxplot() + 
  facet_wrap( ~ nutrient, scales = "free")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

CINE_merge %>% 
  filter(nutrient == "ca_mg") %>% 
  ggplot(aes(x = part, y = concentration)) + geom_boxplot() + 
  facet_wrap( ~ nutrient, scales = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Micronutrients only -----------------------------------------------------


cine_micro <- CINE_merge %>% 
  # filter(part %in% c("whole", "muscle")) %>% 
  filter(nutrient %in% c("ca_mg", "fe_mg", "zn_mg")) %>% 
  filter(!grepl("spp", latin_name))

# whitefish <- common_to_sci("Whitefish")

theme_set(theme_cowplot())
spc <- species(cine_micro$latin_name) %>% 
  filter(!is.na(Genus))

spc %>% 
  mutate(Length = as.numeric(Length)) %>% 
  ggplot(aes(x = Length)) + geom_histogram()


cine_calcium %>% 
  group_by(latin_name, part) %>% 
  summarise(concentration = mean(concentration)) %>% 
  ggplot(aes(x = concentration)) + geom_histogram() + 
  facet_wrap( ~ part)

cine_calcium$latin_name
 
cine_calcium %>% 
  filter(latin_name == "Thunnus spp.") %>% View
 

CINE_merge %>% 
  filter(nutrient %in% c("ca_mg", "zn_mg", "fe_mg", "mn_mg")) %>% 
  filter(part != "not specified") %>% 
  filter(nutrient == "ca_mg", concentration < 1000) %>% 
  # filter(part != "tongues + cheeks") %>% 
  filter(part != "whole, no skin") %>% 
  mutate(nutrient = str_replace(nutrient, "ca_mg", "calcium")) %>% 
  group_by(nutrient, part) %>% 
  summarise_each(funs(mean, std.error), concentration) %>% 
  ggplot(aes(x = reorder(part, mean), y = mean)) + geom_point(size = 5) +
  geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error), width = 0.2) +
  facet_wrap( ~ nutrient, scales = "free")+
  geom_hline(yintercept = 1200/10) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text=element_text(family="Helvetica", size=12)) +
  theme(strip.background = element_blank()) +
  theme(legend.title=element_blank()) +
  theme(strip.text.y = element_text(size = 12)) +
  theme(text = element_text(size=24),
        axis.text.x = element_text(angle=45, hjust=1)) + xlab("") + ylab("mg/100g edible portion")


fe_plot <- CINE_merge %>% 
  filter(nutrient %in% c("ca_mg", "zn_mg", "fe_mg", "mn_mg")) %>% 
  filter(part != "not specified") %>% 
  filter(nutrient == "fe_mg") %>% 
  # filter(part != "tongues + cheeks") %>% 
  # filter(part != "whole, no skin") %>% 
  mutate(nutrient = str_replace(nutrient, "fe_mg", "iron")) %>% 
  group_by(nutrient, part) %>% 
  summarise_each(funs(mean, std.error), concentration) %>%
  ggplot(aes(x = reorder(part, mean), y = mean)) + geom_point(size = 5) +
  geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error), width = 0.2) +
  facet_wrap( ~ nutrient, scales = "free")+
  geom_hline(yintercept = 18/10) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text=element_text(family="Helvetica", size=12)) +
  theme(strip.background = element_blank()) +
  theme(legend.title=element_blank()) +
  theme(strip.text.y = element_text(size = 12)) +
  theme(text = element_text(size=24),
        axis.text.x = element_text(angle=45, hjust=1)) + xlab("") + ylab("mg/100g edible portion")
zn_plot <- CINE_merge %>% 
  filter(nutrient %in% c("ca_mg", "zn_mg", "fe_mg", "mn_mg")) %>% 
  filter(part != "not specified") %>% 
  filter(nutrient == "zn_mg") %>% 
  # filter(part != "tongues + cheeks") %>% 
  # filter(part != "whole, no skin") %>% 
  mutate(nutrient = str_replace(nutrient, "zn_mg", "zinc")) %>% 
  group_by(nutrient, part) %>% 
  summarise_each(funs(mean, std.error), concentration) %>%
  ggplot(aes(x = reorder(part, mean), y = mean)) + geom_point(size = 5) +
  geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error), width = 0.2) +
  facet_wrap( ~ nutrient, scales = "free")+
  geom_hline(yintercept = 11/10) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text=element_text(family="Helvetica", size=12)) +
  theme(strip.background = element_blank()) +
  theme(legend.title=element_blank()) +
    theme(strip.text.y = element_text(size = 12)) +
  theme(text = element_text(size=24),
        axis.text.x = element_text(angle=45, hjust=1)) + xlab("") + ylab("mg/100g edible portion")




  microlement_body_part_plot <- grid.arrange(ca_plot, zn_plot, fe_plot, ncol = 3)
  ggsave("figures/microelements_body_part.png", width = 14, height = 8, plot = microlement_body_part_plot)
  
  
  body_parts_plot <- plot_grid(ca_plot, fe_plot, zn_plot, labels = "AUTO", align = 'h', nrow = 1)  
  save_plot("figures/figureS1_body_parts.pdf", body_parts_plot, base_width = 14, base_height = 7)
 
  ?save_plot 
  
  CINE_merge %>% 
    filter(nutrient %in% c("ca_mg", "zn_mg", "fe_mg")) %>% 
    group_by(nutrient, part) %>% 
    summarise_each(funs(mean, std.error), concentration) %>%
    group_by(nutrient) %>% 
    arrange(mean) %>% 
    ggplot(aes(x = reorder(part, mean), y = mean)) + geom_point(size = 5) +
    facet_wrap( ~ nutrient, scales = "free") +
    geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error), width = 0.2)
  
  
  
  ### now stats
  
  unique(CINE_merge$nutrient)
  
  CINE_merge %>% 
    group_by(nutrient) %>% 
    do(glance(lm(concentration ~ part, data = .), conf.int = TRUE)) %>% View
  
  
  ?xtable
  
CINE_merge %>% 
    filter(part != "not specified") %>% 
   filter(part != "tongues + cheeks") %>% 
    filter(part != "whole, no skin") %>% 
    filter(nutrient =="zn_mg") %>% 
    lm(concentration ~ part, data = .) %>% 
    summary %>% 
   xtable()
  
 print(table1, type = "html")
 
 
 CINE_merge %>% 
   filter(part != "not specified") %>% 
   filter(nutrient == "fapun3") %>% 
   mutate(nutrient = str_replace(nutrient, "fat_g", "fat")) %>% 
   # filter(part != "tongues + cheeks") %>% 
   # filter(part = "whole, no skin") %>% 
   group_by(nutrient, part) %>% 
   summarise_each(funs(mean, std.error), concentration) %>%
   group_by(nutrient) %>% 
   arrange(mean) %>% 
   ggplot(aes(x = reorder(part, mean), y = mean)) + geom_point(size = 5) +
   geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error), width = 0.2) +
   facet_wrap( ~ nutrient, scales = "free") +
   geom_hline(yintercept = 50/10) +
   theme_bw() +
   theme(text = element_text(size=24),
         axis.text.x = element_text(angle=45, hjust=1)) + xlab("body part") + ylab("mg/100g edible portion")
 