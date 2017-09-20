
require(ohicore)

library(rCharts)
library(forcats)
library(tidyverse)
library(stringr)
library(multifunc)
library(stringr)
library(cowplot)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)


trait_data <- read_csv("data-processed/n.long_lat3.csv")
RDIs <- read_csv("data-processed/RDIs.csv")

RDI_10 <- trait_data %>% 
  spread(nutrient, concentration) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(mean_ca = mean(100*ca_mg/1200, na.rm = TRUE),
            mean_zn = mean(100*zn_mg/11, na.rm = TRUE), 
            mean_fe = mean(100*fe_mg/18, na.rm = TRUE),
            mean_protein = mean(100*protcnt_g/50, na.rm = TRUE),
            mean_fat = mean(100*fat_g/70, na.rm = TRUE), 
            mean_epa = mean(100*epa/1, na.rm = TRUE),
            mean_dha = mean(100*dha/1, na.rm = TRUE))


###
RDI_10 %>% 
  filter(!is.na(mean_ca), !is.na(mean_zn), !is.na(mean_epa), !is.na(mean_fat), !is.na(mean_protein)) %>% View

mean_nuts <- read_csv("data-processed/mean_nuts.csv")

rdi_per <- mean_nuts %>% 
  mutate(calcium_rdi = calcium/1200) %>% 
  mutate(zinc_rdi = zinc/11) %>% 
  mutate(iron_rdi = iron/18) %>% 
  mutate(epa_rdi = epa/1) %>% 
  mutate(dha_rdi = dha/1) %>% 
  gather(key = nutrient, value = concentration, 8:12)

View(rdi_per)

rdi_per %>% 
  filter(subgroup == "mollusc") %>% 
  ggplot(aes(x = nutrient, y = concentration, group = species_name, color = species_name)) +
           geom_point() + geom_line() + coord_polar() + theme_bw() + 
  scale_y_log10()



RDI_mean <- trait_data %>% 
  spread(nutrient, concentration) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(mean_ca = mean(ca_mg, na.rm = TRUE),
            mean_zn = mean(zn_mg, na.rm = TRUE), 
            mean_fe = mean(fe_mg, na.rm = TRUE),
            mean_protein = mean(protcnt_g, na.rm = TRUE),
            mean_fat = mean(fat_g, na.rm = TRUE), 
            mean_epa = mean(epa, na.rm = TRUE),
            mean_dha = mean(dha, na.rm = TRUE)) %>% 
  mutate(RDI.CA = ifelse(mean_ca > 300, 2, 1)) %>% 
  mutate(RDI.FE = ifelse(mean_fe > 4.5, 2, 1)) %>% 
  mutate(RDI.ZN = ifelse(mean_zn > 2.75, 2, 1)) %>%
  mutate(RDI.EPA = ifelse(mean_epa > 0.25, 2, 1)) %>% 
  mutate(RDI.DHA = ifelse(mean_dha > 0.25, 2, 1)) %>% 
  mutate(RDI.protein = ifelse(mean_protein > 12.5, 2, 1)) %>% 
  mutate(RDI.fat = ifelse(mean_fat > 17.5, 2, 1)) 


rdi_mean_long <- RDI_mean %>% 
  gather(key = nutrient, value = concentration, 3:9) %>%
  filter(!is.na(concentration))

rdi_10_long <- RDI_10 %>% 
  gather(key = nutrient, value = concentration, 3:9) %>%
  filter(!is.na(concentration))

## flower plot

## flower plot
rdi_10_long2 <- rdi_10_long %>% 
  mutate(nutrient = str_replace(nutrient, "mean_ca", "calcium")) %>% 
  mutate(nutrient = str_replace(nutrient, "mean_fe", "iron")) %>% 
  mutate(nutrient = str_replace(nutrient, "mean_zn", "zinc")) %>% 
  mutate(nutrient = str_replace(nutrient, "mean_fat", "fat")) %>% 
  mutate(nutrient = str_replace(nutrient, "mean_protein", "protein")) %>% 
  mutate(nutrient = str_replace(nutrient, "mean_epa", "EPA")) %>% 
  mutate(nutrient = str_replace(nutrient, "mean_dha", "DHA")) %>% 
  filter(concentration < 2000)


rdi_10_long2 %>% 
  arrange(species_name, nutrient, concentration) %>% 
  ggplot(aes(y = log(concentration), x = nutrient, color = nutrient, group = species_name)) + geom_jitter(size =4, alpha = 0.5) + 
  geom_hline(yintercept = log(10)) + 
  coord_polar() + 
  # labs(x = "", y = "") +
  facet_wrap( ~ subgroup) +
  theme_bw() + 
  theme(text = element_text(size=18)) +
  theme(legend.position="none") 
ggsave("figures/flower_plots_all_nutrients.png", width = 14, height = 8)


rdi_10_long2 %>% 
  group_by(subgroup, nutrient) %>% 
  # summarise(mean_conc = mean(concentration)) %>% 
  # filter(subgroup == "crustacean") %>% 
  # filter(species_name == "Abramis brama") %>% 
  # filter(species_name == "Mullus surmuletus") %>% 
  # filter(species_name == "Nephrops norvegicus") %>% 
  filter(species_name == "Sepia officinalis") %>% 
  ggplot(aes(y = concentration, x = nutrient, fill = nutrient)) + geom_bar(position = "dodge", stat="identity") + 
  # coord_polar() + 
  # labs(x = "", y = "") +
  # facet_wrap( ~ subgroup) +
  # theme_bw() + 
  theme(text=element_text(family="Helvetica", size=16)) +
  theme(legend.title=element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "white"),
        axis.text.x=element_blank(), axis.ticks.x=element_blank())+
  coord_polar() +
  ylab("Percentage of DRI in a 100g portion") + xlab("") +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  theme(strip.background = element_rect(colour="white", fill="white")) +
  geom_hline(yintercept = 10, color = "grey") +
  geom_hline(yintercept = 25, color = "grey") 
ggsave("figures/flower_diagram_mullus.png", width = 6, height = 6)
ggsave("figures/flower_diagram_abramis.png", width = 6, height = 6)
ggsave("figures/flower_diagram_nephrops.png", width = 6, height = 6)
ggsave("figures/flower_diagram_sepia.png", width = 6, height = 6)
  
  

 rdi_10_long2$nutrient <- factor(rdi_10_long2$nutrient, levels = c("protein", "EPA", "zinc", "iron", "DHA", "fat", "calcium"))

### flower plot panels
fin_flower <- rdi_10_long2 %>% 
 filter(subgroup == "finfish") %>% 
  ggplot(aes(y = concentration, x = nutrient, color = nutrient)) +
  # geom_violin(aes(fill = nutrient), scale = "width", adjust = 5) + 
  geom_boxplot(aes(fill = nutrient))+
  geom_hline(yintercept = 10) + 
  scale_y_log10(breaks = c(1,100)) +
   coord_polar() +
   theme_bw() + 
  theme(text = element_text(size=18)) +
  theme(legend.position="none") +xlab("") + ylab("percentage of DR/100g")

moll_flower <- rdi_10_long2 %>% 
  filter(subgroup == "mollusc") %>% 
  ggplot(aes(y = concentration, x = nutrient, color = nutrient)) +
  # geom_violin(aes(fill = nutrient), scale = "width", adjust = 5) + 
  geom_boxplot(aes(fill = nutrient))+
  geom_hline(yintercept = 10) + 
  scale_y_log10(breaks = c(1,100)) +
  coord_polar() +
  theme_bw() + 
  theme(text = element_text(size=18)) +
  theme(legend.position="none") +xlab("") + ylab("")

crus_flower <- rdi_10_long2 %>% 
  filter(subgroup == "crustacean") %>% 
  ggplot(aes(y = concentration, x = nutrient, color = nutrient)) +
  # geom_violin(aes(fill = nutrient), scale = "width", adjust = 5) + 
  geom_boxplot(aes(fill = nutrient))+
  geom_hline(yintercept = 10) + 
  scale_y_log10(breaks = c(1,100)) +
  coord_polar() +
  theme_bw() + 
  theme(text = element_text(size=18)) +
  theme(legend.position="none") +xlab("") + ylab("")



flowers <- plot_grid(fin_flower, moll_flower, crus_flower, labels = c("A) Finfish", "B) Molluscs", "C) Crustaceans"), align = "h", nrow = 1)


save_plot("figures/flowers.pdf", flowers,
          ncol = 3, # we're saving a grid plot of 2 columns
          nrow = 1, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1
)



ggdraw() +
  draw_plot(fin_flower, 0,0) +
  draw_plot(moll_flower, 0.5, 0) +
  draw_plot(crus_flower, 1, 0) 

aligned_plots <- align_plots(fin_flower, moll_flower, crus_flower)
ggdraw() + draw_grob(aligned_plots[[1]]) + draw_grob(aligned_plots[[2]])



rdi_10_long2 %>% 
  filter(nutrient == "protein") %>% View
  ggplot(aes(concentration)) + geom_histogram(aes(fill = subgroup, color = subgroup), binwidth = 0.8, bins = 5) +
  facet_wrap( ~ subgroup, scales = "free")


rdi_10_rename <- rdi_10_long %>% 
  mutate(nutrient = str_replace(nutrient, "mean_ca", "calcium")) %>% 
  mutate(nutrient = str_replace(nutrient, "mean_fe", "iron")) %>% 
  mutate(nutrient = str_replace(nutrient, "mean_zn", "zinc")) %>% 
  mutate(nutrient = str_replace(nutrient, "mean_fat", "fat")) %>% 
  mutate(nutrient = str_replace(nutrient, "mean_protein", "protein")) %>% 
  mutate(nutrient = str_replace(nutrient, "mean_epa", "EPA")) %>% 
  mutate(nutrient = str_replace(nutrient, "mean_dha", "DHA")) 

rdi_targets <- rdi_10_rename %>% 
  filter(concentration < 2000) %>% 
  arrange(species_name, nutrient, concentration) %>%
  group_by(subgroup, species_name, nutrient) %>% 
  summarize(mean_conc = mean(concentration)) %>%
  mutate(rdi = ifelse(mean_conc > 10, "1", "0")) %>%
  mutate(rdi = as.numeric(rdi)) %>% 
  group_by(species_name) %>%  
  summarise(sum_rdi = sum(rdi))

rdi_mean <- rdi_10_rename %>% 
  filter(concentration < 2000) %>% 
  arrange(species_name, nutrient, concentration) %>%
  group_by(subgroup, species_name, nutrient) %>% 
  summarize(mean_conc = mean(concentration))

all <- left_join(rdi_mean, rdi_targets, by = "species_name")

all %>%  
  ungroup() %>% 
  mutate(subgroup = ifelse(subgroup == "crustacean", "c_crustacean", subgroup)) %>% 
  mutate(subgroup = ifelse(subgroup == "finfish", "a_finfish", subgroup)) %>% 
  mutate(subgroup = ifelse(subgroup == "mollusc", "b_mollusc", subgroup)) %>% 
  arrange(subgroup, species_name, nutrient, mean_conc) %>%
  filter(mean_conc < 750) %>% 
ggplot(aes(y = log(mean_conc), x = factor(sum_rdi), group = species_name)) + geom_jitter(size = 3, aes(color = factor(sum_rdi), alpha = 0.5)) + 
  # geom_line(color = "grey") +
  # geom_jitter(size =1, alpha = 0.5) + 
  # geom_hline(yintercept = log(10)) + 
  coord_polar() + 
  # labs(x = "", y = "") +
  facet_wrap( ~ subgroup) +
  theme_bw() + 
  theme(text = element_text(size=18)) +
  theme(legend.position="none") 
ggsave("figures/flower_plots_all_nutrients.png", width = 14, height = 8)


all %>%  
  ungroup() %>% 
  ggplot(aes(sum_rdi)) + geom_histogram(binwidth = 1) +
  facet_wrap( ~ subgroup)



rdi_10_rename %>% 
  filter(concentration < 2000) %>% 
  arrange(species_name, nutrient, concentration) %>%
  mutate(rdi = ifelse(concentration > 10, "1", "0")) %>%
  mutate(rdi = as.numeric(rdi)) %>% 
  group_by(subgroup, species_name) %>%  
  summarise(sum_rdi = sum(rdi)) %>% 
  mutate(sum_rdi = as.integer(sum_rdi)) %>%
  ggplot(aes(sum_rdi)) + geom_density(aes(linetype = subgroup), size = 0.9, alpha = 0.1, adjust = 3) +
  theme_bw() + xlab("number of DRI targets") + ylab("proportion") + theme(text = element_text(size=18)) + geom_vline(xintercept = 1) +
  scale_x_continuous(breaks=seq(0, 6, 1)) + scale_colour_grey()
ggsave("figures/DRI_number_density_plot.png")  



rdi_10_rename %>% 
  filter(concentration < 2000) %>% 
  arrange(species_name, nutrient, concentration) %>%
  mutate(rdi = ifelse(concentration > 10, "1", "0")) %>%
  mutate(rdi = as.numeric(rdi)) %>% 
  group_by(subgroup, species_name) %>%  
  summarise(sum_rdi = sum(rdi)) %>% 
  mutate(sum_rdi = as.integer(sum_rdi)) %>% 
  group_by(subgroup, sum_rdi) %>% 
  tally() %>% 
  mutate(percent = NA) %>% 
  mutate(percent = ifelse(subgroup == "crustacean", n/59, percent)) %>% 
  mutate(percent = ifelse(subgroup == "mollusc", n/70, percent)) %>% 
  mutate(percent = ifelse(subgroup == "finfish", n/552, percent)) %>% 
  ggplot(aes(x = sum_rdi, y = percent, color = subgroup)) + geom_line(size = 2) +
  theme_bw() + xlab("number of DRI targets") + ylab("percentage of species pool")
ggsave("figures/DRI_number_line_plot.png")  
## 59 for crustaceans, 552 for finfish, 70 for molluscs
  
  
rdi_10_rename %>% 
  filter(concentration < 2000) %>% 
  arrange(species_name, nutrient, concentration) %>%
  mutate(rdi = ifelse(concentration > 10, "1", "0")) %>%
  mutate(rdi = as.numeric(rdi)) %>% 
  group_by(subgroup, species_name) %>%  
  summarise(sum_rdi = sum(rdi)) %>% 
  mutate(sum_rdi = as.integer(sum_rdi)) %>% 
  ggplot(aes(sum_rdi)) + geom_histogram(aes(fill = subgroup, color = subgroup), binwidth = 0.8, bins = 5) +
  facet_wrap(~ subgroup, scales = "free_y") + 
  theme_bw() + xlab("number of DRI targets") + ylab("number of species") + theme(text = element_text(size=18))


?geom_histogram

rdi_mean_mollusc <- rdi_mean_long %>% 
  filter(subgroup == "mollusc") %>% 
  arrange(nutrient, concentration)

PlotFlower(lengths=log(rdi_mean_mollusc$concentration), fill.col = factor(rdi_mean_mollusc$nutrient), center = "mollusc")

rdi_mean_finfish <- rdi_mean_long %>% 
  filter(subgroup == "finfish") %>% 
  arrange(nutrient, concentration)

PlotFlower(lengths=log(rdi_mean_finfish$concentration), fill.col = factor(rdi_mean_finfish$nutrient), center = "finfish")



RDI_finfish <- RDI_10 %>% 
  # filter(subgroup == "finfish")

finfish_rdi <- RDI_target_table %>% 
  # filter(subgroup == "finfish")  
PlotFlower(lengths=finfish_rdi$n, labels = finfish_rdi$RDI.micro.tot, fill.col = "grey", center = "finfish")

RDI_finfish <- RDI_finfish %>% 
  filter(!is.na(mean_ca)) %>% 
  arrange(subgroup, mean_ca)
PlotFlower(lengths=log(RDI_finfish$mean_ca), fill.col = factor(RDI_finfish$subgroup))



rdi_long <- RDI_10 %>% 
  gather(key = nutrient, value = concentration, 3:9) %>%
  filter(!is.na(concentration))


rdi_mollusc <- rdi_long %>% 
  filter(subgroup == "mollusc") %>% 
  arrange(nutrient, concentration)

PlotFlower(lengths=log(rdi_mollusc$concentration), fill.col = factor(rdi_mollusc$nutrient), center = "mollusc")


rdi_crustacean <- rdi_long %>% 
  filter(subgroup == "crustacean") %>% 
  arrange(nutrient, concentration)

PlotFlower(lengths=log(rdi_crustacean$concentration), disk = 0.1, lty = 0, fill.col = factor(rdi_crustacean$nutrient))

rdi_finfish <- rdi_long %>% 
  filter(subgroup == "finfish") %>% 
  arrange(nutrient, concentration)

PlotFlower(lengths=rdi_finfish$concentration, disk = 0.1, fill.col = factor(rdi_finfish$nutrient), lty = 0, plot.outline = 1)



RDI_target_table <- RDI_minerals %>% 
  group_by(subgroup, RDI.micro.tot) %>% 
  summarise(n = n()) %>% 
  mutate(RDI.micro.tot = str_replace(RDI.micro.tot, "0", "no DRI targets")) %>% 
  mutate(RDI.micro.tot = str_replace(RDI.micro.tot, "1", "1 DRI target")) %>% 
  mutate(RDI.micro.tot = str_replace(RDI.micro.tot, "2", "2 DRI targets")) %>% 
  mutate(RDI.micro.tot = str_replace(RDI.micro.tot, "3", "3 DRI targets")) 


crus0 <- tribble(
  ~ subgroup, ~ RDI.micro.tot, ~ n,
  "crustacean", "no DRI targets", "0",
  "mollusc", "no DRI targets", "0"
  
)

crus0 <- crus0 %>% 
  mutate(n = as.integer(n))
RDI_target_table <- bind_rows(RDI_target_table, crus0) 


str(RDI_target_table)

RDI_target_table <- RDI_target_table %>% 
  mutate(RDI.micro.tot = as.factor(RDI.micro.tot)) %>% 
  mutate(RDI.micro.tot = fct_relevel(RDI.micro.tot, c("no DRI targets", "1 DRI target", "2 DRI targets", "3 DRI targets")))
  

levels(RDI_target_table$RDI.micro.tot)


finfish_rdi <- RDI_target_table %>% 
  filter(subgroup == "crustacean")  
PlotFlower(lengths=finfish_rdi$n, labels = finfish_rdi$RDI.micro.tot, fill.col = "grey", center = "crustaceans")

finfish_rdi <- RDI_target_table %>% 
  filter(subgroup == "mollusc")  
PlotFlower(lengths=finfish_rdi$n, labels = finfish_rdi$RDI.micro.tot, fill.col = "grey", center = "molluscs")

finfish_rdi <- RDI_target_table %>% 
  filter(subgroup == "finfish")  
PlotFlower(lengths=finfish_rdi$n, labels = finfish_rdi$RDI.micro.tot, fill.col = "grey", center = "finfish")




ntbl.RDI.all <- trait_data %>% 
  spread(nutrient, concentration) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(mean.CA = mean(ca_mg, na.rm = TRUE),
            mean.ZN = mean(zn_mg, na.rm = TRUE), 
            mean.FE = mean(fe_mg, na.rm = TRUE),
            mean.EPA = mean(epa, na.rm = TRUE),
            mean.DHA = mean(dha, na.rm = TRUE)) %>% 
  mutate(RDI.CA = ifelse(mean.CA > 300, 1, 0)) %>% 
  mutate(RDI.FE = ifelse(mean.FE > 4.5, 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(mean.ZN > 2.75, 1, 0)) %>%
  mutate(RDI.EPA = ifelse(mean.EPA > 0.25, 1, 0)) %>% 
  mutate(RDI.DHA = ifelse(mean.DHA > 0.25, 1, 0)) %>% 
  ungroup() %>% 
  mutate(RDI.micro.tot = rowSums(.[8:12])) %>% 
  filter(!is.na(RDI.micro.tot)) 



RDI_target_table_all <- ntbl.RDI.all %>% 
  group_by(subgroup, RDI.micro.tot) %>% 
  summarise(n = n()) %>% 
  mutate(RDI.micro.tot = str_replace(RDI.micro.tot, "0", "no DRI targets")) %>% 
  mutate(RDI.micro.tot = str_replace(RDI.micro.tot, "1", "1 DRI target")) %>% 
  mutate(RDI.micro.tot = str_replace(RDI.micro.tot, "2", "2 DRI targets")) %>% 
  mutate(RDI.micro.tot = str_replace(RDI.micro.tot, "3", "3 DRI targets")) %>% 
  mutate(RDI.micro.tot = str_replace(RDI.micro.tot, "4", "4 DRI targets")) %>%
  mutate(RDI.micro.tot = str_replace(RDI.micro.tot, "5", "5 DRI targets")) %>% 
  filter(subgroup == "finfish") %>% 
  ggplot(., aes(x = RDI.micro.tot, y = n)) + geom_bar(stat = "identity") +
  coord_polar() + labs(x = "", y = "")
  


finfish_rdi_all <- RDI_target_table_all %>% 
  filter(subgroup == "finfish")  
PlotFlower(lengths=finfish_rdi_all$n, disk = 0.4, labels = finfish_rdi_all$RDI.micro.tot, label.offset =.20, label.cex	= 1.75, fill.col = factor(finfish_rdi_all$RDI.micro.tot), center = "finfish")

mollusc_rdi_all <- RDI_target_table_all %>% 
  filter(subgroup == "mollusc")  
PlotFlower(lengths=mollusc_rdi_all$n, disk = 0.4, labels = mollusc_rdi_all$RDI.micro.tot, label.offset =.20, label.cex	= 1.75, fill.col = factor(crustacean_rdi_all$RDI.micro.tot), center = "mollusc")

crustacean_rdi_all <- RDI_target_table_all %>% 
  filter(subgroup == "crustacean")  
PlotFlower(lengths=crustacean_rdi_all$n, disk = 0.4, labels = crustacean_rdi_all$RDI.micro.tot, label.offset =.20, label.cex	= 1.75, fill.col = factor(crustacean_rdi_all$RDI.micro.tot), center = "crustacean")



library(gridExtra)


grid.arrange(crus_plot, moll_plot, fin_plot, ncol = 3)




# try with ggplot ---------------------------------------------------------

RDI_minerals <- trait_data %>% 
  spread(nutrient, concentration) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(mean.CA = mean(ca_mg, na.rm = TRUE),
            mean.ZN = mean(zn_mg, na.rm = TRUE), 
            mean.FE = mean(fe_mg, na.rm = TRUE)) %>% 
  mutate(RDI.CA = ifelse(mean.CA > 120, 1, 0)) %>% 
  mutate(RDI.FE = ifelse(mean.FE > 1.8, 1, 0)) %>% 
  mutate(RDI.ZN = ifelse(mean.ZN > 1.1, 1, 0)) %>% 
  ungroup() %>% 
  mutate(RDI.micro.tot = rowSums(.[6:8])) %>% 
  filter(!is.na(RDI.micro.tot)) %>% 
  arrange(., RDI.micro.tot)

RDI_minerals %>% 
  group_by(subgroup, RDI.micro.tot) %>% 
  summarise(n = n()) %>%  
  mutate(cum.RDI = cumsum(n)) %>% 
  filter(subgroup == "finfish") %>% 
  ggplot(., aes(x = RDI.micro.tot, y = n)) + geom_bar(stat = "identity") +
  coord_polar() + labs(x = "", y = "")

last_plot() + scale_y_continuous() +
   coord_polar() + labs(x = "", y = "") + opts(legend.position = "none",
                                                    axis.text.x = theme_blank(), axis.text.y = theme_blank(),
                                                    axis.ticks = theme_blank())
