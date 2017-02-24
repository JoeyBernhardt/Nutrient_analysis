
require(ohicore)

library(rCharts)
library(forcats)



trait_data <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data-processed/n.long_lat3.csv")
RDIs <- read_csv("/Users/Joey/Documents/Nutrient_Analysis/data-processed/RDIs.csv")

RDI_10 <- trait_data %>% 
  spread(nutrient, concentration) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(mean_ca = mean(100*ca_mg/1200, na.rm = TRUE),
            mean.zn = mean(100*zn_mg/11, na.rm = TRUE), 
            mean.fe = mean(100*fe_mg/18, na.rm = TRUE),
            mean_protein = mean(100*protcnt_g/50, na.rm = TRUE),
            mean_fat = mean(100*fat_g/70, na.rm = TRUE), 
            mean_epa = mean(100*epa/1, na.rm = TRUE),
            mean_dha = mean(100*dha/1, na.rm = TRUE))


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
  gather(key = nutrient, value = concentration, 10:16) %>%
  filter(!is.na(concentration))


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
  mutate(RDI.micro.tot = str_replace(RDI.micro.tot, "5", "5 DRI targets"))


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

