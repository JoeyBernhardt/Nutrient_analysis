library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(extrafont)
library(gridExtra)
library(RColorBrewer)
library(cowplot)
library(viridis)

### making the grouped bar chart for number of DRI targets reached, figure 2

nuts <- read_csv("data-processed/mean_nuts.csv")
View(nuts)
font_import()

rdis <- nuts %>% 
  mutate(calcium_rdi = ifelse(calcium > (1200*.1), 1, 0)) %>% 
  mutate(zinc_rdi = ifelse(zinc > (11*.1), 1, 0)) %>% 
  mutate(iron_rdi = ifelse(iron > (18*.1), 1, 0)) %>% 
  mutate(epa_rdi = ifelse(epa > (1*.1), 1, 0)) %>% 
  mutate(dha_rdi = ifelse(dha > (1*.1), 1, 0)) %>% 
  mutate(rdi_tot = rowSums(.[8:12]))

View(rdis)

rdis %>% 
  summarise_each(funs(sum), 8:12) %>% View


rdi_prop <- rdis %>% 
  group_by(subgroup, rdi_tot) %>% 
  summarise(n = n()) %>% 
  group_by(subgroup) %>% 
  arrange(subgroup, -rdi_tot) %>% 
  mutate(cum.RDI = cumsum(n)) %>%
  mutate(proportion = NA) %>% 
  mutate(proportion = ifelse(subgroup == "crustacean", cum.RDI/6, proportion)) %>% 
  mutate(proportion = ifelse(subgroup == "mollusc", cum.RDI/12, proportion)) %>%
  mutate(proportion = ifelse(subgroup == "finfish", cum.RDI/78, proportion)) %>%
  mutate(rdi_level = NA) %>% 
  mutate(rdi_level = ifelse(rdi_tot == 0, "0+", rdi_level)) %>% 
  mutate(rdi_level = ifelse(rdi_tot == 1, "1+", rdi_level)) %>% 
  mutate(rdi_level = ifelse(rdi_tot == 2, "2+", rdi_level)) %>% 
  mutate(rdi_level = ifelse(rdi_tot == 3, "3+", rdi_level)) %>% 
  mutate(rdi_level = ifelse(rdi_tot == 4, "4+", rdi_level)) %>% 
  mutate(rdi_level = ifelse(rdi_tot == 5, "5", rdi_level)) 


figure2a <- rdi_prop %>% 
  ggplot(., aes(x = rdi_level, y = proportion, group = subgroup, fill = subgroup)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_brewer(type = "qual", palette = "Paired") +
  # scale_fill_manual(values = c("grey94", "grey35", "grey0")) +
    # scale_x_continuous(breaks = c(0:5)) +
    facet_wrap( ~ subgroup) + theme_bw() + xlab("Number of 10% DRI targets reached per 100g portion") +
  ylab("Proportion") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text=element_text(family="Helvetica", size=12)) +
  theme(strip.background = element_blank()) +
  theme(legend.position="none")
  ggsave("figures/figure2b_number_targets_barchart.pdf")
  
  
percentages <- read_csv("data-processed/percentages.csv")  
percentages$nutrient <- factor(percentages$nutrient, levels = c("protein", "fat", "calcium", "zinc", "iron", "epa", "dha"))

pp <- percentages %>% 
  filter(species_name == "Chamelea gallina")

length(unique(percentages$species_name))


  figure2b <- ggplot(percentages, aes(dri_per, fill = subgroup)) + geom_histogram(binwidth = 0.07) +
    scale_fill_brewer(type = "qual", palette = "Paired") +
    # scale_fill_viridis(discrete = TRUE) +
    # scale_color_manual(values = c("white", "white", "white")) +
    scale_x_log10(breaks = c(1, 10, 100)) +
    # scale_fill_brewer(type = "qual", palette = "Paired") +
    # scale_color_brewer(type = "qual", palette = "Paired") +
    facet_grid(nutrient ~ subgroup, scales = "free_y", switch = "x") + theme_bw() + geom_vline(xintercept = 10) +
    xlab("Percentage of DRI in 100g edible portion") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    theme(text=element_text(family="Helvetica", size=12)) +
    theme(strip.background = element_blank()) +
    theme(legend.title=element_blank()) +
    # theme(strip.text = element_text(hjust = 0),
          # strip.text.x = element_text(size=16, face="plain")) +
    theme(strip.text.y = element_text(size = 12)) +
    theme(legend.position="none") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 2))
  ggsave("figures/dri_histogram_figure2.pdf")
  
  
  
p <- plot_grid(figure2b, figure2a, labels = c("A", "B"), ncol = 1, nrow = 2, align = "v")  


p <- ggdraw() +
  draw_plot(figure2b, x = 0, y = 0.3, width = 0.95, height = 0.67) +
  draw_plot(figure2a, x = 0, y = 0, width = 0.95, height = 0.3) +
  draw_plot_label(c("A", "B"), c(0, 0), c(0.95, 0.3), size = 15)

?draw_plot_label

save_plot("figures/figure2.png", p,
          ncol = 1, # we're saving a grid plot of 2 columns
          nrow = 2,
          base_height = 3.8, base_width = 6)

?save_plot
