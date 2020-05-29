library(tidyverse)
library(corrplot)


trait_data <- read_csv("data-processed/n.long_lat3.csv") %>% 
  filter(seanuts_id2 < 1671) ### adding this May 29 to double check on units ets
bg <- read_csv("bangladesh-nutrients.csv")
inuit <- read_csv("data-processed/CINE-inuit-mean-nutrients.csv")

bg2 <- bg %>% 
  select(4:25, 30)

in2 <- inuit %>%
  select(-latin_name)

#### 
trait_data %>% 
  filter(is.na(food_name_clean)) %>% 
  distinct(ref_info) %>% View


wide <- trait_data %>% 
  dplyr::select(species_name, subgroup, seanuts_id2, nutrient, concentration, ref_info) %>% 
  distinct(species_name, nutrient, concentration, .keep_all = TRUE) %>% 
  spread(key = nutrient, value = concentration) 


  wide <- trait_data %>% 
dplyr::select(species_name, subgroup, seanuts_id2, nutrient, concentration, ref_info) %>% 
  distinct(species_name, nutrient, concentration, .keep_all = TRUE) %>% 
  spread(key = nutrient, value = concentration) 
  
  
widenuts <- wide %>% 
  dplyr::select(5:15) %>% 
mutate(protein = ifelse(is.na(protcnt_g), protein_g, protcnt_g)) %>% 
  mutate(protein = ifelse(is.na(protein), prot_g, protein)) %>% 
  dplyr::select(-prot_g) %>% 
  dplyr::select(-protein_g) %>% 
  dplyr::select(-protcnt_g) %>% 
  dplyr::select(-starts_with("fapun")) %>% 
  rename(EPA = epa,
         DHA = dha,
         Protein = protein,
         Fat = fat_g,
         Calcium = ca_mg, 
         Iron = fe_mg, 
         Zinc = zn_mg)



trait_data <- read_csv("data-processed/n.long_lat3.csv")



mean_nuts <- trait_data %>% 
  filter(!grepl("^Mohanty", ref_info)) %>%
  filter(seanuts_id2 < 1671) %>% 
  spread(nutrient, concentration) %>% 
  group_by(species_name, subgroup) %>% 
  summarise(calcium = mean(ca_mg, na.rm = TRUE),
            zinc = mean(zn_mg, na.rm = TRUE), 
            iron = mean(fe_mg, na.rm = TRUE),
            epa = mean(epa, na.rm = TRUE),
            dha = mean(dha, na.rm = TRUE)) %>% 
  filter(!is.na(calcium), !is.na(zinc), !is.na(iron), !is.na(epa), !is.na(dha)) %>% 
  ungroup()

mean_nuts <- mean_nuts %>% 
  select(-species_name) %>%
  select(-subgroup)

all_nuts <- bind_rows(mean_nuts, newtrad)

M <- cor(all_nuts, use = "pairwise.complete.obs")
corrplot(M, method = "number")
corrplot.mixed(M)

m2 <- as.data.frame(M) %>% 
  gather() %>% 
  distinct()



m2 %>% 
  filter(value != 1) %>% 
  summarise(mean_corr = mean(value))
m2 %>% 
  filter(value != 1) %>% 
  ggplot(aes(x = value)) + geom_histogram(bins = 35) +
  geom_vline(xintercept = 0) +
  ylab("Frequency") + xlab("Correlation coefficient") 


wide2 <- wide %>% 
  dplyr::select(species_name, 5:15) %>% 
  mutate(protein = ifelse(is.na(protcnt_g), protein_g, protcnt_g)) %>% 
  mutate(protein = ifelse(is.na(protein), prot_g, protein)) %>% 
  dplyr::select(-prot_g) %>% 
  dplyr::select(-protein_g) %>% 
  dplyr::select(-protcnt_g) %>% 
  dplyr::select(-starts_with("fapun")) %>% 
  rename(EPA = epa,
         DHA = dha,
         Protein = protein,
         Fat = fat_g,
         Calcium = ca_mg, 
         Iron = fe_mg, 
         Zinc = zn_mg)

names(wide2)

nut_sum <- wide2 %>% 
  group_by(species_name) %>% 
  summarise_at(names(wide2)[2:8], mean) %>% 
  select(2:8) %>% 
  filter(complete.cases(.))

mean_nuts <- read.csv("data-processed/mean_nuts.csv") %>% 
  select(-species_name) %>%
  select(-subgroup) %>% 
  filter(epa != 154.0000000)

newtrad <- read_csv("data-processed/newtrad.csv") %>% 
  select(-species_name)

all_nuts <- bind_rows(mean_nuts, newtrad)

seadiv <- read_csv("data-processed/mean_seadiv.csv") %>% 
  # filter(!is.na(protein)) %>% 
  select(-species_name) %>% 
  select(-subgroup) %>% 
  filter(complete.cases(.)) %>% 
  mutate_all(., .funs = log)

tscale <- seadiv
tscale <- scale(seadiv, center = TRUE, scale = TRUE)


M <- cor(tscale, use = "pairwise.complete.obs")
corrplot(M, method = "number")
corrplot.mixed(M)

m2 <- as.data.frame(M) %>% 
  gather() %>% 
  distinct()

m2 %>% 
  filter(value != 1) %>% 
  summarise(mean_corr = mean(value))

library(GGally)
ggcorr(tscale, method = c("everything", "pearson"), label = TRUE) 
ggsave("figures/nutrients_correlation_sum.png", width = 8, height = 6)

ggpairs(seadiv) 
ggsave("figures/nutrient-corr-plots-sum.png", width = 15, height = 15)


m2 %>% 
  filter(value != 1) %>% 
  ggplot(aes(x = value)) + geom_density(fill = "lightblue") +
  geom_vline(xintercept = 0) +
  ylab("Density") + xlab("Pairwise correlation coefficient") 
ggsave("figures/nutrients-pairwise-correlations-coefs.png", width = 6, height = 4)

pca_size <- prcomp(tscale, scale. = TRUE)
pca_size2 <- rda(tscale, scale. = TRUE)

summary(pca_size)
summary(pca_size2)

### do an RDA, conditioned on subgroup!
pcas <- as.data.frame(scores(pca_size2, choices = 1:2)$sites)
pcas1 <- as.data.frame(scores(pca_size2, choices = 1:2)$species) %>% 
  mutate(trait = rownames(.))
pcas %>% 
  ggplot(aes(x = PC1, y = PC2)) + geom_point(size = 3, alpha = 0.5) +
  geom_point(size = 3, color = "black", shape = 1) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  geom_text(data = pcas1, aes(x = PC1, y = PC2, label = trait), col = 'cadetblue') +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2, text =  trait), data = pcas1, color = "cadetblue",
               arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  xlab("PC1 (37.83% of variance)") + ylab("PC2 (26.91% of variance)")
ggsave("figures/nutrients-pca.png", width = 8, height = 6)




#### try an RDA

seadiv <- read_csv("data-processed/mean_seadiv.csv") %>% 
  # select(-protein, -fat) %>% 
  filter(complete.cases(.)) 

seadiv2 <- read_csv("data-processed/mean_seadiv.csv") %>% 
  select(-species_name, -subgroup) %>% 
  filter(complete.cases(.)) %>% 
  mutate_all(.funs = log)

seadiv3 <- read_csv("data-processed/mean_seadiv.csv") %>% 
  # select(-species_name, -subgroup) %>% 
  filter(complete.cases(.))
spe <- seadiv2
FULL.cap <- capscale(spe ~ subgroup, data = seadiv3)


# FULL.cap_str <- capscale(spe ~ Strain + Condition(Treatment), data=prot_table)
library(vegan)
summary(FULL.cap)
anova.cca(FULL.cap, by = "axis", step = 1000)
anova.cca(FULL.cap, step = 1000)
coef(FULL.cap)
RsquareAdj(FULL.cap)$adj.r.squared
RsquareAdj(FULL.cap)$r.squared

Res.dim <- as.data.frame(scores(FULL.cap, display="wa", scaling=3)[,1:2])
Res.dim$subgroup <- seadiv3$subgroup
Res.dim$species_name <- seadiv3$species_name
Res.dim

dims <- Res.dim

dims_summary <- dims %>% dplyr::group_by(subgroup) %>% 
  summarise(CAP1mean=mean(CAP1), CAP2mean=mean(CAP2), CAP1sd=sd(CAP1), CAP2sd=sd(CAP2))
trait_vectors <- as.data.frame(scores(FULL.cap, choices = 1:2, display = "sp", scaling = 2)) %>% 
  mutate(trait = rownames(.)) 

ggplot() + 
  geom_errorbar(aes(x = CAP1mean, ymin = CAP2mean - CAP2sd, ymax = CAP2mean + CAP2sd, color = subgroup),
                width = 0, data = dims_summary, size = 1) +
  geom_errorbarh(aes(y = CAP2mean, xmin = CAP1mean - CAP1sd, xmax = CAP1mean + CAP1sd, color = subgroup), width = 0, data = dims_summary, size = 1) +
  geom_point(aes(x = CAP1, y = CAP2, color = subgroup), data = dims, size = 2) +
  geom_point(aes(x = CAP1, y = CAP2), data = dims, size = 2, shape = 1, color = "black") +
  geom_segment(aes(x = 0, y = 0, xend = CAP1, yend = CAP2, text =  trait), data = trait_vectors,
               arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  geom_text(aes(x = CAP1, y = CAP2, label =  trait), data = trait_vectors) +
 geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  scale_color_brewer(type = "qual") + ylab("RDA2") + xlab("RDA2")
ggsave("figures/rda-subgroups.png", width = 8, height = 6)


dist_test_cap1 <- aov(lm(CAP1~subgroup, data=Res.dim))
summary(dist_test_cap1)
TukeyHSD(dist_test_cap1)






wcor <- cor(widenuts, use = "pairwise.complete.obs")
corrplot(wcor, type="lower")

diag(wcor) = NA
corrplot(wcor, method = "color", type = "lower", na.label = "o")



bg3 <- bg2 %>% 
  select(calcium, iron, zinc, vitamin_b12, vitamin_e_α_tocopherol, total_vitamin_a)
  # select(-protein, -fat, - moisture, -ash)

bcor <- cor(bg3, use = "pairwise.complete.obs" )

rcor2 <- rcor
diag(rcor2) = NA
corrplot(rcor2, method = "color", type = "lower")


col4 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", 
                           "cyan", "#007FFF", "blue","#00007F"))   

corrplot(wcor, type = "lower", method = "number", col=col4(10), p.mat = res1[[1]], sig.level=0.05, diag = FALSE)



rcor <- cor(rdi, use = "pairwise.complete.obs")

res1 <- cor.mtest(wcor,0.95)
corrplot(rcor, p.mat = res1[[1]], sig.level=0.2, col=col4(10), type = "upper", na.label = "o")


td <- read_csv("data-processed/mean_nuts.csv")

rdi <- td %>% 
  mutate(cal_per = calcium/1200,
         iron_per = iron/18,
         zinc_per = zinc/11,
         epa_per = epa/1,
         dha_per = dha/1) %>% 
  select(contains("per"))


nuts <- td %>% 
  select(3:7)

rcor <- cor(rdi)

corrplot(rcor, method = "square")


cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}
res1 <- cor.mtest(rcor,0.95)
corrplot(rcor, p.mat = res1[[1]], sig.level=0.05)
corrplot(rcor, p.mat = res1[[1]])



# try again ---------------------------------------------------------------
library(corrplot)
nuts <- read_csv("data-processed/traits_for_analysis.csv") %>% 
  # dplyr::select(nutrient, log_concentration, seanuts_id2) %>% 
  spread(key = nutrient, value = log_concentration) %>% 
  filter(!is.na(epa), !is.na(dha), !is.na(protein), !is.na(fat_g), !is.na(ca_mg), !is.na(zn_mg), !is.na(fe_mg)) %>% 
  dplyr::select(epa, dha, protein, fat_g, ca_mg, zn_mg, fe_mg)

sM <- cor(nuts)
corrplot(M, method = "circle")

wcor <- cor(widenuts, use = "pairwise.complete.obs", method = "spearman")
corrplot(wcor, type="lower")

diag(wcor) = NA
corrplot(wcor, method = "ellipse", type = "lower", na.label = "o")

plot(log(widenuts))
