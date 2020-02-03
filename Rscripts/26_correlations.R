library(tidyverse)
library(corrplot)


trait_data <- read_csv("data-processed/n.long_lat3.csv")
bg <- read_csv("bangladesh-nutrients.csv")
inuit <- read_csv("data-processed/CINE-inuit-mean-nutrients.csv")

bg2 <- bg %>% 
  select(4:25, 30)

in2 <- inuit %>%
  select(-latin_name)

wide <- trait_data %>% 
  dplyr::select(species_name, subgroup, seanuts_id2, nutrient, concentration, ref_info) %>% 
  distinct(species_name, nutrient, concentration, .keep_all = TRUE) %>% 
  spread(key = nutrient, value = concentration) 



wide %>% 
  filter(!is.na(protein_g)) %>% View

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


widenuts %>% 
  filter(is.na(protein)) %>% View



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
