
library(vegan)
all_traits_nuts1 <- read_csv("data-processed/all-traits-nuts.csv")
all_traits_nuts2 <- read_csv("data-processed/all-traits-nuts-updated.csv")


length(unique(all_traits_nuts1$Species))
length(unique(all_traits_nuts2$Species))


traits <- all_traits_nuts1 %>% 
  distinct(Species, .keep_all = TRUE) %>% 
  dplyr::select(1:10) %>% 
  filter(complete.cases(.)) %>% 
  ungroup() %>% 
  sample_n(20, replace = FALSE) %>% 
  mutate(EnvTemp = as.numeric(as.factor(EnvTemp))) %>%
  mutate(DemersPelag = as.numeric(as.factor(DemersPelag))) %>%
  mutate(BodyShapeI = as.numeric(as.factor(BodyShapeI))) %>%
  mutate(FeedingType = as.numeric(as.factor(FeedingType))) %>%
  mutate(Herbivory2 = as.numeric(as.factor(Herbivory2))) %>% 
  dplyr::select(-Species) %>% 
  scale(.) 


pca_size2 <- rda(traits, scale. = TRUE)

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
  xlab("PC1 (26.4% of variance)") + ylab("PC2 (15.69% of variance)")
