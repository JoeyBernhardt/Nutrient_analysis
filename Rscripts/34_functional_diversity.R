library(FD)

trait_data <- read_csv("data-processed/n.long_lat3.csv") %>% 
  filter(!grepl("^Mohanty", ref_info)) 

ntbl.RDI.mic <- trait_data %>% 
  spread(nutrient, concentration) %>% 
  # select(species_name, zn_mg, fe_mg, ca_mg, epa, dha) %>% 
  dplyr::group_by(species_name) %>% str()
   summarise(mean.CA = mean(ca_mg, na.rm = TRUE),
             mean.EPA = mean(epa, na.rm = TRUE), 
             mean.DHA = mean(dha, na.rm = TRUE), 
             mean.ZN = mean(zn_mg, na.rm = TRUE), 
             mean.FE = mean(fe_mg, na.rm = TRUE)) %>% View
  # summarise_each(funs(mean), zn_mg, fe_mg, ca_mg, epa, dha) %>% 
  filter(!is.na(zn_mg), !is.na(ca_mg), !is.na(fe_mg), !is.na(epa), !is.na(dha)) %>% View
  
  
mean_nuts <- read_csv("data-processed/mean_nuts.csv")  
  

ntbl.matrix.mic <- data.matrix(mean_nuts[, 3:7])
rownames(ntbl.matrix.mic) <- mean_nuts$species_name

write.csv(ntbl.matrix.mic, "RDI.mic.csv")

View(ntbl.matrix.mic)

comm <- matrix(data= c(1,0,0,1,0,1,1,1,0,1,1,0,1,0,0,0,1,0,0,1),nrow = 4, ncol = 5)
row.names(comm) <- c("Spain", "Colombia", "France", "Cambodia")
colnames(comm) <- c("Sp1", "Sp2", "Sp3", "Sp4", "Sp5")




FD.mic <- dbFD(ntbl.matrix.mic)
FD.mic


ntbl.mac.matrix <- data.matrix(ntbl.RDI.mac[, 2:3])
rownames(ntbl.mac.matrix) <- ntbl.RDI.mac$species

View(ntbl.mac.matrix)
FD.mac <- dbFD(ntbl.mac.matrix)
knitr::kable(as.data.frame(FD.mac))
knitr::kable(as.data.frame(FD.mic))
FD.mac$FDiv




fd.exp <- function(mat, n) {
  randsp.data<- mat[sample(1:length(row.names(mat)), n, replace = FALSE),]
  fdiv.exp<- dbFD(randsp.data, messages = FALSE)$FDiv
}

# Replicate the function 10000 times, and save 10,000 random FD values as a csv
fd.null <- as.vector(replicate(1000, fd.exp(ntbl.matrix.mic, 10)))
fd.null <- as.data.frame(fd.null)
write_csv(fd.null, "fd.null.csv")
hist(fd.null$fd.null)

quantile(fd.null$fd.null, c(.05, .95))

fd_null <- read_csv("Functional_Diversity/fd.null.csv")
hist(fd_null$fd.null)
quantile(fd_null$fd.null, c(.05, .95))

###Cambodia, which has 4 species

fd.null.cam <- as.vector(replicate(10000, fd.exp(ntbl.matrix.mic, 4)))
mean(fd.null.cam)
hist(fd.null.cam)
abline(v = 0.9047908, col = "red") #this is the value of FDiv I calculated for the Cambodia species
abline(v = 0.8944811, col = "blue") #95% quantile
abline(v = 0.7021020, col = "blue") # 5% quantile
quantile(fd.null.cam, c(.05, .95))

