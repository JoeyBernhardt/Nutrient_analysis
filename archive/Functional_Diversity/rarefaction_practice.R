###Rarefaction
library(dplyr)
library(readr)
library(vegan)
source("http://www.jennajacobs.org/R/rarefaction.txt")
emend.rare<-rarefaction(emend, col=F)
?rarefaction

##### Examples from Jenna Jacob's website:

#Try this example dataset from Work et al (2010):
# 1) import the emend dataset from my website
emend<-read.csv("http://www.jennajacobs.org/R/EMEND.csv", row.names=1)

# 2) load the vegan library
library(vegan)

# 3) The the rarefaction function
source("C:\\Program Files\\R\\functions\\rarefaction.txt")
#or 
source("http://www.jennajacobs.org/R/rarefaction.txt")

# 4) Rarefy your dataset
emend.rare<-rarefaction(emend, col=F) # I'm a big fan of B&W
View(emend)


5) The rarefaction function creates 3 objects to access them type

View(emend.rare$richness) # a matrix of the mean richness at each subsample
emend.rare$SE  # a matrix of the iterative SE of the mean richness at each subsample
emend.rare$subsample # the subsample sizes used to find the means

You can then use these files to create pretty graphs in R or if you must you can export then with

write.csv(emend.rare$richness, file="EMENDrichness.csv")

#### My data
RDI.mat <- read_csv("RDImat.csv")
View(RDI.mat)
spec <- specaccum(RDI.mat, method = "random")
plot(spec, ylim = c(0,6))


# ### turn the RDI matrix into integer values in micrograms
# RDI.um <- RDI.mat %>% 
#   mutate_each(funs(um = .*1000)) %>% 
#   mutate_each(funs(um = as.integer(.)))
# 
# RDI.rare <- rarefaction(RDI.um)

plot(spec, ylim = c(0,6)) #plots the species accumulation curve and the confidence intervals for sites.
plot(spec, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", xlim = c(0,15), ylim = c(0,6)) #males a prettier plot


RDI.rare <- rarefy(RDI.mat, 2, MARGIN = 2)
RDI.rare

###Example from the Ricolleta paper – requires the 'ade4' and 'FD' libraries
rare_Rao <- function(dframe, distance_matrix, sim = TRUE, resampling = 999)
{
  
  if (!inherits(dframe, "data.frame"))
    stop("Non convenient dframe")
  if (any( dframe < 0))
    stop("Negative value in dataframe")
  if (any(apply(dframe, 2, sum) < 1e-16))
    stop("Empty samples")
  if (!is.null(distance_matrix)) {
    if (!inherits(distance_matrix, "dist"))
      stop("Object of class 'dist' expected for distance")
    if (!is.euclid(sqrt(distance_matrix)))
      warning("Squared Euclidean or Euclidean property expected for distance")
    if (nrow(dframe) != attributes(distance_matrix)$Size)
      stop("Non convenient data.frame")
  }
  N <- ncol(dframe)
  rel_abu <- sweep(dframe, 2, colSums(dframe), "/")
  if(sim){
    p <- resampling
    result <- array(dim = c(p, N))
    for(j in 1:N){
      for(i in 1:p) {
        subsample <- sample(rel_abu, size = j)
        newplot <- rowMeans(subsample)
        newplot <- as.matrix(newplot)
        result[i, j] <- (t(newplot) %*% (as.matrix(distance_matrix)) %*%  newplot)
      }
    }
    aver <- colMeans(result)
    IC_plus <- aver + (1.96*(sd(result)/sqrt(p)))
    IC_neg <- aver - (1.96*(sd(result)/sqrt(p)))
    rrRao <- data.frame(as.matrix(aver), IC_neg, IC_plus)
    names(rrRao) <- c("ExpRao", "LeftIC", "RightIC")
    return(rrRao)
  }
  else{
    a <- mean(diag(t(rel_abu)%*%as.matrix(distance_matrix)%*%as.matrix(rel_abu)))
    f2 <- apply(rel_abu, 1, sum)/sum(rel_abu)
    g <- t(f2)%*%as.matrix(distance_matrix)%*%f2
    b <- g - a
    rrARao <- cbind.data.frame(sapply(1:N, function(M) return(a + N * (M-1) * b / M / (N-1))))
    names(rrARao) <- c("ExpRao")
    return(rrARao)
  }
} 



library(ade4)
data(aviurba)
str(aviurba)
distances<-dist.ktab(ktab.list.df(list(aviurba$traits)), type = "N")
str(distances)
# The distances should be squared Euclidean; note that Euclidean distances can be used as they also are squared Euclidean.
abundances<- as.data.frame(t(aviurba$fau))
View(abundances)
# abundances should be a data frame with species as rows and communities as columns
rare_Rao(abundances, distances, sim = TRUE, resampling = 100)
rare_Rao(abundances, distances, sim = FALSE)

?dist.ktab
