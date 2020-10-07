#  Calcualting Petchey & Gaston's FD, Rao's Q, Shannon-Wiener H', and S for communities
#  Dan Flynn | dff2101@columbia.edu

#  Calculate.Q returns Rao's quadratic entropy (Q) as a measure of functional diversity for a set of communites, given another set of species traits. 
#  Landuse (communities) and trait files are data frames with species codes as rownames, sorted by the codes. All of the species in the landuse file must be within the trait file, but not necessarily vice versa. 
#  Land use types must be in alphbetical order, left to right (for labeling dendrograms correctly; not necessary if not writing the files out to PDFs).
#  Usage: When the files are in the current working directory (check with getwd() ), read them in with, e.g., Landuse.Best <- read.csv("Landuse.Best.csv") (For data from Best et al.)   
#  Then type Calculate.Q(Landuse.Best, BirdTraits).
#  Writing the files to the working directory by adding Calculate.Q(Landuse.Best, BirdTraits, T).
#  By default, the values of Q and FD are scaled to the maximum value in that set of communities, for comparing across studies. Set scale = FALSE to return raw values.

#  Calculate.Q and Calculate.FD wrappers modified by Oliver Soong

Calculate.Q <- function(landfile, traitfile, writefiles = F, scale = T){

	traits <- traitfile
	lands <- landfile

	community.names = names(lands) # get community type names

	lands <- unlist(lands) # Vectorize data frames
	dim(lands) <- NULL # Vectorize matrices

	sppcomm <- data.frame(species = rep(rownames(landfile), length(community.names)), community = rep(community.names, each = nrow(landfile)), abundance = lands)
	sppcomm <- sppcomm[!is.na(sppcomm$abundance), ]

	# Reorders the species and community lists to make a dataframe with three columns: species, community, and abundance.

library(cluster)  # to get the daisy() function -- can use nominal data, unlike dist()

Q <- by(sppcomm, sppcomm$community,   
	function(x) {
		DF.Q(
			traits[!is.na(match(toupper(rownames(traits)), toupper(x$species))),],
			x$abundance
		)
	}
)

Q <- unclass(Q) # Turn the by object into something useable
attributes(Q)$call <- NULL

if (scale == TRUE) {
	Qmax <- max(Q, na.rm = TRUE) # Get maximum value, handle NA values properly
		if(Qmax != 0) {
		Q <- Q / Qmax # Standardize to the community max only if possible
		}
	}
	
# Writing the results to files

if(writefiles==TRUE){
	write.csv(Q, file=paste("Q-", substitute(landfile), ".csv", sep=""))
	invisible(Q)
	}
else Q
}

# Calcualting Petchey & Gaston's FD for communities under different land use management.
# See notes in Q.Calculator.

Calculate.FD <- function(landfile, traitfile, writefiles = F, scale = T){

	traits <- traitfile
	land <- landfile

	community.names = names(land) # get community type names

	# Reorder the species and community lists to make a dataframe with two columns: species and community.

	lands <- unlist(land) # Vectorize data frames
	dim(land) <- NULL # Vectorize matrices

	sppcomm <- data.frame(species = rep(rownames(landfile), length(community.names)), community = rep(community.names, each = 	nrow(landfile)))
	sppcomm <- sppcomm[!is.na(land), ]

	library(cluster)  # to get the daisy function -- can use nominal data, unlike dist
	library(stats) # for hclust

	distances = daisy(traits, metric = "gower")
	tree = hclust(distances)
	xtree = Xtree(tree)

	FD <- tapply(sppcomm$species, sppcomm$community,
                 function(x) Getlength(list(xtree[[1]], 
                 	xtree[[2]][!is.na(match(toupper(dimnames(xtree[[2]])[[1]]),toupper(x))),])))

	# Scaling to 1
	if (scale == TRUE){
		FDmax <- max(FD, na.rm = TRUE) 
			if(FDmax != 0) {
			FD <- FD / FDmax 
			}
		}
	
	# Writing the results to files
	if(writefiles==TRUE){
		write.csv(FD, file=paste("FD-", substitute(landfile), ".csv", sep=""))
		invisible()
		}
	else FD
	}

# Diversity calculations

Calculate.H <- function(landfile){
	library(vegan)
	if(!exists("diversity")){
		stop("Install the 'vegan' package first.") }
		
	land <- landfile
	land <- t(land)

	land[is.na(land)] <- 0
	
	H <- diversity(land)

	(H <- H[sort(names(H))])
	}

### Just species numbers
Calculate.S <- function(landfile){
	### Replaces NA with 0, and non-NA with 1 in the landuse matrix
	land <- landfile

	land <- t(land)

	land[!is.na(land)] <- 1	
	land[is.na(land)] <- 0

	S <- apply(land, 1, sum)

	(S <- S[sort(names(S))])
	}
	
# Utility functions

DF.Q <- function(traits, abundances) {   # Modified from Owen Petchey's code
	abundances <- unclass(abundances)
	distances <- as.matrix(daisy(traits, metric="gower"))^2
	rel.abundance <- abundances/sum(abundances)
	a.a <- outer(rel.abundance, rel.abundance)
	if(length(a.a) != length(distances))
		stop("Size of the distance matrix does not match size of relative abundance matrix.")
	else 
		Q <- 0.5 * sum(distances * a.a)
	}

# From Owen Petchey

Getlength <- function(xtree){
  if(!is.matrix(xtree[[2]]))
    stop("I can't calculate the diversity of one species")
  if(is.matrix(xtree[[2]]))   
    sum(xtree[[1]][colSums(xtree[[2]]) != 0 & colSums(xtree[[2]]) < length(xtree[[2]][,1])])
}

Xtree <- function(h){
  ## Jens Schumacher
  ## evaluate species branch matrix (sensu Petchey&Gaston) from a dendrogram
  ## tested for results of hclust and agnes
  ## hclust - hierarchical clustering 
  ## agnes - agglomerative clustering
  
  ## used components:
  ## merge - history of cluster merging
  ## height - actual heights at merging
  ## order - permutation to achieve nice output (needed only for agnes)

  species.names <- h$labels

  H1 <- matrix(0, length(h$order), 2 * length(h$order) - 2)
  l <- vector("numeric", 2 * length(h$order) - 2)
  for(i in 1:(length(h$order) - 1)) {
    ## evaluate branch lengths
    if(h$merge[i, 1] < 0) {
      l[2 * i - 1] <- h$height[order(h$height)[i]]
      H1[ - h$merge[i, 1], 2 * i - 1] <- 1
    }
    else {
      l[2 * i - 1] <- h$height[order(h$height)[i]] - h$height[order(h$height)[h$merge[i, 1]]]
      H1[, 2 * i - 1] <- H1[, 2 * h$merge[i, 1] - 1] + H1[
                                                          , 2 * h$merge[i, 1]]
    }
    if(h$merge[i, 2] < 0) {
      l[2 * i] <- h$height[order(h$height)[i]]
      H1[ - h$merge[i, 2], 2 * i] <- 1
    }
    else {
      l[2 * i] <- h$height[order(h$height)[i]] - h$height[order(h$height)[h$merge[i, 2]]]
      H1[, 2 * i] <- H1[, 2 * h$merge[i, 2] - 1] + H1[, 2 *
                                                      h$merge[i, 2]]
    }
  }
  dimnames(H1) <- list(species.names,NULL)  
  list("branchlengths"=l, "sppxbranch"=H1)
  ## l contains the length of all the tiny branches
  ## H1: each row represents one species, each column represents one branch
  ##     1 indicates that a branch is part of the pathway from species to top of the dendrogram
  ##     0 otherwise
}

# Setting rownames. Use this for all trait and community files, since all matching is done by rownames.

set.rownames <- function(x){
	rownames(x) <- x[,1]
	x <- x[,-1]
	x
}

