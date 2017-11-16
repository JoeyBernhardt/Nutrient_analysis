Xtree <- function(h)
{
  species.names <- h$labels 
  H1 <- matrix(0, length(h$order), 2 * length(h$order) - 2)
  l <- vector("numeric", 2 * length(h$order) - 2)
  for(i in 1:(length(h$order) - 1)) {
    # evaluate branch lengths
    #
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
  list(h2.prime=l, H1=H1)
}
