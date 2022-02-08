#' Label matrix
#'
#' Convert group indicator into a label maxtrix.
#' @param clust An n-dimensional vector of group partition.
#' @return
#' \item{Z}{ A matrix of group indicator.}
#' @examples
#' cluster<-c(1,1,2,2,3,3)
#' label_maxtrix<-makelabelmatrix(cluster)
#' @export
makelabelmatrix <- function(clust){
  n <- length(clust)
  g <- max(clust)
  Z <- matrix(0, n, g)
  index <- cbind(1:n, clust)
  Z[index] <- 1
  return(Z)
}
