#' Gaussian mixture model generator.
#'
#' Generate random observations from the Gaussian mixture distributions.
#' @param  n Number of observations.
#' @param pi A g-dimensional  initial vector of the mixing proportions.
#' @param mu A initial  \eqn{p \times g} matrix of the location parameters.
#' @param sigma A \eqn{p\times p} covariance matrix if \code{ncov=1}, or a list of g covariance matrices with dimension \eqn{p\times p \times g} if \code{ncov=2}.
#' @param ncov Options of structure of sigma matrix;  the default value is 2;
#'  \code{ncov} = 1 for a common covariance matrix that \code{sigma} is a \eqn{p\times p} matrix.
#'  \code{ncov} = 2 for the unequal  covariance/scale matrices that
#'  \code{sigma} represents a list of g matrices with dimension \eqn{p\times p \times g}.
#' @return
#' \item{Y}{An \eqn{n\times p} numeric matrix with samples drawn in rows.}
#' \item{Z}{ An \eqn{n\times g} numeric matrix; each row represents zero-one indicator variables defining the known group of origin of each.}
#' \item{clust}{An n-dimensional vector of group partition.}
#' @examples
#' n<-150
#' pi<-c(0.25,0.25,0.25,0.25)
#' sigma<-array(0,dim=c(3,3,4))
#' sigma[,,1]<-diag(1,3)
#' sigma[,,2]<-diag(2,3)
#' sigma[,,3]<-diag(3,3)
#' sigma[,,4]<-diag(4,3)
#' mu<-matrix(c(0.2,0.3,0.4,0.2,0.7,0.6,0.1,0.7,1.6,0.2,1.7,0.6),3,4)
#' dat<-rmix(n=n,pi=pi,mu=mu,sigma=sigma,ncov=2)
#' @import mvtnorm
#' @export
rmix <- function(n,pi, mu, sigma,ncov=2){
g=length(pi)
if(ncov==1){
  nn<-table(sample(1:g, n, replace = TRUE, prob = pi))
  p=dim(mu)[1]
  X=NULL
  for(j in 1:g){
    if (nn[j] > 0){
      X1<- mvtnorm::rmvnorm(nn[j], mean=mu[,j], sigma=as.matrix(sigma)) # group 1 :a random number generator for the multivariate normal distribution
      X=rbind(X,X1)
    } else if (nn[j]==0){
      X1=matrix(NA,nn[j],p)
      X=rbind(X,X1)
    }
  }  }else{
    nn<-table(sample(1:g, n, replace = TRUE, prob = pi))
    p=dim(mu)[1]
    X=NULL
    for(j in 1:g){
      if (nn[j] > 0){
        X1<- mvtnorm::rmvnorm(nn[j], mean=mu[,j], sigma=as.matrix(sigma[,,j])) # group 1 :a random number generator for the multivariate normal distribution
        X=rbind(X,X1)
      } else if (nn[j]==0){
        X1=matrix(NA,nn[j],p)
        X=rbind(X,X1)
      }
    }
  }
y <- rep(1:g,nn)
rperm <- sample(n) #re-arrange order
y <- y[rperm]
X <- X[rperm,,drop=FALSE]
clust<-y
Y<-X
Z<-makelabelmatrix(y)
dat <- list(Y=Y,Z=Z,clust=clust)

return(dat)
}
