#' Transfer a list into a vector
#'
#' Transfer a list into a vector
#' @param p Dimension of observation vecor.
#' @param g Number of multivariate normal classes.
#' @param pi A g-dimensional  initial vector of the mixing proportions.
#' @param pi A g-dimensional vector for the initial values of the mixing proportions.
#' @param  mu A \eqn{p \times g} matrix for the initial values of the location parameters.
#' @param sigma A \eqn{p\times p} covariance matrix if \code{ncov=1}, or a list of g covariance matrices with dimension \eqn{p\times p \times g} if \code{ncov=2}.
#' @param ncov Options of structure of sigma matrix;  the default value is 2;
#'  \code{ncov} = 1 for a common covariance matrix;
#'  \code{ncov} = 2 for the unequal  covariance/scale matrices.
#' @param xi A 2-dimensional vector containing the initial values of the coefficients in the logistic function of the Shannon entropy.
#' @param type Three types to fit to the model, 'ign' indicates fitting the model on the basis of the likelihood that ignores the missing label mechanism,
#' 'full' indicates that the model to be fitted on the basis of the full likelihood, taking into account the missing-label mechanism,
#' and 'com' indicate that the model to be fitted to a completed classified sample.
#'
#' @return
#' \item{par}{a vector including all list information}
#' @export
#'
list2par <- function(p,g,pi,mu,sigma,ncov=2,xi=NULL, type=c('ign','full','com')){
  if(ncov==1){
    muvec <- as.vector(mu)
    q <- p*(p+1)/2
    cholpars <- numeric(q)
    cholvec <- cov2vec(sigma)
    pi <- pi
    tpro <- pro2vec(pi)
    if (type=='ign'|| type=='com'){
      par <- c(muvec, cholvec, tpro)
    } else if (type=='full'){
      par <- c(muvec, cholvec, tpro,xi)
    }
  }else{
    muvec <- as.vector(mu)#put all mu vectors into one vector
    q <- p*(p+1)/2 #the number of upper_elements and diagonal elements in variance matrix
    cholpars <- matrix(0, q, g) #generate matrix with g clusters and each cluster including q variance components
    for (h in 1:g){
      cholpars[,h] <- cov2vec(sigma[,,h]) # put the i-th cluster upper elements and diagonal elements into the i-th vector
    }
    cholvec <- as.vector(cholpars) #put the matrix into one vector
    pi <- pi
    tpro <- pro2vec(pi)

    if (type=='ign'|| type=='com'){
      par <- c(muvec, cholvec, tpro)
    } else if (type=='full'){
      par <- c(muvec, cholvec, tpro,xi)
    }
  }
  return(par)
}
