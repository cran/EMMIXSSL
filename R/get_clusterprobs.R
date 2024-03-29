#' Posterior probability
#'
#' Get posterior probabilities of class membership
#' @param dat An \eqn{n\times p} matrix where each row represents an individual observation
#' @param n Number of observations.
#' @param p Dimension of observation vecor.
#' @param g Number of multivariate normal classes.
#' @param pi A g-dimensional vector for the initial values of the mixing proportions.
#' @param  mu A \eqn{p \times g} matrix for the initial values of the location parameters.
#' @param sigma A \eqn{p\times p} covariance matrix if \code{ncov=1}, or a list of g covariance matrices with dimension \eqn{p\times p \times g} if \code{ncov=2}.
#' @param ncov Options of structure of sigma matrix;  the default value is 2;
#'  \code{ncov} = 1 for a common covariance matrix;
#'  \code{ncov} = 2 for the unequal  covariance/scale matrices.
#' @return
#' \item{clusprobs}{Posterior probabilities of class membership for the ith entity}
#' @details
#' The posterior probability can be expressed as
#' \deqn{
#' \tau_i(y_j;\theta)=Prob\{z_{ij}=1|y_j\}=\frac{\pi_i\phi(y_j;\mu_i,\Sigma_i)}{\sum_{h=1}^g\pi_h\phi(y_j;\mu_h,\Sigma_h) },
#' }
#' where \eqn{\phi} is a normal probability function with mean \eqn{\mu_i} and covariance matrix \eqn{\Sigma_i},
#' and \eqn{z_{ij}} is is a zero-one indicator variable denoting the class of origin.
#' @export
#' @import mvtnorm
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
#'tau<-get_clusterprobs(dat=dat$Y,n=150,p=3,g=4,mu=mu,sigma=sigma,pi=pi,ncov=2)

get_clusterprobs <- function(dat, n, p, g, pi,mu, sigma,ncov=2){
  logdens<-matrix(0,n,g)

  if(ncov==1){
    for(i in 1:g){
      logdens[,i]<-mvtnorm::dmvnorm(dat,mean=mu[,i],sigma = as.matrix(sigma),log=TRUE)
    }
  }else{
    for(i in 1:g){
      logdens[,i]<-mvtnorm::dmvnorm(dat,mean=mu[,i],sigma = as.matrix(sigma[,,i]),log=TRUE)
    }
  }


  #logdens <- ddmix(dat=dat, n=n, p=p, g=g, distr=distr, mu=mu, sigma=sigma, dof=dof, delta=delta)
  logprobs <- t(t(logdens)+log(pi))
  clusprobs <- t(apply(logprobs, 1, normalise_logprob))
  return(clusprobs)
}
