#' Error rate of the Bayes rule for two-class Gaussian homoscedastic model
#'
#' The optimal error rate of Bayes rule for two-class Gaussian homoscedastic model
#' @param beta0 An \eqn{n\times p} matrix where each row represents an individual observation
#' @param beta Number of observations.
#' @param pi A g-dimensional vector for the initial values of the mixing proportions.
#' @param  mu A \eqn{p \times g} matrix for the initial values of the location parameters.
#' @param sigma A \eqn{p\times p} covariance matrix if \code{ncov=1}, or a list of g covariance matrices with dimension \eqn{p\times p \times g} if \code{ncov=2}.
#' @return
#' \item{errval}{A vector of error rate.}
#' @details
#' The optimal error rate of Bayes rule for two-class Gaussian homoscedastic model can be expressed as
#' \deqn{
#' err(y_j;\theta)=\pi_1\phi\{-\frac{\beta_0+\beta_1^T\mu_1}{(\beta_1^T\Sigma\beta_1)^{\frac{1}{2}}}\}+\pi_2\phi\{\frac{\beta_0+\beta_1^T\mu_2}{(\beta_1^T\Sigma\beta_1)^{\frac{1}{2}}}\}
#' }
#' where \eqn{\phi} is a normal probability function with mean \eqn{\mu_i} and covariance matrix \eqn{\Sigma_i}.

errorrate <- function(beta0, beta, pi, mu, sigma){
  mean <- numeric(2)
  var <- numeric(2)
  error <- numeric(2)
  mean[1] <- t(beta) %*% mu[,1] + beta0
  var[1] <- t(beta) %*% sigma %*% beta
  error[1] <- pnorm(0, mean=mean[1], sd=sqrt(var[1]), lower.tail = TRUE)
  mean[2] <- t(beta) %*% mu[,2] + beta0
  var[2] <- t(beta) %*% sigma %*% beta
  error[2] <- pnorm(0, mean=mean[2], sd=sqrt(var[2]), lower.tail = FALSE)
  errval <- pi[1]*error[1]+pi[2]*error[2]
  return(errval)
}
