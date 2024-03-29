% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loglk_ig.R
\name{loglk_ig}
\alias{loglk_ig}
\title{Log likelihood for partially classified data with ingoring the missing mechanism}
\usage{
loglk_ig(dat, zm, pi, mu, sigma, ncov = 2)
}
\arguments{
\item{dat}{An \eqn{n\times p} matrix where each row represents an individual observation}

\item{zm}{An n-dimensional vector containing the class labels including the missing-label denoted as NA.}

\item{pi}{A g-dimensional vector for the initial values of the mixing proportions.}

\item{mu}{A \eqn{p \times g} matrix for the initial values of the location parameters.}

\item{sigma}{A \eqn{p\times p} covariance matrix if \code{ncov=1}, or a list of g covariance matrices with dimension \eqn{p\times p \times g} if \code{ncov=2}.}

\item{ncov}{Options of structure of sigma matrix;  the default value is 2;
\code{ncov} = 1 for a common covariance matrix;
\code{ncov} = 2 for the unequal  covariance/scale matrices.}
}
\value{
\item{lk}{Log-likelihood value.}
}
\description{
Log likelihood for partially classified data with ingoring the missing mechanism
}
\details{
The log-likelihood function for  partially classified data with ingoring the missing mechanism can be expressed as
 \deqn{
 \log L_{PC}^{({ig})}(\theta)=\sum_{j=1}^n  \left[
(1-m_j)\sum_{i=1}^g z_{ij}\left\lbrace \log\pi_i+\log  f_i(y_j;\omega_i)\right\rbrace +m_j\log \left\lbrace  \sum_{i=1}^g\pi_i  f_i(y_j;\omega_i)\right\rbrace  \right],
 }
 where \eqn{m_j} is a missing label indicator, \eqn{z_{ij}} is a zero-one indicator variable defining the known group of origin of each,
 and \eqn{f_i(y_j;\omega_i)} is a probability density function with parameters \eqn{\omega_i}.
}
