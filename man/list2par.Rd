% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list2par.R
\name{list2par}
\alias{list2par}
\title{Transfer a list into a vector}
\usage{
list2par(p, g, pi, mu, sigma, ncov = 2, xi = NULL, type = c("ign", "full"))
}
\arguments{
\item{p}{Dimension of observation vecor.}

\item{g}{Number of multivariate Gaussian groups.}

\item{pi}{A g-dimensional  initial vector of the mixing proportions.}

\item{mu}{A initial  \eqn{p \times g} matrix of the location parameters.}

\item{sigma}{A \eqn{p\times p} covariance matrix if \code{ncov=1}, or a list of g covariance matrices with dimension \eqn{p\times p \times g} if \code{ncov=2}.}

\item{ncov}{Options of structure of sigma matrix;  the default value is 2;
\code{ncov} = 1 for a common covariance matrix that \code{sigma} is a \eqn{p\times p} matrix.
\code{ncov} = 2 for the unequal  covariance/scale matrices that
\code{sigma} represents a list of g matrices with dimension \eqn{p\times p \times g}.}

\item{xi}{A 2-dimensional coefficient vector for a logistic function of the Shannon entropy.}

\item{type}{Two types to fit to the model, 'ign' indicates fitting the model on the basis of the missing-label mechanism ignored,
and 'full' indicates fitting the model on the basis of the missing-label mechanism}
}
\value{
\item{par}{a vector including all list information}
}
\description{
Transfer a list into a vector
}
