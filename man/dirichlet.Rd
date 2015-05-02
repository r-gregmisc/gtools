% $Id$
%
\name{rdirichlet}
\alias{rdirichlet}
\alias{ddirichlet}
\title{Functions for the Dirichlet Distribution}
\description{
  Functions to compute the density of or generate random deviates from
  the Dirichlet distribution.
}
\usage{
rdirichlet(n, alpha)
ddirichlet(x, alpha)
}
\arguments{
  \item{x}{A vector containing a single random deviate or matrix
    containg one random deviate per row.} 
  \item{n}{Number of random vectors to generate. }
  \item{alpha}{Vector or (for \code{ddirichlet}) matrix containing shape
    parameters. }
}
\details{
  The Dirichlet distribution is the multidimensional generalization of
  the beta distribution.  It is the canonical Bayesian distribution for
  the parameter estimates of a multinomial distribution.
}
\value{
  \code{ddirichlet} returns a vector containing the Dirichlet density for the
  corresponding rows of \code{x}.

  \code{rdirichlet} returns a matrix with \code{n} rows, each containing
  a single Dirichlet random deviate.
}
\author{
  Code original posted by Ben Bolker to R-News on Fri Dec 15 2000. See
  \url{https://stat.ethz.ch/pipermail/r-help/2000-December/009561.html}.
  Ben attributed the code to Ian Wilson \email{i.wilson@maths.abdn.ac.uk}.
  Subsequent modifications by Gregory R. Warnes
  \email{greg@warnes.net}.
}
\seealso{ \code{\link{dbeta}}, \code{\link{rbeta}} }
\examples{

  x <- rdirichlet(20, c(1,1,1) )

  ddirichlet(x, c(1,1,1) )

}
\keyword{distribution}
