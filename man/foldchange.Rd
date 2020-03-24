\name{foldchange}
\alias{foldchange}
\alias{logratio2foldchange}
\alias{foldchange2logratio}
\title{Compute fold-change or convert between log-ratio and fold-change.}
\description{
  \code{foldchange} computes the fold change for two sets of values.
  \code{logratio2foldchange} converts values from log-ratios to fold
  changes.  \code{foldchange2logratio} does the reverse.
}
\usage{
foldchange(num,denom)
logratio2foldchange(logratio, base=2)
foldchange2logratio(foldchange, base=2)
}
\arguments{
  \item{num,denom}{vector/matrix of numeric values}
  \item{logratio}{vector/matrix of log-ratio values}
  \item{foldchange}{vector/matrix of fold-change values}
  \item{base}{Exponential base for the log-ratio.}
}
\details{
  Fold changes are commonly used in the biological sciences as a
  mechanism for comparing the relative size of two measurements.  They
  are computed as: \eqn{\frac{num}{denom}}{num/denom} if
  \eqn{num>denom}, and as \eqn{\frac{-denom}{num}}{-denom/num}
  otherwise.

  Fold-changes have the advantage of ease of interpretation and symmetry
  about \eqn{num=denom}, but suffer from a discontinuty between -1 and
  1, which can cause significant problems when performing data
  analysis.  Consequently statisticians prefer to use log-ratios.
}
\value{
  A vector or matrix of the same dimensions as the input containing the
  converted values.
}
\author{ Gregory R. Warnes \email{greg@warnes.net} }
\examples{

  a <- 1:21
  b <- 21:1

  f <- foldchange(a,b)

  cbind(a,b,f)

}
\keyword{math}
