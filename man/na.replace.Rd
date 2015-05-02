\name{na.replace}
\alias{na.replace}
\title{
    Replace Missing Values
}
\description{
    Replace missing values
}
\usage{
na.replace(x, replace)
}
\arguments{
  \item{x}{vector possibly contining missing (\code{NA}) values.}
  \item{replace}{scalar replacement value}
}
\details{
   This is a convenience function that is the same as
   x[is.na(x)] <- replace
}
\value{
   Vector with missing values (\code{NA}) replaced by the 
   value of \code{replace}.
}
\author{
   Gregory R. Warnes \email{greg@warnes.net}
}
\seealso{
   \code{\link[base]{is.na}}, \code{\link[stats]{na.omit}}
}
\examples{
   x <- c(1,2,3,NA,6,7,8,NA,NA)
   na.replace(x, '999')
}
\keyword{ manip }
