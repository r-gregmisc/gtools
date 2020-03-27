\name{na.replace}
\alias{na.replace}
\title{
    Replace Missing Values
}
\description{
    Replace missing values
}
\usage{
na.replace(x, replace, ...)
}
\arguments{
  \item{x}{vector possibly containing missing (\code{NA}) values}
  \item{replace}{either a scalar replacement value, or a function returning a scalar value}
  \item{...}{Optional arguments to be passed to \code{replace}}
}
\details{
   This is a convenience function that is the same as
   \code{x[is.na(x)] <- replace}
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
   
   # Replace with a specified value
   na.replace(x, '999')
   
   # Replace with the calculated median
   na.replace(x, median, na.rm=TRUE)    
}
\keyword{ manip }
