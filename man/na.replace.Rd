% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/na.replace.R
\name{na.replace}
\alias{na.replace}
\title{Replace Missing Values}
\usage{
na.replace(x, replace, ...)
}
\arguments{
\item{x}{vector possibly containing missing (\code{NA}) values}

\item{replace}{either a scalar replacement value, or a function returning a
scalar value}

\item{...}{Optional arguments to be passed to \code{replace}}
}
\value{
Vector with missing values (\code{NA}) replaced by the value of
\code{replace}.
}
\description{
Replace missing values
}
\details{
This is a convenience function that is the same as x[is.na(x)] <- replace
}
\examples{

x <- c(1, 2, 3, NA, 6, 7, 8, NA, NA)

# Replace with a specified value
na.replace(x, "999")

# Replace with the calculated median
na.replace(x, median, na.rm = TRUE)
}
\seealso{
\code{\link[base]{is.na}}, \code{\link[stats]{na.omit}}
}
\author{
Gregory R. Warnes \email{greg@warnes.net}
}
\keyword{manip}
