% $Id: binsearch.Rd 1087 2006-11-11 04:09:59Z warnes $

\name{binsearch}
\alias{binsearch}
\title{Binary Search}
\description{
  Search within a specified range to locate an integer parameter which
  results in the the specified monotonic function obtaining a given value.
}
\usage{
binsearch(fun, range, ..., target = 0, lower = ceiling(min(range)),
          upper = floor(max(range)), maxiter = 100, showiter = FALSE)
}
\arguments{
  \item{fun}{Monotonic function over which the search will be performed.}
  \item{range}{2-element vector giving the range for the search.}
  \item{\dots}{Additional parameters to the function \code{fun}.}
  \item{target}{Target value for \code{fun}.  Defaults to 0.}
  \item{lower}{Lower limit of search range. Defaults to \code{min(range)}.}
  \item{upper}{Upper limit of search range. Defaults to \code{max(range)}.}
  \item{maxiter}{ Maximum number of search iterations. Defaults to 100.}
  \item{showiter}{ Boolean flag indicating whether the algorithm state
    should be printed at each iteration. Defaults to FALSE.}
}
\details{
  This function implements an extension to the standard binary search
  algorithm for searching a sorted list.  The algorithm has been
  extended to cope with cases where an exact match is not possible, to
  detect whether that the function may be monotonic increasing or
  decreasing and act appropriately, and to detect when the target value
  is outside the specified range.

  The algorithm initializes two variable \code{lo} and
  \code{high} to the extremes values of \code{range}.  It then generates
  a new value \code{center} halfway between \code{lo} and \code{hi}.  If
  the value of \code{fun} at \code{center} exceeds \code{target}, it
  becomes the new value for \code{lo}, otherwise it becomes the new
  value for \code{hi}.  This process is iterated until \code{lo} and
  \code{hi} are adjacent.  If the function at one or the other equals
  the target, this value is returned, otherwise \code{lo}, \code{hi},
  and the function value at both are returned.

  Note that when the specified target value falls between integers, the
  \emph{two} closest values are returned.  If the specified target falls
  outside of the specified \code{range}, the closest endpoint of the
  range will be returned, and an warning message will be generated.  If
  the maximum number if iterations was reached, the endpoints of the
  current subset of the range under consideration will be returned.
}
\value{
  A list containing:
  \item{call}{How the function was called.}
  \item{numiter}{The number of iterations performed}
  \item{flag }{One of the strings,  "Found", "Between Elements",
    "Maximum number of iterations reached", "Reached lower boundary", or
    "Reached upper boundary."}
  \item{where}{One or two values indicating where the search
    terminated.}
  \item{value}{Value of the function \code{fun} at the values of
    \code{where}.}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Gregory R. Warnes \email{greg@warnes.net} }
\note{This function often returns two values for \code{where} and
  \code{value}.  Be sure to check the \code{flag} parameter to see what
  these values mean.}
\seealso{ \code{\link{optim}}, \code{\link{optimize}},
  \code{\link{uniroot}} }
\examples{

### Toy examples

# search for x=10
binsearch( function(x) x-10, range=c(0,20) )

# search for x=10.1
binsearch( function(x) x-10.1, range=c(0,20) )

### Classical toy example

# binary search for the index of 'M' among the sorted letters
fun <- function(X) ifelse(LETTERS[X] > 'M', 1,
                          ifelse(LETTERS[X] < 'M', -1, 0 ) )

binsearch( fun, range=1:26 ) 
# returns $where=13
LETTERS[13]

### Substantive example, from genetics
\dontrun{
library(genetics)
# Determine the necessary sample size to detect all alleles with
# frequency 0.07 or greater with probability 0.95.
power.fun <- function(N) 1 - gregorius(N=N, freq=0.07)$missprob

binsearch( power.fun, range=c(0,100), target=0.95 )

# equivalent to
gregorius( freq=0.07, missprob=0.05)
}
}
\keyword{optimize}
\keyword{programming}
