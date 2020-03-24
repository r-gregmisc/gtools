\name{combinations}
\alias{combinations}
\alias{permutations}
\title{Enumerate the Combinations or Permutations of the Elements of a Vector}
\description{
  \code{combinations} enumerates the possible combinations of a
  specified size from the elements of a vector.  \code{permutations}
  enumerates the possible permutations.
}
\usage{
combinations(n, r, v=1:n, set=TRUE, repeats.allowed=FALSE)
permutations(n, r, v=1:n, set=TRUE, repeats.allowed=FALSE)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{n}{ Size of the source vector }
  \item{r}{ Size of the target vectors }
  \item{v}{ Source vector. Defaults to \code{1:n}}
  \item{set}{ Logical flag indicating whether duplicates should be
    removed from the source vector \code{v}. Defaults to \code{TRUE}.}
  \item{repeats.allowed}{ Logical flag indicating whether the
    constructed vectors may include duplicated values.  Defaults to
    \code{FALSE}.  }
}
\details{
  Caution: The number of combinations and permutations increases rapidly
  with \code{n} and \code{r}!.

  To use values of \code{n} above about 45, you will need to increase
  R's recursion limit.  See the \code{expression} argument to the
  \code{options} command for details on how to do this.
}
\value{
  Returns a matrix where each row contains a vector of length \code{r}.
}
\references{Venables, Bill.  "Programmers Note", R-News, Vol 1/1,
  Jan. 2001. \url{https://cran.r-project.org/doc/Rnews/} }
\author{ Original versions by Bill Venables
  \email{Bill.Venables@cmis.csiro.au}.  Extended to handle
  \code{repeats.allowed} by Gregory R. Warnes
  \email{greg@warnes.net}.
  }
\seealso{ \code{\link[base]{choose}}, \code{\link[base]{options}} }
\examples{
combinations(3,2,letters[1:3])
combinations(3,2,letters[1:3],repeats=TRUE)

permutations(3,2,letters[1:3])
permutations(3,2,letters[1:3],repeats=TRUE)

# To use large 'n', you need to change the default recusion limit
options(expressions=1e5)
cmat <- combinations(300,2)
dim(cmat) # 44850 by 2
}
\keyword{ manip }
