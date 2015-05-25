\name{mixedsort}
\alias{mixedsort}
\alias{mixedorder}
\title{Order or Sort strings with embedded numbers so that the numbers
  are in the correct order}
\description{
  These functions sort or order character strings containing embedded
  numbers so that the numbers are numerically sorted rather than sorted
  by character value.  I.e. "Asprin 50mg" will come before
  "Asprin 100mg".  In addition, case of character strings is ignored so
  that "a", will come before "B" and "C".
}
\usage{
mixedsort(x,  decreasing=FALSE, na.last=TRUE, blank.last=FALSE, ...)
mixedorder(x, decreasing=FALSE, na.last=TRUE, blank.last=FALSE, ...)
}
\arguments{
  \item{x}{Vector to be sorted.}
  \item{decreasing}{logical.  Should the sort be increasing or
    decreasing? Note that \code{descending=TRUE} reverses the meanings of
    \code{na.lst} and \code{blanks.last}.}
  \item{na.last}{for controlling the treatment of \code{NA}s.  If \code{TRUE}, missing
          values in the data are put last; if \code{FALSE}, they are put
          first; if \code{NA}, they are removed.}
}
\details{
  I often have character vectors (e.g. factor labels), such as compound
  and dose, that contain both text and numeric data.  This function
  is useful for sorting these character vectors into a logical order.

  It does so by splitting each character vector into a sequence of
  character and numeric sections, and then sorting along these sections,
  with numbers being sorted by numeric value (e.g. "50" comes before
  "100"), followed by characters strings sorted by character
  value (e.g. "A" comes before "B").

  By default, sort order is ascending, empty strings are sorted to the front,
  and \code{NA} values to the end.   Setting \code{descending=TRUE}
  changes the sort order to descending and reverses the meanings of
  \code{na.last} and \code{blank.last}.
}
\value{
  \code{mixedorder} returns a vector giving the sort order of the input
  elements. \code{mixedsort} returns the sorted vector.
}
\author{ Gregory R. Warnes \email{greg@warnes.net} }
\seealso{ \code{\link[base]{sort}}, \code{\link[base]{order}} }
\examples{
## compound & dose labels
Treatment <- c("Control", "Asprin 10mg/day", "Asprin 50mg/day",
               "Asprin 100mg/day", "Acetomycin 100mg/day",
               "Acetomycin 1000mg/day")

## ordinary sort puts the dosages in the wrong order
sort(Treatment)

## but mixedsort does the 'right' thing
mixedsort(Treatment)

## Here is a more complex example
x <- rev(c("AA 0.50 ml", "AA 1.5 ml", "AA 500 ml", "AA 1500 ml",
           "EXP 1", "AA 1e3 ml", "A A A", "1 2 3 A", "NA", NA, "1e2",
           "", "-", "1A", "1 A", "100", "100A", "Inf"))

mixedorder(x)

mixedsort(x)  # Notice that plain numbers, including 'Inf' show up
              # before strings, NAs at the end, and blanks at the
              # beginning .


mixedsort(x, na.last=TRUE)  # default
mixedsort(x, na.last=FALSE) # push NAs to the front


mixedsort(x, blank.last=FALSE) # default
mixedsort(x, blank.last=TRUE)  # push blanks to the end

mixedsort(x, decreasing=FALSE) # default
mixedsort(x, decreasing=TRUE)  # reverse sort order
}
\keyword{univar}
\keyword{manip}


\concept{natural sort}
\concept{dictionary sort}

