% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mixedsort.R
\name{mixedsort}
\alias{mixedsort}
\alias{mixedorder}
\title{Order or Sort strings with embedded numbers so that the numbers are in the
correct order}
\usage{
mixedsort(
  x,
  decreasing = FALSE,
  na.last = TRUE,
  blank.last = FALSE,
  numeric.type = c("decimal", "roman"),
  roman.case = c("upper", "lower", "both"),
  scientific = TRUE,
  hyphen.separator = FALSE
)

mixedorder(
  x,
  decreasing = FALSE,
  na.last = TRUE,
  blank.last = FALSE,
  numeric.type = c("decimal", "roman"),
  roman.case = c("upper", "lower", "both"),
  scientific = TRUE,
  hyphen.separator = FALSE
)
}
\arguments{
\item{x}{Vector to be sorted.}

\item{decreasing}{logical.  Should the sort be increasing or decreasing?
Note that \code{descending=TRUE} reverses the meanings of \code{na.last} and
\code{blanks.last}.}

\item{na.last}{for controlling the treatment of \code{NA} values.  If
\code{TRUE}, missing values in the data are put last; if \code{FALSE}, they
are put first; if \code{NA}, they are removed.}

\item{blank.last}{for controlling the treatment of blank values.  If
\code{TRUE}, blank values in the data are put last; if \code{FALSE}, they
are put first; if \code{NA}, they are removed.}

\item{numeric.type}{either "decimal" (default) or "roman".  Are numeric
values represented as decimal numbers (\code{numeric.type="decimal"}) or as
Roman numerals (\code{numeric.type="roman"})?}

\item{roman.case}{one of "upper", "lower", or "both".  Are roman numerals
represented using only capital letters ('IX') or lower-case letters ('ix')
or both?}

\item{scientific}{logical. Should exponential notation be allowed for numeric values.}

\item{hyphen.separator}{logical. Should hyphenated strings be treated as separators? (If not, they will be treated as potential components of numeric values.)}
}
\value{
\code{mixedorder} returns a vector giving the sort order of the
input elements. \code{mixedsort} returns the sorted vector.
}
\description{
These functions sort or order character strings containing embedded numbers
so that the numbers are numerically sorted rather than sorted by character
value.  I.e. "Aspirin 50mg" will come before "Aspirin 100mg".  In addition,
case of character strings is ignored so that "a", will come before "B" and
"C".
}
\details{
I often have character vectors (e.g. factor labels), such as compound and
dose, that contain both text and numeric data.  This function is useful for
sorting these character vectors into a logical order.

It does so by splitting each character vector into a sequence of character
and numeric sections, and then sorting along these sections, with numbers
being sorted by numeric value (e.g. "50" comes before "100"), followed by
characters strings sorted by character value (e.g. "A" comes before "B")
\emph{ignoring case} (e.g. 'A' has the same sort order as 'a').

By default, sort order is ascending, empty strings are sorted to the front,
and \code{NA} values to the end.  Setting \code{descending=TRUE} changes the
sort order to descending and reverses the meanings of \code{na.last} and
\code{blank.last}.

Parsing looks for decimal numbers unless \code{numeric.type="roman"}, in
which parsing looks for roman numerals, with character case specified by
\code{roman.case}.
}
\examples{

## compound & dose labels
Treatment <- c(
  "Control", "Aspirin 10mg/day", "Aspirin 50mg/day",
  "Aspirin 100mg/day", "Acetomycin 100mg/day",
  "Acetomycin 1000mg/day"
)

## ordinary sort puts the dosages in the wrong order
sort(Treatment)

## but mixedsort does the 'right' thing
mixedsort(Treatment)

## Here is a more complex example
x <- rev(c(
  "AA 0.50 ml", "AA 1.5 ml", "AA 500 ml", "AA 1500 ml",
  "EXP 1", "AA 1e3 ml", "A A A", "1 2 3 A", "NA", NA, "1e2",
  "", "-", "1A", "1 A", "100", "100A", "Inf"
))

mixedorder(x)

mixedsort(x) # Notice that plain numbers, including 'Inf' show up
# before strings, NAs at the end, and blanks at the
# beginning .

mixedsort(x, na.last = TRUE) # default
mixedsort(x, na.last = FALSE) # push NAs to the front

mixedsort(x, blank.last = FALSE) # default
mixedsort(x, blank.last = TRUE) # push blanks to the end

mixedsort(x, decreasing = FALSE) # default
mixedsort(x, decreasing = TRUE) # reverse sort order

## Roman numerals
chapters <- c(
  "V. Non Sequiturs", "II. More Nonsense",
  "I. Nonsense", "IV. Nonesensical Citations",
  "III. Utter Nonsense"
)
mixedsort(chapters, numeric.type = "roman")

## Lower-case Roman numerals
vals <- c(
  "xix", "xii", "mcv", "iii", "iv", "dcclxxii", "cdxcii",
  "dcxcviii", "dcvi", "cci"
)
(ordered <- mixedsort(vals, numeric.type = "roman", roman.case = "lower"))
roman2int(ordered)

## Control scientific notation for number matching:
vals <- c("3E1", "2E3", "4e0")

mixedsort(vals) # With scientfic notation
mixedsort(vals, scientific = FALSE) # Without scientfic notation

## Hyphenated strings
hyphx <- c("pat-3", "pat-2", "pat-1")
mixedsort(hyphx)
mixedsort(hyphx, hyphen.separator = TRUE)
}
\seealso{
\code{\link[base]{sort}}, \code{\link[base]{order}}
}
\author{
Gregory R. Warnes \email{greg@warnes.net}
}
\keyword{manip}
\keyword{univar}
