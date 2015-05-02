\name{keywords}
\alias{keywords}
\title{List valid keywords for R man pages}
\description{
  List valid keywords for R man pages
}
\usage{
keywords(topic)
}
\arguments{
  \item{topic}{object or man page topic}
}
\details{
  If \code{topic} is provided, return a list of the keywords associated
  with \code{topic}.  Otherwise, display the list of valid R keywords
  from the R doc/KEYWORDS file.
}
\author{Gregory R. Warnes \email{greg@warnes.net}}
\seealso{ \code{\link[utils]{help}} }
\examples{
## Show all valid R keywords
keywords()

## Show keywords associated with the 'merge' function
keywords(merge)
keywords("merge")
}
\keyword{documentation}
