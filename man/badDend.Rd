\name{badDend}
\alias{badDend}
\docType{data}
\title{Dataset That Crashes Base:::Plot.Dendrogram with 'Node Stack Overflow'}
\description{
  Base:::Plot.Dendrogram() will generate a 'Node Stack Overflow' when run
  on a dendrogram appropriately constructed from this data set.
}
\usage{data("badDend")}
\format{
  The format is:
 num [1:2047, 1:12] 1 2 3 4 5 6 7 8 9 10 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:12] "X" "V1" "V2" "V3" ...
}
\note{
  See help page for \code{\link{unByteCode}} to see how to construct the
  'bad' dendrogram from this data and how to work around the issue.
  }
\examples{
data(badDend)
}
\keyword{datasets}
