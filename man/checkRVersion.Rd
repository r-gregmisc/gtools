\name{checkRVersion}
\alias{checkRVersion}
\title{Check if a newer version of R is available}
\description{
  Check if a newer version of R is available
}
\usage{
checkRVersion(quiet = FALSE)
}
\arguments{
  \item{quiet}{Logical indicating whether printed output should be supressed.}
}
\details{
  This function accesses the R web site to discover the latest released
  version of R.  It then compares this version to the running version.
  If the running version is the same as the latest version, it prints
  the message, "The latest version of R is installed:" followed by the
  version number, and returns NULL.  If the running version is older
  than the current version, it displays the message, "A newer version of
  R is now available:" followed by the corresponding version number, and
  returns the version number.

  If \code{quiet=TRUE}, no printing is performed.
  
}
\value{
  Either the version number of the latest version of R, if the running
  version is less than the latest version, or NULL.
}
\author{Gregory R. Warnes \email{gregory.warnes@rochester.edu>}}
\note{
    This function utilizes the internet to access the R project web
    site.  If internet access is unavailable, the function will fail.
}
\seealso{ \code{\link[base]{R.Version}} }
\examples{

checkRVersion()

ver <- checkRVersion()
print(ver)

}
\keyword{utilities}
