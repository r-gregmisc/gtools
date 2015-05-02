\name{loadedPackages}
\alias{loadedPackages}
\title{
  Provide Name, Version, and Path of Loaded Package Namespaces
}
\description{
  Provide name, version, and path of loaded package namespaces
}
\usage{
loadedPackages(silent = FALSE)
}
\arguments{
  \item{silent}{Logical indicating whether the results should be printed}
}
\value{
  Data frame containing one row per loaded package namespace, with columns:
  \item{Package}{Package name}
  \item{Version}{Version string}
  \item{Path}{Path to package files}
  \item{SearchPath}{Either the index of the package namespace in the current
    search path, or '-' if the package namespace is not in the search
    path. '1' corresponds to the top of the search path (the first namespace
    searched for values).  } 
}
\author{
  Gregory R. Warnes \email{greg@warnes.net}
}
\seealso{
  \code{\link[base]{loadedNamespaces}},
  \code{\link[utils]{packageVersion}},
  \code{\link[base]{search}},
  \code{\link[base]{find.package}}
}
\examples{
  loadedPackages()
}
\keyword{package}
