\name{stars.pval}
\alias{stars.pval}
\title{
Generate significance stars from p-values
}
\description{
  Generate significance stars (e.g. '***', '**', '*', '+') from
  p-values using R's standard definitions.
}
\usage{
stars.pval(p.value)
}
\arguments{
  \item{p.value}{numeric vector of p-values}
}
\details{
  Mapping from p-value ranges to symbols:
  \describe{
    \item{0 - 0.001}{'***'}
    \item{0.001 - 0.01}{'**'}
    \item{0.01 - 0.05}{'*'}
    \item{0.05 - 0.1}{'+'}
    \item{0.1 - 1.0}{'' (No symbol)}
    }
}
\value{
  A character vector containing the same number of elements as
  \code{p-value}, with an attribute "legend" providing the conversion
  pattern.
}
\author{
  Gregory R. Warnes \email{greg@warnes.net}
}
\seealso{
  \code{\link[stats]{symnum}}
}
\examples{
p.val <- c(0.0004, 0.0015, 0.013, 0.044, 0.067, 0.24)
stars.pval(p.val)
}
\keyword{misc}

