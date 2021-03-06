% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roman2int.R
\name{roman2int}
\alias{roman2int}
\title{Convert Roman Numerals to Integers}
\usage{
roman2int(roman)
}
\arguments{
\item{roman}{character vector containing roman numerals}
}
\value{
A integer vector with the same length as \code{roman}.  Character
strings which are not valid roman numerals will be converted to \code{NA}.
}
\description{
Convert roman numerals to integers
}
\details{
This function will convert roman numerals to integers without the upper bound
imposed by R (3899), ignoring case.
}
\examples{

roman2int(c("I", "V", "X", "C", "L", "D", "M"))

# works regardless of case
roman2int("MMXVI")
roman2int("mmxvi")

# works beyond R's limit of 3899
val.3899 <- "MMMDCCCXCIX"
val.3900 <- "MMMCM"
val.4000 <- "MMMM"
as.numeric(as.roman(val.3899))
as.numeric(as.roman(val.3900))
as.numeric(as.roman(val.4000))

roman2int(val.3899)
roman2int(val.3900)
roman2int(val.4000)
}
\seealso{
\code{\link[utils]{as.roman}}
}
\author{
Gregory R. Warnes \email{greg@warnes.net}
}
\keyword{arith}
