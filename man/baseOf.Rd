\name{baseOf}
\alias{baseOf}
\title{
    Transform regular integers of base 10 into list of digits of another base.
}
\description{
    Regular numbers are of base 10. At times though other representations
    are useful, e.g. binary representations (ask electro engineers)
    or hexadecimal (known for assembly programming).
    These are also applications in computer science, e.g. for
    hashing (representation of feature sets) and ... general feature
    representation.  This routine returns a vector of digits, every
    position represented with at least the value 0. The most significant
    digit is at index 1.
}
\usage{
baseOf(v, b, l)
}
\arguments{
  \item{v}{Regular integer value, i.e. of base 10 that shall be represented with digits of another base}
  \item{b}{The base into which the number shall be transformed. Choose 2 for dual, 8 for octal or 16 for hexadecimal.}
  \item{l}{Minimal length of result vector, defaults to 1.}
}
\details{
   The argument l is optional. This comes handy in situations for which
   you expect an exact number of digits to be returned - the left will
   then be padded with 0s.
}
\value{
   Vector with numbers ranging from 0 to the value of (b-1) that
   have the property that for a vector v, the original value is
   sum(rev(v)[i]*b^(i-1)).
}
\author{
   Steffen Möller \email{steffen_moeller@gmx.de},
   Gregory R. Warnes \email{greg@warnes.net}
}
\seealso{
}
\examples{
   # create a list of digits with a dual representation of the value 6
   baseOf(26,2)
   # print this nicely
   paste(c("O","1")[baseOf(26,2)+1],collapse="")
   # analogously for a hexadecimal representation
   paste(c(0:9,LETTERS)[baseOf(26,16)+1],collapse="")
}
\keyword{ math }
