\name{setTCPNoDelay}
\alias{setTCPNoDelay}
\title{Modify the TCP\_NODELAY (`de-Nagle') flag for socket objects}
\description{
  Modify the TCP\_NODELAY (`de-Nagele') flag for socket objects
}
\usage{
setTCPNoDelay(socket, value=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{socket}{A socket connection object}
  \item{value}{Logical indicating whether to set (\code{TRUE}) or unset
    (\code{FALSE}) the flag}
}
\details{
  By default, TCP connections wait a small fixed interval before
  actually sending data, in order to permit small packets to be
  combined.  This algorithm is named after its inventor, John Nagle, and
  is often referred to as 'Nagling'.

  While this reduces network resource utilization in these
  situations, it imposes a delay on all outgoing message data, which can
  cause problems in client/server situations.

  This function allows this feature to be disabled (de-Nagling,
  \code{value=TRUE}) or enabled (Nagling, \code{value=FALSE}) for the
  specified socket.
}
\value{
  The character string "SUCCESS" will be returned invisible if the
  operation was succesful.  On failure, an error will be generated.
}
\references{
  "Nagle's algorithm" at WhatIS.com \url{
    http://searchnetworking.techtarget.com/sDefinition/0,,sid7_gci754347,00.html}

  Nagle, John. "Congestion Control in IP/TCP Internetworks", IETF
  Request for Comments 896, January 1984.
  \url{http://www.ietf.org/rfc/rfc0896.txt?number=896}
}
\author{Gregory R. Warnes \email{greg@warnes.net}}
\seealso{
  \code{\link{make.socket}},
  \code{\link{socketConnection}}
  }
\examples{
\dontrun{
   s <- make.socket(host='www.r-project.org', port=80)
   setTCPNoDelay(s, value=TRUE)
}

}
\keyword{programming}
\keyword{misc}
\keyword{utilities}
