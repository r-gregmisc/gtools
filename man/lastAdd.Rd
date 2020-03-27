\name{lastAdd}
\alias{lastAdd}
\title{Non-destructively construct a .Last function to be executed when R exits.}
\description{
  Non-destructively construct a \code{.Last} function to be executed when R exits.
}
\usage{
lastAdd(fun)
}
\arguments{
  \item{fun}{Function to be called.}
}
\details{
  \code{lastAdd} constructs a new function which can be used to replace
  the existing definition of \code{.Last}, which will be executed when R
  terminates normally.

  If a \code{.Last} function already exists in the global environment,
  the original definition is stored in a private environment, and the
  new function is defined to call the function \code{fun} and then to
  call the previous (stored) definition of \code{.Last}.
  
  If no \code{.Last} function exists in the global environment,
  \code{lastAdd} simply returns the function \code{fun}.
}

\note{
  This function replaces the (now defunct) \code{addLast} function.
}
\value{
  A new function to be used for \code{.Last}.
}
\author{Gregory R. Warnes \email{greg@warnes.net}}
\seealso{ \code{\link[base]{.Last}} }
\examples{

## Print a couple of cute messages when R exits.
helloWorld <- function() cat("\nHello World!\n")
byeWorld <- function() cat("\nGoodbye World!\n")

.Last <- lastAdd(byeWorld)
.Last <- lastAdd(helloWorld)

\dontshow{
.Last()
}
\dontrun{
q("no")

## Should yield:
##
##   Save workspace image? [y/n/c]: n
##
##   Hello World!
##
##   Goodbye World!
##
##   Process R finished at Tue Nov 22 10:28:55 2005
}

## Unix-flavour example: send Rplots.ps to printer on exit.
myLast <- function()
{
  cat("Now sending PostScript graphics to the printer:\n")
  system("lpr Rplots.ps")
  cat("bye bye...\n")
}
.Last <- lastAdd(myLast)
\dontshow{
.Last()
}
\dontrun{
quit("yes")

## Should yield:
##
##  Now sending PostScript graphics to the printer:
##  lpr: job 1341 queued
##  bye bye...
##
##   Process R finished at Tue Nov 22 10:28:55 2005
}

}
\keyword{programming}

