\name{ask}
\alias{ask}
\title{Display a prompt and collect the user's response}
\description{
  Display a prompt and collect the user's response
}
\usage{
ask(msg = "Press <RETURN> to continue: ")
}
\arguments{
  \item{msg}{Character vetor providing the message to be displayed}
}
\details{
  The prompt message will be displayed, and then \code{readLines} is
  used to collect a single input value (possibly empty), which is then
  returned.
}
\value{
  A character scalar containing the input providede by the user.
}
\author{Gregory R. Warnes \email{greg@warnes.net}}
\seealso{ \code{\link{readLines}}, \code{\link{scan}} }
\examples{

# use default prompt
ask()

silly <- function()
{
   age <- ask("How old aroe you? ")
   age <- as.numeric(age)
   cat("In 10 years you will be", age+10, "years old!\n")
}
   
}
\keyword{IO}
