\name{smartbind}
\alias{smartbind}
\title{Efficient rbind of data frames, even if the column names don't match}
\description{
  Efficient rbind of data frames, even if the column names don't match
}
\usage{
smartbind(..., fill=NA, sep=':', verbose=FALSE)
}
\arguments{
  \item{\dots}{Data frames to combine}
  \item{fill}{Value to use when 'filling' missing columns.
    Defaults to \code{NA}. }
  \item{sep}{Character string used to separate column names when pasting
    them together.}
  \item{verbose}{Logical flag indicating whether to display processing
    messages. Defaults to \code{FALSE}.}
}
\value{
  The returned data frame will contain:
  \item{columns}{all columns present in any provided data frame}
  \item{rows}{a set of rows from each provided data frame, with values
  in columns not present in the given data frame filled with missing
  (\code{NA}) values.}
  The data type of columns will be preserved, as long as all data frames
  with a given column name agree on the data type of that column.  If
  the data frames disagree, the column will be converted into a
  character strings.  The user will need to coerce such character
  columns into an appropriate type.
}
\author{Gregory R. Warnes \email{greg@warnes.net}}
\seealso{ \code{\link{rbind}}, \code{\link{cbind}} }
\examples{

  df1 <- data.frame(A=1:10, B=LETTERS[1:10], C=rnorm(10) )
  df2 <- data.frame(A=11:20, D=rnorm(10), E=letters[1:10] )

  # rbind would fail
\dontrun{
  rbind(df1, df2)
  # Error in match.names(clabs, names(xi)) : names do not match previous
  # names:
  #	D, E
}
  # but smartbind combines them, appropriately creating NA entries
  smartbind(df1, df2)

  # specify fill=0 to put 0 into the missing row entries
  smartbind(df1, df2, fill=0)

\dontshow{
  n=10 # number of data frames to create
  s=10 # number of rows in each data frame

  # create a bunch of column names
  names <- LETTERS[2:5]

  # create a list 'Z' containing 'n' data frames, each with 3 columns
  # and 's' rows.  The first column is always named 'A', but the other
  # two have a names randomly selected from 'names'

  Z <- list()
  for(i in 1:n)
    {
      X <- data.frame(A=sample(letters,s,replace=TRUE),
                      B=letters[1:s],
                      C=rnorm(s) )
      colnames(X) <- c("A",sample(names,2,replace=FALSE))
      Z[[i]] <- X
    }

  # Error in match.names(clabs, names(xi)) : names do not match
  # previous names: E

  # But smartbind will 'do the right thing'
  df <- do.call("smartbind",Z)
  df
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

