##
## Replaces the (defunct) addLast() function.
##
lastAdd <- function( fun )
  {
    if (!is.function(fun)) stop("fun must be a function")
  
    if(!exists(".Last", envir=.GlobalEnv, mode="function"))
      {
        return(fun)
      }
    else
      {
        Last <- get(".Last", envir=.GlobalEnv, mode="function")
        newfun <- function(...)
          {
            browser()
            fun()
            Last()
          }
        return(newfun)
      }
  }

