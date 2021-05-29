#' Check if a new version of R is available
#' 
#' Compare the current version of R to the versions available on CRAN and display
#' the results.
#' 
#' @param quiet logical. If TRUE, results are not displayed.
#' 
#' @returns The \code{numeric_version} of a newer version, if available, otherwise NULL.
#' 
#' @examples 
#' 
#' newVersionAvailable() # Display result
#' flag <- newVersionAvailable(quiet=TRUE) # Don't display results
#' str(flag)
#' 
#' @export
newVersionAvailable <- function(quiet=FALSE)
{
  page <- scan(file="http://cran.r-project.org/src/base/R-4", what="", quiet=TRUE)
  matches <- grep("R-[0-9]\\.[0-9]+\\.[0-9]+", page, value=TRUE)
  versionList <- gsub("^.*R-([0-9].[0-9]+.[0-9]+).*$","\\1",matches)
  versionList <- numeric_version(versionList)
  if( max(versionList) > getRversion() )
  {
    if(!quiet)
    {
      message("A newer version of R is now available: ", 
              max(versionList)
      )
    }
    invisible( max(versionList) )
  }
  else
  {
    if(!quiet)
    {
      message("The latest version of R is installed: ",
              as.character(getRversion())
      )
    }
    invisible( NULL );
  }
  
}
