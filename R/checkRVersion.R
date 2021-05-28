#' Check if a newer version of R is available
#' 
#' Check if a newer version of R is available
#' 
#' This function accesses the R web site to discover the latest released
#' version of R.  It then compares this version to the running version.  If the
#' running version is the same as the latest version, it prints the message,
#' "The latest version of R is installed:" followed by the version number, and
#' returns NULL.  If the running version is older than the current version, it
#' displays the message, "A newer version of R is now available:" followed by
#' the corresponding version number, and returns the version number.
#' 
#' If \code{quiet=TRUE}, no printing is performed.
#' 
#' @param quiet Logical indicating whether printed output should be supressed.
#' @return Either the version number of the latest version of R, if the running
#' version is less than the latest version, or NULL.
#' @note This function utilizes the internet to access the R project web site.
#' If internet access is unavailable, the function will fail.
#' @author Gregory R. Warnes \email{gregory.warnes@@rochester.edu>}
#' @seealso \code{\link[base]{R.Version}}
#' @keywords utilities
#' @examples
#' 
#' 
#' checkRVersion()
#' 
#' ver <- checkRVersion()
#' print(ver)
#' 
#' 
checkRVersion <- function(quiet=FALSE)
  {
    page2 <- scan(file="http://cran.r-project.org/src/base/R-2",
                  what="", quiet=TRUE)
    page3 <- scan(file="http://cran.r-project.org/src/base/R-3",
                  what="", quiet=TRUE)

    combined <- c(page2, page3)
    
    matches <- grep("R-[0-9]\\.[0-9]+\\.[0-9]+", combined, value=TRUE)
    versionList <- gsub("^.*R-([0-9].[0-9]+.[0-9]+).*$","\\1",matches)
    versionList <- numeric_version(versionList)
    if( max(versionList) > getRversion() )
      {
        if(!quiet)
          {
            cat("A newer version of R is now available: ")
            cat(as.character(max(versionList)))
            cat("\n")
          }
        invisible( max(versionList) )
     }
    else
      {
        if(!quiet)
          {
            cat("The latest version of R is installed: ")
            cat(as.character(max(versionList)))
            cat("\n")
          }
        invisible( NULL );
      }
    
  }
