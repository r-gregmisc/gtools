#' Determine the directory or full path to the currently executing script
#'
#' @param fail character, one of "stop", "warning", "quiet". specifying what
#'  should be done when the script path cannot be determined: 
#'  "stop" causes an error to be generated, 
#'  "warn" generates a warning message and returns NA,
#'  "quiet" silently returns NA.
#'
#' These function should work with \code{Rscript}, \code{source()},
#' \code{Rmarkdown} \code{RStudio}'s "Run selection", and the \code{RStudio}
#' Console.
#'
#' @author Greg Warnes <greg@warnes.net> based on on a Stack Overflow post by
#'   jerry-t (\url{https://stackoverflow.com/users/2292993/jerry-t}) at
#'   \url{https://stackoverflow.com/a/36777602/2744062}.
#'
#' @return A character scalar containing the full path to the currently
#'   executing script file (\code{script_file}) or its directory
#'   (\code{script_path}).  If unable to determine the script path, it generates
#'   a warning and returns \code{""} (empty string).
#'
#' @examples
#'
#' getwd()
#' commandArgs(trailingOnly = FALSE)
#'
#' script_file("warning")
#' script_path("warning")
#'
#' @name script_file
NULL

#' @describeIn script_file  Determine the full path of the currently executing
#'   script
#' @export
script_file <- function(fail=c("stop","warning","quiet")) {
  
  fail <- match.arg(fail)
  
  # http://stackoverflow.com/a/32016824/2292993
  cmdArgs = commandArgs(trailingOnly = FALSE)
  needle = "--file="
  match = grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript via command line with "--file="
    return(normalizePath(sub(needle, "", cmdArgs[match])))
  } else if ("-f" %in% cmdArgs)
  {
    idx <- which("-f" == cmdArgs) + 1
    return(normalizePath(cmdArgs[idx]))
  } else
  {
    ls_vars = ls(sys.frames()[[1]])
    if ("fileName" %in% ls_vars) 
    {
      # Source'd via RStudio
      return(normalizePath(sys.frames()[[1]]$fileName))
    } else {
      if (!is.null(sys.frames()[[1]]$ofile)) {
        # Source'd via R console
        return(normalizePath(sys.frames()[[1]]$ofile))
      } else {
        tryCatch(
          {
            if(requireNamespace('knitr', quietly=TRUE)) {
              
              # Executing in a RMarkdown file
              pth = knitr::current_input(dir = TRUE)
              if (length(pth) > 0 && nchar(pth)>0) {
                return(normalizePath(pth))
              }
            }
            
            if(requireNamespace('rstudioapi', quietly=TRUE)
               && rstudioapi::isAvailable() ) {
              
              # RStudio Run Selection
              # http://stackoverflow.com/a/35842176/2292993
              pth = rstudioapi::getActiveDocumentContext()$path
              if (length(pth) > 0 && nchar(pth)>0) {
                return(normalizePath(pth))
              }
              
              # RStudio Console
              pth = rstudioapi::getSourceEditorContext()$path
              if (length(pth) > 0 && nchar(pth)>0) {
                return(normalizePath(pth))
              }
              
              # RStudio Source Editor
              pth = rstudioapi::getConsoleEditorContext()$path
              if (length(pth) > 0 && nchar(pth)>0) {
                return(normalizePath(pth))
              }
              
            }
            
            stop()
          },
          error = function(e)
          {
            if(fail=="stop")
              stop("Unable to resolve file path.")
            else if (fail=="warning")
            {
              warning("Unable to resolve file path, returning NA.")
              return(NA)
            }
            else # fail=="quiet"
              return(NA)
          }
        )
      }
    }
  }
  
  NA
}


#' @describeIn script_file  Determine the directory of the currently executing script
#'
#' @export
script_path <- function(fail=c("stop","warning","quiet"))
{
  fail <- match.arg(fail)
  
  file <- script_file(fail=fail)
  if(is.na(file))
    NA
  else
    dirname(file)
}
