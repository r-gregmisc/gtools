#' Determine the directory or full path to the currently executing script
#' 
#' Determine the directory or full path to the currently executing script
#' 
#' These function should work with \code{Rscript}, \code{source()},
#' \code{Rmarkdown} \code{RStudio}'s "Run selection", and the \code{RStudio}
#' Console.
#' 
#' @aliases script_file script_path
#' @return A character scalarr containing the full path to the current script
#' file (\code{script_file}) or its directory (\code{script_path}).  If unable
#' to determine the script path, it generates a warning and returns \code{""}
#' (empty string).
#' @section Functions: \itemize{ \item \code{script_file}: Determine the full
#' path of the currently executing script
#' 
#' \item \code{script_path}: Determine the directory of the currently executing
#' script }
#' @author Greg Warnes <greg@@warnes.net> based on on a Stack Overflow post by
#' jerry-t (\url{https://stackoverflow.com/users/2292993/jerry-t}) at
#' \url{https://stackoverflow.com/a/36777602/2744062}.
#' @examples
#' 
#' 
#' \dontrun{
#' script_file()
#' script_path()
#' }
#' 
#' @export
#' 
#' @importFrom knitr current_input
#' @importFrom rstudioapi getActiveDocumentContext
#' @importFrom rstudioapi getSourceEditorContext
#' @importFrom rstudioapi getConsoleEditorContext

#' 
script_file <- function() {
  # http://stackoverflow.com/a/32016824/2292993
  cmdArgs = commandArgs(trailingOnly = FALSE)
  needle = "--file="
  match = grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript via command line
    return(normalizePath(sub(needle, "", cmdArgs[match])))
  } else {
    ls_vars = ls(sys.frames()[[1]])
    if ("fileName" %in% ls_vars) {
      # Source'd via RStudio
      return(normalizePath(sys.frames()[[1]]$fileName))
    } else {
      if (!is.null(sys.frames()[[1]]$ofile)) {
        # Source'd via R console
        return(normalizePath(sys.frames()[[1]]$ofile))
      } else {
        tryCatch(
          {
            if(requireNamespace('knitr')) {

              # Executing in a RMarkdown file
              pth = knitr::current_input(dir = TRUE)
              if (length(pth) > 0 && nchar(pth)>0) {
                return(normalizePath(pth))
              }
            }

            if(requireNamespace('rstudioapi')) {

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
          },
          error = function(e)
          {
            warning("Unable to resolve file path, returning '': ", e)
            return('')
          }
        )
      }
    }
  }
}


#' @describeIn script_file  Determine the directory of the currently executing script
#'
#' @export
script_path <- function()
{
  return(dirname(script_file()))
}
