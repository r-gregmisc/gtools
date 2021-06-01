#' Non-destructively construct a .Last function to be executed when R exits.
#'
#' Non-destructively construct a \code{.Last} function to be executed when R
#' exits.
#'
#' \code{lastAdd} constructs a new function which can be used to replace the
#' existing definition of \code{.Last}, which will be executed when R terminates
#' normally.
#'
#' If a \code{.Last} function already exists in the global environment, the
#' original definition is stored in a private environment, and the new function
#' is defined to call the function \code{fun} and then to call the previous
#' (stored) definition of \code{.Last}.
#'
#' If no \code{.Last} function exists in the global environment, \code{lastAdd}
#' simply returns the function \code{fun}.
#'
#' @param fun Function to be called.
#' @return A new function to be used for \code{.Last}.
#' @note This function replaces the (now defunct) \code{addLast} function.
#' @author Gregory R. Warnes \email{greg@@warnes.net}
#' @seealso \code{\link[base]{.Last}}
#' @keywords programming
#' @examples
#'
#'
#' ## Print a couple of cute messages when R exits.
#' helloWorld <- function() cat("\nHello World!\n")
#' byeWorld <- function() cat("\nGoodbye World!\n")
#'
#' .Last <- lastAdd(byeWorld)
#' .Last <- lastAdd(helloWorld)
#' \dontshow{
#' .Last()
#' }
#' \dontrun{
#' q("no")
#'
#' ## Should yield:
#' ##
#' ##   Save workspace image? [y/n/c]: n
#' ##
#' ##   Hello World!
#' ##
#' ##   Goodbye World!
#' ##
#' ##   Process R finished at Tue Nov 22 10:28:55 2005
#' }
#'
#' @export
lastAdd <- function(fun) {
  if (!is.function(fun)) stop("fun must be a function")

  if (!exists(".Last", envir = .GlobalEnv, mode = "function")) {
    return(fun)
  }
  else {
    Last <- get(".Last", envir = .GlobalEnv, mode = "function")
    newfun <- function(...) {
      browser()
      fun()
      Last()
    }
    return(newfun)
  }
}
