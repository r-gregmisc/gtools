#' Display a prompt and collect the user's response
#'
#' Display a prompt and collect the user's response
#'
#' The prompt message will be displayed, and then \code{readLines} is used to
#' collect a single input value (possibly empty), which is then returned.
#'
#' In most situations using the default \code{con=stdin()} should work
#' properly.  Under RStudio, it is necessary to specify
#' \code{con=file("stdin")} for proper operation.
#'
#' @param msg Character vector providing the message to be displayed
#' @param con Character connection to query, defaults to \code{stdin()}.
#' @return A character scalar containing the input provided by the user.
#' @author Gregory R. Warnes \email{greg@@warnes.net}
#' @seealso \code{\link{readLines}}, \code{\link{scan}}
#' @keywords IO
#' @examples
#'
#'
#' # use default prompt
#' ask()
#'
#' silly <- function() {
#'   age <- ask("How old aroe you? ")
#'   age <- as.numeric(age)
#'   cat("In 10 years you will be", age + 10, "years old!\n")
#' }
#' @export
ask <- function(msg = "Press <RETURN> to continue: ", con = stdin()) {
  cat(msg)
  readLines(con = con, n = 1)
}
