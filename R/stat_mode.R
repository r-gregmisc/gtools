#' Most frequently occurring value
#'
#' @param x vector of values
#' @param na.rm logical.  Should \code{NA} values be removed before processing?
#' @param ties character. Which value(s) should be returned in the case of ties?
#' @param \dots optional additional parameters.
#'
#' @return vector of the same class as \code{x}
#'
#' @author Genei Ryodan and Gregory R. Warnes \email{greg@warnes.net}.
#'
#' @examples
#'
#' # Character vector
#' chr_vec <- c("a", "d", "d", "h", "h", NA, NA) # Multiple modes
#' stat_mode(x = chr_vec)
#' stat_mode(x = chr_vec, na.rm = FALSE)
#' stat_mode(x = chr_vec, na.rm = FALSE, ties = "first")
#' stat_mode(x = chr_vec, na.rm = FALSE, ties = "last")
#'
#' # - # Numeric vector
#' # See that it keeps the original vector type
#' num_vec <- c(2, 3, 3, 4, 4, NA, NA)
#' stat_mode(x = num_vec)
#' stat_mode(x = num_vec, na.rm = FALSE)
#' stat_mode(x = num_vec, na.rm = FALSE, ties = "first")
#' stat_mode(x = num_vec, na.rm = FALSE, ties = "last")
#'
#' # The default option is ties="all" but it is very easy for the user to control
#' # the ties without changing this parameter.
#' # Select always just one mode, being that the first mode
#' stat_mode(x = num_vec)[1]
#'
#' # Select the first and the second stat_mode
#' stat_mode(x = num_vec)[c(1, 2)]
#'
#' # Logical Vectors
#' stat_mode(x = c(TRUE, TRUE))
#' stat_mode(x = c(FALSE, FALSE, TRUE, TRUE))
#'
#' # - # Single element cases
#' stat_mode(x = c(NA_real_))
#' stat_mode(x = 2)
#' stat_mode(x = NA)
#' stat_mode(x = c("a"))
#'
#' # Not allowing multiple stat_mode, returning NA if that happens
#' stat_mode(x = c(1, 1, 2, 2), multiple_modes = FALSE) # multiple stat_mode
#' stat_mode(x = c(1, 1), multiple_modes = FALSE) # single mode
#'
#' # Empty vector cases
#' # The ties of any empty vector will be itself (an empty vector of the same type)
#' stat_mode(x = double())
#' stat_mode(x = complex())
#' stat_mode(x = vector("numeric"))
#' stat_mode(x = vector("character"))
#' @importFrom stats na.omit
#' @export
stat_mode <- function(x,
                      na.rm = TRUE,
                      ties = c("all", "first", "last", "missing"),
                      ...) {
  ties <- match.arg(ties)

  if (na.rm) {
    uv <- unique(na.omit(x))
  } else {
    uv <- unique(x)
  }

  tab <- tabulate(match(x, uv))

  all_modes <- uv[tab == max(tab)]

  if (length(all_modes) > 1) {
    if (ties == "first") {
      return(all_modes[1])
    } else if (ties == "last") {
      return(all_modes[length(all_modes)])
    } else if (ties == "all") {
      return(all_modes)
    } # ties=="missing"
    else {
      return(NA)
    }
  }
  else {
    return(all_modes)
  }
}
