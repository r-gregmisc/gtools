#' Randomly Permute the Elements of a Vector
#'
#' Randomly Permute the elements of a vector
#'
#' This is simply a wrapper function for \code{\link{sample}}.
#'
#' @param x Vector of items to be permuted
#' @return Vector with the original items reordered.
#' @author Gregory R. Warnes \email{greg@@warnes.net}
#' @seealso \code{\link{sample}}
#' @keywords distribution
#' @examples
#'
#' x <- 1:10
#' permute(x)
#' @export
permute <- function(x) sample(x, size = length(x), replace = FALSE)
