#' Detect odd/even integers
#' 
#' detect odd/even integers
#' 
#' 
#' @aliases odd even
#' @param x vector of integers
#' @return Vector of TRUE/FALSE values.
#' @author Gregory R. Warnes \email{greg@@warnes.net}
#' @seealso \code{\link[base]{round}}
#' @keywords arith
#' @examples
#' 
#' 
#' odd(4)
#' even(4)
#' 
#' odd(1:10)
#' even(1:10)
#' 
#' @name oddeven
NULL

#' @rdname odd
#' @export
odd <- function(x) x %% 2 == 1

#' @rdname odd
#' @export
even <- function(x) x %% 2 == 0 
