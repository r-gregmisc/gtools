#' Replace Missing Values
#' 
#' Replace missing values
#' 
#' This is a convenience function that is the same as x[is.na(x)] <- replace
#' 
#' @param x vector possibly contining missing (\code{NA}) values
#' @param replace either a scalar replacement value, or a function returning a
#' scalar value
#' @param ... Optional arguments to be passed to \code{replace}
#' @return Vector with missing values (\code{NA}) replaced by the value of
#' \code{replace}.
#' @author Gregory R. Warnes \email{greg@@warnes.net}
#' @seealso \code{\link[base]{is.na}}, \code{\link[stats]{na.omit}}
#' @keywords manip
#' @examples
#' 
#'    x <- c(1,2,3,NA,6,7,8,NA,NA)
#'    
#'    # Replace with a specified value
#'    na.replace(x, '999')
#'    
#'    # Replace with the calculated median
#'    na.replace(x, median, na.rm=TRUE)    
#' 
#' @export
na.replace <- function(x, replace, ...)
{
  if(is.function(replace))
    replace <- replace(x, ...)
  
  x[is.na(x)] <- replace
  x
}
