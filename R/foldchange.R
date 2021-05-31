#' Compute fold-change or convert between log-ratio and fold-change.
#'
#' \code{foldchange} computes the fold change for two sets of values.
#' \code{logratio2foldchange} converts values from log-ratios to fold changes.
#' \code{foldchange2logratio} does the reverse.
#'
#' Fold changes are commonly used in the biological sciences as a mechanism for
#' comparing the relative size of two measurements.  They are computed as:
#' \eqn{\frac{num}{denom}}{num/denom} if \eqn{num>denom}, and as
#' \eqn{\frac{-denom}{num}}{-denom/num} otherwise.
#'
#' Fold-changes have the advantage of ease of interpretation and symmetry about
#' \eqn{num=denom}, but suffer from a discontinuty between -1 and 1, which can
#' cause significant problems when performing data analysis.  Consequently
#' statisticians prefer to use log-ratios.
#'
#' @aliases foldchange logratio2foldchange foldchange2logratio
#' @param num,denom vector/matrix of numeric values
#' @param logratio vector/matrix of log-ratio values
#' @param foldchange vector/matrix of fold-change values
#' @param base Exponential base for the log-ratio.
#' @return A vector or matrix of the same dimensions as the input containing
#' the converted values.
#' @author Gregory R. Warnes \email{greg@@warnes.net}
#' @keywords math
#' @examples
#'
#'
#' a <- 1:21
#' b <- 21:1
#'
#' f <- foldchange(a, b)
#'
#' cbind(a, b, f)
#' @name foldchange
NULL

#' @describeIn foldchange Compute fold-change.
#' @export
foldchange <- function(num, denom) {
  ifelse(num >= denom, num / denom, -denom / num)
}

#' @describeIn foldchange Compute foldchange from log-ratio values.
#' @export
logratio2foldchange <- function(logratio, base = 2) {
  retval <- base^(logratio)
  retval <- ifelse(retval < 1, -1 / retval, retval)
  retval
}

#' @describeIn foldchange Compute log-ratio from fold-change values.
#' @export
foldchange2logratio <- function(foldchange, base = 2) {
  retval <- ifelse(foldchange < 0, 1 / -foldchange, foldchange)
  retval <- log(retval, base)
  retval
}
