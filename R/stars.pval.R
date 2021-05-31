#' Generate significance stars from p-values
#'
#' Generate significance stars (e.g. '***', '**', '*', '+') from p-values using
#' R's standard definitions.
#'
#' Mapping from p-value ranges to symbols: \describe{ \item{0 - 0.001}{'***'}
#' \item{0.001 - 0.01}{'**'} \item{0.01 - 0.05}{'*'} \item{0.05 - 0.1}{'+'}
#' \item{0.1 - 1.0}{'' (No symbol)} }
#'
#' @param p.value numeric vector of p-values
#' @return A character vector containing the same number of elements as
#' \code{p-value}, with an attribute "legend" providing the conversion pattern.
#' @author Gregory R. Warnes \email{greg@@warnes.net}
#' @seealso \code{\link[stats]{symnum}}
#' @keywords misc
#' @examples
#'
#' p.val <- c(0.0004, 0.0015, 0.013, 0.044, 0.067, 0.24)
#' stars.pval(p.val)
#' @importFrom stats symnum
#' @export
stars.pval <- function(p.value) {
  unclass(
    symnum(p.value,
      corr = FALSE, na = FALSE,
      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
      symbols = c("***", "**", "*", ".", " ")
    )
  )
}
