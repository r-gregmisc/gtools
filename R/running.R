#' Apply a Function Over Adjacent Subsets of a Vector
#'
#' Applies a function over subsets of the vector(s) formed by taking a fixed
#' number of previous points.
#'
#' \code{running} applies the specified function to a sequential windows on
#' \code{X} and (optionally) \code{Y}.  If \code{Y} is specified the function
#' must be bivariate.
#'
#' @param X data vector
#' @param Y data vector (optional)
#' @param fun Function to apply. Default is \code{mean}
#' @param width Integer giving the number of vector elements to include in the
#' subsets.  Defaults to the lesser of the length of the data and 20 elements.
#' @param allow.fewer Boolean indicating whether the function should be
#' computed for subsets with fewer than \code{width} points
#' @param pad Boolean indicating whether the returned results should be
#' 'padded' with NAs corresponding to sets with less than \code{width}
#' elements.  This only applies when when \code{allow.fewer} is FALSE.
#' @param align One of "right", "center", or "left".  This controls the
#' relative location of `short' subsets with less then \code{width} elements:
#' "right" allows short subsets only at the beginning of the sequence so that
#' all of the complete subsets are at the end of the sequence (i.e. `right
#' aligned'), "left" allows short subsets only at the end of the data so that
#' the complete subsets are `left aligned', and "center" allows short subsets
#' at both ends of the data so that complete subsets are `centered'.
#' @param simplify Boolean.  If FALSE the returned object will be a list
#' containing one element per evaluation.  If TRUE, the returned object will be
#' coerced into a vector (if the computation returns a scalar) or a matrix (if
#' the computation returns multiple values).  Defaults to FALSE.
#' @param by Integer separation between groups. If \code{by=width} will give
#' non-overlapping windows. Default is missing, in which case groups will start
#' at each value in the X/Y range.
#' @param \dots parameters to be passed to \code{fun}
#' @return List (if \code{simplify==TRUE}), vector, or matrix containing the
#' results of applying the function \code{fun} to the subsets of \code{X}
#' (\code{running}) or \code{X} and \code{Y}.
#'
#' Note that this function will create a vector or matrix even for objects
#' which are not simplified by \code{sapply}.
#' @author Gregory R. Warnes \email{greg@@warnes.net}, with contributions by
#' Nitin Jain \email{nitin.jain@@pfizer.com}.
#' @seealso \code{\link[gplots]{wapply}} to apply a function over an x-y window
#' centered at each x point, \code{\link[base]{sapply}},
#' \code{\link[base]{lapply}}
#' @keywords misc
#' @examples
#'
#'
#' # show effect of pad
#' running(1:20, width = 5)
#' running(1:20, width = 5, pad = TRUE)
#'
#' # show effect of align
#' running(1:20, width = 5, align = "left", pad = TRUE)
#' running(1:20, width = 5, align = "center", pad = TRUE)
#' running(1:20, width = 5, align = "right", pad = TRUE)
#'
#' # show effect of simplify
#' running(1:20, width = 5, fun = function(x) x) # matrix
#' running(1:20, width = 5, fun = function(x) x, simplify = FALSE) # list
#'
#' # show effect of by
#' running(1:20, width = 5) # normal
#' running(1:20, width = 5, by = 5) # non-overlapping
#' running(1:20, width = 5, by = 2) # starting every 2nd
#'
#'
#' # Use 'pad' to ensure correct length of vector, also show the effect
#' # of allow.fewer.
#' par(mfrow = c(2, 1))
#' plot(1:20, running(1:20, width = 5, allow.fewer = FALSE, pad = TRUE), type = "b")
#' plot(1:20, running(1:20, width = 5, allow.fewer = TRUE, pad = TRUE), type = "b")
#' par(mfrow = c(1, 1))
#'
#' # plot running mean and central 2 standard deviation range
#' # estimated by *last* 40 observations
#' dat <- rnorm(500, sd = 1 + (1:500) / 500)
#' plot(dat)
#' sdfun <- function(x, sign = 1) mean(x) + sign * sqrt(var(x))
#' lines(running(dat, width = 51, pad = TRUE, fun = mean), col = "blue")
#' lines(running(dat, width = 51, pad = TRUE, fun = sdfun, sign = -1), col = "red")
#' lines(running(dat, width = 51, pad = TRUE, fun = sdfun, sign = 1), col = "red")
#'
#'
#' # plot running correlation estimated by last 40 observations (red)
#' # against the true local correlation (blue)
#' sd.Y <- seq(0, 1, length = 500)
#'
#' X <- rnorm(500, sd = 1)
#' Y <- rnorm(500, sd = sd.Y)
#'
#' plot(running(X, X + Y, width = 20, fun = cor, pad = TRUE), col = "red", type = "s")
#'
#' r <- 1 / sqrt(1 + sd.Y^2) # true cor of (X,X+Y)
#' lines(r, type = "l", col = "blue")
#' @importFrom methods new
#' @export
running <- function(X, Y = NULL,
                    fun = mean,
                    width = min(length(X), 20),
                    allow.fewer = FALSE, pad = FALSE,
                    align = c("right", "center", "left"),
                    simplify = TRUE,
                    by, # added a parameter
                    ...) {
  align <- match.arg(align)

  n <- length(X)

  if (align == "left") {
    from <- 1:n
    to <- pmin((1:n) + width - 1, n)
  }
  else if (align == "right") {
    from <- pmax((1:n) - width + 1, 1)
    to <- 1:n
  }
  else # align=="center"
  {
    from <- pmax((2 - width):n, 1)
    to <- pmin(1:(n + width - 1), n)
    if (!odd(width)) stop("width must be odd for center alignment")
  }

  elements <- apply(cbind(from, to), 1, function(x) seq(x[1], x[2]))

  if (is.matrix(elements)) {
    elements <- as.data.frame(elements)
  } # ensure its a list!

  names(elements) <- paste(from, to, sep = ":")

  if (!allow.fewer) {
    len <- sapply(elements, length)
    skip <- (len < width)
  }
  else {
    skip <- 0
  }


  run.elements <- elements[!skip]

  if (!invalid(by)) {
    run.elements <- run.elements[seq(
      from = 1, to = length(run.elements),
      by = by
    )]
  }


  if (is.null(Y)) # univariate
    {
      funct.uni <- function(which, what, fun, ...) fun(what[which], ...)

      if (simplify) {
        Xvar <- sapply(run.elements, funct.uni, what = X, fun = fun, ...)
      } else {
        Xvar <- lapply(run.elements, funct.uni, what = X, fun = fun, ...)
      }
    }
  else # bivariate
  {
    funct.bi <- function(which, XX, YY, fun, ...) fun(XX[which], YY[which], ...)

    if (simplify) {
      Xvar <- sapply(run.elements, funct.bi, XX = X, YY = Y, fun = fun, ...)
    } else {
      Xvar <- lapply(run.elements, funct.bi, XX = X, YY = Y, fun = fun, ...)
    }
  }


  if (allow.fewer || !pad) {
    return(Xvar)
  }

  if (simplify) {
    if (is.matrix(Xvar)) {
      wholemat <- matrix(new(class(Xvar[1, 1]), NA),
        ncol = length(to), nrow = nrow(Xvar)
      )
      colnames(wholemat) <- paste(from, to, sep = ":")
      wholemat[, -skip] <- Xvar
      Xvar <- wholemat
    }
    else {
      wholelist <- rep(new(class(Xvar[1]), NA), length(from))
      names(wholelist) <- names(elements)
      wholelist[names(Xvar)] <- Xvar
      Xvar <- wholelist
    }
  }

  return(Xvar)
}
