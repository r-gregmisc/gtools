#' Convert a byte-code function to an interpreted-code function
#'
#' The purpose of these functions is to allow a byte coded function to be
#' converted back into a fully interpreted function as a \emph{temporary} work
#' around for issues in byte-code interpretation.
#'
#' \code{unByteCode} returns a copy of the function that is directly
#' interpreted from text rather than from byte-code.
#'
#' \code{assignEdgewise} makes an assignment into a locked environemnt.
#'
#' \code{unByteCodeAssign} changes the specified function \emph{in its source
#' environment} to be directly interpreted from text rather than from
#' byte-code.
#'
#' @aliases unByteCode unByteCodeAssign assignEdgewise
#' @param fun function to be modified
#' @param name object name
#' @param env namespace
#' @param value new function body
#' @return All three functions return a copy of the modified function or
#' assigned value.
#' @note These functions are not intended as a permanent solution to issues
#' with byte-code compilation or interpretation.  Any such issues should be
#' promtply reported to the R maintainers via the R Bug Tracking System at
#' \url{https://bugs.r-project.org} and via the R-devel mailing list
#' \url{https://stat.ethz.ch/mailman/listinfo/r-devel}.
#' @author Gregory R. Warnes \email{greg@@warnes.net}
#' @seealso \code{\link[compiler]{disassemble}}, \code{\link{assign}}
#' @references These functions were inspired as a work-around to R bug
#' \url{https://bugs.r-project.org/bugzilla/show_bug.cgi?id=15215}.
#' @keywords programming utilites
#' @examples
#'
#' data(badDend)
#' dist2 <- function(x) as.dist(1 - cor(t(x), method = "pearson"))
#' hclust1 <- function(x) hclust(x, method = "single")
#'
#' distance <- dist2(badDend)
#' cluster <- hclust1(distance)
#'
#' dend <- as.dendrogram(cluster)
#' \dontrun{
#' ## In R 2.3.0 and earlier crashes with a node stack overflow error
#' plot(dend)
#' ## Error in xy.coords(x, y, recycle = TRUE) : node stack overflow
#' }
#'
#' ## convert stats:::plotNode from byte-code to interpreted-code
#' unByteCodeAssign(stats:::plotNode)
#'
#' # increase recursion limit
#' options("expressions" = 5e4)
#'
#' # now the function does not crash
#' plot(dend)
#' @export
unByteCode <- function(fun) {
  FUN <- eval(parse(text = deparse(fun)))
  environment(FUN) <- environment(fun)
  FUN
}

#' @rdname unByteCode
#' @export
assignEdgewise <- function(name, env, value) {
  unlockBinding(name, env = env)
  assign(name, envir = env, value = value)
  lockBinding(name, env = env)
  invisible(value)
}

#' @rdname unByteCode
#' @export
unByteCodeAssign <- function(fun) {
  name <- gsub("^.*::+", "", deparse(substitute(fun)))
  FUN <- unByteCode(fun)
  retval <- assignEdgewise(
    name = name,
    env = environment(FUN),
    value = FUN
  )
  invisible(retval)
}
