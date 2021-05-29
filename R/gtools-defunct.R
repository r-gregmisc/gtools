#' Defunct Functions in package \code{gtools}
#' 
#' The functions or variables listed here are no longer part of package
#' \code{gtools}.
#' 
#' \itemize{ \item \code{assert} is a defunct synonym for
#' \code{\link[base]{stopifnot}}.  \item \code{addLast} has been replaced by
#' \code{lastAdd}, which has the same purpose but appled using different
#' syntax.  \item \code{capture} and \code{capture.output} have been removed in
#' favor of \code{capture.output} from the \code{utils} package.  }
#' 
#' @aliases gtools-defunct assert addLast capture sprint
#' @seealso \code{\link[base]{Defunct}}, \code{\link[base]{stopifnot}},
#' \code{\link[gtools]{lastAdd}}, \code{\link[utils]{capture.output}}
#' @keywords misc
#' 
#' @rdname gtools-defunct
#' @export
capture <- function( expression, collapse="\n")
    .Defunct("capture.output", "base")

#' @rdname gtools-defunct
#' @export
sprint <- function(x,...)
    .Defunct("capture.output", "base")

