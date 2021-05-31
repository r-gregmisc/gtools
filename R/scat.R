#' Display debugging text
#' 
#' If \code{getOption('DEBUG')==TRUE}, write text to STDOUT and flush so that
#' the text is immediatly displayed. Otherwise, do nothing.
#' 
#' 
#' @param \dots Arguments passed to \code{cat}
#' @return NULL (invisibly)
#' @author Gregory R. Warnes \email{greg@@warnes.net}
#' @seealso \code{\link[base]{cat}}
#' @keywords print
#' @examples
#' 
#' options(DEBUG=NULL) # makee sure DEBUG isn't set
#' scat("Not displayed")
#' 
#' options(DEBUG=TRUE)
#' scat("This will be displayed immediately (even in R BATCH output \n")
#' scat("files), provided options()$DEBUG is TRUE.")
#' 
#' @export
scat <- function(...)
  {
    DEBUG <- options()$DEBUG
    if( !is.null(DEBUG) && DEBUG)
      {
        cat("### ", file=stderr())
        cat(..., file=stderr())
        cat(" ###\n", file=stderr())
        flush(stderr())
      }
    invisible(NULL)
  }
