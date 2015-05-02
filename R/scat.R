# $Id$

# cat to stdout and immediately flush
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
