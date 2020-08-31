#' Modify the TCP\_NODELAY (`de-Nagle') flag for socket objects
#' 
#' Modify the TCP\_NODELAY (`de-Nagele') flag for socket objects
#' 
#' By default, TCP connections wait a small fixed interval before actually
#' sending data, in order to permit small packets to be combined.  This
#' algorithm is named after its inventor, John Nagle, and is often referred to
#' as 'Nagling'.
#' 
#' While this reduces network resource utilization in these situations, it
#' imposes a delay on all outgoing message data, which can cause problems in
#' client/server situations.
#' 
#' This function allows this feature to be disabled (de-Nagling,
#' \code{value=TRUE}) or enabled (Nagling, \code{value=FALSE}) for the
#' specified socket.
#' 
#' @param socket A socket connection object
#' @param value Logical indicating whether to set (\code{TRUE}) or unset
#' (\code{FALSE}) the flag
#' @return The character string "SUCCESS" will be returned invisible if the
#' operation was succesful.  On failure, an error will be generated.
#' @author Gregory R. Warnes \email{greg@@warnes.net}
#' @seealso \code{\link{make.socket}}, \code{\link{socketConnection}}
#' @references "Nagle's algorithm" at WhatIS.com \url{
#' http://searchnetworking.techtarget.com/sDefinition/0,,sid7_gci754347,00.html}
#' 
#' Nagle, John. "Congestion Control in IP/TCP Internetworks", IETF Request for
#' Comments 896, January 1984.
#' \url{http://www.ietf.org/rfc/rfc0896.txt?number=896}
#' @keywords programming misc utilities
#' @examples
#' 
#' \dontrun{
#' host <- "www.r-project.org"
#' socket <- make.socket(host, 80)
#' print(socket)
#' setTCPNoDelay(socket, TRUE)
#' 
#' write.socket(socket, "GET /\n\n")
#' write.socket(socket, "A")
#' write.socket(socket, "B\n")
#' while( (str <- read.socket(socket)) > "")
#'   cat(str)
#' close.socket(socket)
#' }
#' 
#' 
setTCPNoDelay <- function( socket, value=TRUE )
  {
    if(!any(c("socket","sockconn") %in% class(socket)))
      stop("socket must be a socket object")

    buffer <- paste(rep(" ", 1000), sep='', collapse='')

    if("sockconn" %in% class(socket))
      conn <- getConnection(socket[1])
    else
      conn <- socket
 
    
    retval <- .C("R_setTCPNoDelay",
                 socket=as.integer(socket[1]),
                 flag=as.integer(value),
                 status=integer(1),
                 status.str=as.character(buffer),
                 status.len=as.integer(nchar(buffer)),
                 package="gtools"
                 )

    if(retval$status != 0)
      stop( retval$status.str )
    else
      invisible(retval$status.str)
  }
