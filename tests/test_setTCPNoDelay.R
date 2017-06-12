library('gtools')

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



host <- "www.r-project.org"
socket <- make.socket(host, 80)
print(socket)
setTCPNoDelay(socket, TRUE)

write.socket(socket, "GET /\n\n")
write.socket(socket, "A")
write.socket(socket, "B\n")
while( (str <- read.socket(socket)) > "")
  cat(str)
close.socket(socket)
