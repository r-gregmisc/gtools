library('gtools')

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
