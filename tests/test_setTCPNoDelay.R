library("gtools")

host <- "www.r-project.org"

socket <- make.socket(host, 80)
print(socket)

setTCPNoDelay(socket, TRUE)

write.socket(socket, "GET http://www.r-project.org/index.html\r\n")
write.socket(socket, "HOST 127.0.0.1\r\n")
write.socket(socket, "\r\n")

# Read and display the response
while ((str <- read.socket(socket)) > "") {
  cat(str)
}

close.socket(socket)
