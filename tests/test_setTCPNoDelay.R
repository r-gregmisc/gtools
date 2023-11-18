library("gtools")

host <- "www.r-project.org"
host_index <- sprintf("http://%s/index.html", host)

## check that network/CRAN are available
rproj_OK <- try(scan(host_index, character(1), quiet = TRUE))

if (!inherits(rproj_OK, "try-error")) {
    socket <- make.socket(host, 80)
    print(socket)

    setTCPNoDelay(socket, TRUE)
    
    write.socket(socket, sprintf("GET %s\r\n", host_index))
    write.socket(socket, "HOST 127.0.0.1\r\n")
    write.socket(socket, "\r\n")

    ## Read and display the response
    while ((str <- read.socket(socket)) > "") {
        cat(str)
    }

    close.socket(socket)
}
