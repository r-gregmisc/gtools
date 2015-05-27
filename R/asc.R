asc <- function(char) sapply(char, function(x) strtoi(charToRaw(x),16L) )
chr <- function(n) sapply(n, function(x) rawToChar(as.raw(x)) )
