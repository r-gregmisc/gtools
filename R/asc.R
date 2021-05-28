#' Convert between characters and ASCII codes
#' 
#' @aliases asc chr
#' @param char vector of character strings
#' @param simplify logical indicating whether to attempt to convert the result
#' into a vector or matrix object. See \code{\link[base]{sapply}} for details.
#' @param ascii vector or list of vectors containing integer ASCII codes
#' @return \code{asc} returns the integer ASCII values for each character in
#' the elements of \code{char}.  If \code{simplify=FALSE} the result will be a
#' list contining one vector per element of \code{char}.  If
#' \code{simplify=TRUE}, the code will attempt to convert the result into a
#' vector or matrix.
#' 
#' \code{asc} returns the characters corresponding to the provided ASCII
#' values.
#' @author Adapted by Gregory R. Warnes \email{greg@@warnes.net} from code
#' posted on the 'Data Debrief' blog on 2011-03-09 at
#' \url{http://datadebrief.blogspot.com/2011/03/ascii-code-table-in-r.html}.
#' @seealso \code{\link[base]{strtoi}}, \code{\link[base]{charToRaw}},
#' \code{\link[base]{rawToChar}}, \code{\link[base]{as.raw}}
#' @keywords character programming
#' @examples
#' 
#'   ## ascii codes for lowercase letters
#'   asc(letters)
#' 
#'   ## uppercase letters from ascii codes
#'   chr(65:90)
#' 
#'   ## works on muti-character strings
#'   ( tmp <- asc('hello!') )
#'   chr(tmp)
#' 
#'   ## Use 'simplify=FALSE' to return the result as a list
#'   ( tmp <- asc('hello!', simplify=FALSE) )
#'   chr(tmp)
#' 
#'   ## When simplify=FALSE the results can be...
#'   asc( c('a', 'e', 'i', 'o', 'u', 'y' ) ) # a vector
#'   asc( c('ae', 'io', 'uy' ) )             # or a matrix
#' 
#'   ## When simplify=TRUE the results are always a list...
#'   asc( c('a', 'e', 'i', 'o', 'u', 'y' ), simplify=FALSE )
#'   asc( c('ae', 'io', 'uy' ), simplify=FALSE)
#' 
#' @name asc
NULL

#' @describeIn asc return the characters corresponding to the specified ASCII codes
#' @export 
asc <- function(char, simplify=TRUE)
    sapply(char, function(x) strtoi(charToRaw(x),16L), simplify=simplify )

#' @describeIn asc return the ASCII codes for the specified characters.
#' @export
chr <- function(ascii) sapply(ascii, function(x) rawToChar(as.raw(x)) )
