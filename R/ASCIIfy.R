#' Convert Characters to ASCII
#'
#' Convert character vector to ASCII, replacing non-ASCII characters with
#' single-byte (\samp{\x00}) or two-byte (\samp{\u0000}) codes.
#'
#'
#' @param x a character vector, possibly containing non-ASCII characters.
#' @param bytes either \code{1} or \code{2}, for single-byte (\samp{\x00}) or
#' two-byte (\samp{\u0000}) codes.
#' @param fallback an output character to use, when input characters cannot be
#' converted.
#' @return A character vector like \code{x}, except non-ASCII characters have
#' been replaced with \samp{\x00} or \samp{\u0000} codes.
#' @note To render single backslashes, use these or similar
#' techniques:\preformatted{ write(ASCIIfy(x), "file.txt")
#' cat(paste(ASCIIfy(x), collapse="\n"), "\n", sep="")}
#'
#' The resulting strings are plain ASCII and can be used in R functions and
#' datasets to improve package portability.
#' @author Arni Magnusson \email{arnima@@hafro.is}
#' @seealso \code{\link[tools]{showNonASCII}} identifies non-ASCII characters
#' in a character vector.
#' @keywords utilites character
#' @examples
#'
#' cities <- c("S\u00e3o Paulo", "Reykjav\u00edk")
#' print(cities)
#' ASCIIfy(cities, 1)
#' ASCIIfy(cities, 2)
#'
#' athens <- "\u0391\u03b8\u03ae\u03bd\u03b1"
#' print(athens)
#' ASCIIfy(athens)
#' @export
#'
ASCIIfy <- function(x, bytes = 2, fallback = "?") {
  bytes <- match.arg(as.character(bytes), 1:2)
  convert <- function(char) # convert to ASCII, e.g. "z", "\xfe", or "\u00fe"
  {
    raw <- charToRaw(char)
    if (length(raw) == 1 && raw <= 127) { # 7-bit
      ascii <- char
    } else if (length(raw) == 1 && bytes == 1) { # 8-bit to \x00
      ascii <- paste0("\\x", raw)
    } else if (length(raw) == 1 && bytes == 2) { # 8-bit to \u0000
      ascii <- paste0("\\u", chartr(" ", "0", formatC(as.character(raw), width = 4)))
    } else if (length(raw) == 2 && bytes == 1) { # 16-bit to \x00, if possible
      if (utf8ToInt(char) <= 255) {
        ascii <- paste0("\\x", format.hexmode(utf8ToInt(char)))
      } else {
        ascii <- fallback
        warning(char, " could not be converted to 1 byte")
      }
    } else if (length(raw) == 2 && bytes == 2) { # UTF-8 to \u0000
      ascii <- paste0("\\u", format.hexmode(utf8ToInt(char), width = 4))
    } else {
      ascii <- fallback
      warning(char, " could not be converted to ", bytes, " byte")
    }
    return(ascii)
  }

  if (length(x) > 1) {
    sapply(x, ASCIIfy, bytes = bytes, fallback = fallback, USE.NAMES = FALSE)
  }
  else {
    input <- unlist(strsplit(x, "")) # "c"  "a"  "f"  "<\'e>"
    output <- character(length(input)) # ""   ""   ""   ""
    for (i in seq_along(input)) {
      output[i] <- convert(input[i])
    } # "c"  "a"  "f"  "\\u00e9"
    output <- paste(output, collapse = "") # "caf\\u00e9"
    return(output)
  }
}
