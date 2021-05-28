testConvert <- function()
    {
        roman <- 'IVXLCDM'
        retval <- romandigit.convert(roman)
        stopifnot(retval==c(1,5,10,50,100,500,1000))
        return(TRUE)
    }

romandigit.convert <- function(roman)
    {
        retval <- .C(C_convert,
                     roman=as.character(roman),
                     nchar=as.integer(nchar(roman)),
                     values=integer(nchar(roman))
                     )
        retval$values
    }

roman2int.inner <- function(roman)
    {
        results <- .C(C_roman2int,
                      roman = as.character(roman),
                      nchar = as.integer(nchar(roman)),
                      value = integer(1),

                      PACKAGE="gtools")

        return(results$value)
    }



#' Convert Roman Numerals to Integers
#' 
#' Convert roman numerals to integers
#' 
#' This functon will convert roman numerals to integers without the upper bound
#' imposed by R (3899), ignoring case.
#' 
#' @param roman character vector containing roman numerals
#' @return A integer vector with the same length as \code{roman}.  Character
#' strings which are not valid roman numerals will be converted to \code{NA}.
#' @author Gregory R. Warnes \email{greg@@warnes.net}
#' @seealso \code{\link[utils]{as.roman}}
#' @keywords arith
#' @examples
#' 
#' roman2int( c('I', 'V', 'X', 'C', 'L', 'D', 'M' )  )
#' 
#' # works regardless of case
#' roman2int( 'MMXVI' )
#' roman2int( 'mmxvi' )
#' 
#' # works beyond R's limit of 3899
#' val.3899 <- 'MMMDCCCXCIX'
#' val.3900 <- 'MMMCM'
#' val.4000 <- 'MMMM'
#' as.numeric(as.roman( val.3899 ))
#' as.numeric(as.roman( val.3900 ))
#' as.numeric(as.roman( val.4000 ))
#' 
#' roman2int(val.3899)
#' roman2int(val.3900)
#' roman2int(val.4000)
#' 
#' 
roman2int <- function(roman)
    {
        roman <- trimws(toupper(as.character(roman)))

        tryIt <- function(x)
            {
                retval <- try(roman2int.inner(x), silent=TRUE)
                if(is.numeric(retval))
                    retval
                else
                    NA
            }

        retval <- sapply(roman, tryIt)

        retval
    }

