testConvert <- function()
    {
        roman <- 'IVXLCDM'
        retval <- romandigit.convert(roman)
        stopifnot(retval==c(1,5,10,50,100,500,1000))
        return(TRUE)
    }

romandigit.convert <- function(roman)
    {
        retval <- .C('convert',
                     roman=as.character(roman),
                     nchar=as.integer(nchar(roman)),
                     values=integer(nchar(roman))
                     )
        retval$values
    }

roman2int.inner <- function(roman)
    {
        results <- .C("roman2int",
                      roman = as.character(roman),
                      nchar = as.integer(nchar(roman)),
                      value = integer(1),

                      PACKAGE="gtools")

        return(results$value)
    }

roman2int <- function(roman)
    {
        roman <- trim(toupper(as.character(roman)))
        retval <- sapply(roman, roman2int.inner)
        retval
    }

