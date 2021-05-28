##
## Function to do rbind of data frames quickly, even if the columns don't match
##



#' Efficient rbind of data frames, even if the column names don't match
#' 
#' Efficient rbind of data frames, even if the column names don't match
#' 
#' 
#' @param \dots Data frames to combine
#' @param list List containing data frames to combine
#' @param fill Value to use when 'filling' missing columns.  Defaults to
#' \code{NA}.
#' @param sep Character string used to separate column names when pasting them
#' together.
#' @param verbose Logical flag indicating whether to display processing
#' messages. Defaults to \code{FALSE}.
#' @return The returned data frame will contain: \item{columns}{all columns
#' present in any provided data frame} \item{rows}{a set of rows from each
#' provided data frame, with values in columns not present in the given data
#' frame filled with missing (\code{NA}) values.} The data type of columns will
#' be preserved, as long as all data frames with a given column name agree on
#' the data type of that column.  If the data frames disagree, the column will
#' be converted into a character strings.  The user will need to coerce such
#' character columns into an appropriate type.
#' @author Gregory R. Warnes \email{greg@@warnes.net}
#' @seealso \code{\link{rbind}}, \code{\link{cbind}}
#' @keywords manip
#' @examples
#' 
#' 
#'   df1 <- data.frame(A=1:10, B=LETTERS[1:10], C=rnorm(10) )
#'   df2 <- data.frame(A=11:20, D=rnorm(10), E=letters[1:10] )
#' 
#'   # rbind would fail
#' \dontrun{
#'   rbind(df1, df2)
#'   # Error in match.names(clabs, names(xi)) : names do not match previous
#'   # names:
#'   #	D, E
#' }
#'   # but smartbind combines them, appropriately creating NA entries
#'   smartbind(df1, df2)
#' 
#'   # specify fill=0 to put 0 into the missing row entries
#'   smartbind(df1, df2, fill=0)
#' 
#' \dontshow{
#'   n=10 # number of data frames to create
#'   s=10 # number of rows in each data frame
#' 
#'   # create a bunch of column names
#'   names <- LETTERS[2:5]
#' 
#'   # create a list 'Z' containing 'n' data frames, each with 3 columns
#'   # and 's' rows.  The first column is always named 'A', but the other
#'   # two have a names randomly selected from 'names'
#' 
#'   Z <- list()
#'   for(i in 1:n)
#'     {
#'       X <- data.frame(A=sample(letters,s,replace=TRUE),
#'                       B=letters[1:s],
#'                       C=rnorm(s) )
#'       colnames(X) <- c("A",sample(names,2,replace=FALSE))
#'       Z[[i]] <- X
#'     }
#' 
#'   # Error in match.names(clabs, names(xi)) : names do not match
#'   # previous names: E
#' 
#'   # But smartbind will 'do the right thing'
#'   df <- do.call("smartbind",Z)
#'   df
#' 
#'   # Equivalent call:
#'   df <- smartbind(list=Z)
#' 
#' }
#' 
smartbind <- function(..., list, fill=NA, sep=':', verbose=FALSE)
  {
    data <- base::list(...)
    if(!missing(list))
      {
        data <- modifyList(list, data)
      }
    data <- data[!sapply(data, function(l) is.null(l) | ncol(l)==0 | nrow(l)==0)]
    

    defaultNames <- seq.int(length(data))

    if(is.null(names(data)))
      names(data) <- defaultNames

    emptyNames <- names(data)==""
    if (any(emptyNames) )
      names(data)[emptyNames] <- defaultNames[emptyNames]

    data <- lapply(data,
                   function(x)
                   if(is.matrix(x) || is.data.frame(x))
                     x
                   else
                     data.frame(as.list(x), check.names=FALSE)
                   )
    
    #retval <- new.env()
    retval <- base::list()
    rowLens <- unlist(lapply(data, nrow))
    nrows <- sum(rowLens)

    rowNameList <- unlist(lapply( names(data),
                                 function(x)
                                   if(rowLens[x]<=1) x
                                   else paste(x, seq(1,rowLens[x]),sep=sep))
                          )

    colClassList     <- vector(mode="list", length=length(data))
    factorColumnList <- vector(mode="list", length=length(data))
    factorLevelList  <- vector(mode="list", length=length(data))


    start      <- 1
    blockIndex <- 1
    for(block in data)
      {
        colClassList    [[blockIndex]] <- base::list()
        factorColumnList[[blockIndex]] <- character(length=0)
        factorLevelList [[blockIndex]] <- base::list()

        if(verbose) print(head(block))
        end <- start+nrow(block)-1
        for(col in colnames(block))
          {
            classVec <- class(block[,col])

            ## store class and factor level information for later use
            colClassList[[blockIndex]][[col]] <- classVec
            if("factor" %in% classVec)
              {

                factorColumnList[[blockIndex]] <-
                  c(factorColumnList[[blockIndex]], col)

                factorLevelList[[blockIndex]][[col]] <-
                  levels(block[,col])
            }

            if(verbose) cat("Start:", start,
                            "  End:", end,
                            "  Column:", col,
                            "\n", sep="")

            if ("factor" %in% classVec)
              {
                newclass <- "character"
              }
            else
              newclass <- classVec[1]

            ## Coerce everything that isn't a native type to character
            if(! (newclass %in% c("logical", "integer", "numeric",
                                 "complex", "character", "raw") ))
                {
                    newclass <- "character"
                    warning("Converting non-atomic type column '", col,
                            "' to type character.")
                }

            if(! (col %in% names(retval) ) )
              retval[[col]] <- as.vector(rep(fill,nrows), mode=newclass)

            ## Handle case when current and previous native types differ
            oldclass <- class(retval[[col]])

            if(oldclass != newclass)
            {
              # handle conversions in case of conflicts
              #   numeric vs integer --> numeric
              #   complex vs numeric or integer --> complex
              #   anything else:  --> character
              if(oldclass %in% c("integer", "numeric") && newclass %in% c("integer", "numeric") )
                class(retval[[col]]) <- mode <- "numeric"
              else if(oldclass=="complex" && newclass %in% c("integer", "numeric") )
                class(retval[[col]]) <- mode <- "complex"
              else if(oldclass %in% c("integer", "numeric") && newclass=="complex")
                class(retval[[col]]) <- mode <- "complex"
              else
                {
                  class(retval[[col]]) <- mode <- "character"
                  warning("Column class mismatch for '", col, "'. ",
                          "Converting column to class 'character'.")
                }
            }
            else
              mode <- oldclass

            if(mode=="character")
                vals <- as.character(block[,col])
            else
                vals <- block[,col]

            retval[[col]][start:end] <- as.vector(vals, mode=mode)
          }
        start <- end+1
        blockIndex <- blockIndex+1
      }

    all.equal.or.null <- function(x,y)
      {
        if(is.null(x) || is.null(y) )
          return(TRUE)
        else
          return(all.equal(x,y))
      }

    ## Handle factors, merging levels
    for( col in unique(unlist(factorColumnList)) )
      {
        ## Ensure column classes match across blocks
        colClasses <- lapply(colClassList, function(x) x[[col]])
        firstNotNull <- which(!sapply(colClasses, is.null))[1]
        allSameOrNull <- all(sapply(colClasses[-firstNotNull],
                              function(x) isTRUE(all.equal.or.null(colClasses[[firstNotNull]], x))
                              )
                       )

        if(allSameOrNull)
          {
            # grab the first *non-NULL* class information
            colClass <- colClasses[[firstNotNull]]
          }
        else
          {
            warning("Column class mismatch for '", col, "'. ",
                    "Converting column to class 'character'.")
            next()
          }


        ## check if factor levels are all the same
        colLevels <- lapply(factorLevelList, function(x) x[[col]])
        firstNotNull <- which(!sapply(colLevels, is.null))[1]
        allSameOrNull <- all(sapply(colLevels[-firstNotNull],
                                    function(x) isTRUE(all.equal.or.null(colLevels[[firstNotNull]], x))
                                    )
                             )


        if(allSameOrNull)
          {
            if("ordered" %in% colClass)
              retval[[col]] <- ordered(retval[[col]], levels=colLevels[[firstNotNull]] )
            else
              retval[[col]] <- factor(retval[[col]], levels=colLevels[[firstNotNull]] )
          }
        else
          {
            ## Check if longest set of levels is a superset of all others,
            ## and use that one
            longestIndex  <- which.max( sapply(colLevels, length) )
            longestLevels <- colLevels[[longestIndex]]
            allSubset <- all(sapply(colLevels[-longestIndex],
                                function(l) all(l %in% longestLevels)
                                ))
            if(allSubset)
              {
                if("ordered" %in% colClass)
                  retval[[col]] <- ordered(retval[[col]], levels=longestLevels )
                else
                  retval[[col]] <- factor(retval[[col]], levels=longestLevels )
              }
            else
              {
                # form superset by appending to longest level set
                levelSuperSet <- unique(c(longestLevels, unlist(colLevels)))
                retval[[col]] <- factor(retval[[col]], levels=levelSuperSet )

                if(length(colClass)>1) # not just plain factor
                   {
                    warning( "column '", col, "' of class ",
                            paste("'", colClass, "'", collapse=":",
                                  sep="'"),
                            " converted to class 'factor'. Check level ordering." )
                  }

              }
          }
      }

    attr(retval,"row.names") <- rowNameList
    class(retval) <- "data.frame"
    return(retval)
  }

