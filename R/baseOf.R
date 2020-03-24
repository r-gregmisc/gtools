# Transform integer to array of digits in specified base
baseOf <- function(v,
                   base=10,
                   len=1)
{
  if (is.null(v))
    stop("v is null")
  if(length(v)==0)
    return(integer(0))

  if(any(as.integer(v) != v))
    stop("non-integer value(s) provided for v.")

  if (length(v) > 1)
  {
    # this returns a list which may have vectors of varying lenths
    val.list <- lapply(X=v, FUN=baseOf.inner, base=base, len=len)
    longest <- max(sapply(val.list, length))

    # call again, forcing all elements to have the same lenth
    retval  <- t(sapply(X=v, FUN=baseOf.inner, base=base, len=longest))

    # add informative row and column names
    rownames(retval) <- paste0('v.', v)
    colnames(retval) <- paste0('b.', c(0, base^(1: (longest- 1) ) ) )

    retval
  }
  else
    retval <- baseOf.inner(v=v, base=base, len=len)

  retval
}


# Transform integer to array of digits in specified
baseOf.inner <- function(v,
                         base=10,
                         len=1)
{
  if (is.na(v))
    return(rep(NA, len))

  if(v==0)
    return(rep(0, len))

  remainder <- v
  i <- len
  ret <- NULL
  while(remainder > 0 || i >0)
  {
    #print(paste("i=",i," remainder=",remainder))
    m <- remainder%%base
    if (is.null(ret))
    {
      ret <- m
    }
    else
    {
      ret <- c(m,ret)
    }
    remainder  <-  remainder %/% base
    i <- i-1
  }

  if(length(ret)>1)
    names(ret) <-  c(0, base^( 1:(length(ret)- 1 ) ) )

  return(ret)
}



