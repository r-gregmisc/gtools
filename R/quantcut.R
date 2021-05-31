#' Create a Factor Variable Using the Quantiles of a Continuous Variable
#' 
#' Create a factor variable using the quantiles of a continous variable.
#' 
#' 
#' This function uses \code{\link{quantile}} to obtain the specified quantiles
#' of \code{x}, then calls \code{\link{cut}} to create a factor variable using
#' the intervals specified by these quantiles.
#' 
#' It properly handles cases where more than one quantile obtains the same
#' value, as in the second example below.  Note that in this case, there will
#' be fewer generated factor levels than the specified number of quantile
#' intervals.
#' 
#' @param x Continous variable.
#' @param q Either a integer number of equally spaced quantile groups to
#' create, or a vector of quantiles used for creating groups. Defaults to
#' \code{q=4} which is equivalent to \code{q=seq(0, 1, by=0.25)}.  See
#' \code{\link{quantile}} for details.
#' @param na.rm Boolean indicating whether missing values should be removed
#' when computing quantiles.  Defaults to TRUE.
#' @param \dots Optional arguments passed to \code{\link{cut}}.
#' @return Factor variable with one level for each quantile interval.
#' @author Gregory R. Warnes \email{greg@@warnes.net}
#' @seealso \code{\link{cut}}, \code{\link{quantile}}
#' @keywords manip
#' @examples
#' 
#' 
#'   ## create example data
#'   \testonly{
#'   set.seed(1234)
#'   }
#'   x <- rnorm(1000)
#' 
#'   ## cut into quartiles
#'   quartiles <- quantcut( x )
#'   table(quartiles)
#' 
#'   ## cut into deciles
#'   deciles.1 <- quantcut( x, 10 )
#'   table(deciles.1)
#'   # or equivalently
#'   deciles.2 <- quantcut( x, seq(0,1,by=0.1) )
#' 
#'   \testonly{
#'     stopifnot(identical(deciles.1, deciles.2))
#'   }
#' 
#'   ## show handling of 'tied' quantiles.
#'   x <- round(x)  # discretize to create ties
#'   stem(x)        # display the ties
#'   deciles <- quantcut( x, 10 )
#' 
#'   table(deciles) # note that there are only 5 groups (not 10)
#'                  # due to duplicates
#' 
#' @importFrom stats quantile
#' @export
quantcut <- function(x, q=4, na.rm=TRUE, ... )
  {
    if(length(q)==1)
        q <- seq(0,1, length.out=q+1)

    quant <- quantile(x, q, na.rm=na.rm)
    dups <- duplicated(quant)
    if(any(dups))
      {
        flag <- x %in% unique(quant[dups])
        retval <- ifelse(flag,
                         paste("[",
                               as.character(x),
                               "]",
                               sep=''),
                         NA)
        uniqs <- unique(quant)

        # move cut points over a bit...
        reposition <- function(cut)
                           {
                             flag <- x>=cut
                             if(sum(flag, na.rm=na.rm)==0)
                               return(cut)
                             else
                               return(min(x[flag], na.rm=na.rm))
                           }

        newquant <- sapply(uniqs, reposition)
        retval[!flag] <- as.character(cut(x[!flag],
                                          breaks=newquant,
                                          include.lowest=TRUE,...))

        levs <- unique(retval[order(x)]) # ensure factor levels are
                                         # properly ordered
        retval <- factor(retval, levels=levs)

        ## determine open/closed interval ends
        mkpairs <- function(x) # make table of lower, upper
          sapply(x,
                 function(y) if(length(y)==2) y[c(2,2)] else y[2:3]
                 )
        pairs <- mkpairs(strsplit(levs, '[^0-9+\\.\\-]+'))
        rownames(pairs) <- c("lower.bound","upper.bound")
        colnames(pairs) <- levs

        closed.lower <- rep(F,ncol(pairs)) # default lower is open
        closed.upper <- rep(T,ncol(pairs)) # default upper is closed
        closed.lower[1] <- TRUE             # lowest interval is always closed

        for(i in 2:ncol(pairs))            # open lower interval if above singlet
          if(pairs[1,i]==pairs[1,i-1] && pairs[1,i]==pairs[2,i-1])
            closed.lower[i] <- FALSE

        for(i in 1:(ncol(pairs)-1))        # open upper inteval if below singlet
          if(pairs[2,i]==pairs[1,i+1] && pairs[2,i]==pairs[2,i+1])
            closed.upper[i] <- FALSE

        levs <- ifelse(pairs[1,]==pairs[2,],
                       pairs[1,],
                       paste(ifelse(closed.lower,"[","("),
                             pairs[1,],
                             ",",
                             pairs[2,],
                             ifelse(closed.upper,"]",")"),
                             sep='')
                       )
        levels(retval) <- levs
      }
    else
      retval <- cut( x, quant, include.lowest=TRUE,  ... )
    return(retval)
  }
