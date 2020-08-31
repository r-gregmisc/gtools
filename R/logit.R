# $Id$



#' Generalized logit and inverse logit function
#' 
#' Compute generalized logit and generalized inverse logit functions.
#' 
#' 
#' The generalized logit function takes values on [min, max] and transforms
#' them to span [-Inf,Inf] it is defined as:
#' 
#' \deqn{y = log(\frac{p}{(1-p)})}{y = log(p/(1-p))}
#' 
#' where
#' 
#' \deqn{p=\frac{(x-min)}{(max-min)}}{p=(x-min)/(max-min)}
#' 
#' The generized inverse logit function provides the inverse transformation:
#' 
#' \deqn{x = p' (max-min) + min}{x = p * (max-min) + min}
#' 
#' where
#' 
#' \deqn{p'=\frac{exp(y)}{(1+exp(y))}}{exp(y)/(1+exp(y))}
#' 
#' @aliases logit inv.logit
#' @param x value(s) to be transformed
#' @param min Lower end of logit interval
#' @param max Upper end of logit interval
#' @return Transformed value(s).
#' @author Gregory R. Warnes \email{greg@@warnes.net}
#' @seealso \code{\link[car]{logit}}
#' @keywords math
#' @examples
#' 
#' 
#'   x <- seq(0,10, by=0.25)
#'   xt <- logit(x, min=0, max=10)
#'   cbind(x,xt)
#' 
#'   y <- inv.logit(xt, min=0, max=10)
#'   cbind(x,xt,y)  
#' 
#' 
logit <- function(x, min=0, max=1)
  {
    p <- (x-min)/(max-min)
    log(p/(1-p))
  }

inv.logit <- function(x, min=0, max=1)
  {
    p <- exp(x)/(1+exp(x))
    p <- ifelse( is.na(p) & !is.na(x), 1, p ) # fix problems with +Inf
    p * (max-min) + min
  }
                 
