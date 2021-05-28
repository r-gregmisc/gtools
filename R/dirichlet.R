#' Functions for the Dirichlet Distribution
#' 
#' Functions to compute the density of or generate random deviates from the
#' Dirichlet distribution.
#' 
#' The Dirichlet distribution is the multidimensional generalization of the
#' beta distribution.  It is the canonical Bayesian distribution for the
#' parameter estimates of a multinomial distribution.
#' 
#' @aliases rdirichlet ddirichlet
#' @param x A vector containing a single random deviate or matrix containg one
#' random deviate per row.
#' @param n Number of random vectors to generate.
#' @param alpha Vector or (for \code{ddirichlet}) matrix containing shape
#' parameters.
#' @return \code{ddirichlet} returns a vector containing the Dirichlet density
#' for the corresponding rows of \code{x}.
#' 
#' \code{rdirichlet} returns a matrix with \code{n} rows, each containing a
#' single Dirichlet random deviate.
#' @author Code original posted by Ben Bolker to R-News on Fri Dec 15 2000. See
#' \url{https://stat.ethz.ch/pipermail/r-help/2000-December/009561.html}.  Ben
#' attributed the code to Ian Wilson \email{i.wilson@@maths.abdn.ac.uk}.
#' Subsequent modifications by Gregory R. Warnes \email{greg@@warnes.net}.
#' @seealso \code{\link{dbeta}}, \code{\link{rbeta}}
#' @keywords distribution
#' @examples
#' 
#' 
#'   x <- rdirichlet(20, c(1,1,1) )
#' 
#'   ddirichlet(x, c(1,1,1) )
#' 
#' @name dirichlet
NULL

#' @describeIn dirichlet  Dirichlet distribution function.
#' @export
ddirichlet<-function(x,alpha)
{

  dirichlet1 <- function(x, alpha)
    {
      logD <- sum(lgamma(alpha)) - lgamma(sum(alpha))
      s <-(alpha-1)*log(x)
      s <- ifelse(alpha==1 & x==0, -Inf, s)
      exp(sum(s)-logD)
    }

  # make sure x is a matrix
  if(!is.matrix(x))
    if(is.data.frame(x))
      x <- as.matrix(x)
    else
      x <- t(x)

  if(!is.matrix(alpha))
    alpha <- matrix( alpha, ncol=length(alpha), nrow=nrow(x), byrow=TRUE)

  if( any(dim(x) != dim(alpha)) )
    stop("Mismatch between dimensions of 'x' and 'alpha'.")

  pd <- vector(length=nrow(x))
  for(i in 1:nrow(x))
    pd[i] <- dirichlet1(x[i,],alpha[i,])

  # Enforce 0 <= x[i,j] <= 1, sum(x[i,]) = 1
  pd[ apply( x, 1, function(z) any( z <0 | z > 1)) ] <- 0
  pd[ apply( x, 1, function(z) all.equal(sum( z ),1) !=TRUE) ] <- 0
  pd
}

# @describeIn dirichlet Generate dirichlet random values.
# @export
rdirichlet<-function(n,alpha)
## generate n random deviates from the Dirichlet function with shape
## parameter alpha
{
    l<-length(alpha);
    x<-matrix(rgamma(l*n,alpha),ncol=l,byrow=TRUE);
    sm<-x%*%rep(1,l);
    x/as.vector(sm);
}

