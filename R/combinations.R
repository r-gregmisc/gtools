#' Enumerate the Combinations or Permutations of the Elements of a Vector
#' 
#' \code{combinations} enumerates the possible combinations of a specified size
#' from the elements of a vector.  \code{permutations} enumerates the possible
#' permutations.
#' 
#' Caution: The number of combinations and permutations increases rapidly with
#' \code{n} and \code{r}!.
#' 
#' To use values of \code{n} above about 45, you will need to increase R's
#' recursion limit.  See the \code{expression} argument to the \code{options}
#' command for details on how to do this.
#' 
#' @aliases combinations permutations
#' @param n Size of the source vector
#' @param r Size of the target vectors
#' @param v Source vector. Defaults to \code{1:n}
#' @param set Logical flag indicating whether duplicates should be removed from
#' the source vector \code{v}. Defaults to \code{TRUE}.
#' @param repeats.allowed Logical flag indicating whether the constructed
#' vectors may include duplicated values.  Defaults to \code{FALSE}.
#' @return Returns a matrix where each row contains a vector of length
#' \code{r}.
#' @author Original versions by Bill Venables
#' \email{Bill.Venables@@cmis.csiro.au}.  Extended to handle
#' \code{repeats.allowed} by Gregory R. Warnes \email{greg@@warnes.net}.
#' @seealso \code{\link[base]{choose}}, \code{\link[base]{options}}
#' @references Venables, Bill.  "Programmers Note", R-News, Vol 1/1, Jan. 2001.
#' \url{http://cran.r-project.org/doc/Rnews/}
#' @keywords manip
#' 
#' @details 
#' 
#' Taken from an email by Brian D Ripley <ripley@stats.ox.ac.uk> to r-help
#' dated Tue, 14 Dec 1999 11:14:04 +0000 (GMT) in response to
#' Alex Ahgarin <datamanagement@email.com>.  Original version was
#' named "subsets" and was Written by Bill Venables.  
#'
#' @examples
#' 
#' combinations(3,2,letters[1:3])
#' combinations(3,2,letters[1:3],repeats=TRUE)
#' 
#' permutations(3,2,letters[1:3])
#' permutations(3,2,letters[1:3],repeats=TRUE)
#' 
#' # To use large 'n', you need to change the default recusion limit
#' options(expressions=1e5)
#' cmat <- combinations(300,2)
#' dim(cmat) # 44850 by 2 
#' 
#' @export
combinations <- function(n, r, v = 1:n, set = TRUE, repeats.allowed=FALSE)
{
  if(mode(n) != "numeric" || length(n) != 1 
     || n < 1 || (n %% 1) != 0) stop("bad value of n") 
  if(mode(r) != "numeric" || length(r) != 1 
     || r < 1 || (r %% 1) != 0) stop("bad value of r") 
  if(!is.atomic(v) || length(v) < n) 
    stop("v is either non-atomic or too short")
  if( (r > n) & repeats.allowed==FALSE)
    stop("r > n and repeats.allowed=FALSE")
  if(set) {
    v <- unique(sort(v))
    if (length(v) < n) stop("too few different elements")
  }
  v0 <- vector(mode(v), 0)
  ## Inner workhorse
  if(repeats.allowed)
    sub <- function(n, r, v)
      { 
        if(r == 0) v0 else
        if(r == 1) matrix(v, n, 1) else
        if(n == 1) matrix(v, 1, r) else
        rbind( cbind(v[1], Recall(n, r-1, v)),
              Recall(n-1, r, v[-1]))
      }
  else
    sub <- function(n, r, v)
      { 
        if(r == 0) v0 else
        if(r == 1) matrix(v, n, 1) else
        if(r == n) matrix(v, 1, n) else
        rbind(cbind(v[1], Recall(n-1, r-1, v[-1])),
              Recall(n-1, r, v[-1]))
      }
  sub(n, r, v[1:n])
}

##
## Original version by Bill Venables and cited by by Matthew
## Wiener (mcw@ln.nimh.nih.gov) in an email to R-help dated
## Tue, 14 Dec 1999 09:11:32 -0500 (EST) in response to
## Alex Ahgarin <datamanagement@email.com>
##
##

#' @rdname combinations
#' @export
permutations <- function(n, r, v = 1:n, set = TRUE, repeats.allowed=FALSE)
{
  if(mode(n) != "numeric" || length(n) != 1 
     || n < 1 || (n %% 1) != 0) stop("bad value of n") 
  if(mode(r) != "numeric" || length(r) != 1 
     || r < 1 || (r %% 1) != 0) stop("bad value of r") 
  if(!is.atomic(v) || length(v) < n) 
    stop("v is either non-atomic or too short")
  if( (r > n) & repeats.allowed==FALSE)
    stop("r > n and repeats.allowed=FALSE")
  if(set) {
    v <- unique(sort(v))
    if (length(v) < n) stop("too few different elements")
  }
  v0 <- vector(mode(v), 0)
  ## Inner workhorse
  if(repeats.allowed)
    sub <- function(n, r, v)
      {
        if(r==1) matrix(v,n,1) else
        if(n==1) matrix(v,1,r) else
        {
          inner  <-  Recall(n, r-1, v)
          cbind( rep( v, rep(nrow(inner),n)  ),
                 matrix( t(inner), ncol=ncol(inner), nrow=nrow(inner) * n ,
                        byrow=TRUE )
                )
        }
      }
  else
    sub <- function(n, r, v)
      {
        if(r==1) matrix(v,n,1) else
        if(n==1) matrix(v,1,r) else
        {
        X  <-  NULL
        for(i in 1:n)
          X  <-  rbind( X, cbind( v[i], Recall(n-1, r - 1, v[-i])))
        X
        }
      }

  sub(n, r, v[1:n])
}
