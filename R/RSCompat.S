# $Id$
#
# $Log$
# Revision 1.9  2005/06/09 14:20:28  nj7w
# Updating the version number, and various help files to synchronize splitting of gregmisc bundle in 4 individual components.
#
# Revision 1.1.1.1  2005/05/25 22:17:28  nj7w
# Initial submision as individual package
#
# Revision 1.8  2003/04/04 13:58:59  warnes
#
# - Replace 'T' with 'TRUE'
#
# Revision 1.7  2003/03/07 15:48:35  warnes
#
# - Minor changes to code to allow the package to be provided as an
#   S-Plus chapter.
#
# Revision 1.6  2003/01/02 15:42:00  warnes
# - Add nlevels function.
#
# Revision 1.5  2002/03/20 03:44:32  warneg
# - Added definition of is.R function.
#
# - Added boxplot.formula
#
# Revision 1.4  2002/02/05 02:20:07  warneg
#
# - Fix typo that caused code meant to run only under S-Plus to run
#   under R, causing problems.
#
# Revision 1.3  2001/12/19 22:45:44  warneg
# - Added code for %in%.
#
# Revision 1.2  2001/09/18 14:15:44  warneg
#
# Release 0.3.2
#
# Revision 1.1  2001/09/01 19:19:13  warneg
#
# Initial checkin.
#
#
# Code necessary for contrast.lm, boxplot.n to work in S-Plus
 
if(!exists("is.R") || !is.R() )
  {
    is.R <- function() FALSE 
    
    getOption <- function(...) options(...)
    
    if(!exists("parent.frame")) parent.frame <- sys.parent
    
    colnames <- function (x, do.NULL = TRUE, prefix = "col") 
      {
        dn <- dimnames(x)
        if (!is.null(dn[[2]])) 
          dn[[2]]
        else {
          if (do.NULL) 
            NULL
          else paste(prefix, seq(length = NCOL(x)), sep = "")
        }
      }
    
    rownames <- function (x, do.NULL = TRUE, prefix = "row") 
      {
        dn <- dimnames(x)
        if (!is.null(dn[[1]])) 
          dn[[1]]
        else {
          if (do.NULL) 
            NULL
          else paste(prefix, seq(length = NROW(x)), sep = "")
        }
      }
    
    "rownames<-" <- function (x, value) 
      {
        dn <- dimnames(x)
        ndn <- names(dn)
        dn <- list(value, if (!is.null(dn)) dn[[2]])
        names(dn) <- ndn
        dimnames(x) <- dn
        x
      }
    
    "colnames<-" <- function (x, value) 
      {
       dn <- dimnames(x)
       ndn <- names(dn)
       dn <- list(if (!is.null(dn)) dn[[1]], value)
       names(dn) <- ndn
       dimnames(x) <- dn
       x
     }
    
   # from the MASS library by Venables & Ripley 
    ginv <- function (X, tol = sqrt(.Machine$double.eps))
      {
        if (length(dim(X)) > 2 || !(is.numeric(X) || is.complex(X)))
          stop("X must be a numeric or complex matrix")
        if (!is.matrix(X))
          X <- as.matrix(X)
        Xsvd <- svd(X)
        if (is.complex(X))
          Xsvd$u <- Conj(Xsvd$u)
        Positive <- Xsvd$d > max(tol * Xsvd$d[1], 0)
        if (all(Positive)) Xsvd$v %*% (1/Xsvd$d * t(Xsvd$u))
        else if (!any(Positive)) array(0, dim(X)[2:1])
        else Xsvd$v[, Positive] %*% ((1/Xsvd$d[Positive]) * t(Xsvd$u[, Positive]))
      }
    
    
    "format.pval" <- 
      function (pv, digits = max(1, getOption("digits") - 2),
                eps = .Machine$double.eps, 
                na.form = "NA") 
        {
          if ((has.na <- any(ina <- is.na(pv)))) 
            pv <- pv[!ina]
          r <- character(length(is0 <- pv < eps))
          if (any(!is0)) {
            rr <- pv <- pv[!is0]
            expo <- floor(log10(pv))
            fixp <- expo >= -3 | (expo == -4 & digits > 1)
            if (any(fixp)) 
              rr[fixp] <- format(pv[fixp], dig = digits)
            if (any(!fixp)) 
              rr[!fixp] <- format(pv[!fixp], dig = digits)
            r[!is0] <- rr
          }
          if (any(is0)) {
            digits <- max(1, digits - 2)
            if (any(!is0)) {
              nc <- max(nchar(rr))
              if (digits > 1 && digits + 6 > nc) 
                digits <- max(1, nc - 7)
              sep <- if (digits == 1 && nc <= 6) 
                ""
              else " "
            }
            else sep <- if (digits == 1) 
              ""
            else " "
            r[is0] <- paste("<", format(eps, digits = digits), sep = sep)
          }
          if (has.na) {
            rok <- r
            r <- character(length(ina))
            r[!ina] <- rok
            r[ina] <- na.form
          }
          r
        }
    
    "%in%" <- function (x, table)  match(x, table, nomatch = 0) > 0
 
    strwidth   <-  function(...)
      {
        par("cin")[1] / par("fin")[1] * (par("usr")[2] - par("usr")[1])
      }
    
    strheight <-  function(...)
      {
        par("cin")[2] / par("fin")[2] * (par("usr")[4] - par("usr")[3])
      }
    
    boxplot.formula <- function(x, data = sys.parent(), ..., ask = TRUE)
      {
        if(!inherits(x, "formula"))
          x <- as.formula(x)
        
        mf <- model.frame(x, data, na.action = function(z) 	z)
        if(length(names(mf)) > 2) 
          stop("boxplot.formula only accepts models with 1 predictor")
        
        resp <- attr(attr(mf, "terms"), "response")
        class(mf) <- NULL
        y <- mf[[resp]]
        x <- mf[[-resp]]
        xlab <- names(mf)[-resp]
        ylab <- names(mf)[resp]
	
        boxplot(split(y, x), xlab = xlab, ylab = ylab, ...) 
      }

    nlevels <- function(x) length(levels(x))

    NULL
    
  }
