#' gtools: Various R Programming Tools
#'
#' Functions to assist in R programming, including:
#' \describe{
#' \item{developing, updating, and maintaining R and R packages}{'ask', 'checkRVersion', 'getDependencies', 'keywords', 'scat'}
#' \item{calculate the logit and inverse logit transformations}{'logit', 'inv.logit'}
#' \item{test if a value is missing, empty, contains only NA and NULL values, or is a 'try-error'}{'invalid'}
#' \item{manipulate R's .Last function}{'addLast'}
#' \item{define macros}{'defmacro', 'strmacro'}
#' \item{detect odd and even integers}{'odd', 'even'}
#' \item{convert strings containing non-ASCII characters (like single quotes) to plain ASCII}{'ASCIIfy'}
#' \item{perform a binary search}{'binsearch'}
#' \item{sort strings containing both numeric and character components}{'mixedsort', 'mixedorder'}
#' \item{create a factor variable from the quantiles of a continuous variable}{'quantcut'}
#' \item{enumerate permutations and combinations}{'combinations', 'permutation'}
#' \item{calculate and convert between fold-change and log-ratio}{'foldchange', 'logratio2foldchange', 'foldchange2logratio'}
#' \item{calculate probabilities and generate random numbers from Dirichlet distributions}{'rdirichlet', 'ddirichlet'}
#' \item{apply a function over adjacent subsets of a vector}{'running'}
#' \item{modify the TCP_NODELAY ('de-Nagle') flag for socket objects}{'tcpNoDelay'}
#' \item{efficient 'rbind' of data frames, even if the column names don't match}{'smartbind'}
#' \item{generate significance stars from p-values}{'stars.pval'}
#' \item{convert characters to/from ASCII codes}{asc, chr}
#' \item{convert character vector to ASCII representation}{'ASCIIfy'}
#' \item{apply title capitalization rules to a character vector}{'capwords'}
#' }
#'
#' @docType package
#' @name gtools
#' @useDynLib gtools, .registration=TRUE
#' @keywords internal
"_PACKAGE"
