#' Provide Name, Version, and Path of Loaded Package Namespaces
#' 
#' Provide name, version, and path of loaded package namespaces
#' 
#' 
#' @param silent Logical indicating whether the results should be printed
#' @return Invisibly returns a data frame containing one row per loaded package
#' namespace, with columns: \item{Package}{Package name} \item{Version}{Version
#' string} \item{Path}{Path to package files} \item{SearchPath}{Either the
#' index of the package namespace in the current search path, or '-' if the
#' package namespace is not in the search path. '1' corresponds to the top of
#' the search path (the first namespace searched for values).  }
#' @author Gregory R. Warnes \email{greg@@warnes.net}
#' @seealso \code{\link[base]{loadedNamespaces}},
#' \code{\link[utils]{packageVersion}}, \code{\link[base]{search}},
#' \code{\link[base]{find.package}}
#' @keywords package
#' @examples
#' 
#'   loadedPackages()
#' 
#' @importFrom utils packageVersion
#' @export
loadedPackages <- function(silent=FALSE)
{
  packageNames    <- loadedNamespaces()
  packageVersions <- sapply(packageNames, function(package) paste(packageVersion(package), sep=".") )
  packagePaths    <- find.package(packageNames)
  inSearchPath    <- match(packageNames, gsub('^package:', '', grep('^package:', search(), value=TRUE)))
  retval <- data.frame(Name=packageNames, Version=packageVersions, Path=packagePaths, SearchPath=inSearchPath)
  retval$SearchPath <- na.replace(retval$SearchPath, '-')
  retval <- retval[order(inSearchPath),]
  if(!silent) print(retval)
  invisible(retval)
}
