#' Split a File Path into Components
#'
#' @description This function converts a character scalar containing a
#'  \emph{valid} file path into a character vector of path components
#'  (e.g. directories).
#'
#' @param x            character scalar.  Path to be processed.
#' @param depth_first  logical.  Should path be returned depth first?  Defaults
#'   to \code{TRUE}.
#'
#' @return Character vector of path components, depth first.
#'
#' @export
#'
split_path <- function(x, depth_first = TRUE) {
  if (length(x) > 1) {
    warning(
      "This function is not vectorized.",
      "Only processing the first element of x."
    )
  }
  retval <- split_path_inner(x)
  if (!depth_first) {
    retval <- rev(retval)
  }
  retval[retval > ""]
}


split_path_inner <- function(path) {
  if (dirname(path) %in% c(".", path)) {
    return(basename(path))
  }
  return(c(basename(path), split_path_inner(dirname(path))))
}
