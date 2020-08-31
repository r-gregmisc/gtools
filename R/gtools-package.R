

#' Dataset That Crashes Base:::Plot.Dendogram with 'Node Stack Overflow'
#' 
#' Base:::Plot.Dendogram() will generate a 'Node Stack Overflow' when run on a
#' dendrogram appropriately constructed from this data set.
#' 
#' 
#' @name badDend
#' @docType data
#' @format The format is: num [1:2047, 1:12] 1 2 3 4 5 6 7 8 9 10 ...  -
#' attr(*, "dimnames")=List of 2 ..$ : NULL ..$ : chr [1:12] "X" "V1" "V2" "V3"
#' ...
#' @note See help page for \code{\link{unByteCode}} to see how to construct the
#' 'bad' dentrogram from this data and how to work around the issue.
#' @keywords datasets
#' @examples
#' 
#' data(badDend)
#' 
NULL





#' Data from an ELISA assay
#' 
#' Observed signals and (for some observations) nominal concentrations for
#' samples that were aliquoted to multiple assay plates, which were read
#' multiple times on multiple days.
#' 
#' 
#' @name ELISA
#' @docType data
#' @format a data frame with the following columns: \itemize{
#' \itemPlateDayfactor. Specifies one of four physically disctinct 96 well
#' plates \itemReadfactor. The signal was read 3 times for each plate.
#' \itemDescriptioncharacter. Indicates contents of sample.
#' \itemConcentrationnumeric. Nominal concentration of standards (NA for all
#' other samples).  \itemSignalnumeric. Assay signal. Specifically, optical
#' density (a colorimetric assay).  }
#' @source Anonymized data.
#' @keywords datasets
NULL





#' Defunct Functions in package \code{gtools}
#' 
#' The functions or variables listed here are no longer part of package
#' \code{gtools}.
#' 
#' \itemize{ \item \code{assert} is a defunct synonym for
#' \code{\link[base]{stopifnot}}.  \item \code{addLast} has been replaced by
#' \code{lastAdd}, which has the same purpose but appled using different
#' syntax.  \item \code{capture} and \code{capture.output} have been removed in
#' favor of \code{capture.output} from the \code{utils} package.  }
#' 
#' @aliases gtools-defunct assert addLast capture sprint
#' @seealso \code{\link[base]{Defunct}}, \code{\link[base]{stopifnot}},
#' \code{\link[gtools]{lastAdd}}, \code{\link[utils]{capture.output}}
#' @keywords misc
NULL





#' Deprecated Functions in the gtools package
#' 
#' These functions are provided for compatibility with older versions of
#' gtools, and may be defunct as soon as the next release.
#' 
#' gtools currently contains no deprecated functions.
#' 
#' % The original help page for these functions is often % available at
#' \code{help("oldName-deprecated")} (note the quotes). % % \itemize{ % \item{}
#' % }
#' 
#' @seealso \code{\link{Deprecated}}
#' @keywords misc
NULL



