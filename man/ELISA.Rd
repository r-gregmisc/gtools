\name{ELISA}
\docType{data}
\alias{ELISA}
\title{Data from an ELISA assay}
\description{Observed signals and (for some observations) nominal
  concentrations for samples that were aliquoted to multiple assay plates,
  which were read multiple times on multiple days.
}
\usage{
data(ELISA)
}
\format{a data frame with the following columns:
  \itemize{
    \item{PlateDay}{factor. Specifies one of four physically distinct
      96 well plates}
    \item{Read}{factor. The signal was read 3 times for each plate.}
    \item{Description}{character. Indicates contents of sample.}
    \item{Concentration}{numeric. Nominal concentration of standards (NA
      for all other samples).}
    \item{Signal}{numeric. Assay signal. Specifically, optical density
      (a colorimetric assay).}
    }
}
\source{Anonymized data.
}
\keyword{datasets}
