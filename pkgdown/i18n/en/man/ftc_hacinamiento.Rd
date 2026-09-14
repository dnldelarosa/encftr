\name{ftc_hacinamiento}
\alias{ftc_hacinamiento}
\alias{ftc_compute_hacinamiento}
\title{People per dwelling bedroom}
\usage{
ftc_hacinamiento(tbl, breaks = NULL, labels = NULL)

ftc_compute_hacinamiento(tbl, breaks = NULL, labels = NULL)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
\item{breaks}{Numeric interval boundaries or number of groups. Use explicit boundaries for cross-language comparisons.}
\item{labels}{Interval labels. Use explicit labels for cross-language comparisons.}
}
\description{People per dwelling bedroom. See the guide's calculation contract and parameters.}
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("indicadores", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
\dontrun{
encft <- ftc_hacinamiento(encft)
}
}
