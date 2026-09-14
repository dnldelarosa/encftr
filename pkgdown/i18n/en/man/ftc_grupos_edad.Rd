\name{ftc_grupos_edad}
\alias{ftc_grupos_edad}
\alias{ftc_compute_grupos_edad}
\title{Age groups}
\usage{
ftc_grupos_edad(tbl, breaks = 10, labels = NULL)

ftc_compute_grupos_edad(tbl, breaks = 10, labels = NULL)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
\item{breaks}{Numeric interval boundaries or number of groups. Use explicit boundaries for cross-language comparisons.}
\item{labels}{Interval labels. Use explicit labels for cross-language comparisons.}
}
\description{Age groups. See the guide's calculation contract and parameters.}
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("indicadores", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
  encft <- data.frame(EDAD = c(0:100))
  ftc_grupos_edad(encft)
}
