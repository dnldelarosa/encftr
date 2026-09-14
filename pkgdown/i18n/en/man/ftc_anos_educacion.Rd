\name{ftc_anos_educacion}
\alias{ftc_anos_educacion}
\alias{ftc_compute_anos_educacion}
\title{Years of schooling}
\usage{
ftc_anos_educacion(
  tbl,
  breaks = NULL,
  labels = NULL,
  secundaria_base = c("armonizada_6_6", "historica_por_ano", "legacy_8_4"),
  anio_corte = 2022L
)

ftc_compute_anos_educacion(tbl, breaks = NULL, labels = NULL)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
\item{breaks}{Numeric interval boundaries or number of groups. Use explicit boundaries for cross-language comparisons.}
\item{labels}{Interval labels. Use explicit labels for cross-language comparisons.}
\item{secundaria_base}{armonizada_6_6, legacy_8_4 or historica_por_ano. Sets the offset before secondary grade.}
\item{anio_corte}{First year with a six-year secondary offset for historica_por_ano.}
}
\description{Years of schooling. See the guide's calculation contract and parameters.}
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("indicadores", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
\dontrun{
  encft <- ftc_anos_educacion(encft)
}
}
