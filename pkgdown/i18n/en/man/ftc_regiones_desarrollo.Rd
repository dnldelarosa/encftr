\name{ftc_regiones_desarrollo}
\alias{ftc_regiones_desarrollo}
\alias{ftc_compute_region}
\title{Development regions}
\usage{
ftc_regiones_desarrollo(tbl)

ftc_compute_region(tbl)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
}
\description{Calculates development regions. Returns a copy with calculated columns and preserves input rows. }
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("indicadores", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
\dontrun{
  encft <- ftc_regiones_desarrollo(encft)
}
}
