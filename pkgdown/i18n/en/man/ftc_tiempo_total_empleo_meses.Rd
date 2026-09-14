\name{ftc_tiempo_total_empleo_meses}
\alias{ftc_tiempo_total_empleo_meses}
\title{Time in employment in months}
\usage{
ftc_tiempo_total_empleo_meses(tbl)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
}
\description{Calculates time in employment in months. Returns a copy with calculated columns and preserves input rows. }
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("indicadores", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
\dontrun{
encft <- ftc_tiempo_total_empleo_meses(encft)
}
}
