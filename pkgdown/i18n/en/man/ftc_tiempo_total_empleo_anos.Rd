\name{ftc_tiempo_total_empleo_anos}
\alias{ftc_tiempo_total_empleo_anos}
\title{Time in employment in years}
\usage{
ftc_tiempo_total_empleo_anos(tbl)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
}
\description{Calculates time in employment in years. Returns a copy with calculated columns and preserves input rows. }
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("indicadores", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
\dontrun{
encft <- ftc_tiempo_total_empleo_anos(encft)
}
}
