\name{ftc_sobreedad_escolar}
\alias{ftc_sobreedad_escolar}
\alias{ftc_compute_sobreedad_escolar}
\title{School overage}
\usage{
ftc_sobreedad_escolar(tbl, nrezagos = 2)

ftc_compute_sobreedad_escolar(tbl, nrezagos = 2)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
\item{nrezagos}{Years of delay beyond expected age based on completed schooling plus six.}
}
\description{School overage. See the guide's calculation contract and parameters.}
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("indicadores", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
\dontrun{
  encft <- ftc_sobreedad_escolar(encft)
}
}
