\name{ftc_alfabetizacion}
\alias{ftc_alfabetizacion}
\alias{ftc_compute_alfabetizacion}
\title{Literacy}
\usage{
ftc_alfabetizacion(tbl, min_edad = 0, max_edad = Inf)

ftc_compute_alfabetizacion(tbl, min_edad = 0, max_edad = Inf)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
\item{min_edad}{Inclusive lower age bound in years.}
\item{max_edad}{Inclusive upper age bound in years.}
}
\description{Literacy. See the guide's calculation contract and parameters.}
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("indicadores", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
\dontrun{
encft <- ftc_alfabetizacion(encft)
}
}
