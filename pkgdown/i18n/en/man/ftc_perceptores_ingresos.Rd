\name{ftc_perceptores_ingresos}
\alias{ftc_perceptores_ingresos}
\title{Income recipients}
\usage{
ftc_perceptores_ingresos(tbl, min_edad = 15)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
\item{min_edad}{Inclusive lower age bound in years.}
}
\description{Calculates income recipients. Returns a copy with calculated columns and preserves input rows. }
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("indicadores", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
\dontrun{
encft <- ftc_perceptores_ingresos(encft)
}
}
