\name{ftc_fuerza_trabajo_potencial}
\alias{ftc_fuerza_trabajo_potencial}
\title{Potential labour force}
\usage{
ftc_fuerza_trabajo_potencial(tbl)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
}
\description{Calculates potential labour force. Returns a copy with calculated columns and preserves input rows. }
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("indicadores", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
\dontrun{
encft <- ftc_fuerza_trabajo_potencial(encft)
}
}
