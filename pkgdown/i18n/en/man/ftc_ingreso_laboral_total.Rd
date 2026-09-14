\name{ftc_ingreso_laboral_total}
\alias{ftc_ingreso_laboral_total}
\title{Total labour income}
\usage{
ftc_ingreso_laboral_total(tbl)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
}
\description{Calculates total labour income. Returns a copy with calculated columns and preserves input rows. }
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("indicadores", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
\dontrun{
encft <- ftc_ingreso_laboral_total(encft)
}
}
