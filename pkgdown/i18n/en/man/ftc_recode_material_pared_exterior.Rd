\name{ftc_recode_material_pared_exterior}
\alias{ftc_recode_material_pared_exterior}
\title{Recode exterior wall material}
\usage{
ftc_recode_material_pared_exterior(tbl)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
}
\description{Calculates recode exterior wall material. Returns a copy with calculated columns and preserves input rows. }
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("indicadores", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
\dontrun{
  encft <- ftc_recode_material_pared_exterior(encft)
}
}
