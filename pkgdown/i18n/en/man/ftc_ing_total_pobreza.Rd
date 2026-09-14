\name{ftc_ing_total_pobreza}
\alias{ftc_ing_total_pobreza}
\title{Legacy household income composition}
\usage{
ftc_ing_total_pobreza(tbl, .keep = FALSE, .reuse = FALSE)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
\item{.keep}{True retains components; a list retains selected names; false removes calculated helper columns.}
\item{.reuse}{In helpers: reuses available or named components. Poverty 2012/2022 accept this option and always recompute.}
}
\description{Composes legacy helpers into nominal and deflated household income. Use the explicit 2012 or 2022 function for an official methodology workflow.}
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("pobreza-monetaria", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
NULL
}
