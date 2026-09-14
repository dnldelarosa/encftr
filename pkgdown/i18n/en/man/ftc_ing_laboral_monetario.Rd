\name{ftc_ing_laboral_monetario}
\alias{ftc_ing_laboral_monetario}
\title{Monetary labour income}
\usage{
ftc_ing_laboral_monetario(tbl, deflactar = TRUE, .keep = FALSE, .reuse = FALSE)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
\item{deflactar}{Applies the component deflator when true; preserves its nominal output.}
\item{.keep}{True retains components; a list retains selected names; false removes calculated helper columns.}
\item{.reuse}{In helpers: reuses available or named components. Poverty 2012/2022 accept this option and always recompute.}
}
\description{Calculates monetary labour income. Monetary outputs are monthly Dominican pesos; annual receipts are converted to monthly equivalents by the rules below. Missing-value and eligibility conditions are explicit in each case_when expression. Returns a copy with calculated columns and preserves input rows. }
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("pobreza-monetaria", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
NULL
}
