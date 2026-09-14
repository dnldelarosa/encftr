\name{ftc_ing_especie_ocup_sec_cuenta_propia}
\alias{ftc_ing_especie_ocup_sec_cuenta_propia}
\title{In-kind secondary self-employment income}
\usage{
ftc_ing_especie_ocup_sec_cuenta_propia(tbl)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
}
\description{Calculates in-kind secondary self-employment income. Monetary outputs are monthly Dominican pesos; annual receipts are converted to monthly equivalents by the rules below. Missing-value and eligibility conditions are explicit in each case_when expression. Returns a copy with calculated columns and preserves input rows. }
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("pobreza-monetaria", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
NULL
}
