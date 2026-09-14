\name{ftc_ing_dividendos}
\alias{ftc_ing_dividendos}
\title{Employment dividends}
\usage{
ftc_ing_dividendos(tbl)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
}
\description{Calculates employment dividends. Monetary outputs are monthly Dominican pesos; annual receipts are converted to monthly equivalents by the rules below. Missing-value and eligibility conditions are explicit in each case_when expression. Returns a copy with calculated columns and preserves input rows. }
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("pobreza-monetaria", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
\dontrun{
encft <- ftc_ing_dividendos(encft)
}
}
