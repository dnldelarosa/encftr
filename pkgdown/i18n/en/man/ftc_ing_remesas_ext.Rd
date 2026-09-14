\name{ftc_ing_remesas_ext}
\alias{ftc_ing_remesas_ext}
\title{Monthly foreign remittances}
\usage{
ftc_ing_remesas_ext(tbl)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
}
\description{Sums three remittance streams over six months using their respective monthly rates, then divides by six. Missing components contribute zero.}
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("pobreza-monetaria", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
NULL
}
