\name{ftc_factor_expansion_anual}
\alias{ftc_factor_expansion_anual}
\alias{ftc_factor_expansion_semestre}
\title{Annual expansion weights}
\usage{
ftc_factor_expansion_anual(tbl, periods = NULL)

ftc_factor_expansion_semestre(tbl, periods = NULL)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
\item{periods}{Declared divisor: 1–4 annual, 1–2 semester. Omitted uses observed coverage within year.}
}
\description{Annual expansion weights. See the guide's calculation contract and parameters.}
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("encftr", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
x <- data.frame(TRIMESTRE = c(20231, 20232, 20241),
                FACTOR_EXPANSION = c(100, 200, 300))
ftc_factor_expansion_anual(x)
ftc_factor_expansion_anual(data.frame(FACTOR_EXPANSION = 100), periods = 4)
}
