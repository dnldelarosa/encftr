\name{ftc_factor_exp_anual}
\alias{ftc_factor_exp_anual}
\alias{ftc_compute_factor_exp_anual}
\title{Deprecated annual expansion weight aliases}
\usage{
ftc_factor_exp_anual(tbl, periods = NULL)

ftc_compute_factor_exp_anual(tbl, periods = NULL)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
\item{periods}{Declared divisor: 1–4 annual, 1–2 semester. Omitted uses observed coverage within year.}
}
\description{Deprecated annual expansion weight aliases. See the guide's calculation contract and parameters.}
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("encftr", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}

