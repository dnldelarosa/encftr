\name{ftc_prepare_poverty}
\alias{ftc_prepare_poverty}
\title{Prepare poverty questionnaire compatibility fields}
\usage{
ftc_prepare_poverty(tbl)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
}
\description{Prepare poverty questionnaire compatibility fields. See the guide's calculation contract and parameters.}
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("pobreza-monetaria", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}

