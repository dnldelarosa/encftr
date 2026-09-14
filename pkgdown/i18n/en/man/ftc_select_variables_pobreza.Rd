\name{ftc_select_variables_pobreza}
\alias{ftc_select_variables_pobreza}
\title{Select recognized poverty questionnaire inputs}
\usage{
ftc_select_variables_pobreza(tbl)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
}
\description{Select recognized poverty questionnaire inputs. See the guide's calculation contract and parameters.}
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("pobreza-monetaria", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}

