\name{ftc_sexo_jefe}
\alias{ftc_sexo_jefe}
\title{Sex of the household head}
\usage{
ftc_sexo_jefe(tbl)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
}
\description{Sex of the household head. See the guide's calculation contract and parameters.}
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("indicadores", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
  \dontrun{
    ftc_sexo_jefe(tbl)
 }
}
