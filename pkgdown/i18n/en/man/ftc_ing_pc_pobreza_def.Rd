\name{ftc_ing_pc_pobreza_def}
\alias{ftc_ing_pc_pobreza_def}
\title{Legacy deflated per-capita household income}
\usage{
ftc_ing_pc_pobreza_def(tbl, .keep = FALSE, .reuse = FALSE)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
\item{.keep}{True retains components; a list retains selected names; false removes calculated helper columns.}
\item{.reuse}{In helpers: reuses available or named components. Poverty 2012/2022 accept this option and always recompute.}
}
\description{Legacy deflated per-capita household income. See the guide's calculation contract and parameters.}
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("pobreza-monetaria", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
\dontrun{
ftc_ing_pc_pobreza_def(encft)
}
}
