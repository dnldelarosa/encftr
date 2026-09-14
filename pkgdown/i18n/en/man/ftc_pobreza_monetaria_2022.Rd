\name{ftc_pobreza_monetaria_2022}
\alias{ftc_pobreza_monetaria_2022}
\title{Monetary poverty: 2022 methodology}
\usage{
ftc_pobreza_monetaria_2022(tbl, .keep = FALSE, .reuse = FALSE)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
\item{.keep}{True retains components; a list retains selected names; false removes calculated helper columns.}
\item{.reuse}{In helpers: reuses available or named components. Poverty 2012/2022 accept this option and always recompute.}
}
\description{Calculates nominal and deflated income, regional lines and 2022 classification using observed household members.}
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("pobreza-monetaria", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
\dontrun{
encft <- ftc_pobreza_monetaria_2022(encft)
}
}
