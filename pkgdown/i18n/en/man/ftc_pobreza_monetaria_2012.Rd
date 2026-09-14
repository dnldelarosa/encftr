\name{ftc_pobreza_monetaria_2012}
\alias{ftc_pobreza_monetaria_2012}
\title{Monetary poverty: 2012 methodology}
\usage{
ftc_pobreza_monetaria_2012(tbl, .keep = FALSE, .reuse = FALSE)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
\item{.keep}{True retains components; a list retains selected names; false removes calculated helper columns.}
\item{.reuse}{In helpers: reuses available or named components. Poverty 2012/2022 accept this option and always recompute.}
}
\description{Calculates nominal and deflated income, urban/rural lines and 2012 classification using reported household size.}
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("pobreza-monetaria", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
\dontrun{
encft <- ftc_pobreza_monetaria_2012(encft)
}
}
