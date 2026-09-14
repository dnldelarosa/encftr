\name{ftc_iih}
\alias{ftc_iih}
\title{Household income index (IIH)}
\usage{
ftc_iih(
  tbl,
  vivienda_tbl = NULL,
  return_households = FALSE,
  include_details = FALSE,
  filter_valid_households = FALSE
)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
\item{vivienda_tbl}{Optional dwelling table; fills wall text by PERIODO and VIVIENDA.}
\item{return_households}{True returns one row per household; false attaches results to each person.}
\item{include_details}{Includes model components and diagnostics. ICV refreshes existing components even when false.}
\item{filter_valid_households}{Retains households with hconmissing zero; person output preserves rows and leaves filtered scores missing.}
}
\description{Estimates income and IIH category using fixed household models; this is distinct from observed monetary poverty.}
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("iih", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
\dontrun{
encft_iih <- ftc_iih(encft)
hogares_iih <- ftc_iih(encft, return_households = TRUE)
}
}
