\name{ftc_icv_siuben}
\alias{ftc_icv_siuben}
\alias{ftc_compute_icv_siuben}
\title{Historical ICV SIUBEN}
\usage{
ftc_icv_siuben(tbl, include_details = TRUE, method = "encftr0-0.0.2.9002")

ftc_compute_icv_siuben(
  tbl,
  include_details = TRUE,
  method = "encftr0-0.0.2.9002"
)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
\item{include_details}{Includes model components and diagnostics. ICV refreshes existing components even when false.}
\item{method}{Exact historical identifier: encftr0-0.0.2.9002.}
}
\description{Reproduces ICV from encftr0 0.0.2.9002 for complete households with one head. Preserves rows and adds category, score and method. Does not certify current SIUBEN methodology. See the guide for missing-value rules, keys, components and migration.}
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("icv-siuben", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
x <- jsonlite::fromJSON(system.file("examples", "synthetic-icv.json", package = "encftr"))
y <- ftc_icv_siuben(x)
head(y[c("icv_global", "icv_puntaje", "icv_metodo")])
}
