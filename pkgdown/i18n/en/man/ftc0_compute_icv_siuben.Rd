\name{ftc0_compute_icv_siuben}
\alias{ftc0_compute_icv_siuben}
\alias{ftc0_setLabels}
\alias{ftc0_setLabels_icv_global}
\alias{ftc0_useLabels}
\title{Compatibility with encftr0}
\usage{
ftc0_compute_icv_siuben(tbl)

ftc0_setLabels(tbl, vars = NULL)

ftc0_setLabels_icv_global(tbl)

ftc0_useLabels(tbl, vars = NULL)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
\item{vars}{Selected column names; equivalent to subset in legacy labeling helpers.}
}
\description{Reproduces ICV from encftr0 0.0.2.9002 for complete households with one head. Preserves rows and adds category, score and method. Does not certify current SIUBEN methodology. See the guide for missing-value rules, keys, components and migration.}
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("icv-siuben", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}

