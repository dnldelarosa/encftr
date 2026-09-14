\name{ftc_set_labels_icv_siuben}
\alias{ftc_set_labels_icv_siuben}
\alias{ftc_use_labels_icv_siuben}
\title{Label historical ICV results}
\usage{
ftc_set_labels_icv_siuben(tbl, vars = NULL)

ftc_use_labels_icv_siuben(tbl, vars = NULL)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
\item{vars}{Selected column names; equivalent to subset in legacy labeling helpers.}
}
\description{Applies labels to selected columns while preserving numeric codes.}
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("icv-siuben", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}

