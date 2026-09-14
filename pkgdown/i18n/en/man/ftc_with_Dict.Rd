\name{ftc_with_Dict}
\alias{ftc_with_Dict}
\alias{ftc_use_labels}
\alias{ftc_useLabels}
\title{Use ENCFT dictionary labels}
\usage{
ftc_with_Dict(tbl, dict = NULL, subset = NULL, ...)

ftc_use_labels(tbl, dict = encftr::dict, vars = NULL, ...)

ftc_useLabels(tbl, dict = encftr::dict, vars = NULL, ...)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
\item{dict}{Explicit dictionary; NULL uses the selected edition or stored metadata, depending on the function.}
\item{subset}{Column names to label; omitted applies to available matches.}
\item{...}{Additional labeler options: revision metadata or labeling policy, depending on the function.}
\item{vars}{Selected column names; equivalent to subset in legacy labeling helpers.}
}
\description{Use ENCFT dictionary labels. See the guide's calculation contract and parameters.}
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("diccionario", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}

