\name{ftc_set_Dict}
\alias{ftc_set_Dict}
\alias{ftc_set_labels}
\alias{ftc_setLabels}
\title{Apply ENCFT dictionary metadata}
\usage{
ftc_set_Dict(
  tbl,
  dict = NULL,
  subset = NULL,
  ...,
  version = NULL,
  at = NULL,
  con = NULL
)

ftc_set_labels(tbl, dict = encftr::dict, vars = NULL)

ftc_setLabels(tbl, vars = NULL)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
\item{dict}{Explicit dictionary; NULL uses the selected edition or stored metadata, depending on the function.}
\item{subset}{Column names to label; omitted applies to available matches.}
\item{...}{Additional labeler options: revision metadata or labeling policy, depending on the function.}
\item{version, at, con}{Exact edition identifier with no silent fallback. baseline-1 is the only bundled edition. ISO applicability date. Requires documented intervals and a unique match. Connection to the SQLite revision registry. The caller owns its lifecycle.}
\item{vars}{Selected column names; equivalent to subset in legacy labeling helpers.}
}
\description{Apply ENCFT dictionary metadata. See the guide's calculation contract and parameters.}
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("diccionario", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}

