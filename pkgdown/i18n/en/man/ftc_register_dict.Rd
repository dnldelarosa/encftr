\name{ftc_register_dict}
\alias{ftc_register_dict}
\title{Register a dictionary edition}
\usage{
ftc_register_dict(con, dict, version, valid_from = NULL, valid_to = NULL, ...)
}
\arguments{
\item{con}{Connection to the SQLite revision registry. The caller owns its lifecycle.}
\item{dict}{Explicit dictionary; NULL uses the selected edition or stored metadata, depending on the function.}
\item{version}{Exact edition identifier with no silent fallback. baseline-1 is the only bundled edition.}
\item{valid_from, valid_to}{Inclusive first ISO applicability date; omitted if unknown. Inclusive last ISO applicability date; omitted if unknown.}
\item{...}{Additional labeler options: revision metadata or labeling policy, depending on the function.}
}
\description{Registers an immutable edition and reuses unchanged definitions.}
\value{A complete Dict with verified revision metadata and content integrity.}
\details{See \code{vignette("diccionario", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}

