\name{ftc_dict}
\alias{ftc_dict}
\title{Select an ENCFT dictionary edition}
\usage{
ftc_dict(version = NULL, at = NULL, con = NULL)
}
\arguments{
\item{version}{Exact edition identifier with no silent fallback. baseline-1 is the only bundled edition.}
\item{at}{ISO applicability date. Requires documented intervals and a unique match.}
\item{con}{Connection to the SQLite revision registry. The caller owns its lifecycle.}
}
\description{Loads a complete verified edition; baseline-1 asserts no historical applicability.}
\value{A complete Dict with verified revision metadata and content integrity.}
\details{See \code{vignette("diccionario", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
diccionario <- ftc_dict()
labeler::dict_revision(diccionario)$version
}
