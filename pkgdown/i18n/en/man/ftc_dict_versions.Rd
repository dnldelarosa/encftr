\name{ftc_dict_versions}
\alias{ftc_dict_versions}
\title{Available dictionary editions}
\usage{
ftc_dict_versions(con = NULL)
}
\arguments{
\item{con}{Connection to the SQLite revision registry. The caller owns its lifecycle.}
}
\description{Returns a table of available edition metadata, intervals and hashes.}
\value{A data frame of edition identifiers, applicability intervals and content hashes.}
\details{See \code{vignette("diccionario", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}

