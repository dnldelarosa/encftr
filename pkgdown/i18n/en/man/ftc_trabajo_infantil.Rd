\name{ftc_trabajo_infantil}
\alias{ftc_trabajo_infantil}
\alias{ftc_compute_trabajo_infantil}
\title{Child labour}
\usage{
ftc_trabajo_infantil(tbl, summer_fix = FALSE)

ftc_compute_trabajo_infantil(...)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
\item{summer_fix}{Includes coded waiting for classes in June–August.}
\item{...}{Additional labeler options: revision metadata or labeling policy, depending on the function.}
}
\description{Child labour. See the guide's calculation contract and parameters.}
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("indicadores", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
\dontrun{
encft <- ftc_trabajo_infantil(encft)
}
}
