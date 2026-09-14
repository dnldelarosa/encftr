\name{ftc_grupo_rama_pib}
\alias{ftc_grupo_rama_pib}
\title{Economic activity groups}
\usage{
ftc_grupo_rama_pib(tbl)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
}
\description{Calculates economic activity groups. Returns a copy with calculated columns and preserves input rows. }
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("indicadores", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
\dontrun{
encft <- ftc_grupo_rama_pib(encft)
}
}
