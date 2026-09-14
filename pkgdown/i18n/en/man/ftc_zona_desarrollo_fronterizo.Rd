\name{ftc_zona_desarrollo_fronterizo}
\alias{ftc_zona_desarrollo_fronterizo}
\title{Border development zone}
\usage{
ftc_zona_desarrollo_fronterizo(tbl)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
}
\description{Calculates border development zone. Returns a copy with calculated columns and preserves input rows. }
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("indicadores", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
\dontrun{
encft <- ftc_zona_desarrollo_fronterizo(encft)
}
}
