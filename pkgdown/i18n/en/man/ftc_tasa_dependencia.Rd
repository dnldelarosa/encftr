\name{ftc_tasa_dependencia}
\alias{ftc_tasa_dependencia}
\alias{ftc_compute_tasa_dependencia}
\title{Household dependency ratio}
\usage{
ftc_tasa_dependencia(
  tbl,
  min_edad = 15,
  max_edad = 64,
  limit = c("both", "above", "below"),
  breaks = NULL,
  labels = NULL
)

ftc_compute_tasa_dependencia(
  tbl,
  min_edad = 15,
  max_edad = 64,
  limit = c("both", "above", "below"),
  breaks = NULL,
  labels = NULL
)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
\item{min_edad}{Inclusive lower age bound in years.}
\item{max_edad}{Inclusive upper age bound in years.}
\item{limit}{both, above or below: all, older or younger dependents.}
\item{breaks}{Numeric interval boundaries or number of groups. Use explicit boundaries for cross-language comparisons.}
\item{labels}{Interval labels. Use explicit labels for cross-language comparisons.}
}
\description{Household dependency ratio. See the guide's calculation contract and parameters.}
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("indicadores", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
\dontrun{
encft <- ftc_tasa_dependencia(encft)
}
}
