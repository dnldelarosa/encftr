\name{ftc_db_connect}
\alias{ftc_db_connect}
\alias{ftc_dbConnect}
\title{Configured R database connection}
\usage{
ftc_db_connect()

ftc_dbConnect()
}
\description{Opens the encft connection configured by Dmisc in the user's R environment.}
\value{The database connection configured by Dmisc.}
\details{See \code{vignette("indicadores", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
\dontrun{
conn <- ftc_db_connect()
}
}
