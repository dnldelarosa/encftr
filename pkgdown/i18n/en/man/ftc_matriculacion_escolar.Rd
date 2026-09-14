\name{ftc_matriculacion_escolar}
\alias{ftc_matriculacion_escolar}
\alias{ftc_compute_matriculacion_escolar}
\title{School enrollment}
\usage{
ftc_matriculacion_escolar(tbl, min_edad = 6, max_edad = 17, summer_fix = FALSE)

ftc_compute_matriculacion_escolar(
  tbl,
  min_edad = 6,
  max_edad = 17,
  summer_fix = FALSE
)
}
\arguments{
\item{tbl}{Local table of person responses with original ENCFT names and codes.}
\item{min_edad}{Inclusive lower age bound in years.}
\item{max_edad}{Inclusive upper age bound in years.}
\item{summer_fix}{Includes coded waiting for classes in June–August.}
}
\description{School enrollment. See the guide's calculation contract and parameters.}
\value{A table with calculated columns, preserving input rows unless household output is requested.}
\details{See \code{vignette("indicadores", package = "encftr")} for populations, units, missing values and executable workflows. Dictionary edition and poverty methodology are independent choices.}
\examples{
  encft <- data.frame(EDAD = c(10, 12, 9), NIVEL_SE_MATRICULO = c(2, 9, 9),
                      MES = c(1, 4, 7), PORQUE_NO_ESTUDIA = c(3, 7, 1))
 ftc_matriculacion_escolar(encft)
 ftc_matriculacion_escolar(encft, summer_fix = TRUE)
}
