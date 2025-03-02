% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ftc_pobreza_monetaria.R
\name{ftc_pobreza_monetaria}
\alias{ftc_pobreza_monetaria}
\title{Pobreza monetaria por zona de residencia
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}})`}
\usage{
ftc_pobreza_monetaria(tbl, .keep = FALSE, .reuse = FALSE)
}
\arguments{
\item{tbl}{Un data.frame con los datos de la encuesta}

\item{.keep}{Lista de variables intermedias a mantener en el output.
Vea detalles para más información.}

\item{.reuse}{Lista de variables intermedias a reutilizar del input.
Vea detalles para más información.}
}
\value{
los datos del input tbl más las variables:
\itemize{
\item ing_pc_pobreza_def: Ingreso per cápita deflactado (Pobreza Monetaria)
\item ing_total_pobreza_def: Ingreso total deflactado (Pobreza Monetaria)
\item ing_total_pobreza: Ingreso total (Pobreza Monetaria)
\item pobreza_monetaria: Pobreza monetaria
}
}
\description{
Pobreza monetaria por zona de residencia
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}})`
}
\examples{
\dontrun{
tbl <- ftc_pobreza_zona(tbl, conn)
}

}
