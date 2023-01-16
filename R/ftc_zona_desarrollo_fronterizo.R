#' Zona Especial de Desarrollo Fronterizo de la República Dominicana
#' `r lifecycle::badge('stable')`
#'
#' Vea Ley 28-01 de la República Dominicana.
#'
#' @param tbl [data.frame] con datos de la base de datos
#'
#' @return Los datos suministrados en el input \code{tbl} con la variable
#'  \code{zona_desarrollo_fronterizo} adicionada.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' encft <- ftc_zona_desarrollo_fronterizo(encft)
#' }
ftc_zona_desarrollo_fronterizo <- function(tbl) {
  tbl %>%
    ftc_regiones_desarrollo() %>%
    dplyr::mutate(
      zona_desarrollo_fronterizo = dplyr::case_when(
        ID_PROVINCIA %in% c(16, 10, 7, 5, 15, 26, 3) ~ 1,
        TRUE ~ 0
      )
    )
}