
#' Calcula el sexo del jefe del hogar
#' 
#'   `r lifecycle::badge("experimental")`
#'
#' @param tbl datos de la base de datos de la ENCFT
#'
#' @return lo mismo que el parámetro tbl pero con una nueva columna `sexo_jefe`
#' @export
#'
#' @examples
#'   \dontrun{
#'     ftc_sexo_jefe(tbl)
#'  }
ftc_sexo_jefe <- function(tbl){
    keys <- ftc_household_keys(tbl, period = "PERIODO")
    heads <- tbl[which(tbl$PARENTESCO == 1), keys, drop = FALSE]
    if (anyDuplicated(heads)) stop("Mas de un jefe para el mismo hogar.", call. = FALSE)
    PERIODO <- NULL
    VIVIENDA <- NULL
    HOGAR <- NULL
    PARENTESCO <- NULL
    sexo_jefe <- NULL
    tbl %>%
    dplyr::mutate(
        sexo_jefe = dplyr::case_when(PARENTESCO == 1 ~ SEXO)
    ) %>%
    dplyr::group_by(PERIODO, VIVIENDA, HOGAR) %>%
    dplyr::mutate(
        sexo_jefe = if (all(is.na(sexo_jefe))) NA_real_ else min(sexo_jefe, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
}
