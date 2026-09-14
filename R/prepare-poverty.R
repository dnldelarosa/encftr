#' Preparar nombres compatibles con las metodologias de pobreza
#'
#' Resuelve los alias de programas sociales y ayuda anual del cuestionario.
#' Completa solo los campos opcionales documentados por las metodologias; no
#' inventa respuestas para los demas campos. Los componentes auxiliares de una
#' ejecucion anterior se retiran para que los calculos vuelvan a usar las respuestas.
#' @param tbl data.frame con respuestas de personas de la ENCFT.
#' @return Una copia con nombres originales, alias en mayusculas y campos opcionales.
#' @export
ftc_prepare_poverty <- function(tbl) {
  ftc_require_columns(tbl, character())
  if (anyDuplicated(tolower(names(tbl)))) stop("Nombres ambiguos al normalizar mayusculas.", call. = FALSE)
  original <- names(tbl)
  names(tbl) <- tolower(names(tbl))
  tbl <- ftc_pm22_ensure_columns(tbl)
  for (name in setdiff(names(tbl), tolower(original))) original <- c(original, toupper(name))
  names(tbl) <- original
  tbl
}

#' Seleccionar las respuestas necesarias para el calculo de pobreza
#'
#' Selecciona las variables del cuestionario que aparecen en el diccionario
#' empaquetado, mas los alias de compatibilidad y las claves. Los campos ausentes
#' permanecen ausentes para que el calculo informe los requisitos incumplidos.
#' @param tbl data.frame con respuestas de personas de la ENCFT.
#' @return Las columnas de entrada reconocidas, en su orden original.
#' @export
ftc_select_variables_pobreza <- function(tbl) {
  ftc_require_columns(tbl, character())
  vars <- c(names(encftr::dict), names(ftc_pm22_ensure_columns(data.frame())),
    "periodo", "trimestre", "ano", "mes", "vivienda", "hogar", "miembro",
    "gob_programa_superate", "gob_quedate_en_casa", "gob_fondo_asistencia_fase",
    "ayuda_especie_nac_anio", "ayuda_especie_nac_ano_monto")
  tbl[, tolower(names(tbl)) %in% tolower(vars), drop = FALSE]
}
