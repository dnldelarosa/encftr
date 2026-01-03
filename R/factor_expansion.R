#' Factor de expansión anual
#'   `r lifecycle::badge("stable")`
#'
#' @param tbl [data.frame]: Conexión a base de datos o dataframe con los datos.
#'
#' @return Conexión a base de datos o dataframe según input con la variable
#'   \code{factor_expansion_anual} agregada.
#'
#' @export
#'
#' @examples
#'   encft <- data.frame(FACTOR_EXPANSION = c(1028, 848, 400))
#'   ftc_factor_expansion_anual(encft)
ftc_factor_expansion_anual <- function(tbl){
  FACTOR_EXPANSION <- NULL
  tbl %>%
    dplyr::mutate(
      factor_expansion_anual = FACTOR_EXPANSION/4
    )
}


ftc_factor_exp_anual <- function(tbl) {
  deprecate_warn("0.5.0", "ftc_factor_exp_anual()", "ftc_factor_expansion_anual()")
  ftc_factor_expansion_anual(tbl)
}


#' @rdname ftc_factor_exp_anual
#' @export
ftc_compute_factor_exp_anual <- function(tbl) {
  deprecate_warn("0.5.0", "ftc_compute_factor_exp_anual()", "ftc_factor_exp_anual()")
  ftc_factor_exp_anual(tbl)
}


ftc_factor_expansion_semestre <- function(tbl){
  FACTOR_EXPANSION <- NULL
  tbl %>%
    dplyr::mutate(
      factor_expansion_semestre = FACTOR_EXPANSION/2
    )
}