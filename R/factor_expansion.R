#' Factores de expansion para periodos agrupados
#'
#' Promedia las ponderaciones trimestrales sobre los trimestres observados por
#' ano (o por ano y semestre). No convierte un subconjunto de trimestres en una
#' estimacion representativa del ano completo. Calcule el factor antes de filtrar
#' personas. `periods` permite declarar explicitamente el divisor de un extracto
#' cuya cobertura ya se conoce.
#'
#' @param tbl Un data.frame de miembros de la ENCFT.
#' @param periods Divisor entero opcional entre 1 y 4 (anual) o 1 y 2 (semestral).
#'   Si es NULL, requiere TRIMESTRE como AAAAT o como 1:4 junto con ANO.
#' @return Los datos originales con `factor_expansion_anual` o
#'   `factor_expansion_semestre`, conservando el orden de las filas.
#' @export
#' @examples
#' x <- data.frame(TRIMESTRE = c(20231, 20232, 20241),
#'                 FACTOR_EXPANSION = c(100, 200, 300))
#' ftc_factor_expansion_anual(x)
#' ftc_factor_expansion_anual(data.frame(FACTOR_EXPANSION = 100), periods = 4)
ftc_factor_expansion_anual <- function(tbl, periods = NULL) {
  ftc_expansion(tbl, periods, semester = FALSE)
}

#' @rdname ftc_factor_expansion_anual
#' @export
ftc_factor_expansion_semestre <- function(tbl, periods = NULL) {
  ftc_expansion(tbl, periods, semester = TRUE)
}

ftc_expansion <- function(tbl, periods, semester) {
  ftc_require_columns(tbl, "FACTOR_EXPANSION")
  weights <- tbl$FACTOR_EXPANSION
  if (!is.numeric(weights) || any(!is.finite(weights) | weights < 0, na.rm = TRUE)) {
    stop("FACTOR_EXPANSION debe ser numerico, finito y no negativo (NA permitido).", call. = FALSE)
  }
  max_periods <- if (semester) 2L else 4L
  if (!is.null(periods)) {
    if (!is.numeric(periods) || length(periods) != 1L || is.na(periods) ||
        periods != trunc(periods) || periods < 1 || periods > max_periods) {
      stop("periods debe ser un entero entre 1 y ", max_periods, ".", call. = FALSE)
    }
    divisor <- periods
  } else {
    coverage <- ftc_quarter_parts(tbl)
    key <- if (semester) paste(coverage$year, (coverage$quarter - 1L) %/% 2L) else coverage$year
    divisor <- stats::ave(coverage$quarter, key, FUN = function(x) length(unique(x)))
  }
  name <- if (semester) "factor_expansion_semestre" else "factor_expansion_anual"
  tbl[[name]] <- weights / divisor
  tbl
}

ftc_require_columns <- function(tbl, required) {
  if (!is.data.frame(tbl)) stop("Se requiere un data.frame local; use collect() para tablas remotas.", call. = FALSE)
  if (anyDuplicated(names(tbl))) stop("No se permiten nombres de columnas duplicados.", call. = FALSE)
  missing <- setdiff(required, names(tbl))
  if (length(missing)) stop("Faltan columnas requeridas: ", paste(missing, collapse = ", "), call. = FALSE)
  invisible(tbl)
}

ftc_quarter_parts <- function(tbl) {
  ftc_require_columns(tbl, "TRIMESTRE")
  q <- suppressWarnings(as.numeric(as.character(tbl$TRIMESTRE)))
  encoded <- !is.na(q) & q >= 10001
  year <- ifelse(encoded, q %/% 10, NA_real_)
  quarter <- ifelse(encoded, q %% 10, q)
  if ("ANO" %in% names(tbl)) {
    supplied <- suppressWarnings(as.numeric(as.character(tbl$ANO)))
    if (any(encoded & (is.na(supplied) | year != supplied), na.rm = TRUE)) stop("ANO y TRIMESTRE no coinciden.", call. = FALSE)
    year[!encoded] <- supplied[!encoded]
  }
  if (any(!is.finite(year) | year != trunc(year) | year < 1000 | year > 9999 |
          !is.finite(q) | q != trunc(q) | !quarter %in% 1:4)) {
    stop("TRIMESTRE debe ser AAAAT, o 1:4 con ANO; no se permiten periodos faltantes.", call. = FALSE)
  }
  list(year = year, quarter = quarter)
}

ftc_household_keys <- function(tbl, dwelling = FALSE, period = "TRIMESTRE") {
  keys <- c(period, "VIVIENDA", if (!dwelling) "HOGAR")
  if (period == "TRIMESTRE" && "ANO" %in% names(tbl)) keys <- c("ANO", keys)
  ftc_require_columns(tbl, keys)
  if (anyNA(tbl[keys])) stop("Los identificadores de periodo y hogar no pueden faltar.", call. = FALSE)
  keys
}

#' Alias anteriores del factor anual
#' @inheritParams ftc_factor_expansion_anual
#' @inherit ftc_factor_expansion_anual return
#' @export
ftc_factor_exp_anual <- function(tbl, periods = NULL) {
  lifecycle::deprecate_warn("0.5.0", "ftc_factor_exp_anual()", "ftc_factor_expansion_anual()")
  ftc_factor_expansion_anual(tbl, periods = periods)
}

#' @rdname ftc_factor_exp_anual
#' @export
ftc_compute_factor_exp_anual <- function(tbl, periods = NULL) {
  lifecycle::deprecate_warn("0.5.0", "ftc_compute_factor_exp_anual()", "ftc_factor_expansion_anual()")
  ftc_factor_expansion_anual(tbl, periods = periods)
}
