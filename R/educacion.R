#' Tasa de alfabetización del hogar
#'
#'   `r lifecycle::badge("experimental")`
#'
#'   Calcula la tasa de alfabetización del hogar para los miembros con edades
#'   comprendidas entre \code{min_edad} y \code{max_edad}.
#'
#' @param tbl [data.frame]: Conexión a base de datos o data.frame.
#' @param min_edad [numeric]: edad mínima del grupo para el que se quiere calcular
#'   la tasa de alfabetización
#' @param max_edad [numeric]: edad máxima del grupo para el que se quiere calcular
#'   la tasa de alfabetización
#'
#' @return el data.frame del input \code{tbl} con la variable \code{tasa_alfabetizacion_hogar} agregada
#'
#' @export
#'
#' @examples
#' \dontrun{
#' encft <- ftc_tasa_alfabetizacion_hogar(encft)
#' }
ftc_tasa_alfabetizacion_hogar <- function(tbl, min_edad = 0, max_edad = Inf) {
  TRIMESTRE <- NULL
  VIVIENDA <- NULL
  HOGAR <- NULL
  alfabetizacion <- NULL
  if (min_edad > max_edad) {
    stop(glue::glue(
      "ftc_compute_tasa_alfabetizacion_hogar: Edad m\u00EDnima ({min_edad}) mayor que edad m\u00E1xima ({max_edad})"
    ))
  }
  tbl %>%
    ftc_alfabetizacion(min_edad = min_edad, max_edad = max_edad) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(ftc_household_keys(tbl)))) %>%
    dplyr::mutate(
      tasa_alfabetizacion_hogar = sum(alfabetizacion == 1, na.rm = T) /
        sum(!is.na(alfabetizacion), na.rm = T) *
        100
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-alfabetizacion)
}

# make a function rename
#' @rdname ftc_tasa_alfabetizacion_hogar
#' @export
ftc_compute_tasa_alfabetizacion_hogar <- function(
  tbl,
  min_edad = 0,
  max_edad = Inf
) {
  deprecate_warn(
    "0.5.0",
    "ftc_compute_tasa_alfabetizacion_hogar()",
    "ftc_tasa_alfabetizacion_hogar()"
  )
  ftc_tasa_alfabetizacion_hogar(tbl, min_edad, max_edad)
}


#' Sobreedad escolar
#'
#'    Alumnos que tienen más edad que la considerada teóricamente ideal, según
#'    las normas legales de acceso al sistema educativo con respecto al año que cursa.
#'
#' @seealso El Ministerio de Educación de la República Dominicana en el Anuario
#'   de indicadores educativos 2017-2018 define la tasa de sobreedad como
#'   Matriculados al inicio del año escolar en cada grado cuya edad es dos o más
#'   años superiores a la edad legal o normal para el grado, como porcentaje del
#'   total de matriculados en dicho grado.
#'
#' @param tbl [data.frame]: Conexión a base de datos o data.frame con los datos
#' @param nrezagos [numeric]: Número de años de rezago a considerar como sobreedad.
#'   Vea \code{Seealso} para más detalles.
#'
#' @return [data.frame]: Los datos suministrados en el argumento \code{tbl} con
#'   la variable \code{sobreedad_escolar} adicionada.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   encft <- ftc_sobreedad_escolar(encft)
#' }
ftc_sobreedad_escolar <- function(tbl, nrezagos = 2) {
  tbl %>%
    ftc_anos_educacion() %>%
    dplyr::mutate(
      sobreedad_escolar = dplyr::case_when(
        dplyr::between(NIVEL_SE_MATRICULO, 2, 4) &
          EDAD >= anos_educacion + 6 + nrezagos ~ 1,
        dplyr::between(NIVEL_SE_MATRICULO, 2, 4) ~ 0
      )
    )
}


# make a function rename
#' @rdname ftc_sobreedad_escolar
#' @export
ftc_compute_sobreedad_escolar <- function(tbl, nrezagos = 2) {
  deprecate_warn(
    "0.5.0",
    "ftc_compute_sobreedad_escolar()",
    "ftc_sobreedad_escolar()"
  )
  ftc_sobreedad_escolar(tbl, nrezagos)
}


#' Matriculación escolar
#'
#'   `r lifecycle::badge("experimental")`
#'
#'   Calcula la matriculación escolar de los miembros del hogar entre \code{min_edad}
#'   y \code{max_edad}.
#'
#' @param tbl [data.frame]: Conexión a base de datos o data.frame con los datos.
#' @param min_edad [numeric]: edad mínima del grupo para el que se desea calcular
#'   la matriculación escolar
#' @param max_edad  [numeric]: edad máxima del grupo para el que se desea calcular
#'   la matriculación escolar
#' @param summer_fix [logical]: Indica si se ajusta por las personas que están
#'   esperando el inicio de un nuevo año escolar.
#'
#' @return Los datos suministrados en el input \code{tbl} con la variable
#'   \code{matriculacion_escolar} adicionada.
#'
#' @export
#'
#' @examples
#'   encft <- data.frame(EDAD = c(10, 12, 9), NIVEL_SE_MATRICULO = c(2, 9, 9),
#'                       MES = c(1, 4, 7), PORQUE_NO_ESTUDIA = c(3, 7, 1))
#'  ftc_matriculacion_escolar(encft)
#'  ftc_matriculacion_escolar(encft, summer_fix = TRUE)
ftc_matriculacion_escolar <- function(
  tbl,
  min_edad = 6,
  max_edad = 17,
  summer_fix = FALSE
) {
  EDAD <- NULL
  NIVEL_SE_MATRICULO <- NULL
  PORQUE_NO_ESTUDIA <- NULL
  if (min_edad > max_edad) {
    stop(glue::glue(
      "ftc_matriculacion_escolar: Edad m\u00EDnima ({min_edad}) mayor que edad m\u00E1xima ({max_edad})"
    ))
  }
  if (min_edad < 3) {
    warning(
      "ftc_matriculacion_escolar: La encuesta solo recoge informaci\u00F3n educativa para miembros mayores de 2 a\u00F1os."
    )
  }
  tbl %>%
    dplyr::mutate(
      matriculacion_escolar = dplyr::case_when(
        dplyr::between(EDAD, min_edad, max_edad) &
          dplyr::between(NIVEL_SE_MATRICULO, 1, 8) ~ 1,
        dplyr::between(EDAD, min_edad, max_edad) &
          dplyr::between(MES, 6, 8) &
          PORQUE_NO_ESTUDIA == 1 ~ dplyr::if_else(summer_fix, 1, 0),
        dplyr::between(EDAD, min_edad, max_edad) ~ 0
      )
    )
}


#' @rdname ftc_matriculacion_escolar
#' @export
ftc_compute_matriculacion_escolar <- function(
  tbl,
  min_edad = 6,
  max_edad = 17,
  summer_fix = FALSE
) {
  deprecate_warn(
    "0.5.0",
    "ftc_compute_matriculacion_escolar_summer_fix()",
    "ftc_matriculacion_escolar()"
  )
  ftc_matriculacion_escolar(tbl, min_edad, max_edad, summer_fix = summer_fix)
}


#' Asistencia escolar
#'
#'   `r lifecycle::badge("experimental")`
#'
#'   Calcula la asistencia escolar de los miembros del hogar entre \code{min_edad}
#'   y \code{max_edad}.
#'
#' @param tbl [data.frame]: Conexión a base de datos o data.frame con los datos.
#' @param min_edad  [numeric]: edad mínima del grupo para el que se desea calcular
#'   la asistencia escolar
#' @param max_edad  [numeric]: edad máxima del grupo para el que se desea calcular
#'   la asistencia escolar
#' @param summer_fix [logical]: Indica si se ajusta por las personas que están
#'   esperando el inicio de un nuevo año escolar.
#'
#' @return Los datos suministrados en el input \code{tbl} con la variable
#'   \code{asistencia_escolar} adicionada.
#'
#' @export
#'
#' @examples
#'   encft <- data.frame(EDAD = c(10, 12, 9), TANDA_ASISTE = c(2, 9, 7),
#'                       MES = c(1, 4, 7), PORQUE_NO_ESTUDIA = c(3, 7, 1))
#'  ftc_asistencia_escolar(encft)
#'  ftc_asistencia_escolar(encft, summer_fix = TRUE)
ftc_asistencia_escolar <- function(
  tbl,
  min_edad = 6,
  max_edad = 17,
  summer_fix = FALSE
) {
  EDAD <- NULL
  TANDA_ASISTE <- NULL
  MES <- NULL
  PORQUE_NO_ESTUDIA <- NULL
  if (min_edad > max_edad) {
    stop(glue::glue(
      "ftc_compute_asistencia_escolar: Edad m\u00EDnima ({min_edad}) mayor que edad m\u00E1xima ({max_edad})"
    ))
  }
  if (min_edad < 3) {
    warning(
      "ftc_compute_asistencia_escolar: La encuesta solo recoge informaci\u00F3n educativa para miembros mayores de 2 a\u00F1os."
    )
  }
  tbl %>%
    dplyr::mutate(
      asistencia_escolar = dplyr::case_when(
        dplyr::between(EDAD, min_edad, max_edad) & TANDA_ASISTE != 7 ~ 1,
        dplyr::between(EDAD, min_edad, max_edad) &
          dplyr::between(MES, 6, 8) &
          PORQUE_NO_ESTUDIA == 1 ~ dplyr::if_else(summer_fix, 1, 0),
        dplyr::between(EDAD, min_edad, max_edad) ~ 0
      )
    )
}


#' @rdname ftc_asistencia_escolar
#' @export
ftc_compute_asistencia_escolar <- function(
  tbl,
  min_edad = 6,
  max_edad = 17,
  summer_fix = FALSE
) {
  deprecate_warn(
    "0.5.0",
    "ftc_compute_asistencia_escolar()",
    "ftc_asistencia_escolar()"
  )
  ftc_asistencia_escolar(tbl, min_edad, max_edad, summer_fix)
}


#' Años de educación
#'
#'   `r lifecycle::badge("experimental")`
#'
#'   Calcula los años de educación aprobados a partir del nivel y el último
#'   año aprobado. Permite armonizar la base de secundaria entre la estructura
#'   6-6, la estructura histórica 8-4 o una regla por año de corte.
#'
#' @param tbl una conexión a base de datos o data.frame con los datos
#' @param breaks Puntos de corte para la variable. Vea \code{See also} para más detalles.
#' @param labels [character]: Nombres para los grupos de la variable.  Vea \code{See also} para más detalles.
#' @param secundaria_base [character]: Base usada para calcular los años de
#'   secundaria. Puede ser \code{"armonizada_6_6"} para asumir primaria de 6
#'   años, \code{"legacy_8_4"} para asumir primaria de 8 años o
#'   \code{"historica_por_ano"} para decidir según \code{ANO} y
#'   \code{anio_corte} (útil si tienes datos que mezclan los sistemas).
#' @param anio_corte [numeric]: Año a partir del cual
#'   \code{secundaria_base = "historica_por_ano"} usa la estructura 6-6. Para
#'   años anteriores usa la estructura 8-4.
#'
#' @return [data.frame]: Los datos suministrados en el argumento \code{tbl} con
#'   la variable \code{anos_educacion} adicionada.
#'
#' @seealso
#'    Vea \code{Dmisc::\link[Dmisc:cut3]{cut3}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   encft <- ftc_anos_educacion(encft)
#' }
ftc_anos_educacion <- function(
  tbl,
  breaks = NULL,
  labels = NULL,
  secundaria_base = c(
    "armonizada_6_6",
    "historica_por_ano",
    "legacy_8_4"
  ),
  anio_corte = 2022L
) {
  secundaria_base <- match.arg(secundaria_base)

  if (secundaria_base == "armonizada_6_6") {
    tbl <- tbl %>%
      dplyr::mutate(
        anos_educacion = dplyr::case_when(
          NIVEL_ULTIMO_ANO_APROBADO %in% c(0L, 1L, 9L, 10L) ~ 0L,
          NIVEL_ULTIMO_ANO_APROBADO == 2L ~ as.integer(ULTIMO_ANO_APROBADO),
          NIVEL_ULTIMO_ANO_APROBADO %in% c(3L, 4L) ~ 6L +
            as.integer(ULTIMO_ANO_APROBADO),
          NIVEL_ULTIMO_ANO_APROBADO == 5L ~ 12L +
            as.integer(ULTIMO_ANO_APROBADO),
          NIVEL_ULTIMO_ANO_APROBADO %in% c(6L, 7L, 8L) ~ 16L +
            as.integer(ULTIMO_ANO_APROBADO),
          TRUE ~ NA_integer_
        )
      )
  }

  if (secundaria_base == "legacy_8_4") {
    tbl <- tbl %>%
      dplyr::mutate(
        anos_educacion = dplyr::case_when(
          NIVEL_ULTIMO_ANO_APROBADO %in% c(0L, 1L, 9L, 10L) ~ 0L,
          NIVEL_ULTIMO_ANO_APROBADO == 2L ~ as.integer(ULTIMO_ANO_APROBADO),
          NIVEL_ULTIMO_ANO_APROBADO %in% c(3L, 4L) ~ 8L +
            as.integer(ULTIMO_ANO_APROBADO),
          NIVEL_ULTIMO_ANO_APROBADO == 5L ~ 12L +
            as.integer(ULTIMO_ANO_APROBADO),
          NIVEL_ULTIMO_ANO_APROBADO %in% c(6L, 7L, 8L) ~ 16L +
            as.integer(ULTIMO_ANO_APROBADO),
          TRUE ~ NA_integer_
        )
      )
  }

  if (secundaria_base == "historica_por_ano") {
    tbl <- tbl %>%
      dplyr::mutate(
        anos_educacion = dplyr::case_when(
          NIVEL_ULTIMO_ANO_APROBADO %in% c(0L, 1L, 9L, 10L) ~ 0L,
          NIVEL_ULTIMO_ANO_APROBADO == 2L ~ as.integer(ULTIMO_ANO_APROBADO),
          NIVEL_ULTIMO_ANO_APROBADO %in% c(3L, 4L) ~
            dplyr::if_else(ANO >= anio_corte, 6L, 8L) +
            as.integer(ULTIMO_ANO_APROBADO),
          NIVEL_ULTIMO_ANO_APROBADO == 5L ~ 12L +
            as.integer(ULTIMO_ANO_APROBADO),
          NIVEL_ULTIMO_ANO_APROBADO %in% c(6L, 7L, 8L) ~ 16L +
            as.integer(ULTIMO_ANO_APROBADO),
          TRUE ~ NA_integer_
        )
      )
  }

  if (is.null(breaks)) {
    tbl
  } else {
    Dmisc::cut3(tbl, "anos_educacion", breaks, labels = labels)
  }
}


#' @rdname ftc_anos_educacion
#' @export
ftc_compute_anos_educacion <- function(tbl, breaks = NULL, labels = NULL) {
  deprecate_warn(
    "0.5.0",
    "ftc_compute_anos_educacion()",
    "ftc_anos_educacion()"
  )
  ftc_anos_educacion(tbl, breaks, labels)
}


#' Alfabetización de la población
#'
#'   `r lifecycle::badge("experimental")`
#'
#'   Calcula la alfabetización de la población con edades
#'   comprendidas entre \code{min_edad} y \code{max_edad}.
#'
#' @param tbl [data.frame]: Conexión a base de datos o data.frame.
#' @param min_edad [numeric]: edad mínima del grupo para el que se quiere calcular
#'   la alfabetización
#' @param max_edad [numeric]: edad máxima del grupo para el que se quiere calcular
#'   la alfabetización
#'
#' @return el data.frame del input \code{tbl} con la variable \code{alfabetizacion} agregada
#'
#' @export
#'
#' @examples
#' \dontrun{
#' encft <- ftc_alfabetizacion(encft)
#' }
ftc_alfabetizacion <- function(tbl, min_edad = 0, max_edad = Inf) {
  SABE_LEER_ESCRIBIR <- NULL
  EDAD <- NULL
  if (min_edad > max_edad) {
    stop(glue::glue(
      "ftc_compute_alfabetizacion: Edad m\u00EDnima ({min_edad}) mayor que edad m\u00E1xima ({max_edad})"
    ))
  }
  tbl %>%
    dplyr::mutate(
      alfabetizacion = dplyr::case_when(
        SABE_LEER_ESCRIBIR == 1 & dplyr::between(EDAD, min_edad, max_edad) ~ 1,
        dplyr::between(EDAD, min_edad, max_edad) ~ 0
      )
    )
}


#' @rdname ftc_alfabetizacion
#' @export
ftc_compute_alfabetizacion <- function(tbl, min_edad = 0, max_edad = Inf) {
  deprecate_warn(
    "0.5.0",
    "ftc_compute_alfabetizacion()",
    "ftc_alfabetizacion()"
  )
  ftc_alfabetizacion(tbl, min_edad, max_edad)
}


ftc_grado_asiste <- function(tbl) {
  tbl %>%
    ftc_anos_educacion() %>%
    dplyr::mutate(
      grado_asiste = dplyr::if_else(
        NIVEL_SE_MATRICULO < 9,
        anos_educacion + 1,
        NA_integer_
      )
    )
}


ftc_nivel_educativo_completado <- function(tbl, primaria = 6) {
  tbl %>%
    ftc_anos_educacion() %>%
    dplyr::mutate(
      nivel_educativo_completado = dplyr::case_when(
        anos_educacion < primaria ~ 1,
        anos_educacion < 12 ~ 2,
        anos_educacion < 16 ~ 3,
        anos_educacion == 16 ~ 4,
        anos_educacion > 16 ~ 5
      )
    )
}


ftc_nivel_educativo_alcanzado <- function(tbl, primaria = 6) {
  tbl %>%
    ftc_anos_educacion() %>%
    dplyr::mutate(
      nivel_educativo_alcanzado = dplyr::case_when(
        anos_educacion == 0 ~ 1,
        anos_educacion <= primaria ~ 2,
        anos_educacion <= 12 ~ 3,
        NIVEL_ULTIMO_ANO_APROBADO == 5 ~ 4,
        NIVEL_ULTIMO_ANO_APROBADO %in% 6:8 ~ 5
      )
    )
}
