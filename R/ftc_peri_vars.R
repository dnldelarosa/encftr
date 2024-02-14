ftc_peri_vars <- function(tbl, .trimestre = TRUE, .fecha = FALSE, rm = FALSE, .round = "end") {
  TRIMESTRE <- NULL
  nombres <- dplyr::tbl_vars(tbl)
  ano <- FALSE
  if("ano" %in% nombres){
    ano <- TRUE
  }
  if("fecha" %in% nombres){
    tbl <- dplyr::select(tbl, -"fecha")
    .fecha <- TRUE
  }
  if("trimestre" %in% nombres){
    tbl <- dplyr::select(tbl, -"trimestre")
    .trimestre <- TRUE
  }

  peri_vars <- tbl %>%
    dplyr::ungroup() %>%
    dplyr::select(
      TRIMESTRE
    ) %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::mutate(
      ano = stringr::str_sub(as.character(TRIMESTRE), end = -2),
      trimestre = stringr::str_sub(as.character(TRIMESTRE), -1)
    )

  if (.fecha) {
    peri_vars <- peri_vars %>%
      Dmisc::vars_to_date(year = "ano", quarter = "trimestre", drop_vars = FALSE, .round = .round) %>%
      dplyr::rename(fecha = date) %>%
      dplyr::relocate(fecha, .after = "TRIMESTRE")
  }

  if (!ano) {
    peri_vars <- peri_vars %>%
      dplyr::select(-"ano")
  }

  tbl <- tbl %>%
    dplyr::left_join(
      peri_vars,
      by = "TRIMESTRE",
      copy = TRUE
    )

  if (.fecha) {
    tbl <- tbl %>%
      dplyr::relocate(fecha, .after = "TRIMESTRE")
  }

  if (.trimestre) {
    tbl <- tbl %>%
      dplyr::relocate(trimestre, .after = "TRIMESTRE")
  }

  if (ano) {
    tbl <- tbl %>%
      dplyr::relocate(ano, .after = "TRIMESTRE")
  }

  if (rm) {
    tbl <- tbl %>%
      dplyr::select(-"TRIMESTRE")
  }
  tbl
}
ftc_peri_vars <- function(tbl, .trimestre = TRUE, .fecha = FALSE, rm = FALSE, .round = "start") {
  TRIMESTRE <- NULL
  nombres <- dplyr::tbl_vars(tbl)
  ano <- FALSE
  if("ano" %in% nombres){
    ano <- TRUE
  }
  if("fecha" %in% nombres){
    tbl <- dplyr::select(tbl, -"fecha")
    .fecha <- TRUE
  }
  if("trimestre" %in% nombres){
    tbl <- dplyr::select(tbl, -"trimestre")
    .trimestre <- TRUE
  }

  peri_vars <- tbl %>%
    dplyr::ungroup() %>%
    dplyr::select(
      TRIMESTRE
    ) %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::mutate(
      ano = stringr::str_sub(as.character(TRIMESTRE), end = -2),
      trimestre = stringr::str_sub(as.character(TRIMESTRE), -1)
    )

  if (.fecha) {
    peri_vars <- peri_vars %>%
      Dmisc::vars_to_date(year = "ano", quarter = "trimestre", drop_vars = FALSE, .round = .round) %>%
      dplyr::rename(fecha = date) %>%
      dplyr::relocate(fecha, .after = "TRIMESTRE")
  }

  if (!ano) {
    peri_vars <- peri_vars %>%
      dplyr::select(-"ano")
  }

  tbl <- tbl %>%
    dplyr::left_join(
      peri_vars,
      by = "TRIMESTRE",
      copy = TRUE
    )

  if (.fecha) {
    tbl <- tbl %>%
      dplyr::relocate(fecha, .after = "TRIMESTRE")
  }

  if (.trimestre) {
    tbl <- tbl %>%
      dplyr::relocate(trimestre, .after = "TRIMESTRE")
  }

  if (ano) {
    tbl <- tbl %>%
      dplyr::relocate(ano, .after = "TRIMESTRE")
  }

  if (rm) {
    tbl <- tbl %>%
      dplyr::select(-"TRIMESTRE")
  }
  tbl
}
