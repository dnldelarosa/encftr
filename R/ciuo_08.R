ftc_grandes_grupos_ciuo_08 <- function(tbl){
    tbl |>
        dplyr::mutate(
            grandes_grupos_ciuo_08 = stringr::str_sub(
                stringr::str_pad(OCUPACION_PRINCIPAL_COD, 4, pad = "0"),
                1, 1
            )
        )
}

ftc_subgrupos_principales_ciuo_08 <- function(tbl){
    tbl |>
        dplyr::mutate(
            subgrupos_principales_ciuo_08 = stringr::str_sub(
                stringr::str_pad(OCUPACION_PRINCIPAL_COD, 4, pad = "0"),
                1, 2
            )
        )
}

ftc_subgrupos_ciuo_08 <- function(tbl){
    tbl |>
        dplyr::mutate(
            subgrupos_ciuo_08 = stringr::str_sub(
                stringr::str_pad(OCUPACION_PRINCIPAL_COD, 4, pad = "0"),
                1, 3
            )
        )
}

ftc_subgrupos_primarios_ciuo_08 <- function(tbl){
    tbl |>
        dplyr::mutate(
            subgrupos_primarios_ciuo_08 = stringr::str_pad(OCUPACION_PRINCIPAL_COD, 4, pad = "0")
            )
}