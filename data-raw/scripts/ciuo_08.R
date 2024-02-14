data <- readxl::read_excel(
    "data-raw/struct08.xls",
    sheet = 1,
    .name_repair = janitor::make_clean_names
)

data |>
    dplyr::filter(stringr::str_length(ciuo_08_code) == 1) -> grandes_grupos_ciuo_080

grandes_grupos_ciuo_08 <- grandes_grupos_ciuo_080$titulo_sp
names(grandes_grupos_ciuo_08) <- grandes_grupos_ciuo_080$ciuo_08_code
rm(grandes_grupos_ciuo_080)


data |>
    dplyr::filter(stringr::str_length(ciuo_08_code) == 2) -> subgrupos_principales_ciuo_080

subgrupos_principales_ciuo_08 <- subgrupos_principales_ciuo_080$titulo_sp
names(subgrupos_principales_ciuo_08) <- subgrupos_principales_ciuo_080$ciuo_08_code
rm(subgrupos_principales_ciuo_080)

data |>
    dplyr::filter(stringr::str_length(ciuo_08_code) == 3) -> subgrupos_ciuo_080

subgrupos_ciuo_08 <- subgrupos_ciuo_080$titulo_sp
names(subgrupos_ciuo_08) <- subgrupos_ciuo_080$ciuo_08_code
rm(subgrupos_ciuo_080)

data |>
    dplyr::filter(stringr::str_length(ciuo_08_code) == 4) -> subgrupos_primarios_ciuo_080

subgrupos_primarios_ciuo_08 <- subgrupos_primarios_ciuo_080$titulo_sp
names(subgrupos_primarios_ciuo_08) <- subgrupos_primarios_ciuo_080$ciuo_08_code
rm(subgrupos_primarios_ciuo_080)

