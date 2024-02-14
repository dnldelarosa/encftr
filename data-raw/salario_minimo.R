library(dplyr)
library(magrittr)
library(ggplot2)

data_sisdom <- readxl::read_excel(
  "data-raw/SISDOM-2021.-Indicadores-de-Mercado-de-Trabajo-Poblacion-15-anos.xlsx",
  "06 1 003",
  skip = 6,
) %>%
  filter(nchar(Desagregaciones) < 50) %>%
  mutate(
    sector = if_else(is.na(`2002`), Desagregaciones, NA_character_),
    .before = 1
  ) %>%
  tidyr::fill(sector) %>%
  tidyr::drop_na(`2002`) %>%
  mutate(across(everything(), as.character)) %>%
  tidyr::pivot_longer(-c(sector, Desagregaciones)) %>%
  tidyr::drop_na() %>%
  mutate(
    fecha = if_else(startsWith(value, "("), value, NA_character_),
    fecha = lead(fecha)
  ) %>%
  tidyr::drop_na(fecha) %>%
  select(-name) %>%
  distinct() %>%
  tidyr::separate(fecha, c("mes", "ano"), "/") %>%
  mutate(
    across(c(ano, mes), ~ stringr::str_remove_all(.x, "[^A-Za-z0-9]")),
    across(c(ano, value), as.numeric),
    ano = if_else(ano > 90, 1900 + ano, 2000 + ano)
  ) %>%
  Dmisc::vars_to_date("ano", month = "mes") %>%
  tidyr::drop_na()


data_sisdom %>%
  tidyr::pivot_wider(names_from = "sector", values_from = "value")


fechas <- seq(
  data_sisdom %>%
    pull(date) %>%
    min() %>%
    lubridate::year(),
  data_sisdom %>%
    pull(date) %>%
    max() %>%
    lubridate::year()
) %>%
  expand.grid(1:12) %>%
  Dmisc::vars_to_date(year = "Var1", month = "Var2") %>%
  arrange(date)

salario_minimo <- data_sisdom %>%
  select(sector, Desagregaciones) %>%
  tidyr::unite("sector", Desagregaciones, sector, sep = " - ") %>%
  pull(sector) %>%
  unique() %>%
  expand.grid(fechas[['date']], .) %>%
  tidyr::separate(
    Var2,
    c("tamano_salario_minimo", "sector_salario_minimo"),
    sep = " - "
  ) %>%
  as_tibble() %>%
  left_join(
    data_sisdom,
    by = c("Var1" = "date", "tamano_salario_minimo" = "Desagregaciones", "sector_salario_minimo" = "sector")
  ) %>%
  group_by(across(contains("minimo"))) %>%
  tidyr::fill(value) %>%
  rename(date = Var1) %>%
  filter(date >= as.Date("2016-01-01"))

salario_minimo %>%
  ggplot(aes(x = date, y = value)) +
  geom_line(aes(color = sector_salario_minimo)) +
  facet_wrap(~tamano_salario_minimo)

salario_minimo %>%
  select(sector_salario_minimo, tamano_salario_minimo) %>%
  distinct()

salario_minimo <- salario_minimo %>%
  mutate(
    sector_salario_minimo = case_when(
      endsWith(sector_salario_minimo, "sectorizado1") ~ "1",
      startsWith(sector_salario_minimo, "Hoteles") ~ "2",
      endsWith(sector_salario_minimo, "industrial") ~ "3",
      endsWith(sector_salario_minimo, "público") ~ "4",
      TRUE ~ sector_salario_minimo
    ),
    tamano_salario_minimo = case_when(
      endsWith(tamano_salario_minimo, "grande") ~ "1",
      endsWith(tamano_salario_minimo, "mediana") ~ "2",
      endsWith(tamano_salario_minimo, "pequeña") ~ "3",
      tamano_salario_minimo == "Zona franca" ~ "4",
      startsWith(tamano_salario_minimo, "Zona franca") ~ "5",
      endsWith(tamano_salario_minimo, "Estado") ~ "6",
      TRUE ~ tamano_salario_minimo
    )
  )


usethis::use_data(salario_minimo, overwrite = TRUE)
