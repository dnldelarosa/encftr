library(dplyr)
devtools::load_all(".")
devtools::load_all("../enftr/")
library(tidyr)


conn <- Dmisc::db_connect()
encft <- tbl(conn, "encft")


conn2 <- Dmisc::db_connect(db_name = "enft")
enft <- tbl(conn2, "enft")


encft %>%
  ftc_factor_exp_anual() %>%
  count(ANO, RECIBIO_CESANTIA, wt = factor_exp_anual) %>%
  collect() %>%
  bind_rows(
enft %>%
  ft_peri_vars() %>%
  filter(ano < 2016) %>%
  count(ANO = ano, RECIBIO_CESANTIA = EFT_RECIBIO_CESANTIA, wt = EFT_FACTOR_EXP_ANUAL) %>%
  collect()
  ) %>%
  drop_na() %>%
  filter(RECIBIO_CESANTIA != 0) %>%
  ftc_use_labels() %>%
  clipr::write_clip()
