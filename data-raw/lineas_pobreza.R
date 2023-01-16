library(readxl)
lineas_pobreza <- read_excel("data-raw/lineas_pobreza.xlsx")
usethis::use_data(lineas_pobreza, overwrite = TRUE)
