#devtools::load_all("~/Projects/R Packages/domar") #Cambiar por un enlace
#tipo_cambio_monedas <- domar::#tipo_cambio_otras_monedas_mensual()

#Tasa de cambio----

library(dplyr)

#Se carga la hoja de Tasa de Cambio mensual, que se encuentra en la página web del BCRD.
#Se sustituyen los codigos del estándar internacional ISO 4217.
tipo_cambio <- readxl::read_excel("G:/My Drive/Databases/ENCFT/pobrezaMonetaria/2022/TASAS_CONVERTIBLES_OTRAS_MONEDAS.xls", sheet = "Mensual", col_names = T, skip = 2) %>%
  rename( "BRL"= "REAL BRASILENO",
          "CAD"= "DOLAR CANADIENSE",
          "CHF"= "FRANCO SUIZO",
          "CNY"= "YUAN CHINO",
          "DEG"= "DERECHO ESPECIAL DE GIRO",
          "DKK" = "CORONA DANESA",
          "EUR" = "EURO",
          "GBP" = "LIBRA ESTERLINA",
          "JPY" = "YEN JAPONES",
          "NOK" = "CORONA NORUEGA",
          "LESC" = "LIBRA ESCOCESA",
          "SEK" = "CORONA SUECA",
          "USD" = "DOLAR ESTADOUNIDENSE",
          "VEF" = "BOLIVAR FUERTE VENEZOLANO"
  ) %>% mutate(MES= recode(Mes,
                           "Ene" = "01", "Feb" = "02", "Mar" = "03", "Abr" = "04", "May" = "05", "Jun" = "06",
                           "Jul" = "07", "Ago" = "08", "Aug" = "08", "Sep" = "09", "Oct" = "10", "Nov" = "11", "Dic" = "12"
  ),
  PERIODO = as.numeric(paste(Año, MES, sep="")),
  ARS = case_when(
    PERIODO == 201611 ~ 3.07,
    PERIODO == 201612 ~ 2.92,
    PERIODO == 201701 ~ 2.94,
    PERIODO == 201702 ~ 3.01,
    PERIODO == 201703 ~ 3.03,
    PERIODO == 201704 ~ 3.09)) %>%
  select(-Año, -Mes, -MES) %>% filter(PERIODO >= 201507)


usethis::use_data(tipo_cambio, overwrite = TRUE)
