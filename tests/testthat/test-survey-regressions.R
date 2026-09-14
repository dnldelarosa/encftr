synthetic_members <- function() jsonlite::fromJSON(system.file("examples", "synthetic-members.json", package = "encftr"))

test_that("poverty calculations are repeatable and retain missing classifications", {
  x <- synthetic_members()
  for (fn in list(ftc_pobreza_monetaria_2012, ftc_pobreza_monetaria_2022)) {
    one <- fn(x, .keep = TRUE)
    two <- fn(one, .keep = TRUE)
    expect_equal(one$pobreza_monetaria, two$pobreza_monetaria)
    expect_equal(one$ing_total_pobreza_def, two$ing_total_pobreza_def)
    expect_equal(one$ing_total_pobreza, two$ing_total_pobreza)
    expect_equal(nrow(fn(x[FALSE, ])), 0)
    unsupported <- x
    unsupported$PERIODO <- 209901; unsupported$TRIMESTRE <- 20991; unsupported$ANO <- 2099
    expect_true(all(is.na(fn(unsupported)$pobreza_monetaria)))
  }
  no_region <- x; no_region$ID_PROVINCIA <- NA; no_region$GRUPO_REGION <- NA
  expect_equal(ftc_pobreza_monetaria_2012(x)$ing_pc_pobreza_def,
               ftc_pobreza_monetaria_2012(no_region)$ing_pc_pobreza_def)
})

test_that("education and household boundaries have explicit denominators", {
  x <- data.frame(NIVEL_ULTIMO_ANO_APROBADO = c(3,3,5), ULTIMO_ANO_APROBADO = c(4,6,2), ANO = c(2021,2022,2022))
  expect_equal(ftc_anos_educacion(x)$anos_educacion, c(10,12,14))
  expect_equal(ftc_anos_educacion(x, secundaria_base = "historica_por_ano")$anos_educacion, c(12,12,14))
  household <- data.frame(ANO = c(2023,2023,2024), TRIMESTRE = 1, VIVIENDA = 1, HOGAR = 1, EDAD = c(10,30,30))
  expect_equal(ftc_tasa_dependencia(household)$tasa_dependencia, c(100,100,0))
  expect_true(all(is.na(ftc_tasa_dependencia(transform(household, EDAD = 5))$tasa_dependencia)))
  heads <- transform(household, PERIODO = c(202301,202301,202401), PARENTESCO = c(1,2,2), SEXO = c(1,2,2))
  expect_equal(ftc_sexo_jefe(heads)$sexo_jefe, c(1,1,NA))
  expect_error(ftc_sexo_jefe(transform(heads, PARENTESCO = 1)), "Mas de un jefe")
})

test_that("IIH overwrites previous scores and distinguishes household output", {
  x <- synthetic_members()
  result <- ftc_iih(x)
  expect_equal(nrow(result), 12)
  expect_equal(nrow(ftc_iih(x, return_households = TRUE)), 4)
  expect_equal(ftc_iih(transform(x, IIH = -99))$IIH, result$IIH)
  expect_equal(ftc_iih(result)$IIH, result$IIH)
  expect_error(ftc_iih(x[, setdiff(names(x), "ID_HOGAR")]), "ID_HOGAR")
})

test_that("foreign income never depends on the next person's currency", {
  x <- synthetic_members()
  x$SUELDO_BRUTO_AP_MONEDA <- rep(c("USD", "EUR", "DOP"), 4)
  x$SUELDO_BRUTO_AP_MONTO <- 100
  one <- ftc_ing_mensual_ocup_prin_asalariado(x)$ing_mensual_ocup_prin_asalariado
  order <- rev(seq_len(nrow(x)))
  two <- ftc_ing_mensual_ocup_prin_asalariado(x[order, ])$ing_mensual_ocup_prin_asalariado
  expect_equal(one[order], two)
})
