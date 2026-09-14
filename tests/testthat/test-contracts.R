test_that("weights use observed coverage within year and semester", {
  x <- data.frame(TRIMESTRE = c(20231, 20232, 20232, 20234, 20241),
                  FACTOR_EXPANSION = c(120, 240, 60, 90, 50))
  expect_equal(ftc_factor_expansion_anual(x)$factor_expansion_anual, c(40, 80, 20, 30, 50))
  expect_equal(ftc_factor_expansion_semestre(x)$factor_expansion_semestre, c(60, 120, 30, 90, 50))
  expect_equal(ftc_factor_expansion_anual(x, periods = 4)$factor_expansion_anual, x$FACTOR_EXPANSION / 4)
  alternate <- transform(x, ANO = TRIMESTRE %/% 10, TRIMESTRE = TRIMESTRE %% 10)
  expect_equal(ftc_factor_expansion_anual(alternate)$factor_expansion_anual, c(40, 80, 20, 30, 50))
  expect_equal(names(x), c("TRIMESTRE", "FACTOR_EXPANSION"))
  expect_error(ftc_factor_expansion_anual(transform(x, TRIMESTRE = NA)), "TRIMESTRE")
  expect_error(ftc_factor_expansion_anual(transform(x, ANO = 2000)), "coinciden")
  expect_error(ftc_factor_expansion_anual(transform(x, FACTOR_EXPANSION = -1)), "negativo")
  expect_error(ftc_factor_expansion_anual(x, periods = 0), "periods")
  expect_error(ftc_factor_expansion_semestre(x, periods = 4), "periods")
  expect_equal(nrow(ftc_factor_expansion_anual(x[FALSE, ])), 0)
})

test_that("education aliases terminate and preserve options", {
  x <- data.frame(EDAD = c(5, 10, 18), NIVEL_SE_MATRICULO = c(9, 9, 9),
    SABE_LEER_ESCRIBIR = c(1, 2, NA), MES = 7, PORQUE_NO_ESTUDIA = 1,
    TANDA_ASISTE = 7, OCUPADO = 1,
    NIVEL_ULTIMO_ANO_APROBADO = 2, ULTIMO_ANO_APROBADO = 1)
  expect_equal(suppressWarnings(ftc_compute_alfabetizacion(x)), ftc_alfabetizacion(x))
  expect_equal(suppressWarnings(ftc_compute_sobreedad_escolar(x)), ftc_sobreedad_escolar(x))
  expect_equal(suppressWarnings(ftc_compute_matriculacion_escolar(x, summer_fix = TRUE)),
               ftc_matriculacion_escolar(x, summer_fix = TRUE))
  expect_equal(ftc_trabajo_infantil(x, summer_fix = TRUE)$trabajo_infantil, c(1, 1, NA))
})

test_that("missing poverty inputs never become nonpoor", {
  x <- data.frame(.encftr_rowid = 1:6, macro_region = "Norte", Ihog_ENCFT_a = 100,
    IPCm_ENCFT = c(NA, 50, 100, 200, Inf, 50),
    linea_pob = c(200, 200, 200, 200, 200, NA), linea_ind = 100)
  y <- encftr:::ftc_pm22_build_outputs(x, character())
  expect_identical(y$pobreza_monetaria, c(NA_integer_, 1L, 2L, 3L, NA_integer_, NA_integer_))
  expect_identical(y$pobre, c(NA, TRUE, TRUE, FALSE, NA, NA))
})
