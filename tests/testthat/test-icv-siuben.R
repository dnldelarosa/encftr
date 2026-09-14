icv_example <- function() {
  jsonlite::fromJSON(system.file("examples", "synthetic-icv.json", package = "encftr"))
}

test_that("integrated ICV preserves every legacy component and all four classes", {
  x <- icv_example()
  expected <- jsonlite::fromJSON(test_path("fixtures", "icv-legacy-expected.json"))
  y <- ftc_icv_siuben(x)
  expect_equal(as.data.frame(y[names(expected)]), expected, tolerance = 1e-12)
  expect_equal(sort(unique(y$icv_global)), 1:4)
  expect_equal(y[names(x)], x)
  expect_equal(nrow(y), 108)
  expect_true(all(y$icv_metodo == "encftr0-0.0.2.9002"))
  expect_equal(y$icv_puntaje[1], sum(unlist(y[1, c("saniv1", "techv1", "pisov1",
    "paredv1", "equiv1", "aguav1", "provin1", "escoj1", "escov1", "alfav1",
    "cociv1", "luzv1", "basuv1", "tipov1", "hacina1")])))
  expect_equal(ftc_compute_icv_siuben(x), y)
  expect_equal(suppressWarnings(ftc0_compute_icv_siuben(x)), y)
})

test_that("periods isolate households while crowding still aggregates dwellings", {
  first <- transform(icv_example()[1:3, ], ANO = 2019, TRIMESTRE = 1)
  second <- transform(first, ANO = 2020, NIVEL_ULTIMO_ANO_APROBADO = 6,
                      ULTIMO_ANO_APROBADO = 4, SABE_LEER_ESCRIBIR = 1)
  expected <- rbind(ftc_icv_siuben(first), ftc_icv_siuben(second))
  both <- rbind(first, second)
  expect_equal(ftc_icv_siuben(both), expected)
  order <- c(6, 2, 4, 1, 5, 3)
  expect_equal(ftc_icv_siuben(both[order, ])$icv_puntaje, expected$icv_puntaje[order])
  family <- transform(second, ANO = 2019, HOGAR = 2)
  shared <- transform(rbind(first, family), CANT_DORMITORIOS_VIVIENDA = 2)
  y <- ftc_icv_siuben(shared)
  expect_equal(y$hacina1, rep(3.4806, 6))
  expect_false(identical(y$escoj1[1], y$escoj1[4]))
  expect_error(ftc_icv_siuben(transform(first, ANO = NA)), "TRIMESTRE")
  expect_error(ftc_icv_siuben(transform(first, TRIMESTRE = 20201)), "coinciden")
})

test_that("ICV preserves input and refreshes pre-existing calculation columns", {
  x <- icv_example()[1:3, ]
  x$estufa <- 987
  grouped <- dplyr::group_by(x, HOGAR)
  y <- ftc_icv_siuben(grouped, include_details = FALSE)
  expect_s3_class(y, "grouped_df")
  expect_equal(dplyr::group_vars(y), "HOGAR")
  expect_equal(y$estufa, x$estufa)
  expect_equal(setdiff(names(y), names(x)), ftc_variables_icv_siuben()$output)
  stale <- ftc_icv_siuben(x)
  stale$icv_global <- stale$icv_puntaje <- stale$equiv1 <- -999
  expect_equal(ftc_icv_siuben(stale, include_details = FALSE), ftc_icv_siuben(x))
  expect_equal(nrow(ftc_icv_siuben(x[FALSE, ])), 0)
  without_text <- x[setdiff(names(x), "MATERIAL_PARED_EXTERIOR_ESP")]
  expect_equal(ftc_icv_siuben(without_text)$icv_global, ftc_icv_siuben(x)$icv_global)
})

test_that("ICV rejects ambiguous structural inputs with explicit errors", {
  x <- icv_example()[1:3, ]
  expect_error(ftc_icv_siuben(x[setdiff(names(x), "EDAD")]), "EDAD")
  expect_error(ftc_icv_siuben(transform(x, PARENTESCO = 2)), "jefatura")
  expect_error(ftc_icv_siuben(transform(x, PARENTESCO = 1)), "jefatura")
  expect_error(ftc_icv_siuben(transform(x, PARENTESCO = c(1, NA, 3))), "PARENTESCO")
  expect_error(ftc_icv_siuben(transform(x, HOGAR = NA)), "identificadores")
  expect_error(ftc_icv_siuben(transform(x, ZONA = NA_real_)), "no pueden faltar")
  expect_error(ftc_icv_siuben(transform(x, ZONA = c(1, 2, 1))), "inconsistente")
  expect_error(ftc_icv_siuben(transform(x, EDAD = Inf)), "EDAD")
  expect_error(ftc_icv_siuben(transform(x, EDAD = factor(EDAD))), "EDAD")
  expect_error(ftc_icv_siuben(x, method = "vigente"), "Metodo")
  expect_error(ftc_icv_siuben(x, include_details = NA), "include_details")
  expect_equal(ftc_icv_siuben(transform(x, MATERIAL_PISO = NA_real_))$pisov1, c(0, 0, 0))
})

test_that("ICV labels use labeler without changing the frozen questionnaire", {
  before <- labeler::dict_revision(ftc_dict())$content_hash
  x <- data.frame(icv_global = c(1, 2, 3, 4, NA), SEXO = c(1, 2, 1, 2, 1))
  labelled <- ftc_set_labels_icv_siuben(x)
  expect_equal(unname(attr(labelled$icv_global, "labels")), 1:4)
  expect_null(attr(labelled$SEXO, "labels"))
  y <- ftc_use_labels_icv_siuben(x)
  expect_true(is.factor(y$icv_global))
  expect_equal(as.character(y$icv_global), c(paste("ICV", 1:4), NA_character_))
  expect_equal(y$SEXO, x$SEXO)
  expect_equal(labeler::dict_revision(ftc_dict())$content_hash, before)
  subset <- suppressWarnings(ftc0_setLabels(x, vars = "icv_global"))
  expect_equal(subset, labelled)
  expect_true(is.factor(suppressWarnings(ftc0_useLabels(x, vars = "icv_global"))$icv_global))
  expect_equal(suppressWarnings(ftc0_setLabels_icv_global(x)), labelled)
})
