test_that("bundled dictionary is verified without invented dates", {
  d <- ftc_dict()
  expect_equal(labeler::dict_revision(d)$version, "baseline-1")
  expect_equal(nrow(ftc_dict_versions()), 1)
  expect_error(ftc_dict(at = "2019-01-01"))
  expect_error(ftc_dict(version = "unknown"))
})

test_that("survey editions reuse definitions and reject ambiguous applicability", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  base <- labeler::dict_draft(ftc_dict())
  first <- ftc_register_dict(con, base, "example-1", valid_from = "2020-01-01", valid_to = "2020-12-31")
  draft <- labeler::dict_draft(first)
  draft$SEXO$label <- "Sexo de la persona"
  second <- ftc_register_dict(con, draft, "example-2", valid_from = "2021-01-01", valid_to = "2021-12-31")
  a <- labeler::dict_revision(first)$variable_refs
  b <- labeler::dict_revision(second)$variable_refs
  expect_equal(sum(vapply(names(a), function(n) a[[n]]$definition_hash == b[[n]]$definition_hash, logical(1))), 635)
  expect_equal(labeler::dict_revision(ftc_dict(at = "2021-04-01", con = con))$version, "example-2")
  expect_error(ftc_dict(at = "2022-01-01", con = con))
  expect_error(ftc_register_dict(con, draft, "example-2"))
  expect_equal(nrow(ftc_dict_versions(con)), 2)
})
