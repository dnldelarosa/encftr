# Development only. Run from the ENDOM workspace after loading labeler >= 0.11.0.
.libPaths(c("artifacts/labeler/bilingual/package/labeler.Rcheck", .libPaths()))
pkgload::load_all("encftr", quiet = TRUE)
node <- function(x) {
  if (is.symbol(x)) return(list(symbol = as.character(x)))
  if (is.call(x)) {
    args <- as.list(x)[-1]
    nms <- names(args)
    if (is.null(nms)) nms <- rep("", length(args))
    return(list(call = paste(deparse(x[[1]]), collapse = ""),
                args = unname(lapply(args, node)), names = as.list(nms)))
  }
  if (is.pairlist(x)) return(lapply(x, node))
  list(value = x)
}
functions <- list()
for (file in list.files("encftr/R", pattern = "\\.R$", full.names = TRUE)) {
  for (expr in parse(file, keep.source = FALSE)) {
    if (!is.call(expr) || !identical(expr[[1]], as.name("<-")) ||
        !is.call(expr[[3]]) || !identical(expr[[3]][[1]], as.name("function"))) next
    name <- as.character(expr[[2]])
    fn <- get(name, envir = asNamespace("encftr"))
    functions[[name]] <- list(file = basename(file),
      formals = lapply(formals(fn), function(x) if (identical(x, quote(expr = ))) list(missing = TRUE) else node(x)),
      body = node(expr[[3]][[3]]))
  }
}
dir.create("endompy/scripts/generated", showWarnings = FALSE, recursive = TRUE)
jsonlite::write_json(functions, "endompy/scripts/generated/r-functions.json",
  auto_unbox = TRUE, null = "null", na = "null", digits = NA)
specs <- encftr:::encft_iih_model_specs()
specs <- lapply(specs, function(spec) {
  spec$coefficients <- lapply(spec$coefficients, as.list)
  spec
})
jsonlite::write_json(list(required = as.list(encftr:::encft_iih_required_columns()),
  optional = as.list(encftr:::encft_iih_optional_columns()),
  output = as.list(encftr:::encft_iih_output_columns(FALSE)),
  details = as.list(encftr:::encft_iih_output_columns(TRUE)),
  walls = as.list(encftr:::encft_iih_wall_material_lookup()), models = specs),
  "endompy/endompy/encftr/resources/iih-models.json", auto_unbox = TRUE, digits = NA)
