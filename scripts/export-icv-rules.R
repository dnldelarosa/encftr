# Export only the closed historical ICV pipeline and expressions; no runtime R.
script <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)), winslash = "/")
workspace <- dirname(dirname(dirname(script)))
setwd(workspace)
node <- function(x) {
  if (is.symbol(x)) return(list(symbol = as.character(x)))
  if (!is.call(x)) return(list(value = x))
  op <- paste(deparse(x[[1]]), collapse = "")
  args <- as.list(x)[-1]; nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  plain <- sub("^.*::", "", op)
  if (plain %in% c("mean", "first", "n")) op <- paste0("encft_icv_", plain)
  if (plain == "[" && "PARENTESCO" %in% all.vars(args[[2]])) op <- "encft_icv_head_value"
  list(call = op, args = unname(lapply(args, node)), names = as.list(nms))
}
steps <- function(x) {
  if (is.symbol(x) && as.character(x) == "tbl") return(list())
  if (!is.call(x)) stop("Unsupported ICV pipeline node")
  op <- paste(deparse(x[[1]]), collapse = ""); args <- as.list(x)[-1]
  if (op == "{") return(unlist(lapply(args, steps), recursive = FALSE))
  if (op == "<-") {
    if (as.character(args[[1]]) != "tbl") stop("Unsupported ICV assignment")
    return(steps(args[[2]]))
  }
  if (op == "%>%") return(c(steps(args[[1]]), steps(args[[2]])))
  if (startsWith(op, "encft_icv_legacy_")) return(list(list(op = "invoke", function_name = op)))
  plain <- sub("^.*::", "", op)
  if (plain == "group_by") return(list(list(op = "group_by", dwelling = grepl("dwelling = TRUE", paste(deparse(x), collapse = " "), fixed = TRUE))))
  if (!plain %in% c("mutate", "select", "ungroup")) stop("Unsupported ICV operation: ", plain)
  nms <- names(args); if (is.null(nms)) nms <- rep("", length(args))
  list(list(op = plain, args = unname(lapply(args, node)), names = as.list(nms)))
}
functions <- list()
for (x in parse("encftr/R/icv-siuben-legacy.R", keep.source = FALSE)) {
  if (is.call(x) && identical(x[[1]], as.name("<-")) && is.call(x[[3]]) && identical(x[[3]][[1]], as.name("function")))
    functions[[as.character(x[[2]])]] <- steps(x[[3]][[3]])
}
environment <- new.env()
sys.source("encftr/R/icv-siuben.R", environment)
model <- list(method = "encftr0-0.0.2.9002", variables = environment$ftc_variables_icv_siuben(), functions = functions)
jsonlite::write_json(model, "endompy/endompy/encftr/resources/icv-siuben-rules.json", auto_unbox = TRUE, pretty = FALSE, null = "null", na = "null", digits = NA)
file.copy("encftr/inst/examples/synthetic-icv.json", "endompy/endompy/encftr/resources/synthetic-icv.json", overwrite = TRUE)
cat("Exported", length(functions), "closed ICV pipelines.\n")
