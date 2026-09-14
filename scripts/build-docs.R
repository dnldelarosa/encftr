# Rscript scripts/build-docs.R [output directory]. Builds locally; never publishes.
args <- commandArgs(trailingOnly = TRUE)
script <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)), winslash = "/")
pkg <- dirname(dirname(script)); workspace <- dirname(pkg)
output <- if (length(args)) args[[1]] else file.path(workspace, "artifacts/encftr-release/sites/r")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
output <- normalizePath(output, winslash = "/")
if (tolower(output) == tolower(pkg) || startsWith(tolower(pkg), paste0(tolower(output), "/"))) stop("Unsafe output directory")
marker <- file.path(output, ".encftr-docs-output")
if (length(setdiff(list.files(output, all.files = TRUE), c(".", ".."))) && !file.exists(marker)) stop("Choose an empty output directory")
writeLines("encftr docs output", marker)
Sys.setenv(RENV_CONFIG_AUTOLOADER_ENABLED = "false", NOT_CRAN = "true")
if (.Platform$OS.type == "windows") Sys.setenv(LC_ALL = "English_United States.utf8")
Sys.setenv(R_USER_CACHE_DIR = file.path(dirname(output), "cache"))
dir.create(Sys.getenv("R_USER_CACHE_DIR"), recursive = TRUE, showWarnings = FALSE)
.libPaths(c(file.path(workspace, "artifacts/dmisc-release/package/zip-installed"),
  file.path(workspace, "artifacts/dmisc-release/library"),
  file.path(workspace, "artifacts/labeler/bilingual/package/labeler.Rcheck"), .libPaths()))
if (packageVersion("labeler") < "0.11.0") stop("Install labeler >= 0.11.0 first")
lib <- file.path(dirname(output), "r-library")
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
r <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")
Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
status <- system2(r, c("CMD", "INSTALL", paste0("--library=", shQuote(lib)), shQuote(pkg)))
if (status != 0L) stop("Source package installation failed")
.libPaths(c(lib, .libPaths()))
library(encftr)
if (packageVersion("encftr") != as.character(read.dcf(file.path(pkg, "DESCRIPTION"))[1, "Version"])) stop("Wrong source package version")
Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
pkgdown::build_site_github_pages(pkg = pkg, dest_dir = output, clean = TRUE,
  install = FALSE, new_process = FALSE, examples = TRUE)
stage <- file.path(dirname(output), "r-source-en")
dir.create(stage, recursive = TRUE, showWarnings = FALSE)
for (entry in c("DESCRIPTION", "LICENSE", "NAMESPACE", "R", "data", "inst", "man", "vignettes", list.files(pkg, "\\.md$"), "_pkgdown.yml")) {
  if (file.exists(file.path(pkg, entry))) file.copy(file.path(pkg, entry), stage, recursive = TRUE, overwrite = TRUE)
}
translation <- file.path(pkg, "pkgdown/i18n/en")
for (entry in list.files(translation, full.names = TRUE)) file.copy(entry, stage, recursive = TRUE, overwrite = TRUE)
pkgdown::build_site_github_pages(pkg = stage, dest_dir = file.path(output, "en"), clean = TRUE,
  install = FALSE, new_process = FALSE, examples = TRUE)
writeLines("encftr docs output", marker)
jsonlite::write_json(list(package = "encftr", version = as.character(read.dcf(file.path(pkg, "DESCRIPTION"))[1, "Version"]), languages = c("es", "en"),
  r = as.character(getRversion()), pkgdown = as.character(packageVersion("pkgdown")),
  built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")), file.path(output, "build-info.json"), auto_unbox = TRUE, pretty = TRUE)
cat("Built bilingual pkgdown:", output, "\n")
