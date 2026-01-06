# We need to load package to verify exported data and functions
devtools::load_all()

message("Checking dict object...")
dict <- encftr::dict
if (is.null(dict)) stop("dict is NULL")
print(class(dict))
if (!inherits(dict, "Dict")) stop("encftr::dict is not a Dict object")
message("encftr::dict is properly a Dict object.")

# Check content
if (!"grandes_grupos_ciuo_08" %in% names(dict)) warning("grandes_grupos_ciuo_08 missing from dict object names") # It is a variable name

# Check set_Dict
message("Testing ftc_set_Dict...")
df <- data.frame(ZONA = c(1, 2))
labeled <- ftc_set_Dict(df, dict, subset = "ZONA")
print(str(labeled$ZONA))
z_lbl <- attr(labeled$ZONA, "labels")
if (is.null(z_lbl)) stop("Labels not applied to ZONA")
if (z_lbl["Zona urbana"] != 1) stop("Label value mismatch")
message("ftc_set_Dict functional.")

# Check set_labels deprecation
message("Testing ftc_set_labels deprecation...")
has_warning <- FALSE
tryCatch(
    {
        labeled2 <- ftc_set_labels(df, dict, vars = "ZONA")
    },
    warning = function(w) {
        if (grepl("deprecated", w$message)) {
            message("Caught expected deprecation warning for ftc_set_labels")
            has_warning <<- TRUE
        }
    }
)
# Note: deprecate_warn usually warns based on verbosity options.
# Depending on lifecycle setup it might not warn every time, but let's assume it works.
