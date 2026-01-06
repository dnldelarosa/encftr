#' Asigna etiquetas de datos a las variables especificadas
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param tbl [data.frame]: Conexión a base de datos o dataframe con los datos
#' @param dict [Dict]: Diccionario con las etiquetas de las variables
#' @param subset [character]: Si especificado, solo se asignaran las etiquetas a esas variables.
#' @param ... argumentos adicionales de \code{labeler::set_Dict}
#'
#' @return Los datos introducidos en el argumento \code{tbl} pero con etiquetas de datos
#'
#' @seealso
#'   Etiquetas de datos \code{vignette("labeler", package = "labeler")}
#'
#' @export
ftc_set_Dict <- function(tbl, dict = encftr::dict, subset = NULL, ...) {
  labeler::set_Dict(tbl, dict, subset, ...)
}

#' Utiliza las etiquetas de datos de una variable/dataset
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param tbl [data.frame]: Conexión a base de datos o dataframe con los datos
#' @param dict [Dict]: Diccionario con las etiquetas de las variables
#' @param subset [character]: Si especificado, solo se asignaran las etiquetas a esas variables.
#' @param ... argumentos adicionales de \code{labeler::with_Dict}
#'
#' @return El dataset modificado
#'
#' @export
ftc_with_Dict <- function(tbl, dict = NULL, subset = NULL, ...) {
  labeler::with_Dict(tbl, dict, subset, ...)
}


#' @rdname ftc_set_Dict
#' @export
ftc_set_labels <- function(tbl, dict = encftr::dict, vars = NULL) {
  lifecycle::deprecate_warn("0.6.0", "ftc_set_labels()", "ftc_set_Dict()")
  ftc_set_Dict(tbl, dict, subset = vars)
}

#' @rdname ftc_set_Dict
#' @export
ftc_setLabels <- function(tbl, vars = NULL) {
  lifecycle::deprecate_warn("0.1.1", "encftr::ftc_setLabels()", "ftc_set_Dict()")
  ftc_set_Dict(tbl, subset = vars)
}

#' @rdname ftc_with_Dict
#' @export
ftc_use_labels <- function(tbl, dict = encftr::dict, vars = NULL, ...) {
  lifecycle::deprecate_warn("0.6.0", "ftc_use_labels()", "ftc_with_Dict()")
  ftc_with_Dict(tbl, dict, subset = vars, use_label = FALSE, use_labels = TRUE, ...)
}

#' @rdname ftc_with_Dict
#' @export
ftc_useLabels <- function(tbl, dict = encftr::dict, vars = NULL, ...) {
  lifecycle::deprecate_warn("0.1.1", "encftr::ftc_useLabels()", "ftc_use_labels()")
  ftc_use_labels(tbl, dict, vars, ...)
}
