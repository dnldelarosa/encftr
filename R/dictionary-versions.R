#' Seleccionar una revision del diccionario ENCFT
#'
#' La revision incluida `baseline-1` conserva el diccionario disponible en el
#' proyecto y no declara vigencia historica. Para seleccionar por fecha se
#' requiere un registro con intervalos documentados. La version del diccionario
#' no selecciona la metodologia de pobreza.
#'
#' @param version Identificador exacto. NULL selecciona baseline-1 sin registro;
#'   con registro debe especificarse version o at.
#' @param at Fecha ISO opcional de aplicabilidad, AAAA-MM-DD.
#' @param con Conexion DBI/RSQLite opcional a un registro de labeler.
#' @return Un Dict completo con revision e integridad verificadas.
#' @export
#' @examples
#' diccionario <- ftc_dict()
#' labeler::dict_revision(diccionario)$version
ftc_dict <- function(version = NULL, at = NULL, con = NULL) {
  if (!is.null(con)) return(labeler::db_load_dict_version(con, "encft", version = version, at = at))
  if (is.null(version)) version <- "baseline-1"
  if (!is.character(version) || length(version) != 1L || is.na(version) || version != "baseline-1") {
    stop("Revision no incluida; use ftc_dict_versions() o proporcione con.", call. = FALSE)
  }
  path <- system.file("dictionaries", "baseline-1.json", package = "encftr")
  result <- labeler::from_json(path)
  labeler::dict_revision(result, at = at)
  result
}

#' Revisiones disponibles del diccionario ENCFT
#' @inheritParams ftc_dict
#' @return Un data.frame con las revisiones y sus intervalos de aplicabilidad.
#' @export
ftc_dict_versions <- function(con = NULL) {
  if (!is.null(con)) return(labeler::db_list_dict_versions(con, "encft"))
  revision <- labeler::dict_revision(ftc_dict())
  fields <- c("dictionary_id", "version", "parent_version", "created_at", "author",
              "message", "valid_from", "valid_to", "content_hash")
  as.data.frame(stats::setNames(lapply(fields, function(n) if (is.null(revision[[n]])) NA_character_ else revision[[n]]), fields))
}

#' Registrar una edicion documentada de ENCFT
#' @param con Conexion DBI/RSQLite al registro de labeler.
#' @param dict Diccionario completo, normalmente un borrador de una revision.
#' @param version Identificador nuevo e inmutable de la edicion.
#' @param valid_from,valid_to Limites ISO inclusivos de aplicabilidad, o NULL
#'   cuando no existe evidencia suficiente para fechar la edicion.
#' @param ... Metadatos adicionales para labeler::db_register_dict(), incluidos
#'   parent_version, author, message y renames.
#' @return El Dict registrado. Las definiciones sin cambios se reutilizan.
#' @export
ftc_register_dict <- function(con, dict, version, valid_from = NULL, valid_to = NULL, ...) {
  labeler::db_register_dict(con, dict, version, dictionary_id = "encft",
                           valid_from = valid_from, valid_to = valid_to, ...)
}
