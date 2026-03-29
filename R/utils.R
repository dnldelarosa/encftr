#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL



#' Visualizar el diccionario de forma interactiva
#'
#' @return Un elemento html con la información del diccionario
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ftc_browse_dict()
#' }
ftc_browse_dict <- function() {
  labeler::dict_browser(encftr::dict, .interactive = TRUE)
}




#' Conexión a base de datos
#'
#'   `r lifecycle::badge("experimental")`
#'
#'   Requiere el paquete `Dmisc` para abrir la conexion a la base `encft`.
#'
#' @return Conexión a base de datos.
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- ftc_db_connect()
#' }
ftc_db_connect <- function() {
  utils::getFromNamespace("db_connect", "Dmisc")(db_name = "encft")
}


#' @rdname ftc_db_connect
#' @export
ftc_dbConnect <- function() ftc_db_connect()




#' Tipo de cambio de las monedas extranjeras publicadas por el Banco Central
#'
#'
#' @format [data.frame]
"tipo_cambio_monedas"
