#' Diccionario para los datos de la ENCFT
#'
#'   Contiene las etiquetas tanto para las variables como para los valores de estas.
#'
#' @format [list]
#'
#' @source \url{https://github.com/endomer-py/encftr/tree/master/Files/Cuestionarios/}
"dict"






#' Ejemplo agregado de la ENCFT de 2016
#'
#'   Contiene 256 celdas agregadas por trimestre, ano, sexo y provincia,
#'   con la suma del factor de expansion. Es un agregado historico de 2016,
#'   no una base de personas ni el ejemplo sintetico de las nuevas guias.
#'
#' @format Un data.frame de 256 filas con TRIMESTRE, ANO, SEXO,
#'   ID_PROVINCIA y FACTOR_EXPANSION.
#'
#' @source ENCFT 2016; procedimiento de agregacion en data-raw/encft.R.
"encft"
