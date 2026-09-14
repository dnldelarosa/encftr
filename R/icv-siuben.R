#' Variables del ICV SIUBEN historico
#'
#' Contrato de entrada de [ftc_icv_siuben()]. La metodologia integrada procede
#' de encftr0 0.0.2.9002 y es independiente del IIH y de pobreza monetaria.
#' @return Lista con columnas requeridas, opcionales y de salida.
#' @export
ftc_variables_icv_siuben <- function() {
  list(required = c("TRIMESTRE", "VIVIENDA", "HOGAR", "ZONA", "ID_PROVINCIA",
    "ID_MUNICIPIO", "EDAD", "PARENTESCO", "NIVEL_ULTIMO_ANO_APROBADO",
    "ULTIMO_ANO_APROBADO", "SABE_LEER_ESCRIBIR", "MES", "NIVEL_SE_MATRICULO",
    "PORQUE_NO_ESTUDIA", "ESTUFA", "REFRIGERADOR", "TELEVISOR", "LAVADORA",
    "AUTOMOVIL", "AIRE_ACONDICIONADO", "COMPUTADOR", "MATERIAL_PISO",
    "MATERIAL_PARED_EXTERIOR", "MATERIAL_TECHO", "DONDE_PROVIENE_AGUA",
    "TIPO_ALUMBRADO", "TIPO_SANITARIO", "COMBUSTIBLE_PARA_COCINAR",
    "COMO_ELIMINA_BASURA", "TIPO_VIVIENDA", "CANT_DORMITORIOS_VIVIENDA",
    "CANTIDAD_MIEMBROS_HOGAR"),
    optional = c("ANO", "MATERIAL_PARED_EXTERIOR_ESP"),
    output = c("icv_global", "icv_puntaje", "icv_metodo"),
    details = c("zona_geografica", "MATERIAL_PARED_EXTERIOR2", "equiv1", "pisov1",
      "paredv1", "techv1", "aguav1", "escoj1", "escov1", "alfav1", "luzv1",
      "saniv1", "cociv1", "hacina1", "pninme1", "asiscv1", "basuv1", "tipov1", "provin1"))
}

#' Calcular el ICV SIUBEN historico integrado desde encftr0
#'
#' Conserva los coeficientes y las reglas de encftr0 0.0.2.9002. No selecciona
#' ni certifica una metodologia vigente de SIUBEN. La categoria `icv_global`
#' toma valores 1 a 4; `icv_puntaje` conserva el puntaje y `icv_metodo`
#' identifica la implementacion historica.
#'
#' @param tbl data.frame o tibble local de miembros. Debe contener hogares
#'   completos con una sola jefatura (PARENTESCO == 1) por hogar y periodo.
#' @param include_details Logico; incluir los componentes historicos del indice.
#'   Los componentes que ya existen se recalculan aunque sea FALSE.
#' @param method Identificador exacto: `encftr0-0.0.2.9002`.
#' @return La tabla original, en el mismo orden y con las columnas calculadas.
#' @details
#' TRIMESTRE acepta AAAAT o 1:4 con ANO. La clave del hogar incluye el ano
#' suministrado, trimestre, vivienda y hogar. El hacinamiento conserva la
#' agregacion historica por vivienda, incluyendo todos sus hogares.
#'
#' Los coeficientes, los limites de clasificacion, la precedencia de las
#' recodificaciones y el tratamiento historico de NA se conservan: catorce
#' componentes ausentes pasan a cero antes de sumar; provincia, basura y tipo
#' de vivienda pueden dejar sin clasificacion una zona urbana/rural. EDAD <= 5
#' conserva el limite original del componente denominado menores de cinco.
#' La columna opcional de texto de pared ausente se trata como texto vacio.
#' Los codigos numericos deben ser enteros finitos o NA; las claves y la zona
#' no pueden faltar. PARENTESCO debe estar completo. Se rechazan hogares sin
#' jefatura o con varias jefaturas.
#' No se reutilizan columnas intermedias de calculos anteriores.
#' @export
#' @examples
#' x <- jsonlite::fromJSON(system.file("examples", "synthetic-icv.json", package = "encftr"))
#' y <- ftc_icv_siuben(x)
#' head(y[c("icv_global", "icv_puntaje", "icv_metodo")])
ftc_icv_siuben <- function(tbl, include_details = TRUE, method = "encftr0-0.0.2.9002") {
  spec <- ftc_variables_icv_siuben()
  ftc_require_columns(tbl, spec$required)
  if (!is.logical(include_details) || length(include_details) != 1L || is.na(include_details))
    stop("include_details debe ser TRUE o FALSE.", call. = FALSE)
  if (!is.character(method) || length(method) != 1L || is.na(method) || method != "encftr0-0.0.2.9002")
    stop("Metodo ICV no disponible; use encftr0-0.0.2.9002.", call. = FALSE)
  ftc_quarter_parts(tbl)
  keys <- ftc_household_keys(tbl)
  keep <- unique(c(spec$required, intersect(spec$optional, names(tbl))))
  work <- as.data.frame(tbl[keep])
  for (name in setdiff(spec$required, c("TRIMESTRE", "VIVIENDA", "HOGAR"))) {
    value <- work[[name]]
    if (is.factor(value) || is.logical(value) || !is.numeric(value) ||
        any(!is.na(value) & (!is.finite(value) | value != trunc(value))))
      stop("La columna ", name, " debe contener codigos numericos enteros o NA.", call. = FALSE)
  }
  if (anyNA(work$ZONA) || any(!work$ZONA %in% 1:2) || anyNA(work$ID_PROVINCIA) || anyNA(work$ID_MUNICIPIO))
    stop("ZONA (1/2), ID_PROVINCIA e ID_MUNICIPIO no pueden faltar.", call. = FALSE)
  if (anyNA(work$PARENTESCO))
    stop("PARENTESCO no puede faltar para identificar la jefatura.", call. = FALSE)
  if (!"MATERIAL_PARED_EXTERIOR_ESP" %in% names(work)) work$MATERIAL_PARED_EXTERIOR_ESP <- rep("", nrow(work))
  if (!is.character(work$MATERIAL_PARED_EXTERIOR_ESP))
    stop("MATERIAL_PARED_EXTERIOR_ESP debe ser texto.", call. = FALSE)
  output <- unique(c(spec$output, if (include_details) spec$details else intersect(spec$details, names(tbl))))
  if (!nrow(work)) {
    for (name in output) tbl[[name]] <- if (name == "icv_metodo") character() else numeric()
    return(tbl)
  }
  groups <- dplyr::group_rows(dplyr::group_by(work, dplyr::across(dplyr::all_of(keys))))
  for (rows in groups) {
    if (sum(work$PARENTESCO[rows] == 1, na.rm = TRUE) != 1L)
      stop("Cada hogar y periodo requiere exactamente una jefatura (PARENTESCO == 1).", call. = FALSE)
    for (name in c("ZONA", "ID_PROVINCIA", "ID_MUNICIPIO")) {
      if (length(unique(work[[name]][rows])) != 1L)
        stop("Ubicacion inconsistente dentro del hogar: ", name, ".", call. = FALSE)
    }
  }
  result <- encft_icv_legacy_compute_icv_siuben(work)
  result$icv_puntaje <- result$icv_nuevo
  result$icv_metodo <- method
  for (name in output) tbl[[name]] <- result[[name]]
  tbl
}

#' @rdname ftc_icv_siuben
#' @export
ftc_compute_icv_siuben <- function(tbl, include_details = TRUE, method = "encftr0-0.0.2.9002") {
  ftc_icv_siuben(tbl, include_details = include_details, method = method)
}

#' Diccionario del ICV SIUBEN historico
#'
#' Define etiquetas del resultado calculado sin modificar la edicion inmutable
#' baseline-1 del cuestionario ENCFT. La metodologia se identifica por separado.
#' @return Un Dict de labeler para las columnas de clasificacion y puntaje.
#' @export
ftc_dict_icv_siuben <- function() {
  labeler::Dict(
    icv_global = list(label = "ICV SIUBEN historico", labels = stats::setNames(1:4, paste("ICV", 1:4))),
    icv_puntaje = list(label = "Puntaje ICV SIUBEN historico"),
    icv_metodo = list(label = "Implementacion ICV SIUBEN"),
    metadata = list(name = "encft-icv-siuben", method = "encftr0-0.0.2.9002"))
}

#' Etiquetar resultados del ICV SIUBEN historico
#' @param tbl data.frame o tibble local con columnas calculadas del ICV.
#' @param vars Nombres de columnas a etiquetar; NULL aplica a las disponibles.
#' @return Los datos con etiquetas; use_labels convierte los codigos a factor.
#' @export
ftc_set_labels_icv_siuben <- function(tbl, vars = NULL) {
  labeler::set_Dict(tbl, ftc_dict_icv_siuben(), subset = vars, dtypes = FALSE)
}

#' @rdname ftc_set_labels_icv_siuben
#' @export
ftc_use_labels_icv_siuben <- function(tbl, vars = NULL) {
  labeler::with_Dict(tbl, ftc_dict_icv_siuben(), subset = vars, use_label = FALSE, use_labels = TRUE)
}

#' Compatibilidad con encftr0
#'
#' Los nombres historicos delegan en encftr y emiten una advertencia de
#' deprecacion. No requieren instalar encftr0. La seleccion vars ahora se respeta.
#' @param tbl data.frame o tibble local.
#' @param vars Nombres de variables a etiquetar o convertir; NULL usa disponibles.
#' @return Tabla con el calculo o las etiquetas solicitadas.
#' @export
ftc0_compute_icv_siuben <- function(tbl) {
  lifecycle::deprecate_warn("0.10.0", "ftc0_compute_icv_siuben()", "ftc_icv_siuben()")
  ftc_icv_siuben(tbl)
}

#' @rdname ftc0_compute_icv_siuben
#' @export
ftc0_setLabels <- function(tbl, vars = NULL) {
  lifecycle::deprecate_warn("0.10.0", "ftc0_setLabels()", "ftc_set_labels_icv_siuben()")
  ftc_set_labels_icv_siuben(ftc_set_Dict(tbl, subset = vars), vars = vars)
}

#' @rdname ftc0_compute_icv_siuben
#' @export
ftc0_setLabels_icv_global <- function(tbl) {
  lifecycle::deprecate_warn("0.10.0", "ftc0_setLabels_icv_global()", "ftc_set_labels_icv_siuben()")
  ftc_set_labels_icv_siuben(tbl, vars = "icv_global")
}

#' @rdname ftc0_compute_icv_siuben
#' @export
ftc0_useLabels <- function(tbl, vars = NULL) {
  lifecycle::deprecate_warn("0.10.0", "ftc0_useLabels()", "ftc_use_labels_icv_siuben()")
  labelled <- ftc_set_labels_icv_siuben(ftc_set_Dict(tbl, subset = vars), vars = vars)
  labeler::with_Dict(labelled, subset = vars, use_label = FALSE, use_labels = TRUE)
}
