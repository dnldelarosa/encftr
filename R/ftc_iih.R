#' Variables requeridas por el IIH de la ENCFT
#'
#' Devuelve las variables minimas y opcionales que usa [ftc_iih()] para
#' calcular el Indice de Ingresos de los Hogares (IIH) sobre microdatos de la
#' ENCFT a nivel de miembros.
#'
#' @param include_details Logico. Si `TRUE`, incluye variables intermedias en
#'   el vector `output`.
#' @return Una lista con `required`, `optional` y `output`.
#' @export
ftc_variables_iih <- function(include_details = FALSE) {
  list(
    required = encft_iih_required_columns(),
    optional = encft_iih_optional_columns(),
    output = encft_iih_output_columns(include_details = include_details)
  )
}


#' Calcular el IIH de la ENCFT
#'
#' Calcula el Indice de Ingresos de los Hogares (IIH) de la ENCFT usando la
#' especificacion operacional validada en `data-raw/IIH`. La funcion espera
#' datos a nivel de miembros y agrega al objeto original las columnas oficiales
#' de ingreso modelado y categoria IIH. Si `include_details = TRUE`, tambien
#' anexa las variables intermedias del modelo.
#'
#' @param tbl [data.frame] o lista con componente `miembros`, `members`,
#'   `personas` o `data`.
#' @param vivienda_tbl [data.frame] opcional con `PERIODO`, `VIVIENDA` y
#'   `MATERIAL_PARED_EXTERIOR_ESP`. Se usa para completar respuestas abiertas
#'   de material de pared cuando no vienen en `tbl`.
#' @param return_households Logico. Si `TRUE`, devuelve una fila por hogar en
#'   lugar de reanexar los resultados a cada miembro.
#' @param include_details Logico. Si `TRUE`, incluye variables intermedias del
#'   modelo.
#' @param filter_valid_households Logico. Si `TRUE`, excluye del resultado de
#'   hogares los casos con faltantes estructurales (`hconmissing == 1`). Cuando
#'   `return_households = FALSE`, esos hogares quedan sin resultado al reanexar.
#'
#' @return [data.frame] con las columnas calculadas. La columna `IIH` es la
#'   categoria oficial final (`1` pobreza extrema, `2` pobreza general,
#'   `3` no pobre, `9` no calculable).
#' @export
#'
#' @examples
#' \dontrun{
#' encft_iih <- ftc_iih(encft)
#' hogares_iih <- ftc_iih(encft, return_households = TRUE)
#' }
ftc_iih <- function(tbl,
                    vivienda_tbl = NULL,
                    return_households = FALSE,
                    include_details = FALSE,
                    filter_valid_households = FALSE) {
  calculate_iih_encft(
    data = tbl,
    vivienda_data = vivienda_tbl,
    return_households = return_households,
    include_details = include_details,
    filter_valid_households = filter_valid_households
  )
}

encft_iih_required_columns <- function() {
  c(
    "PERIODO", "MES", "VIVIENDA", "HOGAR", "ID_HOGAR", "MIEMBRO",
    "ID_PROVINCIA", "ZONA", "PARENTESCO", "SEXO", "EDAD",
    "SABE_LEER_ESCRIBIR", "ULTIMO_ANO_APROBADO", "NIVEL_ULTIMO_ANO_APROBADO",
    "NIVEL_SE_MATRICULO", "PORQUE_NO_ESTUDIA", "TIPO_CENTRO_ESTUDIOS",
    "OCUPADO", "DESOCUPADO", "INACTIVO", "CATEGORIA_PRINCIPAL",
    "AFILIADO_SEGURO_SALUD_PRINC",
    "MATERIAL_PISO", "MATERIAL_PARED_EXTERIOR", "MATERIAL_TECHO",
    "DONDE_PROVIENE_AGUA", "TIPO_SANITARIO", "TIPO_ALUMBRADO",
    "COMO_ELIMINA_BASURA", "COMBUSTIBLE_PARA_COCINAR", "TIPO_VIVIENDA",
    "TENENCIA_VIVIENDA", "ESTUFA", "REFRIGERADOR", "TELEVISOR", "LAVADORA",
    "AUTOMOVIL", "AIRE_ACONDICIONADO", "HORNO_MICROONDAS", "COMPUTADOR",
    "CELULAR", "TELEFONO", "INVERSOR", "CANT_DORMITORIOS_VIVIENDA"
  )
}


encft_iih_optional_columns <- function() {
  c("MATERIAL_PARED_EXTERIOR_ESP")
}


encft_iih_output_columns <- function(include_details = TRUE) {
  base_cols <- c(
    "calc_id", "PERIODO", "ID_HOGAR",
    "ingpctransln_calc_ecuacion_NCRO_50var_aj_C37_d1010_C939",
    "ingresopc_oficial_NCRO_calc_C37_d1010_C939",
    "pobofizona_ingresopc_oficial_NCRO_calc_C37_d1010_C939",
    "IIH"
  )

  if (!include_details) {
    return(base_cols)
  }

  c(
    base_cols,
    "hconmissing",
    "MATERIAL_PARED_EXTERIOR2",
    "escojefeviviS3", "escoviviS3", "alfaviviS3",
    "pninme5viviS3", "asiscolviviS3", "conyjefeviviS3",
    "enveviviS3", "mujeviviS3", "pobmen12viviS3", "pobmen17viviS3",
    "seguroviviS3", "nini1524viviS3", "tipohogviviS3",
    "ciclofamviviS3", "trabajoviviS3", "sexjefeviviS3",
    "pisoviviS3", "pareviviS3", "techviviS3", "luzviviS3",
    "basuviviS3", "cociviviS3", "basicos", "modernos", "equiviviS3",
    "saniviviS3", "aguaviviS3", "aguaviviS3A", "hacinadoviviS3",
    "TIPOVIviS3", "teneviviS3",
    "TELEFONO0", "CELULAR0", "MICROHONDA0", "ESTUFA0", "REFRIGERADOR0",
    "LAVADORA0", "TELEVISOR0", "RADIO0", "PLANTA_ELECTRICA0", "aire0",
    "auto0", "computador0", "ZONA0", "personas0", "dormitorios0",
    "Inversor0", "tipocentro0", "edadmediacat",
    "condicion_laboral_jefe", "condicion_laboral_pob_entropy_cat",
    "condicion_laboral_pob_0_cat", "condicion_laboral_pob_1_cat",
    "condicion_laboral_pob_2_cat", "condicion_laboral_pob_3_cat",
    "condicion_laboral_pob_4_cat", "condicion_laboral_pob_5_cat",
    "condicion_laboral_pob_6_cat",
    "lindofizona2019oficial2012jun", "lpobofizona2019oficial2012jun",
    "ingpctransln_calc_ecuacion_RCNO_C37_50var",
    "ingresopc_oficial_RCNOC37_calc_ecuacion",
    "pobofizona_ingresopc_oficial_RCNOC37_calc_ecuacion",
    "ingpcfoctransln_RCNOC30_calc_ecuacion",
    "ingresopc_focal_RCNOC30_calc_ecuacion",
    "pobofizona_ingresopc_focal_RCNOC30_calc_ecuacion",
    "ingpcfoctransln_RCNC30_calc_ecuacion",
    "ingresopc_focal_RCNC30_calc_ecuacion",
    "pobofizona_ingresopc_focal_RCNC30_calc_ecuacion"
  )
}


encft_iih_to_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}


encft_iih_normalize_text <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(trimws(x))
  gsub("[[:space:]]+", " ", x)
}

encft_iih_format_key <- function(x) {
  x_num <- encft_iih_to_numeric(x)
  ifelse(is.na(x_num), NA_character_, format(x_num, scientific = FALSE, trim = TRUE))
}


encft_iih_build_calc_id <- function(df) {
  periodo <- encft_iih_format_key(df$PERIODO)
  hogar <- encft_iih_format_key(df$ID_HOGAR)

  if (anyNA(periodo) || anyNA(hogar)) {
    stop("PERIODO e ID_HOGAR deben estar presentes para construir la llave de calculo")
  }

  paste(periodo, hogar, sep = "__")
}


encft_iih_validate_columns <- function(data) {
  missing_cols <- setdiff(encft_iih_required_columns(), names(data))

  if (length(missing_cols) > 0) {
    stop(
      "Faltan columnas requeridas en la ENCFT: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  invisible(data)
}


encft_iih_resolve_inputs <- function(data, vivienda_data = NULL) {
  if (is.data.frame(data)) {
    return(list(data = data, vivienda_data = vivienda_data))
  }

  if (!is.list(data)) {
    stop("data debe ser un data.frame o una lista con un componente de miembros")
  }

  members_name <- c("miembros", "members", "personas", "data")
  members_name <- members_name[members_name %in% names(data)][1]

  if (is.na(members_name)) {
    stop("No se encontro un componente de miembros en la lista de entrada")
  }

  if (is.null(vivienda_data) && "vivienda" %in% names(data)) {
    vivienda_data <- data$vivienda
  }

  list(data = data[[members_name]], vivienda_data = vivienda_data)
}


encft_iih_complete_input <- function(data, vivienda_data = NULL) {
  encft_iih_validate_columns(data)

  for (col in setdiff(encft_iih_optional_columns(), names(data))) {
    data[[col]] <- NA_character_
  }

  data <- data %>%
    dplyr::mutate(
      MATERIAL_PARED_EXTERIOR_ESP = as.character(MATERIAL_PARED_EXTERIOR_ESP)
    )

  required_vivienda_cols <- c("PERIODO", "VIVIENDA", "MATERIAL_PARED_EXTERIOR_ESP")

  if (is.null(vivienda_data) || !all(required_vivienda_cols %in% names(vivienda_data))) {
    return(data)
  }

  vivienda_lookup <- vivienda_data %>%
    dplyr::transmute(
      .join_periodo = encft_iih_to_numeric(PERIODO),
      .join_vivienda = encft_iih_to_numeric(VIVIENDA),
      MATERIAL_PARED_EXTERIOR_ESP_VIV = as.character(MATERIAL_PARED_EXTERIOR_ESP)
    ) %>%
    dplyr::distinct(.join_periodo, .join_vivienda, .keep_all = TRUE)

  data %>%
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      .join_periodo = encft_iih_to_numeric(PERIODO),
      .join_vivienda = encft_iih_to_numeric(VIVIENDA)
    ) %>%
    dplyr::left_join(vivienda_lookup, by = c(".join_periodo", ".join_vivienda")) %>%
    dplyr::mutate(
      MATERIAL_PARED_EXTERIOR_ESP = dplyr::coalesce(
        MATERIAL_PARED_EXTERIOR_ESP,
        MATERIAL_PARED_EXTERIOR_ESP_VIV
      )
    ) %>%
    dplyr::arrange(.row_id) %>%
    dplyr::select(-.row_id, -.join_periodo, -.join_vivienda, -MATERIAL_PARED_EXTERIOR_ESP_VIV)
}


encft_iih_wall_material_lookup <- local({
  lookup <- 
  c(`ARENA CEMENTO ALAMBRE Y MADERA` = 4L, `ARENA Y CEMENTO` = 4L, 
  `ARENA Y VARILLA Y CEMENTO` = 4L, `ARENA, CEMENTO, ALAMABRE Y MADERA` = 7L, 
  `ARENA, CEMENTO, ALAMBRE Y MADERA` = 7L, `ARENA, VARILA, CEMENTO Y ALAMBRE` = 4L, 
  `ARENA, VARILLA, CEMENTO Y ALAMBRE` = 4L, `BACIADO DE CEMENTO Y ARENA` = 4L, 
  `BACIADO DE CEMENTO Y ARENAS` = 4L, `BACIADO DE CEMENTO, ARENA Y VARILLA` = 4L, 
  `BACIADO DE CEMENTO, ARENA, VARILLA` = 4L, `BACIADO DE CEMENTO, VARILLA Y ARENA` = 4L, 
  `BAIADO DE CEMENTO` = 4L, `BASIADO Y MADERA` = 7L, `BLOCK MADERA` = 7L, 
  `BVACIADO DE CEMENTO Y ARENA` = 4L, `CEEMNTO Y ARENA` = 4L, `CEMENTO ARENA SIN VARILLA` = 4L, 
  `CEMENTO SIN VARILLA` = 4L, `CEMENTO, ARENA, ALAMBRE Y MADERA` = 7L, 
  `CEMENTO, VARILLA, ALAMBRE, ARENA` = 4L, `COCRETO VACIADO` = 4L, 
  `CONCRETO BACIADO` = 4L, `CONCRETO DE ARENA Y CEMENTO` = 4L, 
  `CONCRETO NACIADO` = 4L, `CONCRETO VACIADO` = 4L, `CONCRETO VACIADO DE ARENA Y CEMENTO` = 4L, 
  `CONCRETO VACIANDO` = 4L, `CONCRETO VARIADO` = 4L, `CONCRETO VASIADO` = 4L, 
  CONCRETRO = 4L, `MEZCLA DE CEMENTO Y ARENA` = 4L, `VACADO DE CEMENTO Y ARENA` = 4L, 
  `VACAIDO DE ARENA Y CEMENTO` = 4L, `VACAIDO DE CEMENTO` = 4L, 
  `VACEADI DE CEMENTO Y ARENA` = 4L, `VACEADO BLOCK Y CEMENTO` = 4L, 
  `VACEADO CEMENTO Y ARENA MADERA` = 7L, `VACEADO CEMENTO Y VARILLA` = 4L, 
  `VACEADO CEMENTO YARENA` = 4L, `VACEADO CEMETO Y ARENA` = 4L, 
  `VACEADO CEMNTO Y ARENA` = 4L, `VACEADO CONCRETO` = 4L, `VACEADO DE ARENA Y CEMENTO` = 4L, 
  `VACEADO DE CEMENTO` = 4L, `VACEADO DE CEMENTO Y ARENA` = 4L, 
  `VACEADO DE CEMENTO Y MADERA` = 7L, `VACEADO DE CEMENTO Y VARILLA` = 4L, 
  `VACEADO DE CEMETO Y ARENA` = 4L, `VACEADO DE CONCRETO` = 4L, 
  `VACEDO CEMENTO Y ARENA` = 4L, `VACEDO DE CEMNTO` = 4L, `VACIA DE ARENA Y CEMENTO` = 4L, 
  `VACIADIO DE ARENA Y CEMENTO` = 4L, VACIADO = 4L, `VACIADO ALAMBRE CEMENTO ARENA, MADERO` = 7L, 
  `VACIADO ALAMBRE Y CONCRETO, MADERA` = 7L, `VACIADO ALAMBRE, Y CONCRETO Y MADERA` = 7L, 
  `VACIADO ARENA ALAMBRE Y VARILLA` = 4L, `VACIADO ARENA ALAMBRE Y VARILLA Y CEMENTO` = 4L, 
  `VACIADO ARENA CEMENTO VARILLA Y ALAMBRE` = 4L, `VACIADO ARENA CEMENTO Y VARILLA` = 4L, 
  `VACIADO ARENA VARILLA ALAMBRE Y CEMENTO` = 4L, `VACIADO ARENA VARILLA CEMENTO` = 4L, 
  `VACIADO ARENA VARILLA Y CEMENTO` = 4L, `VACIADO ARENA VARRILA Y CEMENTO` = 4L, 
  `VACIADO ARENA VASILLA CEMENTE` = 4L, `VACIADO ARENA Y CEMENTO` = 4L, 
  `VACIADO ARENA Y CEMENTO, VARRILLA, ALMABRE` = 4L, `VACIADO ARENA Y CMENTO` = 4L, 
  `VACIADO ARENA, CEMENTO,` = 4L, `VACIADO ARENA, VARILLA Y ALAMBRE` = 4L, 
  `VACIADO ARENA, VARILLA Y CEMENTO` = 4L, `VACIADO ARENA, VARILLA, CEMENTO` = 4L, 
  `VACIADO CAMENTO Y ARENA` = 4L, `VACIADO CEMENTO` = 4L, `VACIADO CEMENTO ARENA` = 4L, 
  `VACIADO CEMENTO ARENA ALAMBRE Y MADERA` = 7L, `VACIADO CEMENTO Y ARE4NA` = 4L, 
  `VACIADO CEMENTO, ALAMBRE, MADERA` = 7L, `VACIADO CEMENTO, ARENA, ALAMBRE Y MADERA` = 7L, 
  `VACIADO CON CEMENTO` = 4L, `VACIADO CON CEMENTO SIN VARILLAS` = 4L, 
  `VACIADO CONCRETO` = 4L, `VACIADO CONCRETO Y ALAMBRE MADERA` = 7L, 
  `VACIADO CONCRETO Y MADERO` = 4L, `VACIADO DE ALAMBRE CON CEMENTO Y MADERA` = 7L, 
  `VACIADO DE ALAMBRE VARILLA Y ARENA` = 4L, `VACIADO DE ALAMBRE Y CONCRETO Y MADERA` = 7L, 
  `VACIADO DE ALAMBRES Y CONCRETOS` = 4L, `VACIADO DE AREAN Y CEMENTO` = 4L, 
  `VACIADO DE ARENA` = 4L, `VACIADO DE ARENA ALAMBRA` = 4L, `VACIADO DE ARENA ALAMBRADO Y VARILLA` = 4L, 
  `VACIADO DE ARENA ALAMBRE Y MADERA` = 7L, `VACIADO DE ARENA ALAMBRE Y VARILLA` = 4L, 
  `VACIADO DE ARENA CEMENTO` = 4L, `VACIADO DE ARENA CON CEMENTO` = 4L, 
  `VACIADO DE ARENA VARILLA` = 4L, `VACIADO DE ARENA Y ALAMBRE` = 4L, 
  `VACIADO DE ARENA Y ALAMBRE Y CEMENTO` = 4L, `VACIADO DE ARENA Y ALAMBRES` = 4L, 
  `VACIADO DE ARENA Y CEMENO` = 4L, `VACIADO DE ARENA Y CEMENTA` = 4L, 
  `VACIADO DE ARENA Y CEMENTE` = 4L, `VACIADO DE ARENA Y CEMENTO ALAMBRE Y VARILLA` = 4L, 
  `VACIADO DE ARENA Y CEMENTO SORRILLA Y ALAMBRE` = 4L, `VACIADO DE ARENA Y CEMENTO Y VARILLA Y CEMENTO` = 4L, 
  `VACIADO DE ARENA Y CEMENTO.` = 4L, `VACIADO DE ARENA Y CEMENTOC` = 4L, 
  `VACIADO DE ARENA Y CMENTO` = 4L, `VACIADO DE ARENA Y DE CEMENTO` = 4L, 
  `VACIADO DE ARENA Y VARILLA` = 4L, `VACIADO DE ARENA YCEMENTO` = 4L, 
  `VACIADO DE ARENA, ALAMBRE VARILLA Y CEMENTO` = 4L, `VACIADO DE ARENA, ALAMBRE Y VARILLA` = 4L, 
  `VACIADO DE ARENA, BARILLA Y CEMENTO` = 4L, `VACIADO DE ARENA, CEMENTO Y BERILLAS` = 4L, 
  `VACIADO DE ARENA, CEMENTO Y VARILLA` = 4L, `VACIADO DE ARENA, CEMENTO, ALAMBRE` = 4L, 
  `VACIADO DE ARENA, CEMENTO, ARENA` = 4L, `VACIADO DE ARENA, CEMENTO, VARILLA, ALAMBRE` = 4L, 
  `VACIADO DE ARENA, LAMBRE Y VARILLA` = 4L, `VACIADO DE ARENA, VARILLA Y CEMENTO` = 4L, 
  `VACIADO DE ARENA, VARILLA, ALAMBRE Y CEMENTO` = 4L, `VACIADO DE ARENA,VARRILLA Y ALAMBRE` = 4L, 
  `VACIADO DE ARENA. ALAMBRE, CEMENTO` = 4L, `VACIADO DE ARENAY ALAMBRE` = 4L, 
  `VACIADO DE ARGRO, GRAVA Y VARILLA CEMENTO` = 4L, `VACIADO DE ATRENA Y CEMENTO` = 4L, 
  `VACIADO DE CAMENTO Y ARADO` = 4L, `VACIADO DE CEMENETO Y VARILLA` = 4L, 
  `VACIADO DE CEMENTIO Y ARENA` = 4L, `VACIADO DE CEMENTO , ALAMBRE Y MADERA` = 7L, 
  `VACIADO DE CEMENTO ARENA` = 4L, `VACIADO DE CEMENTO AY ARENA` = 4L, 
  `VACIADO DE CEMENTO TY ARENA` = 4L, `VACIADO DE CEMENTO Y AREANA` = 4L, 
  `VACIADO DE CEMENTO Y ARENA Y VARILLA` = 4L, `VACIADO DE CEMENTO Y ARENAS` = 4L, 
  `VACIADO DE CEMENTO Y MADERA` = 4L, `VACIADO DE CEMENTO, ALAMBRE, ARENA Y MADERA` = 7L, 
  `VACIADO DE CEMENTO, ARENA Y BARILLA` = 4L, `VACIADO DE CEMENTO, ARENA Y BARILLAS` = 4L, 
  `VACIADO DE CEMENTO, ARENA Y VARILLA` = 4L, `VACIADO DE CEMENTO, ARENA, ALAMBRE Y MADERA` = 7L, 
  `VACIADO DE CEMENTO, ARENA, BARILLAS` = 4L, `VACIADO DE CEMENTO, ARENA, CEMENTO` = 4L, 
  `VACIADO DE CEMENTO, VARILLA, CEMENTO` = 4L, `VACIADO DE CEMNETO` = 4L, 
  `VACIADO DE CEMNETO Y ARENA` = 4L, `VACIADO DE CMENTO Y ARENA` = 4L, 
  `VACIADO DE CONCRETO Y ARENA` = 4L, `VACIADO DE CONCRETO, ALAMBRE Y MADERA` = 7L, 
  `VACIADO DE DE CONCRETO MADERA` = 7L, `VACIADO DE MADERA` = 7L, 
  `VACIADO DE MEZCLA CEMENTO Y ARENA` = 4L, `VACIADO DE MEZCLA DE CEMENTO` = 4L, 
  `VACIADO DE MEZCLA DE CEMENTO Y ARENA` = 4L, `VACIADO DE MEZCLA Y CEMENTO` = 4L, 
  `VACIADO DE MEZLCA DE CEMENTOS` = 4L, `VACIADO DE TABLA Y PALMA` = 7L, 
  `VACIADO DE VARILLA, ARENA Y ALAMBRE` = 4L, `VACIADO EN CEMENTO` = 4L, 
  `VACIADO EN CEMENTO Y ARENA` = 4L, `VACIADO EN CEMENTO Y VARILLA` = 4L, 
  `VACIADO MEZCLA CEMENTO Y ARENA` = 4L, `VACIADO MEZCLA DE ARENA Y CEMENTO` = 4L, 
  `VACIADO MEZCLA DE CEMENTO` = 4L, `VACIADO MEZCLA DE CEMENTO ARMADO Y ARENA` = 4L, 
  `VACIADO MEZCLA DE CEMENTO Y ARENA` = 4L, `VACIADO MEZLCA ARENA Y CEMENTO` = 4L, 
  `VACIADO Y ARENA` = 4L, `VACIADO Y CEMENTO` = 4L, `VACIADO Y CEMENTO DE ARENA` = 4L, 
  `VACIADO Y CEMENTO Y ARENA` = 4L, `VACIADO Y MADERA` = 4L, `VACIADO Y TABLA DE PALMA` = 4L, 
  `VACIADO, ARENA Y CEMENTO` = 4L, `VACIADO, ARENA, VARILLA Y ALAMBRE` = 4L, 
  `VACIADO, CEMENTO Y ARENA` = 4L, `VACIADO, CEMENTO Y ARENAS` = 4L, 
  `VACIADO, CEMENTO, ARENA Y VARILLA` = 4L, `VACIADO, CEMENTO, Y ARENA` = 4L, 
  `VACIADO, MEZCLA DE CEMENTO Y ARENA` = 4L, `VACIADODE ARENA Y CEMENTO` = 4L, 
  `VACIADOR DE ARENA Y CEMENTO` = 4L, `VACIADOR DE MEZCLA CEMENTO` = 4L, 
  `VACIADOS DE ARENA Y CEMENTO` = 4L, `VACIADOS EN CEMENTO` = 4L, 
  VACIANDO = 4L, `VACIANDO DE ARENA Y CEMENTO` = 4L, `VACIAO MEZCLA DE CEENTI Y ARENA` = 4L, 
  `VACIJA BUENA CEMENTO Y MADERA` = 4L, `VACIOADO DE ARENA Y CEMENTO` = 4L, 
  `VACIODO DE ARENA Y CEMENTO` = 4L, `VACIODO DE CEMENTO Y ARENA` = 4L, 
  `VAIADO DE CEMENTOS` = 4L, `VARILLA CEMENTO ALAMBRE Y ARENA` = 4L, 
  `VARILLA,CEMENTO, ARENA Y ALAMBRE` = 4L, `VECIADO DE ARENA Y CEMENTO` = 4L, 
  `VIACIADO DE ARENA Y CEMENTO` = 4L, `MAJA REBESTIDA DE CEMENTO Y PLYWOOD` = 7L, 
  `MAJA REBESTIDO DE CEMENTO PLYWOOD` = 7L, `MATA REBESTIDA DE CEMENTO Y PLYWOOD` = 7L, 
  `MATA REBISTIDORA DE CEMENTO Y PLYWOOD` = 7L, `MATA REVESTIDA CON CEMENTO Y PLYWOOD` = 7L, 
  `MAYA CON CEMENTO Y PLYWOOD` = 7L, `MAYA DE REBESTIDO DE CEMENTO Y PLYWOOD` = 7L, 
  `MAYA REBESTIBLE DE CEMENTO Y PLYWOOD` = 7L, `MAYA REBESTIDA` = 7L, 
  `MAYA REBESTIDA CON CEMENTO Y PLYWOOD` = 7L, `MAYA REBESTIDA DE CEMENTO PLYWOOD` = 7L, 
  `MAYA REBESTIDA DE CEMENTO Y PLEYWOOD` = 7L, `MAYA REBESTIDA DE CEMENTO Y PLYWOOD` = 7L, 
  `MAYA REBESTIDAS DE CEMENTO Y PLYWOOD` = 7L, `MAYA REBESTIDO DE CEMENTO Y PLYWOOD` = 7L, 
  `MAYA REVERCIBLE Y PLYWOOD` = 7L, `MAYA REVESTIBLE DE CEMENTO Y PLEWOOD` = 7L, 
  `MAYA REVESTIBLE DE CEMENTO Y PLYWOOD` = 7L, `MAYA REVESTIDA CEMENTO Y PLEWOOD` = 7L, 
  `MAYA REVESTIDA DE CEMENTO Y PLAYWOOD` = 7L, `MAYA REVESTIDA DE CEMENTO Y PLYWOOD` = 7L, 
  `MAYA REVESTIDA POR CEMENTO Y PLAYWOOD` = 7L, `MAYA REVUELTA` = 7L, 
  `MAYOR REVESTIDO CEMENTO Y PLAYWOOD` = 7L, `REBESTIDA DE CEMENTO Y PLYWOOD` = 7L, 
  `VARILLA, ARENA, CEMENTO, ALAMBRE, MADERA` = 7L, `VECIADO DE CEMENETO Y MADERA` = 7L, 
  `CEMENTO Y AREN` = 4L, `CEMENTO Y PLAYWODD` = 7L, `CEMENTO Y PLAYWOOD` = 7L, 
  `PLAQUETAS PREFABRICACION` = 7L, `PLAQUETAS PREFABRICADA` = 7L, 
  `VACEADO CEMENTO Y ARENA` = 4L, `VACIADO CEMENTO Y ARENA, MADERA` = 7L, 
  `VACIADO DE ARENA VARILLA MADERA Y CEMENTO` = 7L, `VACIADO DE ARENA Y CEMENTO, VARILLA Y MADERA` = 7L, 
  `VACIADO DE ARENA, CEMENTO, VARILLA, MADERA` = 7L, `VACIADO DE ARENA, VARILLA, CEMENTO Y MADERA` = 7L, 
  `VACIADO DE CEENTO Y ARENA` = 4L, `VACIADO DE CEMENRO Y ARENA` = 4L, 
  `VACIADO DE CEMENTO Y ARENA Y MADERA` = 7L, `VACIADO DE CEMENTO, VARILLA, MADERA` = 7L, 
  `VACIADO DE CEMENTYO Y ARENA` = 4L, `VACIADO DE CEMNTO Y ARENA` = 4L, 
  `VACIADO DE EMENTO Y ARENA.` = 4L, `VACIASDO DE CEMENTO Y ARENA` = 4L, 
  `VARIADO DE ARENA Y CEMENTO` = 4L, `VARIADO DE CEMENTO Y ARENA` = 4L, 
  `VACIADO DE ARENA, CEMENTO Y VARILLA, MADERA` = 4L, `VACIADO DE ARENA, CEMENTO, VARILLA Y MADERA` = 4L, 
  `CEMENTO ARENA Y MADERA` = 7L, `CEMENTO Y ARENA` = 4L, `CEMENTO Y ATRENA` = 4L, 
  `CEMENTOY ARENA` = 4L, `PLAQUETE PREFABRICADA` = 7L, `VACIADO ARENA ALAMBRE CEMENTO MADERA` = 7L, 
  `VACIADO ARENA CEMENTO ALAMBRE MADERA` = 7L, `VACIADO ARENA CEMENTO MADERA` = 7L, 
  `VACIADO ARENA CEMENTO VARILLA MADERA ALAMBRE` = 7L, `VACIADO ARENA MADERA VARILLA CEMENTO ALAMBRE` = 7L, 
  `VACIADO ARENA Y CEMENTO, VARILLA, MADERA` = 7L, `VACIADO ARENA, CEMENTO, VARILLA, MADERA` = 7L, 
  `VACIADO CEMENTI ARENA ALAMBRE Y MADERA` = 7L, `VACIADO CEMENTO VARILLA MADERA` = 7L, 
  `VACIADO CEMENTO Y ARENA` = 4L, `VACIADO CEMENTO Y HARINA , ALAMBRE, MADERA` = 7L, 
  `VACIADO DE ARENA Y CEMENTO` = 4L, `VACIADO DE ARENA Y VARILLA ALAMBRE CEMENTO` = 4L, 
  `VACIADO DE CEMENTO` = 4L, `VACIADO DE CEMENTO ARENA ALAMBRE Y MADERA` = 7L, 
  `VACIADO DE CEMENTO Y AREN` = 4L, `VACIADO DE CEMENTO Y ARENA` = 4L, 
  `VACIADO DE CEMENTO Y VARILLA` = 4L, `VACIADO DE CEMENTO, ARENA` = 4L, 
  `VACIADO DE CEMENTRO Y ARENA` = 4L, `VACIADO DEMENTO ALAMBRE ARENA Y MADERA` = 7L, 
  `VACIADO DEMENTO ARENA ALAMBRE Y MADERA` = 7L, `VACIOADO DE CEMENTO Y ARENA` = 4L, 
  `VACUADO DE CEMENTO Y ARENA` = 4L, `PLAQUETON PREFABRICADOR` = 7L, 
  `PLAQUETON PREPARADO` = 7L, `PLAQUETON RELOBRICADO` = 7L, `PLOQUETOR PREFABRICADO` = 7L, 
  `VICIADO DE ARENA` = 4L, `PLAQUEDOR PREPARADA` = 7L, `CHIROT Y VACIADO DE CEMENTO` = 7L, 
  `CHIROT Y VARIADO DE CEMENTO` = 7L, `CHIRROT Y CEMENTO` = 7L, 
  `PLAQUETA FRE FABRICADA` = 7L, `PLAQUETAR PREFABRICADA` = 7L, 
  `PLAQUETAR PREFABRUICADA` = 7L, `PLAQUETAS PRE FABRICADA` = 7L, 
  `PLAQUETAS PRE FABRICADAS` = 7L, `PLAQUETAS PRE FABRICADO` = 7L, 
  PLAQUETON = 7L, `PLAQUETON PRE FABRICACION` = 7L, `PLAQUETON PRE FABRICADO` = 7L, 
  `PREFABRICADA DE PLAQUETAS` = 7L, `PAQUETA PREFABRICADA` = 7L, 
  `PLAQUETA PRE FABRICADA` = 7L, `PLAQUETA PREFABRICABA` = 7L, 
  `PLAQUETA PREFABRICACDA` = 7L, `PLAQUETA PREFABRICADO` = 7L, 
  `PLAQUETAS PREFABRICADAS` = 7L, `MAYA REBESTIBLE CON CEMENTO` = 7L, 
  `MAYA REBESTIDA DE CEMENTO` = 7L, `MAYA REBESTIDA DE CEMENTO Y PLAYWOOD` = 7L, 
  `MAYA REBESTIDA DE CEMETO` = 7L, `MAYA REBESTIDA Y PLYWOOD` = 7L, 
  `MAYA REVESTIDA DE CEMENTO` = 7L, `PLAQUETA GORDA` = 7L, `PLAQUETA PREFABRICADA` = 7L, 
  `PLAQUETON PREFABRICADA` = 7L, `PLAQUETON PREFABRICADO` = 7L, 
  `PLAQUETON PREFABRICADO CARTON` = 7L, `VACIADO DE ARENA CEMENTO Y TABLA` = 7L, 
  `VACIADIO DE ARENA Y CEMENTO, TABLA Y PALMA` = 7L, `VACIADO DE ARENA Y CEMENTO Y TABLA DE PALMA` = 7L, 
  `MADERA REBESTIDA DE CEMENTO Y PLAYWOOD` = 7L, `PIEDRA Y CEMENTO` = 7L
  )
  function() lookup
})

encft_iih_normalize_wall_material <- function(raw, other_text) {
  raw_num <- encft_iih_to_numeric(raw)
  other_norm <- encft_iih_normalize_text(other_text)

  corrected <- raw_num
  corrected[raw_num == 14] <- 4

  is_other <- raw_num == 99
  lookup <- encft_iih_wall_material_lookup()
  lookup_value <- unname(lookup[other_norm])
  has_lookup <- !is.na(lookup_value)
  corrected[is_other & has_lookup] <- lookup_value[is_other & has_lookup]

  unresolved_other <- is_other & !has_lookup
  mentions_concrete <- grepl(
    "VACIAD|VACEAD|VACED|CONCRETO|CEMENTO|MEZCLA|ARENA|VARILLA|ALAMBRE|BLOCK",
    other_norm
  )
  mentions_prefab <- grepl(
    "PLAQUET|PREFABRIC|MAYA REB|MAYA REV|CHIRROT|PLAYWOOD|PLYWOOD|TABLA|PALMA|MADERA|PIEDRA Y CEMENTO",
    other_norm
  )

  corrected[unresolved_other & mentions_prefab] <- 7
  corrected[unresolved_other & mentions_concrete & !mentions_prefab] <- 4

  corrected
}

encft_iih_detect_secondary_base <- function(periodo, level, grade) {
  education_layout <- dplyr::tibble(
    anio = floor(encft_iih_to_numeric(periodo) / 100),
    level = encft_iih_to_numeric(level),
    grade = encft_iih_to_numeric(grade)
  ) %>%
    dplyr::group_by(anio) %>%
    dplyr::summarise(
      primary_max_grade = suppressWarnings(max(grade[level == 2], na.rm = TRUE)),
      secondary_max_grade = suppressWarnings(max(grade[level %in% c(3, 4)], na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(primary_max_grade, secondary_max_grade),
        ~ dplyr::if_else(is.infinite(.x), NA_real_, .x)
      ),
      # ENCFT workbooks use at least two schooling layouts:
      # 2019-style primaria/secundaria 6+6 and 2021-style 8+4.
      secondary_base = dplyr::case_when(
        primary_max_grade >= 8 & secondary_max_grade <= 4 ~ 8,
        primary_max_grade <= 6 & secondary_max_grade >= 6 ~ 6,
        primary_max_grade >= 8 ~ 8,
        TRUE ~ 6
      )
    ) %>%
    dplyr::select(anio, secondary_base)

  education_layout
}


encft_iih_years_of_education <- function(level, grade, age, secondary_base = 6) {
  level_num <- encft_iih_to_numeric(level)
  grade_num <- encft_iih_to_numeric(grade)
  age_num <- encft_iih_to_numeric(age)
  secondary_base_num <- encft_iih_to_numeric(secondary_base)

  dplyr::case_when(
    age_num <= 2 ~ 0,
    level_num == 99 ~ NA_real_,
    level_num %in% c(1, 9, 10) ~ 0,
    level_num == 2 ~ grade_num,
    level_num %in% c(3, 4) ~ secondary_base_num + grade_num,
    level_num == 5 ~ 12 + grade_num,
    level_num %in% c(6, 7, 8) ~ 16 + grade_num,
    TRUE ~ NA_real_
  )
}


encft_iih_prepare_persons <- function(data) {
  encft_iih_validate_columns(data)

  education_layout <- encft_iih_detect_secondary_base(
    periodo = data$PERIODO,
    level = data$NIVEL_ULTIMO_ANO_APROBADO,
    grade = data$ULTIMO_ANO_APROBADO
  )

  persons <- data %>%
    dplyr::mutate(
      calc_id = encft_iih_build_calc_id(.),
      dplyr::across(
        dplyr::all_of(encft_iih_required_columns()),
        ~ if (inherits(.x, "haven_labelled")) encft_iih_to_numeric(.x) else .x
      ),
      PERIODO = encft_iih_to_numeric(PERIODO),
      MES = encft_iih_to_numeric(MES),
      VIVIENDA = encft_iih_to_numeric(VIVIENDA),
      HOGAR = encft_iih_to_numeric(HOGAR),
      ID_HOGAR = encft_iih_to_numeric(ID_HOGAR),
      MIEMBRO = encft_iih_to_numeric(MIEMBRO),
      ID_PROVINCIA = encft_iih_to_numeric(ID_PROVINCIA),
      ZONA = encft_iih_to_numeric(ZONA),
      PARENTESCO = encft_iih_to_numeric(PARENTESCO),
      SEXO = encft_iih_to_numeric(SEXO),
      EDAD = encft_iih_to_numeric(EDAD),
      SABE_LEER_ESCRIBIR = encft_iih_to_numeric(SABE_LEER_ESCRIBIR),
      ULTIMO_ANO_APROBADO = encft_iih_to_numeric(ULTIMO_ANO_APROBADO),
      NIVEL_ULTIMO_ANO_APROBADO = encft_iih_to_numeric(NIVEL_ULTIMO_ANO_APROBADO),
      NIVEL_SE_MATRICULO = encft_iih_to_numeric(NIVEL_SE_MATRICULO),
      PORQUE_NO_ESTUDIA = encft_iih_to_numeric(PORQUE_NO_ESTUDIA),
      TIPO_CENTRO_ESTUDIOS = encft_iih_to_numeric(TIPO_CENTRO_ESTUDIOS),
      OCUPADO = encft_iih_to_numeric(OCUPADO),
      DESOCUPADO = encft_iih_to_numeric(DESOCUPADO),
      INACTIVO = encft_iih_to_numeric(INACTIVO),
      CATEGORIA_PRINCIPAL = encft_iih_to_numeric(CATEGORIA_PRINCIPAL),
      AFILIADO_SEGURO_SALUD_PRINC = encft_iih_to_numeric(AFILIADO_SEGURO_SALUD_PRINC),
      MATERIAL_PISO = encft_iih_to_numeric(MATERIAL_PISO),
      MATERIAL_PARED_EXTERIOR = encft_iih_to_numeric(MATERIAL_PARED_EXTERIOR),
      MATERIAL_TECHO = encft_iih_to_numeric(MATERIAL_TECHO),
      DONDE_PROVIENE_AGUA = encft_iih_to_numeric(DONDE_PROVIENE_AGUA),
      TIPO_SANITARIO = encft_iih_to_numeric(TIPO_SANITARIO),
      TIPO_ALUMBRADO = encft_iih_to_numeric(TIPO_ALUMBRADO),
      COMO_ELIMINA_BASURA = encft_iih_to_numeric(COMO_ELIMINA_BASURA),
      COMBUSTIBLE_PARA_COCINAR = encft_iih_to_numeric(COMBUSTIBLE_PARA_COCINAR),
      TIPO_VIVIENDA = encft_iih_to_numeric(TIPO_VIVIENDA),
      TENENCIA_VIVIENDA = encft_iih_to_numeric(TENENCIA_VIVIENDA),
      ESTUFA = encft_iih_to_numeric(ESTUFA),
      REFRIGERADOR = encft_iih_to_numeric(REFRIGERADOR),
      TELEVISOR = encft_iih_to_numeric(TELEVISOR),
      LAVADORA = encft_iih_to_numeric(LAVADORA),
      AUTOMOVIL = encft_iih_to_numeric(AUTOMOVIL),
      AIRE_ACONDICIONADO = encft_iih_to_numeric(AIRE_ACONDICIONADO),
      HORNO_MICROONDAS = encft_iih_to_numeric(HORNO_MICROONDAS),
      COMPUTADOR = encft_iih_to_numeric(COMPUTADOR),
      CELULAR = encft_iih_to_numeric(CELULAR),
      TELEFONO = encft_iih_to_numeric(TELEFONO),
      INVERSOR = encft_iih_to_numeric(INVERSOR),
      CANT_DORMITORIOS_VIVIENDA = encft_iih_to_numeric(CANT_DORMITORIOS_VIVIENDA),
      MATERIAL_PARED_EXTERIOR_ESP = as.character(MATERIAL_PARED_EXTERIOR_ESP),
      anio = floor(PERIODO / 100),
      idenprov = ID_PROVINCIA,
      inscrito0 = dplyr::case_when(
        EDAD <= 2 ~ NA_real_,
        NIVEL_SE_MATRICULO >= 1 & NIVEL_SE_MATRICULO <= 8 ~ 1,
        TRUE ~ 0
      ),
      inscrito = dplyr::case_when(
        EDAD <= 2 ~ NA_real_,
        NIVEL_SE_MATRICULO >= 1 & NIVEL_SE_MATRICULO <= 8 ~ 1,
        NIVEL_SE_MATRICULO == 9 & MES >= 6 & MES <= 8 & PORQUE_NO_ESTUDIA == 1 ~ 1,
        TRUE ~ 0
      ),
      inscrito6a14 = dplyr::case_when(
        EDAD <= 5 | EDAD >= 15 ~ NA_real_,
        TRUE ~ inscrito
      ),
      analfa = dplyr::case_when(
        EDAD <= 5 ~ NA_real_,
        SABE_LEER_ESCRIBIR == 1 ~ 0,
        SABE_LEER_ESCRIBIR <= 2 ~ 1,
        TRUE ~ NA_real_
      ),
      analfa15 = dplyr::if_else(EDAD >= 15, analfa, NA_real_),
    ) %>%
    dplyr::left_join(education_layout, by = "anio") %>%
    dplyr::mutate(
      anosedu1 = encft_iih_years_of_education(
        NIVEL_ULTIMO_ANO_APROBADO,
        ULTIMO_ANO_APROBADO,
        EDAD,
        secondary_base = secondary_base
      ),
      anosedu15 = dplyr::if_else(EDAD >= 15, anosedu1, NA_real_),
      educjefe = dplyr::if_else(PARENTESCO == 1, anosedu1, NA_real_),
      sexojefe = dplyr::if_else(PARENTESCO == 1, SEXO, NA_real_),
      alfabeto = dplyr::case_when(
        EDAD < 5 ~ NA_real_,
        analfa == 0 ~ 1,
        analfa == 1 ~ 0,
        is.na(analfa) & anosedu15 == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      alfa15 = dplyr::if_else(EDAD >= 15, alfabeto, NA_real_),
      alfajefe = dplyr::if_else(PARENTESCO == 1, alfabeto, NA_real_),
      pobmeno5 = dplyr::if_else(EDAD < 6, 1, 0),
      pobmeno6 = dplyr::if_else(EDAD <= 6, 1, 0),
      pob6a14 = dplyr::if_else(EDAD >= 6 & EDAD <= 14, 1, 0),
      asist614 = dplyr::case_when(
        EDAD < 6 | EDAD > 14 ~ NA_real_,
        inscrito == 1 ~ 1,
        inscrito == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      ocupa = dplyr::case_when(
        EDAD < 15 ~ 0,
        TRUE ~ OCUPADO
      ),
      tiencony = dplyr::if_else(PARENTESCO == 2, 1, 0),
      sexcony = dplyr::if_else(PARENTESCO == 2, SEXO, NA_real_),
      enve60 = dplyr::if_else(!is.na(EDAD) & EDAD >= 60, 1, 0),
      muje = dplyr::if_else(SEXO == 2, 1, 0),
      pobmen12 = dplyr::if_else(EDAD <= 12, 1, 0),
      pobmen17 = dplyr::if_else(EDAD <= 17, 1, 0),
      afilsegsal = dplyr::case_when(
        AFILIADO_SEGURO_SALUD_PRINC == 1 ~ 1,
        TRUE ~ 0
      ),
      ninijoven15a24 = dplyr::case_when(
        EDAD >= 15 & EDAD <= 24 & OCUPADO == 0 & inscrito == 0 ~ 1,
        EDAD >= 15 & EDAD <= 24 ~ 0,
        TRUE ~ NA_real_
      ),
      jefe = dplyr::if_else(PARENTESCO == 1, 1, 0),
      conyuge = dplyr::if_else(PARENTESCO == 2, 1, 0),
      hijo = dplyr::if_else(PARENTESCO == 3, 1, 0),
      hijastro = dplyr::if_else(PARENTESCO == 4, 1, 0),
      nieto = dplyr::if_else(PARENTESCO == 5, 1, 0),
      yerno = dplyr::if_else(PARENTESCO == 6, 1, 0),
      padre = dplyr::if_else(PARENTESCO == 7, 1, 0),
      suegro = dplyr::if_else(PARENTESCO == 8, 1, 0),
      hermano = dplyr::if_else(PARENTESCO == 9, 1, 0),
      abuelo = dplyr::if_else(PARENTESCO == 10, 1, 0),
      otrop = dplyr::if_else(PARENTESCO == 11, 1, 0),
      otronp = dplyr::if_else(PARENTESCO == 12, 1, 0),
      hijoc = dplyr::if_else(PARENTESCO %in% c(3, 4), 1, 0),
      conyugec = dplyr::if_else(PARENTESCO == 2 & EDAD >= 10, 1, 0),
      edadhijosc = dplyr::if_else(PARENTESCO %in% c(3, 4), EDAD, 999),
      edadconyc = dplyr::if_else(PARENTESCO == 2 & EDAD >= 10, EDAD, 999),
      edadjefec = dplyr::if_else(PARENTESCO == 1, EDAD, 999),
      sexconyc = dplyr::if_else(PARENTESCO == 2 & EDAD >= 10, SEXO, 9),
      sexjefec = dplyr::if_else(PARENTESCO == 1, SEXO, 9),
      alfaj0 = dplyr::case_when(
        PARENTESCO == 1 & SABE_LEER_ESCRIBIR == 1 ~ 1,
        PARENTESCO == 1 ~ 0,
        TRUE ~ NA_real_
      ),
      nivelunij0 = dplyr::case_when(
        PARENTESCO == 1 & NIVEL_ULTIMO_ANO_APROBADO >= 5 & NIVEL_ULTIMO_ANO_APROBADO <= 8 ~ 1,
        PARENTESCO == 1 ~ 0,
        TRUE ~ NA_real_
      ),
      patronj0 = dplyr::case_when(
        PARENTESCO == 1 & CATEGORIA_PRINCIPAL == 6 ~ 1,
        PARENTESCO == 1 ~ 0,
        TRUE ~ NA_real_
      ),
      categsalariadoj0 = dplyr::case_when(
        PARENTESCO == 1 & OCUPADO == 1 & CATEGORIA_PRINCIPAL <= 4 ~ 1,
        PARENTESCO == 1 ~ 0,
        TRUE ~ NA_real_
      ),
      tipocentro = dplyr::case_when(
        inscrito == 1 & TIPO_CENTRO_ESTUDIOS == 3 ~ 1,
        inscrito == 1 ~ 0,
        TRUE ~ NA_real_
      ),
      condicion_laboral_jefe = dplyr::case_when(
        PARENTESCO != 1 ~ NA_real_,
        DESOCUPADO == 1 ~ 0,
        INACTIVO == 1 ~ 1,
        OCUPADO == 1 & CATEGORIA_PRINCIPAL <= 4 ~ 2,
        OCUPADO == 1 & CATEGORIA_PRINCIPAL == 6 ~ 3,
        OCUPADO == 1 & CATEGORIA_PRINCIPAL == 8 ~ 4,
        OCUPADO == 1 & CATEGORIA_PRINCIPAL == 7 ~ 5,
        OCUPADO == 1 ~ 6,
        TRUE ~ NA_real_
      ),
      condicion_laboral_pob_dum_0 = dplyr::if_else(DESOCUPADO == 1, 1, 0),
      condicion_laboral_pob_dum_1 = dplyr::if_else(INACTIVO == 1, 1, 0),
      condicion_laboral_pob_dum_6 = dplyr::if_else(OCUPADO == 1, 1, 0),
      condicion_laboral_pob_dum_2 = dplyr::if_else(OCUPADO == 1 & CATEGORIA_PRINCIPAL <= 4, 1, 0),
      condicion_laboral_pob_dum_3 = dplyr::if_else(OCUPADO == 1 & CATEGORIA_PRINCIPAL == 6, 1, 0),
      condicion_laboral_pob_dum_4 = dplyr::if_else(OCUPADO == 1 & CATEGORIA_PRINCIPAL == 8, 1, 0),
      condicion_laboral_pob_dum_5 = dplyr::if_else(OCUPADO == 1 & CATEGORIA_PRINCIPAL == 7, 1, 0),
      condicion_laboral_pob_dum_7 = dplyr::if_else(EDAD <= 14, 1, 0),
      MATERIAL_PARED_EXTERIOR2 = encft_iih_normalize_wall_material(
        MATERIAL_PARED_EXTERIOR,
        MATERIAL_PARED_EXTERIOR_ESP
      )
    ) %>%
    dplyr::select(-anio, -secondary_base)

  persons
}


encft_iih_nan_to_na <- function(x) {
  ifelse(is.nan(x) | is.infinite(x), NA_real_, x)
}


encft_iih_aggregate_households <- function(persons) {
  persons %>%
    dplyr::group_by(calc_id) %>%
    dplyr::summarise(
      PERIODO = dplyr::first(PERIODO),
      VIVIENDA = dplyr::first(VIVIENDA),
      HOGAR = dplyr::first(HOGAR),
      ID_HOGAR = dplyr::first(ID_HOGAR),
      ZONA = dplyr::first(ZONA),
      idenprov = dplyr::first(idenprov),
      MATERIAL_PISO = dplyr::first(MATERIAL_PISO),
      MATERIAL_PARED_EXTERIOR2 = dplyr::first(MATERIAL_PARED_EXTERIOR2),
      MATERIAL_TECHO = dplyr::first(MATERIAL_TECHO),
      TIPO_ALUMBRADO = dplyr::first(TIPO_ALUMBRADO),
      DONDE_PROVIENE_AGUA = dplyr::first(DONDE_PROVIENE_AGUA),
      COMO_ELIMINA_BASURA = dplyr::first(COMO_ELIMINA_BASURA),
      COMBUSTIBLE_PARA_COCINAR = dplyr::first(COMBUSTIBLE_PARA_COCINAR),
      TIPO_SANITARIO = dplyr::first(TIPO_SANITARIO),
      TIPO_VIVIENDA = dplyr::first(TIPO_VIVIENDA),
      TENENCIA_VIVIENDA = dplyr::first(TENENCIA_VIVIENDA),
      ESTUFA = dplyr::first(ESTUFA),
      REFRIGERADOR = dplyr::first(REFRIGERADOR),
      TELEVISOR = dplyr::first(TELEVISOR),
      LAVADORA = dplyr::first(LAVADORA),
      AUTOMOVIL = dplyr::first(AUTOMOVIL),
      AIRE_ACONDICIONADO = dplyr::first(AIRE_ACONDICIONADO),
      HORNO_MICROONDAS = dplyr::first(HORNO_MICROONDAS),
      COMPUTADOR = dplyr::first(COMPUTADOR),
      CELULAR = dplyr::first(CELULAR),
      TELEFONO = dplyr::first(TELEFONO),
      INVERSOR = dplyr::first(INVERSOR),
      CANT_DORMITORIOS_VIVIENDA = dplyr::first(CANT_DORMITORIOS_VIVIENDA),
      educjefh = mean(educjefe, na.rm = TRUE),
      anosedu15h = mean(anosedu15, na.rm = TRUE),
      alfa15h = mean(alfa15, na.rm = TRUE),
      sexojefeh = mean(sexojefe, na.rm = TRUE),
      trabajah = mean(ocupa, na.rm = TRUE),
      pobmen5h = mean(pobmeno5, na.rm = TRUE),
      pobmen6h = sum(pobmeno6, na.rm = TRUE),
      asis614h = mean(asist614, na.rm = TRUE),
      pob614h = sum(pob6a14, na.rm = TRUE),
      alfaj0_min = min(alfaj0, na.rm = TRUE),
      nivelunij0_min = min(nivelunij0, na.rm = TRUE),
      patronj0_min = min(patronj0, na.rm = TRUE),
      categsalariadoj0_min = min(categsalariadoj0, na.rm = TRUE),
      tiencony_sum = sum(tiencony, na.rm = TRUE),
      sexcony_sum = mean(sexcony, na.rm = TRUE),
      enveh = sum(enve60, na.rm = TRUE),
      mujeh = mean(muje, na.rm = TRUE),
      pobmen12h = mean(pobmen12, na.rm = TRUE),
      pobmen17h = mean(pobmen17, na.rm = TRUE),
      ninijoven15a24_sum = sum(ninijoven15a24, na.rm = TRUE),
      ninijoven15a24_mean = mean(ninijoven15a24, na.rm = TRUE),
      seguromean = mean(afilsegsal, na.rm = TRUE),
      jefe_sum = sum(jefe, na.rm = TRUE),
      conyuge_sum = sum(conyuge, na.rm = TRUE),
      hijo_sum = sum(hijo, na.rm = TRUE),
      hijastro_sum = sum(hijastro, na.rm = TRUE),
      nieto_sum = sum(nieto, na.rm = TRUE),
      yerno_sum = sum(yerno, na.rm = TRUE),
      padre_sum = sum(padre, na.rm = TRUE),
      suegro_sum = sum(suegro, na.rm = TRUE),
      hermano_sum = sum(hermano, na.rm = TRUE),
      abuelo_sum = sum(abuelo, na.rm = TRUE),
      otrop_sum = sum(otrop, na.rm = TRUE),
      otronp_sum = sum(otronp, na.rm = TRUE),
      hijos_sum = sum(hijoc, na.rm = TRUE),
      conyug_sum = sum(conyugec, na.rm = TRUE),
      edadhijos_min = suppressWarnings(min(edadhijosc[edadhijosc != 999], na.rm = TRUE)),
      edadhijos_max = suppressWarnings(max(edadhijosc[edadhijosc != 999], na.rm = TRUE)),
      edadjefe_sum = sum(dplyr::if_else(edadjefec == 999, 0, edadjefec), na.rm = TRUE),
      edadconyuge_sum = sum(dplyr::if_else(edadconyc == 999, 0, edadconyc), na.rm = TRUE),
      sexjefe_sum = sum(dplyr::if_else(sexjefec == 9, 0, sexjefec), na.rm = TRUE),
      sexconyuge_sum = sum(dplyr::if_else(sexconyc == 9, 0, sexconyc), na.rm = TRUE),
      tipocentro_mean = mean(tipocentro, na.rm = TRUE),
      EDAD_mean = mean(EDAD, na.rm = TRUE),
      condicion_laboral_jefe = mean(condicion_laboral_jefe, na.rm = TRUE),
      condicion_laboral_pob_0 = mean(condicion_laboral_pob_dum_0, na.rm = TRUE),
      condicion_laboral_pob_1 = mean(condicion_laboral_pob_dum_1, na.rm = TRUE),
      condicion_laboral_pob_2 = mean(condicion_laboral_pob_dum_2, na.rm = TRUE),
      condicion_laboral_pob_3 = mean(condicion_laboral_pob_dum_3, na.rm = TRUE),
      condicion_laboral_pob_4 = mean(condicion_laboral_pob_dum_4, na.rm = TRUE),
      condicion_laboral_pob_5 = mean(condicion_laboral_pob_dum_5, na.rm = TRUE),
      condicion_laboral_pob_6 = mean(condicion_laboral_pob_dum_6, na.rm = TRUE),
      condicion_laboral_pob_7 = mean(condicion_laboral_pob_dum_7, na.rm = TRUE),
      personas = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(
          educjefh, anosedu15h, alfa15h, sexojefeh, trabajah, pobmen5h,
          asis614h, alfaj0_min, nivelunij0_min, patronj0_min,
          categsalariadoj0_min, sexcony_sum, mujeh, pobmen12h, pobmen17h,
          ninijoven15a24_mean, seguromean, tipocentro_mean, EDAD_mean,
          condicion_laboral_jefe, condicion_laboral_pob_0, condicion_laboral_pob_1,
          condicion_laboral_pob_2, condicion_laboral_pob_3,
          condicion_laboral_pob_4, condicion_laboral_pob_5,
          condicion_laboral_pob_6, condicion_laboral_pob_7,
          edadhijos_min, edadhijos_max
        ),
        encft_iih_nan_to_na
      )
    )
}


encft_iih_entropy <- function(...) {
  probs <- cbind(...)
  if (!is.matrix(probs)) {
    probs <- matrix(probs, ncol = length(list(...)))
  }
  probs[is.na(probs)] <- 0
  terms <- ifelse(probs > 0, probs * log(probs), 0)
  -rowSums(terms)
}


encft_iih_classify_households <- function(households) {
  households %>%
    dplyr::mutate(
      escojefeviviS3 = dplyr::case_when(
        educjefh == 0 ~ 1,
        educjefh >= 1 & educjefh < 4 ~ 2,
        educjefh >= 4 & educjefh < 8 ~ 3,
        educjefh >= 8 & educjefh < 12 ~ 4,
        educjefh >= 12 & educjefh < 16 ~ 5,
        educjefh >= 16 ~ 6,
        TRUE ~ 7
      ),
      escoviviS3 = dplyr::case_when(
        anosedu15h < 1 ~ 1,
        anosedu15h >= 1 & anosedu15h < 4 ~ 2,
        anosedu15h >= 4 & anosedu15h < 8 ~ 3,
        anosedu15h >= 8 & anosedu15h < 12 ~ 4,
        anosedu15h >= 12 & anosedu15h < 16 ~ 5,
        anosedu15h >= 16 ~ 6,
        TRUE ~ 7
      ),
      alfaviviS3 = dplyr::case_when(
        alfa15h == 0 ~ 1,
        alfa15h > 0 & alfa15h < 0.50 ~ 2,
        alfa15h >= 0.50 & alfa15h < 0.70 ~ 3,
        alfa15h >= 0.70 & alfa15h < 0.99 ~ 4,
        alfa15h >= 0.99 ~ 5,
        TRUE ~ 6
      ),
      pninme5viviS3 = dplyr::case_when(
        pobmen5h > 0.25 & pobmen5h <= 1 ~ 1,
        pobmen5h > 0 & pobmen5h <= 0.25 ~ 2,
        pobmen5h == 0 ~ 3,
        TRUE ~ 4
      ),
      asiscolviviS3 = dplyr::case_when(
        asis614h == 1 ~ 3,
        asis614h > 0 & asis614h < 1 ~ 2,
        asis614h == 0 ~ 1,
        is.na(asis614h) ~ 4,
        TRUE ~ 5
      ),
      conyjefeviviS3 = dplyr::case_when(
        sexojefeh == 1 & tiencony_sum %in% c(1, 2) ~ 1,
        sexojefeh == 2 & tiencony_sum == 1 ~ 2,
        sexojefeh == 1 & tiencony_sum == 0 ~ 3,
        sexojefeh == 2 & tiencony_sum == 0 ~ 4,
        TRUE ~ 5
      ),
      enveviviS3 = dplyr::case_when(
        enveh >= 2 ~ 1,
        enveh == 1 ~ 2,
        enveh == 0 ~ 3,
        TRUE ~ 9
      ),
      mujeviviS3 = dplyr::case_when(
        mujeh >= 0.50 ~ 1,
        mujeh > 0.25 & mujeh < 0.50 ~ 2,
        mujeh > 0 & mujeh <= 0.25 ~ 3,
        mujeh == 0 ~ 4,
        TRUE ~ 9
      ),
      pobmen12viviS3 = dplyr::case_when(
        pobmen12h >= 0.50 ~ 1,
        pobmen12h >= 0.25 & pobmen12h < 0.50 ~ 2,
        pobmen12h > 0 & pobmen12h < 0.25 ~ 3,
        pobmen12h == 0 ~ 4,
        TRUE ~ 9
      ),
      pobmen17viviS3 = dplyr::case_when(
        pobmen17h >= 0.75 ~ 1,
        pobmen17h >= 0.50 & pobmen17h < 0.75 ~ 2,
        pobmen17h >= 0.25 & pobmen17h < 0.50 ~ 3,
        pobmen17h > 0 & pobmen17h < 0.25 ~ 4,
        pobmen17h == 0 ~ 5,
        TRUE ~ 9
      ),
      seguroviviS3 = dplyr::case_when(
        seguromean == 0 ~ 1,
        seguromean > 0 & seguromean < 0.50 ~ 2,
        seguromean >= 0.50 ~ 3,
        TRUE ~ NA_real_
      ),
      nini1524viviS3 = dplyr::case_when(
        is.na(ninijoven15a24_mean) ~ 1,
        ninijoven15a24_mean == 1 ~ 2,
        ninijoven15a24_mean > 0 & ninijoven15a24_mean < 1 ~ 3,
        ninijoven15a24_mean == 0 ~ 4,
        TRUE ~ 8
      ),
      tipohogviviS3 = NA_real_,
      tipohogviviS3 = dplyr::if_else(personas == 2 & conyuge_sum == 1, 2, tipohogviviS3),
      tipohogviviS3 = dplyr::if_else(hijo_sum >= 1 | hijastro_sum >= 1, 3, tipohogviviS3),
      tipohogviviS3 = dplyr::if_else(
        conyuge_sum == 1 & (hijo_sum >= 1 | hijastro_sum >= 1),
        4,
        tipohogviviS3
      ),
      tipohogviviS3 = dplyr::if_else(
        nieto_sum >= 1 | yerno_sum >= 1 | padre_sum >= 1 |
          suegro_sum >= 1 | hermano_sum >= 1 | abuelo_sum >= 1 | otrop_sum >= 1,
        5,
        tipohogviviS3
      ),
      tipohogviviS3 = dplyr::if_else(otronp_sum >= 1, 6, tipohogviviS3),
      tipohogviviS3 = dplyr::if_else(
        conyuge_sum == 0 & hijo_sum == 0 & hijastro_sum == 0,
        7,
        tipohogviviS3
      ),
      tipohogviviS3 = dplyr::if_else(personas == 1, 1, tipohogviviS3),
      edadmadre = dplyr::case_when(
        conyug_sum == 1 & sexconyuge_sum == 2 ~ edadconyuge_sum,
        conyug_sum == 1 & sexjefe_sum == 2 ~ edadjefe_sum,
        TRUE ~ NA_real_
      ),
      difedadhijos = edadhijos_max - edadhijos_min,
      ciclofamviviS3 = NA_real_,
      ciclofamviviS3 = dplyr::if_else(conyug_sum == 0 & hijos_sum == 0, 1, ciclofamviviS3),
      ciclofamviviS3 = dplyr::if_else(
        conyug_sum == 1 & hijos_sum == 0 & edadmadre < 40,
        2,
        ciclofamviviS3
      ),
      ciclofamviviS3 = dplyr::if_else(hijos_sum >= 1 & edadhijos_max <= 5, 3, ciclofamviviS3),
      ciclofamviviS3 = dplyr::if_else(
        hijos_sum >= 1 & edadhijos_max >= 6 & edadhijos_max <= 12,
        4,
        ciclofamviviS3
      ),
      ciclofamviviS3 = dplyr::if_else(
        hijos_sum >= 1 & (
          (edadhijos_max >= 13 & edadhijos_max <= 18) |
            (edadhijos_min >= 13 & edadhijos_min <= 18)
        ),
        5,
        ciclofamviviS3
      ),
      ciclofamviviS3 = dplyr::if_else(hijos_sum >= 1 & difedadhijos >= 12, 5, ciclofamviviS3),
      ciclofamviviS3 = dplyr::if_else(hijos_sum >= 1 & edadhijos_min >= 19, 6, ciclofamviviS3),
      ciclofamviviS3 = dplyr::if_else(
        conyug_sum == 1 & hijos_sum == 0 & edadmadre >= 40,
        7,
        ciclofamviviS3
      ),
      ciclofamviviS3 = dplyr::if_else(is.na(ciclofamviviS3), 8, ciclofamviviS3),
      ciclofamviviS3 = dplyr::if_else(
        hijos_sum >= 1 &
          edadhijos_max >= 19 & edadhijos_max <= 23 &
          edadhijos_min >= 8 & edadhijos_min <= 12,
        5,
        ciclofamviviS3
      ),
      trabajoviviS3 = dplyr::case_when(
        trabajah == 0 ~ 1,
        trabajah > 0 & trabajah <= 0.30 ~ 2,
        trabajah > 0.30 & trabajah <= 0.60 ~ 3,
        trabajah > 0.60 & trabajah < 1 ~ 4,
        trabajah == 1 ~ 5,
        TRUE ~ 6
      ),
      sexjefeviviS3 = dplyr::if_else(sexojefeh == 2, 1, 2),
      tipocentro0 = dplyr::case_when(
        tipocentro_mean == 1 ~ 1,
        tipocentro_mean > 0 & tipocentro_mean < 1 ~ 2,
        tipocentro_mean == 0 ~ 3,
        TRUE ~ 4
      ),
      edadmediacat = dplyr::case_when(
        EDAD_mean < 20 ~ 1,
        EDAD_mean >= 20 & EDAD_mean < 30 ~ 2,
        EDAD_mean >= 30 & EDAD_mean < 50 ~ 3,
        EDAD_mean >= 50 ~ 4,
        TRUE ~ 9
      ),
      condicion_laboral_pob_entropy = encft_iih_entropy(
        condicion_laboral_pob_0,
        condicion_laboral_pob_1,
        condicion_laboral_pob_2,
        condicion_laboral_pob_3,
        condicion_laboral_pob_4,
        condicion_laboral_pob_5,
        condicion_laboral_pob_6,
        condicion_laboral_pob_7
      ),
      condicion_laboral_pob_entropy_cat = dplyr::case_when(
        condicion_laboral_pob_entropy == 0 ~ 0,
        condicion_laboral_pob_entropy > 0 & condicion_laboral_pob_entropy <= 1.00 ~ 1,
        condicion_laboral_pob_entropy > 1.00 & condicion_laboral_pob_entropy <= 1.27 ~ 2,
        condicion_laboral_pob_entropy > 1.27 & condicion_laboral_pob_entropy <= 1.45 ~ 3,
        condicion_laboral_pob_entropy > 1.45 ~ 4,
        TRUE ~ NA_real_
      ),
      condicion_laboral_pob_0_cat = dplyr::if_else(condicion_laboral_pob_0 > 0, 1, 0),
      condicion_laboral_pob_1_cat = dplyr::case_when(
        condicion_laboral_pob_1 == 0 ~ 0,
        condicion_laboral_pob_1 > 0 & condicion_laboral_pob_1 <= 0.40 ~ 1,
        condicion_laboral_pob_1 > 0.40 & condicion_laboral_pob_1 <= 0.60 ~ 2,
        condicion_laboral_pob_1 > 0.60 ~ 3,
        TRUE ~ NA_real_
      ),
      condicion_laboral_pob_2_cat = dplyr::case_when(
        condicion_laboral_pob_2 == 0 ~ 0,
        condicion_laboral_pob_2 > 0 & condicion_laboral_pob_2 <= 0.40 ~ 1,
        condicion_laboral_pob_2 > 0.40 & condicion_laboral_pob_2 <= 0.60 ~ 2,
        condicion_laboral_pob_2 > 0.60 ~ 3,
        TRUE ~ NA_real_
      ),
      condicion_laboral_pob_3_cat = dplyr::if_else(condicion_laboral_pob_3 > 0, 1, 0),
      condicion_laboral_pob_4_cat = dplyr::if_else(condicion_laboral_pob_4 > 0, 1, 0),
      condicion_laboral_pob_5_cat = dplyr::case_when(
        condicion_laboral_pob_5 == 0 ~ 0,
        condicion_laboral_pob_5 > 0 & condicion_laboral_pob_5 <= 0.40 ~ 1,
        condicion_laboral_pob_5 > 0.40 ~ 2,
        TRUE ~ NA_real_
      ),
      condicion_laboral_pob_6_cat = dplyr::case_when(
        condicion_laboral_pob_6 == 0 ~ 0,
        condicion_laboral_pob_6 > 0 & condicion_laboral_pob_6 <= 0.40 ~ 1,
        condicion_laboral_pob_6 > 0.40 & condicion_laboral_pob_6 <= 0.60 ~ 2,
        condicion_laboral_pob_6 > 0.60 ~ 3,
        TRUE ~ NA_real_
      ),
      pisoviviS3 = dplyr::case_when(
        MATERIAL_PISO == 9 ~ 1,
        MATERIAL_PISO %in% c(2, 3, 6, 8) ~ 3,
        is.na(MATERIAL_PISO) ~ 4,
        TRUE ~ 2
      ),
      pareviviS3 = dplyr::case_when(
        MATERIAL_PARED_EXTERIOR2 == 9 ~ 2,
        MATERIAL_PARED_EXTERIOR2 %in% c(6, 7) ~ 3,
        MATERIAL_PARED_EXTERIOR2 %in% c(2, 4, 5) ~ 4,
        is.na(MATERIAL_PARED_EXTERIOR2) ~ 5,
        TRUE ~ 1
      ),
      techviviS3 = dplyr::case_when(
        MATERIAL_TECHO %in% c(1, 4) ~ 2,
        MATERIAL_TECHO == 2 ~ 3,
        is.na(MATERIAL_TECHO) ~ 5,
        TRUE ~ 1
      ),
      luzviviS3 = dplyr::case_when(
        TIPO_ALUMBRADO %in% c(1, 2, 3) ~ 2,
        is.na(TIPO_ALUMBRADO) ~ 3,
        TRUE ~ 1
      ),
      basuviviS3 = dplyr::case_when(
        COMO_ELIMINA_BASURA == 4 ~ 2,
        COMO_ELIMINA_BASURA %in% c(1, 2) ~ 3,
        is.na(COMO_ELIMINA_BASURA) ~ 4,
        TRUE ~ 1
      ),
      cociviviS3 = dplyr::case_when(
        COMBUSTIBLE_PARA_COCINAR %in% c(1, 2, 3) ~ 3,
        COMBUSTIBLE_PARA_COCINAR == 6 ~ 2,
        is.na(COMBUSTIBLE_PARA_COCINAR) ~ 4,
        TRUE ~ 1
      ),
      basicos = dplyr::if_else(ESTUFA == 1, 1, 0) +
        dplyr::if_else(REFRIGERADOR == 1, 1, 0) +
        dplyr::if_else(TELEVISOR == 1, 1, 0) +
        dplyr::if_else(LAVADORA == 1, 1, 0),
      modernos = dplyr::if_else(AUTOMOVIL == 1, 1, 0) +
        dplyr::if_else(AIRE_ACONDICIONADO == 1, 1, 0) +
        dplyr::if_else(COMPUTADOR == 1, 1, 0),
      equiviviS3 = dplyr::case_when(
        modernos == 3 ~ 5,
        basicos == 0 ~ 1,
        basicos >= 1 & basicos < 4 ~ 2,
        basicos == 4 & modernos == 0 ~ 3,
        basicos == 4 & modernos > 0 & modernos <= 2 ~ 4,
        TRUE ~ 6
      ),
      saniviviS3 = dplyr::case_when(
        TIPO_SANITARIO %in% c(1, 2) ~ 3,
        TIPO_SANITARIO %in% c(3, 4) ~ 2,
        TIPO_SANITARIO == 5 ~ 1,
        TRUE ~ 4
      ),
      aguaviviS3 = dplyr::if_else(DONDE_PROVIENE_AGUA <= 2, 2, 1),
      aguaviviS3A = dplyr::case_when(
        DONDE_PROVIENE_AGUA == 1 ~ 5,
        DONDE_PROVIENE_AGUA == 2 ~ 3,
        DONDE_PROVIENE_AGUA %in% c(3, 4) ~ 2,
        DONDE_PROVIENE_AGUA == 9 ~ 4,
        TRUE ~ 1
      ),
      hacin = personas / CANT_DORMITORIOS_VIVIENDA,
      hacinadoviviS3 = dplyr::case_when(
        CANT_DORMITORIOS_VIVIENDA == 0 ~ 1,
        hacin >= 4 ~ 1,
        hacin >= 2.5 & hacin < 4 ~ 2,
        hacin >= 2 & hacin < 2.5 ~ 3,
        hacin < 2 ~ 4,
        is.na(CANT_DORMITORIOS_VIVIENDA) ~ 5,
        TRUE ~ 5
      ),
      TIPOVIviS3 = dplyr::case_when(
        TIPO_VIVIENDA %in% c(6, 7, 99) ~ 1,
        TIPO_VIVIENDA %in% c(1, 2, 3) ~ 2,
        TIPO_VIVIENDA %in% c(4, 5) ~ 3,
        TRUE ~ 1
      ),
      teneviviS3 = dplyr::case_when(
        TENENCIA_VIVIENDA %in% c(4, 6, 7, 8) ~ 2,
        TENENCIA_VIVIENDA <= 3 | TENENCIA_VIVIENDA == 5 ~ 3,
        is.na(TENENCIA_VIVIENDA) ~ 9,
        TRUE ~ 1
      ),
      TELEFONO0 = dplyr::if_else(TELEFONO == 1, 1, 0),
      CELULAR0 = dplyr::if_else(CELULAR == 1, 1, 0),
      MICROHONDA0 = dplyr::if_else(HORNO_MICROONDAS == 1, 1, 0),
      ESTUFA0 = dplyr::if_else(ESTUFA == 1, 1, 0),
      REFRIGERADOR0 = dplyr::if_else(REFRIGERADOR == 1, 1, 0),
      LAVADORA0 = dplyr::if_else(LAVADORA == 1, 1, 0),
      TELEVISOR0 = dplyr::if_else(TELEVISOR == 1, 1, 0),
      RADIO0 = 0,
      PLANTA_ELECTRICA0 = 0,
      aire0 = dplyr::if_else(AIRE_ACONDICIONADO == 1, 1, 0),
      auto0 = dplyr::if_else(AUTOMOVIL == 1, 1, 0),
      computador0 = dplyr::if_else(COMPUTADOR == 1, 1, 0),
      ZONA0 = dplyr::if_else(ZONA == 1, 1, 0),
      personas0 = dplyr::if_else(personas >= 6, 6, personas),
      dormitorios0 = dplyr::if_else(CANT_DORMITORIOS_VIVIENDA >= 4, 4, CANT_DORMITORIOS_VIVIENDA),
      Inversor0 = dplyr::if_else(INVERSOR == 1, 1, 0),
      hconmissing = dplyr::if_else(
        escojefeviviS3 == 7 | escoviviS3 == 7 | alfaviviS3 == 6,
        1,
        0
      )
    )
}


encft_iih_normalize_var_name <- function(x) {
  tolower(gsub("[^A-Za-z0-9]+", "_", trimws(x)))
}


encft_iih_model_specs <- local({
  specs <- 
  list(official = list(alpha = 10.2399645751, coefficients = list(
      pisovivis3 = c(`1` = -0.1071982588, `2` = -0.0605735793, 
      `3` = 0), parevivis3 = c(`1` = -0.0942313111, `2` = -0.0363478457, 
      `3` = -0.038632759, `4` = 0), techvivis3 = c(`1` = -0.0899685009, 
      `2` = -0.0761981036, `3` = 0), aguavivis3a = c(`1` = 0.0085736942, 
      `2` = -0.0267125336, `3` = -0.0120446068, `4` = -0.0320455718, 
      `5` = 0), sanivivis3 = c(`1` = -0.0814404283, `2` = -0.1000267018, 
      `3` = 0), luzvivis3 = c(`1` = -0.0534274874, `2` = 0), cocivivis3 = c(`1` = -0.0681787893, 
      `2` = 0.0837367421, `3` = 0), basuvivis3 = c(`1` = -0.0287871212, 
      `2` = -0.064457259, `3` = 0), tipovivis3 = c(`1` = -0.0870562985, 
      `2` = -0.0911657405, `3` = 0), hacinadovivis3 = c(`1` = 0.0911018036, 
      `2` = 0.0247052788, `3` = 0.0399207777, `4` = 0), escojefevivis3 = c(`1` = -0.0854903355, 
      `2` = -0.1584301708, `3` = -0.1571730513, `4` = -0.1554881925, 
      `5` = -0.1618831586, `6` = 0), escovivis3 = c(`1` = -0.4615825623, 
      `2` = -0.3684192002, `3` = -0.3348391514, `4` = -0.2927814009, 
      `5` = -0.2229380956, `6` = 0), alfavivis3 = c(`1` = -0.0528750284, 
      `2` = -0.0334050194, `3` = -0.0871514715, `4` = -0.0666121464, 
      `5` = 0), pninme5vivis3 = c(`1` = -0.0787470412, `2` = -0.0295896437, 
      `3` = 0), asiscolvivis3 = c(`1` = -0.023418397, `2` = -0.0875630391, 
      `3` = -0.0238685107, `4` = 0), idenprov = c(`1` = 0.0314555399, 
      `2` = 0.0116418835, `3` = -0.1939237466, `4` = 0.0516952868, 
      `5` = 0.1018779136, `6` = -0.0152744439, `7` = -0.0948813567, 
      `8` = 0.4051364546, `9` = 0.0613516032, `10` = -0.1512147897, 
      `11` = 0.4297836268, `12` = 0.0702182062, `13` = 0.0571468077, 
      `14` = -0.1124611769, `15` = 0.1248897935, `16` = 0.043370712, 
      `17` = 0.1513966953, `18` = 0.1292267079, `19` = 0.2643922798, 
      `20` = -0.0526996279, `21` = -0.0142181573, `22` = -0.0544982019, 
      `23` = 0.0809447333, `24` = 0.1198280572, `25` = -0.0178498298, 
      `26` = -0.0574727081, `27` = 0.0662231237, `28` = 0.0755023981, 
      `29` = -0.1002957119, `30` = 0.0777937379, `31` = -0.0062706796, 
      `32` = 0), conyjefevivis3 = c(`1` = 0.0595916214, `2` = 0.0748456856, 
      `3` = 0.0902138567, `4` = 0), trabajovivis3 = c(`1` = -0.4034843631, 
      `2` = -0.3776902224, `3` = -0.1310411689, `4` = 0.1090935268, 
      `5` = 0), tipohogvivis3 = c(`1` = 0.6239884898, `2` = 0.0128851978, 
      `3` = -0.0593369184, `4` = -0.028005134, `5` = -0.0045641822, 
      `6` = 0.0757282106, `7` = 0), ciclofamvivis3 = c(`1` = 0.0641764118, 
      `2` = 0.0581976318, `3` = 0.0674351096, `4` = 0.0194786266, 
      `5` = 0.072120387, `6` = 0.0448458314, `7` = 0), nini1524vivis3 = c(`1` = 0.0179964612, 
      `2` = -0.0495892149, `3` = -0.0445632646, `4` = 0), tenevivis3 = c(`1` = -0.1951386054, 
      `2` = -0.0764501954, `3` = 0), envevivis3 = c(`1` = -0.0756158263, 
      `2` = -0.0079392324, `3` = 0), mujevivis3 = c(`1` = -0.0200224491, 
      `2` = 0.0188291665, `3` = -0.0124094438, `4` = 0), pobmen12vivis3 = c(`1` = -0.0574509272, 
      `2` = -0.0512059417, `3` = 0.0097834973, `4` = 0), pobmen17vivis3 = c(`1` = -0.289363623, 
      `2` = -0.1710278954, `3` = -0.0996007398, `4` = 0.0057400067, 
      `5` = 0), aire0 = c(`0` = -0.14373783, `1` = 0), auto0 = c(`0` = -0.1796808489, 
      `1` = 0), computador0 = c(`0` = -0.0924812343, `1` = 0), 
      nivelunij0_min = c(`0` = -0.0896028666, `1` = 0), refrigerador0 = c(`0` = -0.0620524033, 
      `1` = 0), lavadora0 = c(`0` = -0.0372496236, `1` = 0), telefono0 = c(`0` = -0.0833525189, 
      `1` = 0), celular0 = c(`0` = -0.012030407, `1` = 0), microhonda0 = c(`0` = -0.1316125745, 
      `1` = 0), televisor0 = c(`0` = -0.0395768835, `1` = 0), radio0 = c(`0` = -0.004997871, 
      `1` = 0), zona0 = c(`0` = -0.0326996959, `1` = 0), personas0 = c(`1` = 0, 
      `2` = 0.4865818974, `3` = 0.1995339803, `4` = 0.2414704833, 
      `5` = 0.0846816451, `6` = 0), dormitorios0 = c(`0` = -0.3121749313, 
      `1` = -0.2545646671, `2` = -0.1802002892, `3` = -0.0928011786, 
      `4` = 0), tipocentro0 = c(`1` = -0.051540792, `2` = -0.0192317561, 
      `3` = 0.0287829835, `4` = 0), edadmediacat = c(`1` = -0.0788556248, 
      `2` = -0.0354796212, `3` = -0.0107695399, `4` = 0), condicion_laboral_jefe = c(`0` = -0.1685875768, 
      `1` = 0.0815530001, `2` = 0.0196346723, `3` = -0.076436033, 
      `4` = -0.0278436572, `5` = 0.0366583503, `6` = 0), condicion_laboral_pob_entropy_cat = c(`0` = 0.0121001888, 
      `1` = -0.0589905632, `2` = -0.076905562, `3` = 0.0033338507, 
      `4` = 0), condicion_laboral_pob_0_cat = c(`0` = 0.1659150273, 
      `1` = 0), condicion_laboral_pob_1_cat = c(`0` = 0.180902272, 
      `1` = 0.0789035475, `2` = 0.0127871756, `3` = 0), condicion_laboral_pob_4_cat = c(`0` = 0.2332527239, 
      `1` = 0), condicion_laboral_pob_3_cat = c(`0` = -0.1968337503, 
      `1` = 0), estufa0 = c(`0` = -0.0226905988, `1` = 0), inversor0 = c(`0` = -0.0622259729, 
      `1` = 0))), focal_occ = list(alpha = 10.3276039938, coefficients = list(
      pisovivis3 = c(`1` = -0.1223506494, `2` = -0.0364944429, 
      `3` = 0), parevivis3 = c(`1` = -0.0457782818, `2` = 6.01784e-05, 
      `3` = -0.0293804199, `4` = 0), techvivis3 = c(`1` = -0.0639249885, 
      `2` = -0.0759220263, `3` = 0), aguavivis3a = c(`1` = 0.0219394235, 
      `2` = -0.031345293, `3` = -0.0148037039, `4` = -0.0455053308, 
      `5` = 0), sanivivis3 = c(`1` = -0.0253765612, `2` = -0.0896932893, 
      `3` = 0), luzvivis3 = c(`1` = 0.0238200911, `2` = 0), cocivivis3 = c(`1` = -0.0673781175, 
      `2` = 0.0892517752, `3` = 0), basuvivis3 = c(`1` = -0.0111203708, 
      `2` = -0.0644321607, `3` = 0), tipovivis3 = c(`1` = -0.096682736, 
      `2` = -0.0907702177, `3` = 0), hacinadovivis3 = c(`1` = 0.0603531818, 
      `2` = 0.0117721396, `3` = 0.0276237785, `4` = 0), escojefevivis3 = c(`1` = -0.1051649902, 
      `2` = -0.1921367858, `3` = -0.1904968238, `4` = -0.183943185, 
      `5` = -0.1766008962, `6` = 0), escovivis3 = c(`1` = -0.58417136, 
      `2` = -0.3898407421, `3` = -0.332119235, `4` = -0.2906607171, 
      `5` = -0.2289344402, `6` = 0), alfavivis3 = c(`1` = 0.0187159147, 
      `2` = -0.0820664023, `3` = -0.1039006908, `4` = -0.0730859058, 
      `5` = 0), pninme5vivis3 = c(`1` = -0.0615404203, `2` = -0.0253421245, 
      `3` = 0), asiscolvivis3 = c(`1` = -0.0935735481, `2` = -0.0807160166, 
      `3` = -0.0183145156, `4` = 0), idenprov = c(`1` = 0.0084857464, 
      `2` = 0.0495769301, `3` = -0.1700444545, `4` = 0.1006553259, 
      `5` = 0.1301744298, `6` = 0.0083437512, `7` = -0.0420594472, 
      `8` = 0.4939867221, `9` = 0.0459040407, `10` = -0.1376295207, 
      `11` = 0.4580701106, `12` = 0.0986684698, `13` = 0.0997972879, 
      `14` = -0.1047886869, `15` = 0.1208453012, `16` = 0.0080418394, 
      `17` = 0.1443200909, `18` = 0.1615307609, `19` = 0.3097439544, 
      `20` = -0.0369281481, `21` = -0.0221011029, `22` = -0.0432211193, 
      `23` = 0.1233786587, `24` = 0.1598055673, `25` = -0.0020626823, 
      `26` = -0.0400820379, `27` = 0.096930579, `28` = 0.1178988006, 
      `29` = -0.1149435813, `30` = 0.0635109416, `31` = 0.0520274621, 
      `32` = 0), conyjefevivis3 = c(`1` = 0.0672425619, `2` = 0.06155432, 
      `3` = 0.1035302002, `4` = 0), trabajovivis3 = c(`1` = -0.4664374484, 
      `2` = -0.3503553562, `3` = -0.0951369562, `4` = 0.1299776362, 
      `5` = 0), tipohogvivis3 = c(`1` = 0.4165928916, `2` = -0.0941812749, 
      `3` = -0.1262971369, `4` = -0.1165969404, `5` = -0.0745111191, 
      `6` = 0.030820824, `7` = 0), ciclofamvivis3 = c(`1` = -0.0205538987, 
      `2` = 0.0717572525, `3` = 0.0629527133, `4` = 0.0100984837, 
      `5` = 0.0617240895, `6` = 0.0288524078, `7` = 0), nini1524vivis3 = c(`1` = 0.0365115791, 
      `2` = -0.0193785464, `3` = -0.0484963296, `4` = 0), tenevivis3 = c(`1` = -6.89872e-05, 
      `2` = -0.1069387888, `3` = 0), envevivis3 = c(`1` = -0.0860850785, 
      `2` = -0.0219356691, `3` = 0), mujevivis3 = c(`1` = -0.0212455619, 
      `2` = 0.0194840749, `3` = -0.0137461859, `4` = 0), pobmen12vivis3 = c(`1` = -0.0664179482, 
      `2` = -0.0533385398, `3` = 0.0156445807, `4` = 0), pobmen17vivis3 = c(`1` = -0.3233475455, 
      `2` = -0.191128897, `3` = -0.1116845909, `4` = 0.0228009592, 
      `5` = 0), aire0 = c(`0` = -0.1414827197, `1` = 0), auto0 = c(`0` = -0.1776879372, 
      `1` = 0), computador0 = c(`0` = -0.1001945337, `1` = 0), 
      nivelunij0_min = c(`0` = -0.0788934367, `1` = 0), estufa0 = c(`0` = -0.0092191688, 
      `1` = 0), refrigerador0 = c(`0` = -0.0347521646, `1` = 0), 
      lavadora0 = c(`0` = -0.0512559489, `1` = 0), telefono0 = c(`0` = -0.074118031, 
      `1` = 0), celular0 = c(`0` = -0.0054523639, `1` = 0), microhonda0 = c(`0` = -0.12956688, 
      `1` = 0), televisor0 = c(`0` = -0.0492403693, `1` = 0), radio0 = c(`0` = -0.0002961725, 
      `1` = 0), zona0 = c(`0` = 9.01842e-05, `1` = 0), personas0 = c(`1` = 0, 
      `2` = 0.3749428797, `3` = 0.1520339076, `4` = 0.2224914911, 
      `5` = 0.0652114197, `6` = 0), dormitorios0 = c(`0` = -0.2182968461, 
      `1` = -0.183927323, `2` = -0.1464005738, `3` = -0.0817852728, 
      `4` = 0), tipocentro0 = c(`1` = -0.0471226765, `2` = 0.0185535759, 
      `3` = 0.0387509524, `4` = 0), edadmediacat = c(`1` = -0.0411456352, 
      `2` = -0.0088387904, `3` = 0.0031391586, `4` = 0), condicion_laboral_jefe = c(`0` = -0.2660915849, 
      `1` = 0.0125288494, `2` = -0.073525715, `3` = -0.1352563977, 
      `4` = -0.1076895763, `5` = -0.0314982954, `6` = 0), condicion_laboral_pob_entropy_cat = c(`0` = 0.1242434688, 
      `1` = -0.0263548115, `2` = -0.0624048715, `3` = 0.0163072542, 
      `4` = 0), condicion_laboral_pob_0_cat = c(`0` = 0.1696115863, 
      `1` = 0), condicion_laboral_pob_1_cat = c(`0` = 0.1789353498, 
      `1` = 0.0874254959, `2` = 0.0192058756, `3` = 0), condicion_laboral_pob_2_cat = c(`0` = -0.175642523, 
      `1` = -0.0950849931, `2` = -0.0139069597, `3` = 0), condicion_laboral_pob_3_cat = c(`0` = -0.2365161095, 
      `1` = 0), condicion_laboral_pob_4_cat = c(`0` = 0.1870133136, 
      `1` = 0), condicion_laboral_pob_5_cat = c(`0` = -0.1130283209, 
      `1` = -0.0588501044, `2` = 0), inversor0 = c(`0` = -0.0904007971, 
      `1` = 0))), focal_no_occ = list(alpha = 10.1433075617, coefficients = list(
      pisovivis3 = c(`1` = -0.11660313, `2` = -0.0146855625, `3` = 0
      ), parevivis3 = c(`1` = -0.0497859939, `2` = 0.0067343431, 
      `3` = -0.0184923226, `4` = 0), techvivis3 = c(`1` = -0.1133097209, 
      `2` = -0.0730667089, `3` = 0), aguavivis3a = c(`1` = 0.00838899, 
      `2` = -0.0326731647, `3` = -0.0263209096, `4` = -0.0047968408, 
      `5` = 0), sanivivis3 = c(`1` = -0.0233536234, `2` = -0.0796388632, 
      `3` = 0), luzvivis3 = c(`1` = -0.0390088555, `2` = 0), cocivivis3 = c(`1` = -0.0230269271, 
      `2` = 0.0281731037, `3` = 0), basuvivis3 = c(`1` = -0.0259575652, 
      `2` = -0.0696315808, `3` = 0), tipovivis3 = c(`1` = -0.0797773744, 
      `2` = -0.0961102827, `3` = 0), hacinadovivis3 = c(`1` = 0.0315249172, 
      `2` = -0.0008624731, `3` = 0.0161954768, `4` = 0), escojefevivis3 = c(`1` = -0.0636293729, 
      `2` = -0.1718816955, `3` = -0.147855968, `4` = -0.1599000148, 
      `5` = -0.1880582012, `6` = 0), escovivis3 = c(`1` = -0.7845120388, 
      `2` = -0.5234259164, `3` = -0.4501003135, `4` = -0.3525783186, 
      `5` = -0.267406758, `6` = 0), alfavivis3 = c(`1` = 0.0337044966, 
      `2` = -0.0365416991, `3` = -0.1087873656, `4` = -0.0383999189, 
      `5` = 0), pninme5vivis3 = c(`1` = -0.0528371313, `2` = 0.0247900146, 
      `3` = 0), asiscolvivis3 = c(`1` = -0.083352176, `2` = -0.1183554048, 
      `3` = -0.0037330117, `4` = 0), idenprov = c(`1` = 0.0242273544, 
      `2` = 0.0626933112, `3` = -0.1725260672, `4` = 0.1310571386, 
      `5` = 0.1443730944, `6` = -0.008895438, `7` = 0.0353952129, 
      `8` = 0.6256218178, `9` = 0.1683114677, `10` = -0.2103048309, 
      `11` = 0.4842174103, `12` = 0.1076145601, `13` = 0.1338376661, 
      `14` = -0.0858048538, `15` = 0.1568259532, `16` = 0.1266929834, 
      `17` = 0.0967898368, `18` = 0.2298709626, `19` = 0.3029886343, 
      `20` = -0.0604791375, `21` = -0.0688223563, `22` = -0.0076388863, 
      `23` = 0.1242944148, `24` = 0.2192375487, `25` = 0.0341602241, 
      `26` = -0.0742902387, `27` = 0.1410728998, `28` = 0.1033759526, 
      `29` = -0.1090026921, `30` = 0.0220774639, `31` = -0.0195838906, 
      `32` = 0), conyjefevivis3 = c(`1` = 0.1078764398, `2` = 0.1616233909, 
      `3` = 0.1277957185, `4` = 0), tipohogvivis3 = c(`1` = 0.5739688918, 
      `2` = 0.1440534951, `3` = 0.0473012284, `4` = 0.1040307464, 
      `5` = 0.119217276, `6` = 0.2523107596, `7` = 0), ciclofamvivis3 = c(`1` = 0.1743295708, 
      `2` = 0.0199386842, `3` = 0.1124066567, `4` = 0.0680449895, 
      `5` = 0.0249848311, `6` = 0.0636732219, `7` = 0), tenevivis3 = c(`1` = 0.0188457764, 
      `2` = -0.1176099372, `3` = 0), envevivis3 = c(`1` = -0.2173479825, 
      `2` = -0.1138458627, `3` = 0), mujevivis3 = c(`1` = -0.1551848768, 
      `2` = -0.0838203667, `3` = -0.100483901, `4` = 0), pobmen12vivis3 = c(`1` = -0.0622059637, 
      `2` = -0.0329858147, `3` = -0.0017987899, `4` = 0), pobmen17vivis3 = c(`1` = -0.6823140559, 
      `2` = -0.4047626423, `3` = -0.2195637782, `4` = -0.0422686464, 
      `5` = 0), aire0 = c(`0` = -0.153061516, `1` = 0), auto0 = c(`0` = -0.2314214697, 
      `1` = 0), computador0 = c(`0` = -0.1154273196, `1` = 0), 
      nivelunij0_min = c(`0` = -0.1091730126, `1` = 0), estufa0 = c(`0` = 0.0688156965, 
      `1` = 0), refrigerador0 = c(`0` = -0.0363991912, `1` = 0), 
      lavadora0 = c(`0` = -0.0533453537, `1` = 0), telefono0 = c(`0` = -0.0543175761, 
      `1` = 0), celular0 = c(`0` = -0.0247981495, `1` = 0), microhonda0 = c(`0` = -0.1146465687, 
      `1` = 0), televisor0 = c(`0` = -0.0599346342, `1` = 0), radio0 = c(`0` = -0.0315563317, 
      `1` = 0), zona0 = c(`0` = -0.0301394245, `1` = 0), personas0 = c(`1` = 0, 
      `2` = 0.4352517609, `3` = 0.2527057718, `4` = 0.2276103371, 
      `5` = 0.0553810511, `6` = 0), dormitorios0 = c(`0` = -0.1696959219, 
      `1` = -0.1362875285, `2` = -0.125767312, `3` = -0.0393568234, 
      `4` = 0), tipocentro0 = c(`1` = -0.0884753828, `2` = -0.033126168, 
      `3` = 0.0207827431, `4` = 0), edadmediacat = c(`1` = -0.0452421152, 
      `2` = 0.0677826654, `3` = 0.0851564575, `4` = 0), inversor0 = c(`0` = -0.0806909167, 
      `1` = 0))))
  function() specs
})

encft_iih_apply_model_spec <- function(data, spec) {
  column_map <- stats::setNames(names(data), encft_iih_normalize_var_name(names(data)))
  score <- rep(spec$alpha, nrow(data))

  for (var_name in names(spec$coefficients)) {
    actual_name <- column_map[[var_name]]

    if (is.null(actual_name)) {
      stop("No se encontro la variable requerida por el modelo: ", var_name)
    }

    keys <- encft_iih_format_key(data[[actual_name]])
    coef_map <- spec$coefficients[[var_name]]
    score <- score + unname(coef_map[keys])
  }

  score
}


encft_iih_apply_models <- function(households) {
  specs <- encft_iih_model_specs()

  households <- households %>%
    dplyr::mutate(
      ingpctransln_calc_ecuacion_RCNO_C37_50var = encft_iih_apply_model_spec(., specs$official),
      ingresopc_oficial_RCNOC37_calc_ecuacion = exp(ingpctransln_calc_ecuacion_RCNO_C37_50var) - 1,
      lindofizona2019oficial2012jun = dplyr::case_when(
        ZONA == 1 ~ 2342.5,
        ZONA == 2 ~ 2244.5,
        TRUE ~ NA_real_
      ),
      lpobofizona2019oficial2012jun = dplyr::case_when(
        ZONA == 1 ~ 5202.4,
        ZONA == 2 ~ 4632.1,
        TRUE ~ NA_real_
      ),
      pobofizona_ingresopc_oficial_RCNOC37_calc_ecuacion = dplyr::case_when(
        is.na(ingresopc_oficial_RCNOC37_calc_ecuacion) ~ 9,
        ingresopc_oficial_RCNOC37_calc_ecuacion >= lpobofizona2019oficial2012jun ~ 3,
        ingresopc_oficial_RCNOC37_calc_ecuacion >= lindofizona2019oficial2012jun ~ 2,
        TRUE ~ 1
      ),
      ingpcfoctransln_RCNOC30_calc_ecuacion = encft_iih_apply_model_spec(., specs$focal_occ),
      ingresopc_focal_RCNOC30_calc_ecuacion = exp(ingpcfoctransln_RCNOC30_calc_ecuacion) - 1,
      pobofizona_ingresopc_focal_RCNOC30_calc_ecuacion = dplyr::case_when(
        is.na(ingresopc_focal_RCNOC30_calc_ecuacion) ~ 9,
        ingresopc_focal_RCNOC30_calc_ecuacion >= lpobofizona2019oficial2012jun ~ 3,
        ingresopc_focal_RCNOC30_calc_ecuacion >= lindofizona2019oficial2012jun ~ 2,
        TRUE ~ 1
      ),
      ingpcfoctransln_RCNC30_calc_ecuacion = encft_iih_apply_model_spec(., specs$focal_no_occ),
      ingresopc_focal_RCNC30_calc_ecuacion = exp(ingpcfoctransln_RCNC30_calc_ecuacion) - 1,
      pobofizona_ingresopc_focal_RCNC30_calc_ecuacion = dplyr::case_when(
        is.na(ingresopc_focal_RCNC30_calc_ecuacion) ~ 9,
        ingresopc_focal_RCNC30_calc_ecuacion >= lpobofizona2019oficial2012jun ~ 3,
        ingresopc_focal_RCNC30_calc_ecuacion >= lindofizona2019oficial2012jun ~ 2,
        TRUE ~ 1
      ),
      Lognlineapobgen_2019 = dplyr::case_when(
        ZONA == 1 ~ 8.556875,
        ZONA == 2 ~ 8.440766,
        TRUE ~ NA_real_
      ),
      loglinearesidual = dplyr::case_when(
        ZONA == 2 ~ 10.353407,
        TRUE ~ 10.588294
      ),
      ingpctransln_calc_ecuacion_RCNO_C37_50var_min_2019 = 7.084766,
      ingpctransln_calc_ecuacion_RCNO_C37_50var_max_2019 = 11.574154,
      logmin_C10 = 6.454376,
      logmax_C10 = 10.951764,
      logmax_C939 = 12.742003,
      ingpctransln_calc_ecuacion_RCNO_C37_d1010_50var_max_2019 = 11.37,
      ingpctransln_calc_ecuacion_RCNO_C37_50var_aj_C37_d10 = dplyr::case_when(
        ingpctransln_calc_ecuacion_RCNO_C37_50var < Lognlineapobgen_2019 ~
          ingpctransln_calc_ecuacion_RCNO_C37_50var -
          (ingpctransln_calc_ecuacion_RCNO_C37_50var - logmin_C10) *
          (Lognlineapobgen_2019 - ingpctransln_calc_ecuacion_RCNO_C37_50var) /
          ((Lognlineapobgen_2019 - ingpctransln_calc_ecuacion_RCNO_C37_50var_min_2019) ^ 4),
        TRUE ~ ingpctransln_calc_ecuacion_RCNO_C37_50var
      ),
      ingpctransln_calc_ecuacion_RCNO_50var_aj_C37_d1010 = dplyr::case_when(
        ingpctransln_calc_ecuacion_RCNO_C37_50var >= Lognlineapobgen_2019 ~
          ingpctransln_calc_ecuacion_RCNO_C37_50var +
          (logmax_C10 - ingpctransln_calc_ecuacion_RCNO_C37_50var) *
          (ingpctransln_calc_ecuacion_RCNO_C37_50var - Lognlineapobgen_2019) /
          ((ingpctransln_calc_ecuacion_RCNO_C37_50var_max_2019 - Lognlineapobgen_2019) ^ 2),
        TRUE ~ ingpctransln_calc_ecuacion_RCNO_C37_50var_aj_C37_d10
      ),
      ingpctransln_calc_ecuacion_NCRO_50var_aj_C37_d1010_C939 = dplyr::case_when(
        ingpctransln_calc_ecuacion_RCNO_50var_aj_C37_d1010 >= loglinearesidual ~
          ingpctransln_calc_ecuacion_RCNO_50var_aj_C37_d1010 +
          (logmax_C939 - ingpctransln_calc_ecuacion_RCNO_50var_aj_C37_d1010) *
          (ingpctransln_calc_ecuacion_RCNO_50var_aj_C37_d1010 - loglinearesidual) /
          ((ingpctransln_calc_ecuacion_RCNO_C37_d1010_50var_max_2019 - loglinearesidual) ^ 2),
        TRUE ~ ingpctransln_calc_ecuacion_RCNO_50var_aj_C37_d1010
      ),
      ingpctransln_calc_ecuacion_RCNO_50var_aj_C37_d1010_C939 =
        ingpctransln_calc_ecuacion_NCRO_50var_aj_C37_d1010_C939,
      ingresopc_oficial_NCRO_calc_C37_d1010_C939 =
        exp(ingpctransln_calc_ecuacion_NCRO_50var_aj_C37_d1010_C939) - 1,
      ingresopc_oficial_RCNO_calc_C37_d1010_C939 =
        ingresopc_oficial_NCRO_calc_C37_d1010_C939,
      pobofizona_ingresopc_oficial_NCRO_calc_C37_d1010_C939 = dplyr::case_when(
        is.na(ingresopc_oficial_NCRO_calc_C37_d1010_C939) ~ 9,
        ingresopc_oficial_NCRO_calc_C37_d1010_C939 >= lpobofizona2019oficial2012jun ~ 3,
        ingresopc_oficial_NCRO_calc_C37_d1010_C939 >= lindofizona2019oficial2012jun ~ 2,
        TRUE ~ 1
      )
    )

  invalid_households <- households$hconmissing == 1
  model_columns <- c(
    "ingpctransln_calc_ecuacion_RCNO_C37_50var",
    "ingresopc_oficial_RCNOC37_calc_ecuacion",
    "pobofizona_ingresopc_oficial_RCNOC37_calc_ecuacion",
    "ingpcfoctransln_RCNOC30_calc_ecuacion",
    "ingresopc_focal_RCNOC30_calc_ecuacion",
    "pobofizona_ingresopc_focal_RCNOC30_calc_ecuacion",
    "ingpcfoctransln_RCNC30_calc_ecuacion",
    "ingresopc_focal_RCNC30_calc_ecuacion",
    "pobofizona_ingresopc_focal_RCNC30_calc_ecuacion",
    "ingpctransln_calc_ecuacion_RCNO_C37_50var_aj_C37_d10",
    "ingpctransln_calc_ecuacion_RCNO_50var_aj_C37_d1010",
    "ingpctransln_calc_ecuacion_NCRO_50var_aj_C37_d1010_C939",
    "ingpctransln_calc_ecuacion_RCNO_50var_aj_C37_d1010_C939",
    "ingresopc_oficial_NCRO_calc_C37_d1010_C939",
    "ingresopc_oficial_RCNO_calc_C37_d1010_C939",
    "pobofizona_ingresopc_oficial_NCRO_calc_C37_d1010_C939"
  )

  households[invalid_households, model_columns] <- NA_real_
  households[invalid_households, c(
    "pobofizona_ingresopc_oficial_RCNOC37_calc_ecuacion",
    "pobofizona_ingresopc_focal_RCNOC30_calc_ecuacion",
    "pobofizona_ingresopc_focal_RCNC30_calc_ecuacion",
    "pobofizona_ingresopc_oficial_NCRO_calc_C37_d1010_C939"
  )] <- 9

  households$IIH <- households$pobofizona_ingresopc_oficial_NCRO_calc_C37_d1010_C939

  households
}

calculate_iih_encft <- function(
  data,
  vivienda_data = NULL,
  return_households = FALSE,
  include_details = TRUE,
  filter_valid_households = FALSE
) {
  inputs <- encft_iih_resolve_inputs(
    data = data,
    vivienda_data = vivienda_data
  )

  calc_data <- encft_iih_complete_input(
    data = inputs$data,
    vivienda_data = inputs$vivienda_data
  )

  households <- calc_data %>%
    encft_iih_prepare_persons() %>%
    encft_iih_aggregate_households() %>%
    encft_iih_classify_households() %>%
    encft_iih_apply_models()

  if (isTRUE(filter_valid_households)) {
    households <- households %>% dplyr::filter(hconmissing == 0)
  }

  output_cols <- encft_iih_output_columns(include_details = include_details)

  if (isTRUE(return_households)) {
    return(households %>% dplyr::select(dplyr::all_of(output_cols)))
  }

    value_cols <- setdiff(output_cols, c("calc_id", "PERIODO", "ID_HOGAR"))
    inputs$data <- dplyr::select(inputs$data, -dplyr::any_of(value_cols))
    join_cols <- unique(c("calc_id", value_cols))

  inputs$data %>%
    dplyr::mutate(calc_id = encft_iih_build_calc_id(.)) %>%
    dplyr::left_join(
      households %>% dplyr::select(dplyr::all_of(join_cols)),
      by = "calc_id"
    ) %>%
    dplyr::select(-calc_id)
}
