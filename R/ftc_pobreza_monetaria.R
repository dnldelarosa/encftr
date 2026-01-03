left_join_tipo_cambio <- function(tbl){
  nombres <- c("PERIODO")
  for(nombre in names(tipo_cambio)){
    if(!nombre %in% dplyr::tbl_vars(tbl)){
      nombres <- c(nombres, nombre)
    }
  }
tbl %>%
  dplyr::left_join(
    tipo_cambio %>%
      dplyr::select(nombres),
    by = "PERIODO",
    copy = TRUE
  )
}

left_join_ipc_2020 <- function(tbl){
  nombres <- c("PERIODO")
  for(nombre in names(ipc_2020)){
    if(!nombre %in% tbl_vars(tbl)){
      nombres <- c(nombres, nombre)
    }
  }
tbl %>%
  dplyr::left_join(
    ipc_2020 %>%
      dplyr::select(nombres),
    by = "PERIODO",
    copy = TRUE
  )
}


#' Ingreso monetario mensual por ocupación principal asalariada (Pobreza Monetaria)
#' `r lifecycle::badge("stable")`
#'
#' @param tbl [data.frame] data.frame con los datos de la ENCFT
#'
#' @return [data.frame] los datos suministrados en el data.frame \code{tbl}
#'   con la variable \code{ing_mensual_ocup_prin_asalariado} adicionada.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  encft <- ftc_ing_mensual_ocup_prin_asalariado(encft)
#' }
ftc_ing_mensual_ocup_prin_asalariado <- function(tbl) {
  tbl <- tbl %>%
  left_join_tipo_cambio() %>%
    dplyr::mutate(
      ingasal = dplyr::case_when(
        is.na(SUELDO_BRUTO_AP_MONEDA) ~ 0,
        is.na(SUELDO_BRUTO_AP_MONTO) ~ 0,
        SUELDO_BRUTO_AP_MONEDA == "DOP" ~ SUELDO_BRUTO_AP_MONTO,
        SUELDO_BRUTO_AP_MONEDA == "BRL" ~ SUELDO_BRUTO_AP_MONTO * dplyr::lead(BRL),
        SUELDO_BRUTO_AP_MONEDA == "CAD" ~ SUELDO_BRUTO_AP_MONTO * dplyr::lead(CAD),
        SUELDO_BRUTO_AP_MONEDA == "CHF" ~ SUELDO_BRUTO_AP_MONTO * dplyr::lead(CHF),
        SUELDO_BRUTO_AP_MONEDA == "CNY" ~ SUELDO_BRUTO_AP_MONTO * dplyr::lead(CNY),
        SUELDO_BRUTO_AP_MONEDA == "DEG" ~ SUELDO_BRUTO_AP_MONTO * dplyr::lead(DEG),
        SUELDO_BRUTO_AP_MONEDA == "DKK" ~ SUELDO_BRUTO_AP_MONTO * dplyr::lead(DKK),
        SUELDO_BRUTO_AP_MONEDA == "EUR" ~ SUELDO_BRUTO_AP_MONTO * dplyr::lead(EUR),
        SUELDO_BRUTO_AP_MONEDA == "GBP" ~ SUELDO_BRUTO_AP_MONTO * dplyr::lead(GBP),
        SUELDO_BRUTO_AP_MONEDA == "JPY" ~ SUELDO_BRUTO_AP_MONTO * dplyr::lead(JPY),
        SUELDO_BRUTO_AP_MONEDA == "NOK" ~ SUELDO_BRUTO_AP_MONTO * dplyr::lead(NOK),
        SUELDO_BRUTO_AP_MONEDA == "LESC" ~ SUELDO_BRUTO_AP_MONTO * dplyr::lead(LESC),
        SUELDO_BRUTO_AP_MONEDA == "SEK" ~ SUELDO_BRUTO_AP_MONTO * dplyr::lead(SEK),
        SUELDO_BRUTO_AP_MONEDA == "USD" ~ SUELDO_BRUTO_AP_MONTO * dplyr::lead(USD),
        SUELDO_BRUTO_AP_MONEDA == "VEF" ~ SUELDO_BRUTO_AP_MONTO * dplyr::lead(VEF),
        SUELDO_BRUTO_AP_MONEDA == "ARS" ~ SUELDO_BRUTO_AP_MONTO * dplyr::lead(ARS),
        TRUE ~ 0
      ),
      ingasal = as.double(ingasal),
      horasocupprin = dplyr::if_else(is.na(HORAS_TRABAJO_EFECT_TOTAL), 0, HORAS_TRABAJO_EFECT_TOTAL),
      periocupprin = dplyr::if_else(is.na(TIEMPO_RECIBE_PAGO_AP), 0, TIEMPO_RECIBE_PAGO_AP),
      diasocupprin = dplyr::if_else(is.na(TIEMPO_RECIBE_PAGO_DIAS_AP), 0, TIEMPO_RECIBE_PAGO_DIAS_AP)
      )
  #tbl %>%
  #  dplyr::select(c(ingasal, horasocupprin, periocupprin, diasocupprin)) %>%
  #  assign("res", ., envir = .GlobalEnv)
  tbl %>%
    dplyr::mutate(
      ing_mensual_ocup_prin_asalariado = dplyr::case_when(
        is.na(periocupprin) ~ 0,
        periocupprin == 1 ~ as.numeric(diasocupprin) * ingasal * 4.3,
        periocupprin == 2 ~ ingasal * 4.3,
        periocupprin == 3 ~ ingasal * 2,
        periocupprin == 4 ~ ingasal,
        SUELDO_BRUTO_AP == 2 ~ dplyr::if_else(is.na(SALARIO_PRINC_IMP_MONTO), 0, as.numeric(SALARIO_PRINC_IMP_MONTO)),
        TRUE ~ 0
      ),
      ing_mensual_ocup_prin_asalariado = as.double(ing_mensual_ocup_prin_asalariado)
    ) %>%
    dplyr::select(-c(ingasal, horasocupprin, periocupprin, diasocupprin))
}


#' Ingreso monetario mensual por ocupación principal de independiente agropecuario o contratista (Pobreza Monetaria)
#' `r lifecycle::badge("stable")`
#'
#' @param tbl [data.frame] data.frame con los datos de la ENCFT
#'
#' @return [data.frame] los datos suministrados en el data.frame \code{tbl}
#'  con la variable \code{ing_mensual_ocup_prin_independiente} adicionada.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' encft <- ftc_ing_mensual_ocup_prin_independiente(encft)
#' }
ftc_ing_mensual_ocup_prin_independiente <- function(tbl) {
  tbl %>%
    dplyr::mutate(
      ing_mensual_ocup_prin_independiente = dplyr::case_when(
        is.na(GANANCIA_IN_PRODUCTOR) ~ 0,
        is.na(GANANCIA_IN_PRODUCTOR_MONTO) & is.na(GANANCIA_PRINC_IMP_MONTO) ~ 0,
        GANANCIA_IN_PRODUCTOR_MONEDA == "DOP" ~ as.numeric(GANANCIA_IN_PRODUCTOR_MONTO) / 6,
        GANANCIA_IN_PRODUCTOR == 2 ~ as.numeric(GANANCIA_PRINC_IMP_MONTO),
        TRUE ~ 0
      ),
      ing_mensual_ocup_prin_independiente = as.double(ing_mensual_ocup_prin_independiente)
    )
}


#' Ingreso monetario por ocupacion principal para patronos y cuenta propia (Pobreza Monetaria)
#' `r lifecycle::badge("stable")`
#'
#' @param tbl [data.frame] data.frame con los datos de la ENCFT
#'
#' @return [data.frame] los datos suministrados en el data.frame \code{tbl}
#' con la variable \code{ing_mensual_ocup_prin_cuenta_propia} adicionada.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' encft <- ftc_ing_mensual_ocup_prin_cuenta_propia(encft)
#' }
ftc_ing_mensual_ocup_prin_cuenta_propia <- function(tbl) {
  tbl <- tbl %>%
    left_join_tipo_cambio() %>%
    dplyr::mutate(
      INGRESO_ACTIVIDAD_IN_MONTO = as.double(INGRESO_ACTIVIDAD_IN_MONTO),
      ingprinctaprop = case_when(
        INGRESO_ACTIVIDAD_IN_MONEDA == "DOP" ~ INGRESO_ACTIVIDAD_IN_MONTO,
        INGRESO_ACTIVIDAD_IN_MONEDA == "BRL" ~ INGRESO_ACTIVIDAD_IN_MONTO * dplyr::lead(BRL),
        INGRESO_ACTIVIDAD_IN_MONEDA == "CAD" ~ INGRESO_ACTIVIDAD_IN_MONTO * dplyr::lead(CAD),
        INGRESO_ACTIVIDAD_IN_MONEDA == "CHF" ~ INGRESO_ACTIVIDAD_IN_MONTO * dplyr::lead(CHF),
        INGRESO_ACTIVIDAD_IN_MONEDA == "CNY" ~ INGRESO_ACTIVIDAD_IN_MONTO * dplyr::lead(CNY),
        INGRESO_ACTIVIDAD_IN_MONEDA == "DEG" ~ INGRESO_ACTIVIDAD_IN_MONTO * dplyr::lead(DEG),
        INGRESO_ACTIVIDAD_IN_MONEDA == "DKK" ~ INGRESO_ACTIVIDAD_IN_MONTO * dplyr::lead(DKK),
        INGRESO_ACTIVIDAD_IN_MONEDA == "EUR" ~ INGRESO_ACTIVIDAD_IN_MONTO * dplyr::lead(EUR),
        INGRESO_ACTIVIDAD_IN_MONEDA == "GBP" ~ INGRESO_ACTIVIDAD_IN_MONTO * dplyr::lead(GBP),
        INGRESO_ACTIVIDAD_IN_MONEDA == "JPY" ~ INGRESO_ACTIVIDAD_IN_MONTO * dplyr::lead(JPY),
        INGRESO_ACTIVIDAD_IN_MONEDA == "NOK" ~ INGRESO_ACTIVIDAD_IN_MONTO * dplyr::lead(NOK),
        INGRESO_ACTIVIDAD_IN_MONEDA == "LESC" ~ INGRESO_ACTIVIDAD_IN_MONTO* dplyr::lead(LESC),
        INGRESO_ACTIVIDAD_IN_MONEDA == "SEK" ~ INGRESO_ACTIVIDAD_IN_MONTO * dplyr::lead(SEK),
        INGRESO_ACTIVIDAD_IN_MONEDA == "USD" ~ INGRESO_ACTIVIDAD_IN_MONTO * dplyr::lead(USD),
        INGRESO_ACTIVIDAD_IN_MONEDA == "VEF" ~ INGRESO_ACTIVIDAD_IN_MONTO * dplyr::lead(VEF),
        INGRESO_ACTIVIDAD_IN_MONEDA == "ARS" ~ INGRESO_ACTIVIDAD_IN_MONTO * ARS,
        TRUE ~ 0
      ),
      ingprinctaprop = as.double(ingprinctaprop),
      periprinctaprop = ifelse(
        is.na(INGRESO_ACTIVIDAD_IN_PERIODO), 0, INGRESO_ACTIVIDAD_IN_PERIODO
      ),
      diasprinctaprop = ifelse(
        is.na(INGRESO_ACTIVIDAD_IN_DIAS), 0, INGRESO_ACTIVIDAD_IN_DIAS
      ),
      ing_mensual_ocup_prin_cuenta_propia = case_when(
        periprinctaprop == 1 ~ diasprinctaprop * ingprinctaprop * 4.3,
        periprinctaprop == 2 ~ ingprinctaprop * 4.3,
        periprinctaprop == 3 ~ ingprinctaprop * 2,
        periprinctaprop == 4 ~ ingprinctaprop,
        INGRESO_ACTIVIDAD_IN == 2 ~ dplyr::if_else(
          is.na(GANANCIA_PRINC_IMP_MONTO), 0, as.numeric(GANANCIA_PRINC_IMP_MONTO)
        ),
        TRUE ~ 0
      ),
      ing_mensual_ocup_prin_cuenta_propia = as.double(ing_mensual_ocup_prin_cuenta_propia)
    ) %>%
    dplyr::select(-c(ingprinctaprop, periprinctaprop, diasprinctaprop))
}

ftc_ing_mensual_ocup_prin <- function(tbl){
  tbl %>%
    ftc_ing_mensual_ocup_prin_asalariado() %>%
    ftc_ing_mensual_ocup_prin_cuenta_propia() %>%
    ftc_ing_mensual_ocup_prin_independiente() %>%
    dplyr::mutate(
      ing_mensual_ocup_prin = ing_mensual_ocup_prin_asalariado + ing_mensual_ocup_prin_cuenta_propia + ing_mensual_ocup_prin_independiente
    ) %>%
    dplyr::select(-c(
      ing_mensual_ocup_prin_asalariado,
      ing_mensual_ocup_prin_cuenta_propia,
      ing_mensual_ocup_prin_independiente
    ))
}


#'  Ingreso monetario adicional mensual de comisiones por ocupacion principal (Pobreza Monetaria)
#' `r lifecycle::badge("stable")`
#'
#' @param tbl [data.frame] data.frame con los datos de la ENCFT
#'
#' @return [data.frame] los datos suministrados en el data.frame \code{tbl}
#' con la variable \code{ing_comisiones} adicionada.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' encft <- ftc_ing_comisiones(encft)
#' }
ftc_ing_comisiones <- function(tbl) {
  tbl %>%
    dplyr::mutate(
      COMISIONES_AP_MONTO = as.double(COMISIONES_AP_MONTO),
      ing_comisiones = dplyr::case_when(
        is.na(COMISIONES_AP_MONTO) ~ 0,
        COMISIONES_AP_MONTO >= 0 ~ COMISIONES_AP_MONTO,
        TRUE ~ 0
      ),
      ing_comisiones = as.double(ing_comisiones)
    )
}

#' Ingreso monetario adicional mensual de propinas por ocupacion principal (Pobreza Monetaria)
#' `r lifecycle::badge("stable")`
#'
#' @param tbl [data.frame] data.frame con los datos de la ENCFT
#'
#' @return [data.frame] los datos suministrados en el data.frame \code{tbl}
#' con la variable \code{ing_propinas} adicionada.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' encft <- ftc_ing_propinas(encft)
#' }
ftc_ing_propinas <- function(tbl) {
  tbl %>%
    dplyr::mutate(
      PROPINAS_AP_MONTO = as.double(PROPINAS_AP_MONTO),
      ing_propinas = dplyr::case_when(
        is.na(PROPINAS_AP_MONTO) ~ 0,
        PROPINAS_AP_MONTO >= 0 ~ PROPINAS_AP_MONTO,
        TRUE ~ 0
      ),
      ing_propinas = as.double(ing_propinas)
    )
}

#' Ingreso monetario adicional mensual de horas extras por ocupacion principal (Pobreza Monetaria)
#' `r lifecycle::badge("stable")`
#'
#' @param tbl [data.frame] data.frame con los datos de la ENCFT
#'
#' @return  [data.frame] los datos suministrados en el data.frame \code{tbl}
#' con la variable \code{ing_horas_extra} adicionada.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' encft <- ftc_ing_horas_extra(encft)
#' }
ftc_ing_horas_extra <- function(tbl) {
  tbl %>%
    dplyr::mutate(
      HORAS_EXTRA_AP_MONTO = as.double(HORAS_EXTRA_AP_MONTO),
      ing_horas_extra = dplyr::case_when(
        is.na(HORAS_EXTRA_AP_MONTO) ~ 0,
        HORAS_EXTRA_AP_MONTO >= 0 ~ HORAS_EXTRA_AP_MONTO,
        TRUE ~ 0
      ),
      ing_horas_extra = as.double(ing_horas_extra)
    )
}

#' Ingreso monetario adicional mensual de vacaciones pagadas por ocupacion principal (Pobreza Monetaria)
#' `r lifecycle::badge("stable")`
#'
#' @param tbl [data.frame] data.frame con los datos de la ENCFT
#'
#' @return [data.frame] los datos suministrados en el data.frame \code{tbl}
#' con la variable \code{ing_vacaciones} adicionada.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' encft <- ftc_ing_vacaciones(encft)
#' }
ftc_ing_vacaciones <- function(tbl) {
  tbl %>%
    dplyr::mutate(
      ing_vacaciones = dplyr::case_when(
        is.na(VACACIONES_AP_MONTO) ~ 0,
        VACACIONES_AP_MONTO >= 0 ~ VACACIONES_AP_MONTO / 12,
        TRUE ~ 0
      ),
      ing_vacaciones = as.double(ing_vacaciones)
    )
}

#' Ingreso monetario adicional mensual de dividendos por ocupacion principal (Pobreza Monetaria)
#' `r lifecycle::badge("stable")`
#'
#' @param tbl [data.frame] data.frame con los datos de la ENCFT
#'
#' @return [data.frame] los datos suministrados en el data.frame \code{tbl}
#' con la variable \code{ing_dividendos} adicionada.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' encft <- ftc_ing_dividendos(encft)
#' }
ftc_ing_dividendos <- function(tbl) {
  tbl %>%
    dplyr::mutate(
      ing_dividendos = dplyr::case_when(
        is.na(DIVIDENDOS_AP_MONTO) ~ 0,
        DIVIDENDOS_AP_MONTO >= 0 ~ as.numeric(DIVIDENDOS_AP_MONTO) / 12,
        TRUE ~ 0
      ),
      ing_dividendos = as.double(ing_dividendos)
    )
}

#' Ingreso monetario adicional mensual de bonificacion por ocupacion principal (Pobreza Monetaria)
#' `r lifecycle::badge("stable")`
#'
#' @param tbl [data.frame] data.frame con los datos de la ENCFT
#'
#' @return [data.frame] los datos suministrados en el data.frame \code{tbl}
#' con la variable \code{ing_bonificaciones} adicionada.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' encft <- ftc_ing_bonificaciones(encft)
#' }
ftc_ing_bonificaciones <- function(tbl) {
  tbl %>%
    dplyr::mutate(
      ing_bonificaciones = dplyr::case_when(
        is.na(BONIFICACION_AP_MONTO) ~ 0,
        BONIFICACION_AP_MONTO >= 0 ~ BONIFICACION_AP_MONTO / 12,
        TRUE ~ 0
      ),
      ing_bonificaciones = as.double(ing_bonificaciones)
    )
}


#' Ingreso monetario adicional mensual de regalia pascual por ocupacion principal (Pobreza Monetaria)
#' `r lifecycle::badge("stable")`
#'
#' @param tbl [data.frame] data.frame con los datos de la ENCFT
#'
#' @return [data.frame] los datos suministrados en el data.frame \code{tbl}
#' con la variable \code{ing_regalia_pascual} adicionada.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' encft <- ftc_ing_regalia_pascual(encft)
#' }
ftc_ing_regalia_pascual <- function(tbl) {
  tbl %>%
    dplyr::mutate(
      ing_regalia_pascual = dplyr::case_when(
        REGALIA_AP_MONTO >= 0 ~ REGALIA_AP_MONTO / 12,
        REGALIA_AP == 2 ~ REGALIA_PASCUAL,
        TRUE ~ 0
      ),
      ing_regalia_pascual = as.double(ing_regalia_pascual)
    )
}

#' Ingreso monetario adicional mensual de utilidades empresariales por ocupacion principal (Pobreza Monetaria)
#' `r lifecycle::badge("stable")`
#'
#' @param tbl [data.frame] data.frame con los datos de la ENCFT
#'
#' @return [data.frame] los datos suministrados en el data.frame \code{tbl}
#' con la variable \code{ing_utilidades_empresariales} adicionada.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' encft <- ftc_ing_utilidades_empresariales(encft)
#' }
ftc_ing_utilidades_empresariales <- function(tbl) {
  tbl %>%
    dplyr::mutate(
      ing_utilidades_empresariales = dplyr::case_when(
        is.na(UTILIDAD_EMPRESARIAL_AP_MONTO) ~ 0,
        UTILIDAD_EMPRESARIAL_AP_MONTO >= 0 ~ as.numeric(UTILIDAD_EMPRESARIAL_AP_MONTO) / 12,
        TRUE ~ 0
      ),
      ing_utilidades_empresariales = as.double(ing_utilidades_empresariales)
    )
}


#' Ingreso monetario adicional mensual de beneficios marginales por ocupacion principal (Pobreza Monetaria)
#' `r lifecycle::badge("stable")`
#'
#' @param tbl [data.frame] data.frame con los datos de la ENCFT
#'
#' @return [data.frame] los datos suministrados en el data.frame \code{tbl}
#' con la variable \code{ing_beneficios_marginales} adicionada.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' encft <- ftc_ing_beneficios_marginales(encft)
#' }
ftc_ing_beneficios_marginales <- function(tbl) {
  tbl %>%
    dplyr::mutate(
      ing_beneficios_marginales = dplyr::case_when(
        is.na(BENEFICIOS_MARGINALES_AP_MONTO) ~ 0,
        BENEFICIOS_MARGINALES_AP_MONTO >= 0 ~ as.numeric(BENEFICIOS_MARGINALES_AP_MONTO) / 12,
        TRUE ~ 0
      ), ing_beneficios_marginales = as.double(ing_beneficios_marginales)
    )
}

#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_otros_ocup_sec_asalariado <- function(tbl) {

  # Ingreso monetario mensual de otros ingresos monetarios mensuales por ocupación secundaria asalariada----
  tbl %>%
    mutate(
      OTROS_PAGO_AS_MONTO = as.double(OTROS_PAGO_AS_MONTO),
      ing_otros_ocup_sec_asalariado = case_when(
        is.na(OTROS_PAGO_AS_MONTO) ~ 0,
        OTROS_PAGO_AS_MONTO >= 0 ~ OTROS_PAGO_AS_MONTO,
        TRUE ~ 0
      ), ing_otros_ocup_sec_asalariado = as.double(ing_otros_ocup_sec_asalariado)
    )
}


#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_beneficios_marginales_anual <- function(tbl) {

  # Ingreso monetario mensual de otros ingresos por beneficios marginales anuales en ocupación secundaria asalariada----
  tbl %>%
    mutate(
      ing_beneficios_marginales_anual = case_when(
        is.na(OTROS_BENEFICIOS_AS_MONTO) ~ 0,
        OTROS_BENEFICIOS_AS_MONTO >= 0 ~ OTROS_BENEFICIOS_AS_MONTO / 12,
        TRUE ~ 0
      ), ing_beneficios_marginales_anual = as.double(ing_beneficios_marginales_anual)
    )
}


#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_especie_alimentos <- function(tbl) {

  # Ingreso en especie por alimentos adicional mensual por ocupacion principal----
  tbl %>%
    mutate(
      ing_especie_alimentos = case_when(
        is.na(ALIMENTACION_ESPECIE_AP_MONTO) ~ 0,
        ALIMENTACION_ESPECIE_AP == 1 ~ ALIMENTACION_ESPECIE_AP_MONTO,
        TRUE ~ 0
      ), ing_especie_alimentos = as.double(ing_especie_alimentos)
    )
}

#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_especie_viviendas <- function(tbl) {

  # Ingreso en especie por pago de viviendas adicional mensual por ocupacion principal----
  tbl %>%
    mutate(
      ing_especie_viviendas = case_when(
        is.na(VIVIENDA_ESPECIE_AP_MONTO) ~ 0,
        VIVIENDA_ESPECIE_AP == 1 ~ as.numeric(VIVIENDA_ESPECIE_AP_MONTO),
        TRUE ~ 0
      ), ing_especie_viviendas = as.double(ing_especie_viviendas)
    )
}

#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_especie_transporte <- function(tbl) {

  # Ingreso en especie por pago de transporte adicional mensual por ocupacion principal----
  tbl %>%
    mutate(
      TRANSPORTE_ESPECIE_AP_MONTO = as.double(TRANSPORTE_ESPECIE_AP_MONTO),
      ing_especie_transporte = case_when(
        is.na(TRANSPORTE_ESPECIE_AP_MONTO) ~ 0,
        TRANSPORTE_ESPECIE_AP == 1 ~ TRANSPORTE_ESPECIE_AP_MONTO,
        TRUE ~ 0
      ), ing_especie_transporte = as.double(ing_especie_transporte)
    )
}

#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_especie_combustible <- function(tbl) {

  # Ingreso en especie por pago de combustible mensual por ocupacion principal----
  tbl %>%
    mutate(
      GASOLINA_ESPECIE_AP_MONTO = as.double(GASOLINA_ESPECIE_AP_MONTO),
      ing_especie_combustible = case_when(
        is.na(GASOLINA_ESPECIE_AP_MONTO) ~ 0,
        GASOLINA_ESPECIE_AP == 1 ~ GASOLINA_ESPECIE_AP_MONTO,
        TRUE ~ 0
      ), ing_especie_combustible = as.double(ing_especie_combustible)
    )
}

#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_especie_celular <- function(tbl) {

  # Ingreso en especie por pago de celular adicional mensual por ocupaciones----
  tbl %>%
    mutate(
      CELULAR_ESPECIE_AP_MONTO = as.double(CELULAR_ESPECIE_AP_MONTO),
      ing_especie_celular = case_when(
        is.na(CELULAR_ESPECIE_AP_MONTO) ~ 0,
        CELULAR_ESPECIE_AP == 1 ~ CELULAR_ESPECIE_AP_MONTO,
        TRUE ~ 0
      ), ing_especie_celular = as.double(ing_especie_celular)
    )
}

#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_especie_otros <- function(tbl) {

  # Otros ingresos en especie ocupación principal asalariado----
  tbl %>%
    mutate(
      ing_especie_otros = case_when(
        is.na(OTROS_ESPECIE_AP_MONTO) ~ 0,
        OTROS_ESPECIE_AP == 1 ~ as.numeric(OTROS_ESPECIE_AP_MONTO),
        TRUE ~ 0
      ), ing_especie_otros = as.double(ing_especie_otros)
    )
}

#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_especie_cuenta_propia <- function(tbl) {

  # Ingreso en especie ocupación principal patronos y cuenta propia----
  tbl %>%
    mutate(
      ing_especie_cuenta_propia = case_when(
        is.na(PAGO_ESPECIES_IN_MONTO) ~ 0,
        PAGO_ESPECIES_IN_MONTO > 0 ~ as.numeric(PAGO_ESPECIES_IN_MONTO),
        TRUE ~ 0
      ), ing_especie_cuenta_propia = as.double(ing_especie_cuenta_propia)
    )
}

#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_especie_ocup_sec_asalarariado <- function(tbl) {

  # Ingreso en especie ocupación secundaria asalariado----
  tbl %>%
    mutate(
      PAGO_EN_ESPECIE_AS_MONTO = as.double(PAGO_EN_ESPECIE_AS_MONTO),
      ing_especie_ocup_sec_asalarariado = case_when(
        is.na(PAGO_EN_ESPECIE_AS_MONTO) ~ 0,
        PAGO_EN_ESPECIE_AS_MONTO > 0 ~ PAGO_EN_ESPECIE_AS_MONTO,
        TRUE ~ 0
      ), ing_especie_ocup_sec_asalarariado = as.double(ing_especie_ocup_sec_asalarariado)
    )
}

#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_especie_ocup_sec_cuenta_propia <- function(tbl) {

  # Ingreso en especie ocupación secundaria patronos y cuenta propia----
  tbl %>%
    mutate(
      ing_especie_ocup_sec_cuenta_propia = case_when(
        PAGO_ESPECIES_IS_MONTO > 0 ~ as.numeric(PAGO_ESPECIES_IS_MONTO),
        TRUE ~ 0
      ), ing_especie_ocup_sec_cuenta_propia = as.double(ing_especie_ocup_sec_cuenta_propia)
    )
}


#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_mensual_ocup_sec_asalariado <- function(tbl) {

  # Ingreso monetario mensual por ocupación secundaria asalariada----
  tbl %>%
  left_join_tipo_cambio() %>%
    mutate(
      SALARIO_SECUN_IMP_MONTO = ifelse(
        is.na(SALARIO_SECUN_IMP_MONTO), 0, SALARIO_SECUN_IMP_MONTO
      ),
      SUELDO_BRUTO_AS_MONTO = as.double(SUELDO_BRUTO_AS_MONTO),
      ing_mensual_ocup_sec_asalariado = case_when(
        SUELDO_BRUTO_AS_MONEDA == "DOP" ~ SUELDO_BRUTO_AS_MONTO,
        SUELDO_BRUTO_AS_MONEDA == "BRL" ~ SUELDO_BRUTO_AS_MONTO * lead(BRL),
        SUELDO_BRUTO_AS_MONEDA == "CAD" ~ SUELDO_BRUTO_AS_MONTO * lead(CAD),
        SUELDO_BRUTO_AS_MONEDA == "CHF" ~ SUELDO_BRUTO_AS_MONTO * lead(CHF),
        SUELDO_BRUTO_AS_MONEDA == "CNY" ~ SUELDO_BRUTO_AS_MONTO * lead(CNY),
        SUELDO_BRUTO_AS_MONEDA == "DEG" ~ SUELDO_BRUTO_AS_MONTO * lead(DEG),
        SUELDO_BRUTO_AS_MONEDA == "DKK" ~ SUELDO_BRUTO_AS_MONTO * lead(DKK),
        SUELDO_BRUTO_AS_MONEDA == "EUR" ~ SUELDO_BRUTO_AS_MONTO * lead(EUR),
        SUELDO_BRUTO_AS_MONEDA == "GBP" ~ SUELDO_BRUTO_AS_MONTO * lead(GBP),
        SUELDO_BRUTO_AS_MONEDA == "JPY" ~ SUELDO_BRUTO_AS_MONTO * lead(JPY),
        SUELDO_BRUTO_AS_MONEDA == "NOK" ~ SUELDO_BRUTO_AS_MONTO * lead(NOK),
        SUELDO_BRUTO_AS_MONEDA == "LESC" ~ SUELDO_BRUTO_AS_MONTO * lead(LESC),
        SUELDO_BRUTO_AS_MONEDA == "SEK" ~ SUELDO_BRUTO_AS_MONTO * lead(SEK),
        SUELDO_BRUTO_AS_MONEDA == "USD" ~ SUELDO_BRUTO_AS_MONTO * lead(USD),
        SUELDO_BRUTO_AS_MONEDA == "VEF" ~ SUELDO_BRUTO_AS_MONTO * lead(VEF),
        SUELDO_BRUTO_AS_MONEDA == "ARS" ~ SUELDO_BRUTO_AS_MONTO * ARS,
        SUELDO_BRUTO_AS == 2 ~ SALARIO_SECUN_IMP_MONTO,
        TRUE ~ 0
      ), ing_mensual_ocup_sec_asalariado = as.double(ing_mensual_ocup_sec_asalariado)
    )
}


#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_mensual_ocup_sec_independiente <- function(tbl) {

  # Ingreso monetario mensual por ocupación secundaria independiente agropecuario o contratista----
  tbl %>%
    mutate(
      ing_mensual_ocup_sec_independiente = case_when(
        GANANCIA_IS_PRODUCTOR_MONEDA == "DOP" ~ as.numeric(GANANCIA_IS_PRODUCTOR_MONTO) / 6,
        GANANCIA_IS_PRODUCTOR == 2 ~ as.numeric(GANANCIA_SECUN_IMP_MONTO),
        TRUE ~ 0
      ), ing_mensual_ocup_sec_independiente = as.double(ing_mensual_ocup_sec_independiente)
    )
}


#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_mensual_ocup_sec_cuenta_propia <- function(tbl) {

  # Ingreso monetario mensual por ocupacion secundaria para cuenta propia o patron----
  tbl <- tbl %>%
    left_join_tipo_cambio() %>%
    mutate(
      ingsecctaprop = case_when(
        INGRESO_ACTIVIDAD_IS_MONEDA == "DOP" ~ INGRESO_ACTIVIDAD_IS_MONTO,
        INGRESO_ACTIVIDAD_IS_MONEDA == "BRL" ~ INGRESO_ACTIVIDAD_IS_MONTO * lead(BRL),
        INGRESO_ACTIVIDAD_IS_MONEDA == "CAD" ~ INGRESO_ACTIVIDAD_IS_MONTO * lead(CAD),
        INGRESO_ACTIVIDAD_IS_MONEDA == "CHF" ~ INGRESO_ACTIVIDAD_IS_MONTO * lead(CHF),
        INGRESO_ACTIVIDAD_IS_MONEDA == "CNY" ~ INGRESO_ACTIVIDAD_IS_MONTO * lead(CNY),
        INGRESO_ACTIVIDAD_IS_MONEDA == "DEG" ~ INGRESO_ACTIVIDAD_IS_MONTO * lead(DEG),
        INGRESO_ACTIVIDAD_IS_MONEDA == "DKK" ~ INGRESO_ACTIVIDAD_IS_MONTO * lead(DKK),
        INGRESO_ACTIVIDAD_IS_MONEDA == "EUR" ~ INGRESO_ACTIVIDAD_IS_MONTO * lead(EUR),
        INGRESO_ACTIVIDAD_IS_MONEDA == "GBP" ~ INGRESO_ACTIVIDAD_IS_MONTO * lead(GBP),
        INGRESO_ACTIVIDAD_IS_MONEDA == "JPY" ~ INGRESO_ACTIVIDAD_IS_MONTO * lead(JPY),
        INGRESO_ACTIVIDAD_IS_MONEDA == "NOK" ~ INGRESO_ACTIVIDAD_IS_MONTO * lead(NOK),
        INGRESO_ACTIVIDAD_IS_MONEDA == "LESC" ~ INGRESO_ACTIVIDAD_IS_MONTO * lead(LESC),
        INGRESO_ACTIVIDAD_IS_MONEDA == "SEK" ~ INGRESO_ACTIVIDAD_IS_MONTO * lead(SEK),
        INGRESO_ACTIVIDAD_IS_MONEDA == "USD" ~ INGRESO_ACTIVIDAD_IS_MONTO * lead(USD),
        INGRESO_ACTIVIDAD_IS_MONEDA == "VEF" ~ INGRESO_ACTIVIDAD_IS_MONTO * lead(VEF),
        INGRESO_ACTIVIDAD_IS_MONEDA == "ARS" ~ INGRESO_ACTIVIDAD_IS_MONTO * ARS,
        TRUE ~ 0
      ), ingsecctaprop = as.double(ingsecctaprop)
    )

  # Conversión del ingreso de la ocupación secundaria de los cuenta propia y patronos a ingreso mensual
  tbl %>%
    mutate(
      perisecctaprop = ifelse(
        is.na(INGRESO_ACTIVIDAD_IS_PERIODO), 0, as.numeric(INGRESO_ACTIVIDAD_IS_PERIODO)
      ),
      diassecctaprop = ifelse(
        is.na(INGRESO_ACTIVIDAD_IS_DIAS), 0, as.numeric(INGRESO_ACTIVIDAD_IS_DIAS)
      ),
      ing_mensual_ocup_sec_cuenta_propia = case_when(
        perisecctaprop == 1 ~ 4.3 * diassecctaprop * ingsecctaprop,
        perisecctaprop == 2 ~ 4.3 * ingsecctaprop,
        perisecctaprop == 3 ~ 2 * ingsecctaprop,
        perisecctaprop == 4 ~ ingsecctaprop,
        INGRESO_ACTIVIDAD_IS == 2 ~ ifelse(
          is.na(GANANCIA_SECUN_IMP_MONTO), 0, as.numeric(GANANCIA_SECUN_IMP_MONTO)
        ),
        TRUE ~ 0
      ), ing_mensual_ocup_sec_cuenta_propia = as.double(ing_mensual_ocup_sec_cuenta_propia)
    ) %>%
    dplyr::select(-c(ingsecctaprop, perisecctaprop, diassecctaprop))
}

#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_pension_jubilacion <- function(tbl) {
  # Ingreso monetario no laboral por pensión o jubilación----
  tbl %>%
    mutate(
      PENSION_NAC_MONTO = as.double(PENSION_NAC_MONTO),
      ing_pension_jubilacion = case_when(
        is.na(PENSION_NAC_MONTO) ~ 0,
        PENSION_NAC == 1 ~ as.numeric(PENSION_NAC_MONTO),
        PENSION_NAC == 3 ~ as.numeric(PENSION_IMP_MONTO),
        TRUE ~ 0
      ), ing_pension_jubilacion = as.double(ing_pension_jubilacion)
    )
}

#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_intereses_dividendos <- function(tbl) {
  # Ingreso monetario no laboral por Intereses o dividendos----
  tbl %>%
    mutate(
      INTERESES_NAC_MONTO = as.double(INTERESES_NAC_MONTO),
      ing_intereses_dividendos = case_when(
        is.na(INTERESES_NAC_MONTO) ~ 0,
        INTERESES_NAC == 1 ~ INTERESES_NAC_MONTO,
        TRUE ~ 0
      ), ing_intereses_dividendos = as.double(ing_intereses_dividendos)
    )
}

#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_alquileres_renta <- function(tbl) {
  # Ingreso monetario no laboral por alquileres o rentas de propiedades----
  tbl %>%
    mutate(
      ALQUILER_NAC_MONTO = as.double(ALQUILER_NAC_MONTO),
      ing_alquileres_renta = case_when(
        is.na(ALQUILER_NAC_MONTO) ~ 0,
        ALQUILER_NAC == 1 ~ ALQUILER_NAC_MONTO,
        TRUE ~ 0
      ), ing_alquileres_renta = as.double(ing_alquileres_renta)
    )
}

#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_remesas_nacionales <- function(tbl) {
  # Ingreso monetario no laboral por remesas nacionales----
  tbl %>%
    mutate(
      REMESAS_NAC_MONTO = as.double(REMESAS_NAC_MONTO),
      ing_remesas_nacionales = case_when(
        is.na(REMESAS_NAC_MONTO) ~ 0,
        REMESAS_NAC == 1 ~ REMESAS_NAC_MONTO,
        TRUE ~ 0
      ), ing_remesas_nacionales = as.double(ing_remesas_nacionales)
    )
}

#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_pension_jubilacion_anual <- function(tbl) {
  # Ingreso monetario no laboral por pensión o jubilación, anual----
  tbl %>%
    mutate(
      ing_pension_jubilacion_anual = case_when(
        is.na(REGALIA_PENSION_NAC_ANO) ~ 0,
        REGALIA_PENSION_NAC_ANO == 1 ~ REGALIA_PENSION_NAC_ANO_MONTO / 12,
        TRUE ~ 0
      ), ing_pension_jubilacion_anual = as.double(ing_pension_jubilacion_anual)
    )
}

#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_intereses_dividendos_anual <- function(tbl) {
  # Ingreso monetario no laboral por Intereses o dividendos, anual----
  tbl %>%
    mutate(
      ing_intereses_dividendos_anual = case_when(
        is.na(INTERESES_NAC_ANO) ~ 0,
        INTERESES_NAC_ANO == 1 ~ as.numeric(INTERESES_NAC_ANO_MONTO) / 12,
        TRUE ~ 0
      ), ing_intereses_dividendos_anual = as.double(ing_intereses_dividendos_anual)
    )
}

#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_alquileres_renta_anual <- function(tbl) {

  # Ingreso monetario no laboral por alquileres o rentas de propiedades, anual----
  tbl %>%
    mutate(
      ing_alquileres_renta_anual = case_when(
        is.na(ALQUILER_NAC_ANO) ~ 0,
        ALQUILER_NAC_ANO == 1 ~ as.numeric(ALQUILER_NAC_ANO_MONTO) / 12,
        TRUE ~ 0
      ), ing_alquileres_renta_anual = as.double(ing_alquileres_renta_anual)
    )
}

#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_remesas_nacionales_anual <- function(tbl) {

  # Ingreso monetario no laboral por remesas nacionales, anual----
  tbl %>%
    mutate(
      ing_remesas_nacionales_anual = case_when(
        is.na(REMESAS_NAC_ANO) ~ 0,
        REMESAS_NAC_ANO == 1 ~ REMESAS_NAC_ANO_MONTO / 12,
        TRUE ~ 0
      ), ing_remesas_nacionales_anual = as.double(ing_remesas_nacionales_anual)
    )
}


#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_especie_ayuda_ong <- function(tbl) {

  # Ingreso en especie no laboral por ayuda de familiares, no familiares e instituciones---
  tbl %>%
    mutate(
      #AYUDA_ESPECIE_NAC_MONTO = as.double(AYUDA_ESPECIE_NAC_MONTO),
      ing_especie_ayuda_ong = case_when(
        is.na(AYUDA_ESPECIE_NAC_MONTO) ~ 0,
        AYUDA_ESPECIE_NAC_MONTO > 0 ~ AYUDA_ESPECIE_NAC_MONTO,
        TRUE ~ 0
      ),
      ing_especie_ayuda_ong = as.double(ing_especie_ayuda_ong)
    )
}


#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_especie_ayuda_ong_anual <- function(tbl) {

  # Ingreso en especie no laboral por ayuda de familiares, no familiares e instituciones---
  tbl %>%
    mutate(
      #AYUDA_ESPECIE_NAC_ANO_MONTO = as.double(AYUDA_ESPECIE_NAC_ANO_MONTO),
      ing_especie_ayuda_ong_anual = case_when(
        is.na(AYUDA_ESPECIE_NAC_ANO_MONTO) ~ 0,
        AYUDA_ESPECIE_NAC_ANO_MONTO > 0 ~ AYUDA_ESPECIE_NAC_ANO_MONTO / 12,
        TRUE ~ 0
      ),
      ing_especie_ayuda_ong_anual = as.double(ing_especie_ayuda_ong_anual)
    )
}

#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_autoconsumo_autosuministro <- function(tbl) {

  # Ingreso en especie no laboral por autoconsumo y auto suministro----
  tbl <- tbl %>%
    mutate(
      autoconsumoprin = case_when(
        is.na(CONSUMIO_BIENES_IN) | is.na(CONSUMIO_BIENES_IN_MONTO) ~ 0,
        CONSUMIO_BIENES_IN_MONTO > 0 ~ CONSUMIO_BIENES_IN_MONTO,
        TRUE ~ 0
      ),
      ing_autoconsumo_autosuministro = case_when(
        CONSUMIO_BIENES_IS_MONTO > 0 ~ CONSUMIO_BIENES_IS_MONTO,
        TRUE ~ 0
      ), ing_autoconsumo_autosuministro = as.double(ing_autoconsumo_autosuministro+autoconsumoprin)
    ) %>%
    dplyr::select(-autoconsumoprin)
}

#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_pension_ext <- function(tbl) {

  # Ingresos mensuales externos por pensión----
  tbl %>%
    left_join_tipo_cambio() %>%
    mutate(
      PENSION_EXT_MONTO = ifelse(
        is.na(PENSION_EXT_MONTO), 0, PENSION_EXT_MONTO
      ),
      PENSION_EXT_MONEDA = ifelse(
        is.na(PENSION_EXT_MONEDA), 0, PENSION_EXT_MONEDA
      ),
      PENSION_EXT_MONTO = as.double(PENSION_EXT_MONTO),
      ing_pension_ext = case_when(
        PENSION_EXT_MONEDA == "DOP" ~ PENSION_EXT_MONTO,
        PENSION_EXT_MONEDA == "BRL" ~ PENSION_EXT_MONTO * lead(BRL),
        PENSION_EXT_MONEDA == "CAD" ~ PENSION_EXT_MONTO * lead(CAD),
        PENSION_EXT_MONEDA == "CHF" ~ PENSION_EXT_MONTO * lead(CHF),
        PENSION_EXT_MONEDA == "CNY" ~ PENSION_EXT_MONTO * lead(CNY),
        PENSION_EXT_MONEDA == "DEG" ~ PENSION_EXT_MONTO * lead(DEG),
        PENSION_EXT_MONEDA == "DKK" ~ PENSION_EXT_MONTO * lead(DKK),
        PENSION_EXT_MONEDA == "EUR" ~ PENSION_EXT_MONTO * lead(EUR),
        PENSION_EXT_MONEDA == "GBP" ~ PENSION_EXT_MONTO * lead(GBP),
        PENSION_EXT_MONEDA == "JPY" ~ PENSION_EXT_MONTO * lead(JPY),
        PENSION_EXT_MONEDA == "NOK" ~ PENSION_EXT_MONTO * lead(NOK),
        PENSION_EXT_MONEDA == "LESC" ~ PENSION_EXT_MONTO * lead(LESC),
        PENSION_EXT_MONEDA == "SEK" ~ PENSION_EXT_MONTO * lead(SEK),
        PENSION_EXT_MONEDA == "USD" ~ PENSION_EXT_MONTO * lead(USD),
        PENSION_EXT_MONEDA == "VEF" ~ PENSION_EXT_MONTO * lead(VEF),
        PENSION_EXT_MONEDA == "ARS" ~ PENSION_EXT_MONTO * ARS,
        TRUE ~ 0
      ), ing_pension_ext = as.double(ing_pension_ext)
    )
}

#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_intereses_dividendos_ext <- function(tbl) {

  # Ingresos mensuales externos por intereses o dividendos en RD$----
  tbl %>%
    left_join_tipo_cambio() %>%
    mutate(
      ing_intereses_dividendos_ext = case_when(
        INTERES_EXT_MONEDA == "DOP" ~ as.numeric(INTERES_EXT_MONTO),
        INTERES_EXT_MONEDA == "BRL" ~ as.numeric(INTERES_EXT_MONTO) * lead(BRL),
        INTERES_EXT_MONEDA == "CAD" ~ as.numeric(INTERES_EXT_MONTO) * lead(CAD),
        INTERES_EXT_MONEDA == "CHF" ~ as.numeric(INTERES_EXT_MONTO) * lead(CHF),
        INTERES_EXT_MONEDA == "CNY" ~ as.numeric(INTERES_EXT_MONTO) * lead(CNY),
        INTERES_EXT_MONEDA == "DEG" ~ as.numeric(INTERES_EXT_MONTO) * lead(DEG),
        INTERES_EXT_MONEDA == "DKK" ~ as.numeric(INTERES_EXT_MONTO) * lead(DKK),
        INTERES_EXT_MONEDA == "EUR" ~ as.numeric(INTERES_EXT_MONTO) * lead(EUR),
        INTERES_EXT_MONEDA == "GBP" ~ as.numeric(INTERES_EXT_MONTO) * lead(GBP),
        INTERES_EXT_MONEDA == "JPY" ~ as.numeric(INTERES_EXT_MONTO) * lead(JPY),
        INTERES_EXT_MONEDA == "NOK" ~ as.numeric(INTERES_EXT_MONTO) * lead(NOK),
        INTERES_EXT_MONEDA == "LESC" ~ as.numeric(INTERES_EXT_MONTO) * lead(LESC),
        INTERES_EXT_MONEDA == "SEK" ~ as.numeric(INTERES_EXT_MONTO) * lead(SEK),
        INTERES_EXT_MONEDA == "USD" ~ as.numeric(INTERES_EXT_MONTO) * lead(USD),
        INTERES_EXT_MONEDA == "VEF" ~ as.numeric(INTERES_EXT_MONTO) * lead(VEF),
        INTERES_EXT_MONEDA == "ARS" ~ as.numeric(INTERES_EXT_MONTO) * ARS,
        TRUE ~ 0
      ), ing_intereses_dividendos_ext = as.double(ing_intereses_dividendos_ext)
    )
}


#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_alquileres_renta_ext <- function(tbl) {

  # Ingresos mensuales externos por alquileres---
  tbl %>%
    left_join_tipo_cambio() %>%
    mutate(
      ing_alquileres_renta_ext = case_when(
        ALQUILER_EXT_MONEDA == "DOP" ~ as.numeric(ALQUILER_EXT_MONTO),
        ALQUILER_EXT_MONEDA == "BRL" ~ as.numeric(ALQUILER_EXT_MONTO) * lead(BRL),
        ALQUILER_EXT_MONEDA == "CAD" ~ as.numeric(ALQUILER_EXT_MONTO) * lead(CAD),
        ALQUILER_EXT_MONEDA == "CHF" ~ as.numeric(ALQUILER_EXT_MONTO) * lead(CHF),
        ALQUILER_EXT_MONEDA == "CNY" ~ as.numeric(ALQUILER_EXT_MONTO) * lead(CNY),
        ALQUILER_EXT_MONEDA == "DEG" ~ as.numeric(ALQUILER_EXT_MONTO) * lead(DEG),
        ALQUILER_EXT_MONEDA == "DKK" ~ as.numeric(ALQUILER_EXT_MONTO) * lead(DKK),
        ALQUILER_EXT_MONEDA == "EUR" ~ as.numeric(ALQUILER_EXT_MONTO) * lead(EUR),
        ALQUILER_EXT_MONEDA == "GBP" ~ as.numeric(ALQUILER_EXT_MONTO) * lead(GBP),
        ALQUILER_EXT_MONEDA == "JPY" ~ as.numeric(ALQUILER_EXT_MONTO) * lead(JPY),
        ALQUILER_EXT_MONEDA == "NOK" ~ as.numeric(ALQUILER_EXT_MONTO) * lead(NOK),
        ALQUILER_EXT_MONEDA == "LESC" ~ as.numeric(ALQUILER_EXT_MONTO) * lead(LESC),
        ALQUILER_EXT_MONEDA == "SEK" ~ as.numeric(ALQUILER_EXT_MONTO) * lead(SEK),
        ALQUILER_EXT_MONEDA == "USD" ~ as.numeric(ALQUILER_EXT_MONTO) * lead(USD),
        ALQUILER_EXT_MONEDA == "VEF" ~ as.numeric(ALQUILER_EXT_MONTO) * lead(VEF),
        ALQUILER_EXT_MONEDA == "ARS" ~ as.numeric(ALQUILER_EXT_MONTO) * ARS,
        TRUE ~ 0
      ), ing_alquileres_renta_ext = as.double(ing_alquileres_renta_ext)
    )
}

#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_regalos_ext <- function(tbl) {

  # Ingresos externos por regalos----
  tbl %>%
    mutate(
      REGALOS_EXT_MONTO = as.double(REGALOS_EXT_MONTO),
      ing_regalos_ext = case_when(
        is.na(REGALOS_EXT_MONTO) ~ 0,
        REGALOS_EXT == 1 ~ REGALOS_EXT_MONTO,
        TRUE ~ 0
      ), ing_regalos_ext = as.double(ing_regalos_ext)
    )
}

#' Title
#'
#' @param tbl
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_remesas_ext <- function(tbl) {
  tbl <- tbl

  # Ingreso monetarios remesas del exterior----
  REMEXMONTO <- tbl %>%
    select(
      TRIMESTRE, PERIODO, ANO, MES, VIVIENDA, HOGAR, MIEMBRO, MES1_1_EXT_MONTO, MES2_1_EXT_MONTO, MES3_1_EXT_MONTO,
      MES4_1_EXT_MONTO, MES5_1_EXT_MONTO, MES6_1_EXT_MONTO, MES1_2_EXT_MONTO, MES2_2_EXT_MONTO, MES3_2_EXT_MONTO,
      MES4_2_EXT_MONTO, MES5_2_EXT_MONTO, MES6_2_EXT_MONTO, MES1_3_EXT_MONTO, MES2_3_EXT_MONTO, MES3_3_EXT_MONTO,
      MES4_3_EXT_MONTO, MES5_3_EXT_MONTO, MES6_3_EXT_MONTO
    ) %>%
    mutate(across(everything(), as.numeric)) %>%
    #gather(key = "MES_EXT_MONTO", value = "montoenvio", MES1_1_EXT_MONTO:MES6_3_EXT_MONTO, na.rm = T) %>%
    tidyr::pivot_longer( MES1_1_EXT_MONTO:MES6_3_EXT_MONTO, names_to = "MES_EXT_MONTO", values_to = "montoenvio", values_drop_na = TRUE) %>%
    mutate(
      mes1_6 = substr(MES_EXT_MONTO, 4, 4),
      bloque = substr(MES_EXT_MONTO, 6, 6),
      mesenvio = case_when(
        MES == 1 & mes1_6 == 1 ~ 07,
        MES == 1 & mes1_6 == 2 ~ 08,
        MES == 1 & mes1_6 == 3 ~ 09,
        MES == 1 & mes1_6 == 4 ~ 10,
        MES == 1 & mes1_6 == 5 ~ 11,
        MES == 1 & mes1_6 == 6 ~ 12,
        MES == 2 & mes1_6 == 1 ~ 08,
        MES == 2 & mes1_6 == 2 ~ 09,
        MES == 2 & mes1_6 == 3 ~ 10,
        MES == 2 & mes1_6 == 4 ~ 11,
        MES == 2 & mes1_6 == 5 ~ 12,
        MES == 2 & mes1_6 == 6 ~ 01,
        MES == 3 & mes1_6 == 1 ~ 09,
        MES == 3 & mes1_6 == 2 ~ 10,
        MES == 3 & mes1_6 == 3 ~ 11,
        MES == 3 & mes1_6 == 4 ~ 12,
        MES == 3 & mes1_6 == 5 ~ 01,
        MES == 3 & mes1_6 == 6 ~ 02,
        MES == 4 & mes1_6 == 1 ~ 10,
        MES == 4 & mes1_6 == 2 ~ 11,
        MES == 4 & mes1_6 == 3 ~ 12,
        MES == 4 & mes1_6 == 4 ~ 01,
        MES == 4 & mes1_6 == 5 ~ 02,
        MES == 4 & mes1_6 == 6 ~ 03,
        MES == 5 & mes1_6 == 1 ~ 11,
        MES == 5 & mes1_6 == 2 ~ 12,
        MES == 5 & mes1_6 == 3 ~ 01,
        MES == 5 & mes1_6 == 4 ~ 02,
        MES == 5 & mes1_6 == 5 ~ 03,
        MES == 5 & mes1_6 == 6 ~ 04,
        MES == 6 & mes1_6 == 1 ~ 12,
        MES == 6 & mes1_6 == 2 ~ 01,
        MES == 6 & mes1_6 == 3 ~ 02,
        MES == 6 & mes1_6 == 4 ~ 03,
        MES == 6 & mes1_6 == 5 ~ 04,
        MES == 6 & mes1_6 == 6 ~ 05,
        MES == 7 & mes1_6 == 1 ~ 01,
        MES == 7 & mes1_6 == 2 ~ 02,
        MES == 7 & mes1_6 == 3 ~ 03,
        MES == 7 & mes1_6 == 4 ~ 04,
        MES == 7 & mes1_6 == 5 ~ 05,
        MES == 7 & mes1_6 == 6 ~ 06,
        MES == 8 & mes1_6 == 1 ~ 02,
        MES == 8 & mes1_6 == 2 ~ 03,
        MES == 8 & mes1_6 == 3 ~ 04,
        MES == 8 & mes1_6 == 4 ~ 05,
        MES == 8 & mes1_6 == 5 ~ 06,
        MES == 8 & mes1_6 == 6 ~ 07,
        MES == 9 & mes1_6 == 1 ~ 03,
        MES == 9 & mes1_6 == 2 ~ 04,
        MES == 9 & mes1_6 == 3 ~ 05,
        MES == 9 & mes1_6 == 4 ~ 06,
        MES == 9 & mes1_6 == 5 ~ 07,
        MES == 9 & mes1_6 == 6 ~ 08,
        MES == 10 & mes1_6 == 1 ~ 04,
        MES == 10 & mes1_6 == 2 ~ 05,
        MES == 10 & mes1_6 == 3 ~ 06,
        MES == 10 & mes1_6 == 4 ~ 07,
        MES == 10 & mes1_6 == 5 ~ 08,
        MES == 10 & mes1_6 == 6 ~ 09,
        MES == 11 & mes1_6 == 1 ~ 05,
        MES == 11 & mes1_6 == 2 ~ 06,
        MES == 11 & mes1_6 == 3 ~ 07,
        MES == 11 & mes1_6 == 4 ~ 08,
        MES == 11 & mes1_6 == 5 ~ 09,
        MES == 11 & mes1_6 == 6 ~ 10,
        MES == 12 & mes1_6 == 1 ~ 06,
        MES == 12 & mes1_6 == 2 ~ 07,
        MES == 12 & mes1_6 == 3 ~ 08,
        MES == 12 & mes1_6 == 4 ~ 09,
        MES == 12 & mes1_6 == 5 ~ 10,
        MES == 12 & mes1_6 == 6 ~ 11
      ),
      periodoenvio = case_when(
        MES > 6 & mesenvio < 10 ~ as.numeric(paste(ANO, mesenvio, sep = "0")),
        MES > 6 & mesenvio >= 10 ~ as.numeric(paste(ANO, mesenvio, sep = "")),
        MES <= 6 & mesenvio < 6 ~ as.numeric(paste(ANO, mesenvio, sep = "0")),
        MES <= 6 & mesenvio > 6 & mesenvio < 10 ~ as.numeric(paste(ANO - 1, mesenvio, sep = "0")),
        MES <= 6 & mesenvio > 6 & mesenvio >= 10 ~ as.numeric(paste(ANO - 1, mesenvio, sep = ""))
      )
    )

  REMEXMONEDA <- tbl %>%
    select(
      TRIMESTRE, PERIODO, ANO, MES, VIVIENDA, HOGAR, MIEMBRO, MES1_1_EXT_MONEDA, MES2_1_EXT_MONEDA, MES3_1_EXT_MONEDA,
      MES4_1_EXT_MONEDA, MES5_1_EXT_MONEDA, MES6_1_EXT_MONEDA, MES1_2_EXT_MONEDA, MES2_2_EXT_MONEDA, MES3_2_EXT_MONEDA,
      MES4_2_EXT_MONEDA, MES5_2_EXT_MONEDA, MES6_2_EXT_MONEDA, MES1_3_EXT_MONEDA, MES2_3_EXT_MONEDA, MES3_3_EXT_MONEDA,
      MES4_3_EXT_MONEDA, MES5_3_EXT_MONEDA, MES6_3_EXT_MONEDA
    ) %>%
    #gather(key = "MES_EXT_MONEDA", value = "monedaenvio", MES1_1_EXT_MONEDA:MES6_3_EXT_MONEDA, na.rm = T) %>%
    tidyr::pivot_longer(MES1_1_EXT_MONEDA:MES6_3_EXT_MONEDA, names_to = "MES_EXT_MONEDA", values_to = "monedaenvio", values_drop_na = TRUE) %>%
    mutate(
      mes1_6 = substr(MES_EXT_MONEDA, 4, 4),
      bloque = substr(MES_EXT_MONEDA, 6, 6)
    )

  REMESA <- left_join(REMEXMONTO, REMEXMONEDA, by = c("TRIMESTRE", "PERIODO", "ANO", "MES", "VIVIENDA", "HOGAR", "MIEMBRO", "mes1_6", "bloque"), copy = TRUE)
  rm(REMEXMONTO, REMEXMONEDA)

  TDC <- tipo_cambio %>%
    rename(periodoenvio = PERIODO)

  REMESA <- full_join(REMESA, TDC, by = "periodoenvio", copy = TRUE)
  #REMESA <- REMESA[order(REMESA$periodoenvio), ]

  REMESA <- REMESA %>%
    dplyr::arrange(periodoenvio) %>%
    mutate(
    remedop = case_when(
      monedaenvio == "DOP" ~ montoenvio,
      monedaenvio == "BRL" ~ montoenvio * lead(BRL),
      monedaenvio == "CAD" ~ montoenvio * lead(CAD),
      monedaenvio == "CHF" ~ montoenvio * lead(CHF),
      monedaenvio == "CNY" ~ montoenvio * lead(CNY),
      monedaenvio == "DEG" ~ montoenvio * lead(DEG),
      monedaenvio == "DKK" ~ montoenvio * lead(DKK),
      monedaenvio == "EUR" ~ montoenvio * lead(EUR),
      monedaenvio == "GBP" ~ montoenvio * lead(GBP),
      monedaenvio == "JPY" ~ montoenvio * lead(JPY),
      monedaenvio == "NOK" ~ montoenvio * lead(NOK),
      monedaenvio == "LESC" ~ montoenvio * lead(LESC),
      monedaenvio == "SEK" ~ montoenvio * lead(SEK),
      monedaenvio == "USD" ~ montoenvio * lead(USD),
      monedaenvio == "VEF" ~ montoenvio * lead(VEF),
      monedaenvio == "ARS" ~ montoenvio * ARS,
      TRUE ~ 0
    ),
    remedop = as.double(remedop)
  )

  REMESA <- REMESA %>%
    group_by(TRIMESTRE, PERIODO, ANO, MES, VIVIENDA, HOGAR, MIEMBRO, periodoenvio, mesenvio, mes1_6) %>%
    summarise(mestotal = sum(remedop, na.rm = T))

  REMESA <- REMESA %>%
    group_by(TRIMESTRE, PERIODO, ANO, MES, VIVIENDA, HOGAR, MIEMBRO) %>%
    summarise(ing_remesas_ext = mean(mestotal, na.rm = T)) %>%
    mutate(ing_remesas_ext = as.double(ing_remesas_ext))

  left_join(tbl, REMESA, by = c("PERIODO", "TRIMESTRE", "ANO", "MES", "VIVIENDA", "HOGAR", "MIEMBRO"), copy = TRUE) %>%
    mutate(ing_remesas_ext = replace_na(ing_remesas_ext, 0))
}


#' Title
#'
#' @param tbl
#' @param deflactar
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_alquiler_imputado <- function(tbl, deflactar = TRUE) {

  # Monto probable de alquiler vivienda no alquilada----
  tbl <- tbl %>%
    mutate(
      ing_alquiler_imputado = case_when(
        is.na(MONTO_ALQUILARIA_VIVIENDA_MES) ~ 0,
        MONTO_ALQUILARIA_VIVIENDA_MES >= 0 ~ MONTO_ALQUILARIA_VIVIENDA_MES,
        TRUE ~ 0
      ), ing_alquiler_imputado = as.double(ing_alquiler_imputado)
    )
    if(deflactar){

  # Ingreso por alquiler imputado deflactado----
  tbl <- tbl %>%
    left_join(ipc_2020, by = c("PERIODO"), copy = TRUE) %>%
    mutate(
      ing_alquiler_imputado_def = IPCcentral / IPCanterior * ing_alquiler_imputado,
      ing_alquiler_imputado_def = as.double(ing_alquiler_imputado_def)
    ) %>%
    dplyr::select(-dplyr::starts_with("IPC"))
    }

  tbl

  }


#' Title
#'
#' @param tbl
#' @param deflactar
#' @param .keep
#' @param .reuse
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_laboral_monetario <- function(tbl, deflactar = TRUE, .keep = FALSE, .reuse = FALSE) {
  # - [ ] TODO: NO hay algunos no monetarios aquí?
  ingresos <- c(
    "ing_mensual_ocup_prin_asalariado",
    "ing_mensual_ocup_prin_cuenta_propia",
    "ing_comisiones",
    "ing_propinas",
    "ing_horas_extra",
    "ing_especie_alimentos",
    "ing_especie_viviendas",
    "ing_especie_transporte",
    "ing_especie_combustible",
    "ing_especie_celular",
    "ing_mensual_ocup_sec_asalariado",
    "ing_mensual_ocup_sec_cuenta_propia",
    "ing_otros_ocup_sec_asalariado",
    "ing_beneficios_marginales_anual",
    "ing_vacaciones",
    "ing_dividendos",
    "ing_bonificaciones",
    "ing_regalia_pascual",
    "ing_utilidades_empresariales",
    "ing_beneficios_marginales",
    "ing_pension_jubilacion_anual",
    "ing_mensual_ocup_prin_independiente",
    "ing_mensual_ocup_sec_independiente"
  )

  if (is.logical(.reuse)) {
    if (.reuse) {
      for (ingreso in ingresos) {
        if (ingreso %in% dplyr::tbl_vars(tbl)) {
          ingresos <- ingresos[ingresos != ingreso]
        }
      }
    }
  } else {
    for (ingreso in .reuse) {
      ingresos <- ingresos[ingresos != ingreso]
    }
  }

  cli::cli_progress_bar("Ingresos monetarios laborales", total = length(ingresos))
  for (ingreso in ingresos) {
    tbl <- get(paste0("ftc_", ingreso))(tbl)
    cli::cli_progress_update()
  }
  cli::cli_progress_done()

  # ml = monetario laboral----
  tbl <- tbl %>%
    mutate(
      ml_1 = ing_mensual_ocup_prin_asalariado + ing_mensual_ocup_prin_cuenta_propia + ing_comisiones + ing_propinas + ing_horas_extra
        + ing_especie_alimentos + ing_especie_viviendas + ing_especie_transporte + ing_especie_combustible + ing_especie_celular + ing_mensual_ocup_sec_asalariado
        + ing_mensual_ocup_sec_cuenta_propia + ing_otros_ocup_sec_asalariado + ing_beneficios_marginales_anual,
      ml_2 = ing_vacaciones + ing_dividendos + ing_bonificaciones + ing_regalia_pascual + ing_utilidades_empresariales + ing_beneficios_marginales + ing_pension_jubilacion_anual,
      ml_3 = ing_mensual_ocup_prin_independiente + ing_mensual_ocup_sec_independiente,
      ing_laboral_monetario = ml_1 + ml_2 + ml_3,
      ing_laboral_monetario = as.double(ing_laboral_monetario),
      ml_1 = as.double(ml_1),
      ml_2 = as.double(ml_2),
      ml_3 = as.double(ml_3)
    )
  if (deflactar) {

    # Ingreso monetario laboral deflactado
    # Ingresos laborales de los agricultores y contratistas se deflactan con ipc promedio de los ultimos seis meses
    # Ingreso monetario laboral total deflactado
    tbl <- tbl %>%
      left_join(ipc_2020, by = c("PERIODO"), copy = TRUE) %>%
      mutate(
        ml_1_def = IPCcentral / IPCanterior * ml_1,
        ml_1_def = as.double(ml_1_def),
        ml_3_def = IPCcentral / IPCprom * ml_3,
        ml_3_def = as.double(ml_3_def),
        ing_laboral_monetario_def = ml_1_def + ml_2 + ml_3_def,
        ing_laboral_monetario_def = as.double(ing_laboral_monetario_def)
      ) %>%
      select(-dplyr::starts_with("IPC"))
  }



  if (is.logical(.keep)) {
    if (.keep) {
      ingresos <- c()
    }
  } else {
    for (ingreso in .keep) {
      ingresos <- ingresos[ingresos != ingreso]
    }
  }

  tbl %>%
    dplyr::select(-dplyr::all_of(ingresos)) %>%
    dplyr::select(-c(ml_1, ml_2, ml_3, ml_1_def, ml_3_def))
}

#' Title
#'
#' @param tbl
#' @param deflactar
#' @param .keep
#' @param .reuse
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_no_monetario_laboral <- function(tbl, deflactar = TRUE, .keep = FALSE, .reuse = FALSE) {
  ingresos <- c(
    "ing_especie_ocup_sec_cuenta_propia",
    "ing_especie_ocup_sec_asalarariado",
    "ing_especie_cuenta_propia",
    "ing_especie_otros"
  )

  if (is.logical(.reuse)) {
    if (.reuse) {
      for (ingreso in ingresos) {
        if (ingreso %in% dplyr::tbl_vars(tbl)) {
          ingresos <- ingresos[ingresos != ingreso]
        }
      }
    }
  } else {
    for (ingreso in .reuse) {
      ingresos <- ingresos[ingresos != ingreso]
    }
  }

  cli::cli_progress_bar("Ingresos no monetarios laboral", total = length(ingresos))
  for (ingreso in ingresos) {
    tbl <- get(paste0("ftc_", ingreso))(tbl)
    cli::cli_progress_update()
  }
  cli::cli_progress_done()

  # ing_no_monetario_laboral= no monetario laboral----
  tbl <- tbl %>%
    mutate(
      ing_no_monetario_laboral = ing_especie_otros + ing_especie_cuenta_propia + ing_especie_ocup_sec_asalarariado + ing_especie_ocup_sec_cuenta_propia,
      ing_no_monetario_laboral = as.double(ing_no_monetario_laboral)
    )

  if (deflactar) {

    # Ingreso no monetario laboral total deflactado----
    tbl <- tbl %>%
      left_join(ipc_2020, by = c("PERIODO"), copy = TRUE) %>%
      mutate(
        ing_no_monetario_laboral_def = IPCcentral / IPCanterior * ing_no_monetario_laboral,
        ing_no_monetario_laboral_def = as.double(ing_no_monetario_laboral_def)
      ) %>%
      select(-dplyr::starts_with("IPC"))
  }



  if (is.logical(.keep)) {
    if (.keep) {
      ingresos <- c()
    }
  } else {
    for (ingreso in .keep) {
      ingresos <- ingresos[ingresos != ingreso]
    }
  }

  tbl %>%
    dplyr::select(-dplyr::all_of(ingresos))
}

#' Title
#'
#' @param tbl
#' @param deflactar
#' @param .keep
#' @param .reuse
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_monetario_no_laboral <- function(tbl, deflactar = TRUE, .keep = FALSE, .reuse = FALSE) {
  ingresos <- c(
    "ing_remesas_nacionales",
    "ing_alquileres_renta",
    "ing_intereses_dividendos",
    "ing_pension_jubilacion",
    "ing_especie_ayuda_ong_anual",
    "ing_remesas_nacionales_anual",
    "ing_alquileres_renta_anual",
    "ing_intereses_dividendos_anual"
  )

  if (is.logical(.reuse)) {
    if (.reuse) {
      for (ingreso in ingresos) {
        if (ingreso %in% dplyr::tbl_vars(tbl)) {
          ingresos <- ingresos[ingresos != ingreso]
        }
      }
    }
  } else {
    for (ingreso in .reuse) {
      ingresos <- ingresos[ingresos != ingreso]
    }
  }

  cli::cli_progress_bar("Ingresos monetarios no laborales", total = length(ingresos))
  for (ingreso in ingresos) {
    tbl <- get(paste0("ftc_", ingreso))(tbl)
    cli::cli_progress_update()
  }
  cli::cli_progress_done()

  # ing_monetario_no_laboral= monetario no laboral----
  tbl <- tbl %>% mutate(
    mnl_1 = ing_intereses_dividendos_anual + ing_alquileres_renta_anual + ing_remesas_nacionales_anual + ing_especie_ayuda_ong_anual,
    mnl_2 = ing_pension_jubilacion + ing_intereses_dividendos + ing_alquileres_renta + ing_remesas_nacionales,
    ing_monetario_no_laboral = mnl_1 + mnl_2,
    ing_monetario_no_laboral = as.double(ing_monetario_no_laboral),
    mnl_1 = as.double(mnl_1),
    mnl_2 = as.double(mnl_2)
  )
  if (deflactar) {

    # Ingreso monetario no laboral deflactado----
    tbl <- tbl %>%
      left_join(ipc_2020, by = c("PERIODO"), copy = TRUE) %>%
      mutate(
        mnl_2_def = IPCcentral / IPCanterior * mnl_2,
        mnl_2_def = as.double(mnl_2_def),
        ing_monetario_no_laboral_def = mnl_1 + mnl_2_def,
        ing_monetario_no_laboral_def = as.double(ing_monetario_no_laboral_def)
      ) %>%
      select(-dplyr::starts_with("IPC"))
  }



  if (is.logical(.keep)) {
    if (.keep) {
      ingresos <- c()
    }
  } else {
    for (ingreso in .keep) {
      ingresos <- ingresos[ingresos != ingreso]
    }
  }

  tbl %>%
    dplyr::select(-dplyr::all_of(ingresos)) %>%
    dplyr::select(-c(mnl_1, mnl_2, mnl_2_def))
}

#' Title
#'
#' @param tbl
#' @param deflactar
#' @param .keep
#' @param .reuse
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_no_monetario_no_laboral <- function(tbl, deflactar = TRUE, .keep = FALSE, .reuse = FALSE) {
  ingresos <- c(
    "ing_autoconsumo_autosuministro",
    "ing_especie_ayuda_ong"
  )

  if (is.logical(.reuse)) {
    if (.reuse) {
      for (ingreso in ingresos) {
        if (ingreso %in% dplyr::tbl_vars(tbl)) {
          ingresos <- ingresos[ingresos != ingreso]
        }
      }
    }
  } else {
    for (ingreso in .reuse) {
      ingresos <- ingresos[ingresos != ingreso]
    }
  }

  cli::cli_progress_bar("Ingresos no monetarios no laborales", total = length(ingresos))
  for (ingreso in ingresos) {
    tbl <- get(paste0("ftc_", ingreso))(tbl)
    cli::cli_progress_update()
  }
  cli::cli_progress_done()

  # ing_no_monetario_no_laboral = no monetario no laboral----
  tbl <- tbl %>% mutate(
    ing_no_monetario_no_laboral = ing_especie_ayuda_ong + ing_autoconsumo_autosuministro,
    ing_no_monetario_no_laboral = as.double(ing_no_monetario_no_laboral)
  )

  if (deflactar) {

    # Ingreso no monetario no laboral total deflactado----
    tbl <- tbl %>%
      left_join(ipc_2020, by = c("PERIODO"), copy = TRUE) %>%
      mutate(
        ing_no_monetario_no_laboral_def = IPCcentral / IPCanterior * ing_no_monetario_no_laboral,
        ing_no_monetario_no_laboral_def = as.double(ing_no_monetario_no_laboral_def)
      ) %>%
      select(-dplyr::starts_with("IPC"))
  }



  if (is.logical(.keep)) {
    if (.keep) {
      ingresos <- c()
    }
  } else {
    for (ingreso in .keep) {
      ingresos <- ingresos[ingresos != ingreso]
    }
  }

  tbl %>%
    dplyr::select(-dplyr::all_of(ingresos))
}

#' Title
#'
#' @param tbl
#' @param deflactar
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_transferencias_sociales <- function(tbl, deflactar = TRUE) {
    # Ingreso monetario no laboral por ayuda del gobierno----
   tbl<- tbl %>% mutate(
      comeresprimero = case_when(
        is.na(GOBIERNO_NAC) ~ 0,
        is.na(GOB_COMER_PRIMERO_MONTO) ~ 0,
        GOBIERNO_NAC == 1 ~ as.numeric(GOB_COMER_PRIMERO_MONTO),
        TRUE ~ 0
      ),
      comeresprimero = as.double(comeresprimero),
      asistescolar = case_when(
        is.na(GOBIERNO_NAC) ~ 0,
        is.na(GOB_INC_ASIS_ESCOLAR_MONTO) ~ 0,
        GOBIERNO_NAC == 1 ~ GOB_INC_ASIS_ESCOLAR_MONTO,
        TRUE ~ 0
      ),
      asistescolar = as.double(asistescolar),
      bonoluzhog = case_when(
        is.na(GOBIERNO_NAC) ~ 0,
        is.na(GOB_BONO_LUZ_MONTO) ~ 0,
        GOBIERNO_NAC == 1 ~ GOB_BONO_LUZ_MONTO,
        TRUE ~ 0
      ),
      bonoluzhog = as.double(bonoluzhog),
      bonogaschof = case_when(
        is.na(GOBIERNO_NAC) ~ 0,
        is.na(GOB_BONOGAS_CHOFERES_MONTO) ~ 0,
        GOBIERNO_NAC == 1 ~ GOB_BONOGAS_CHOFERES_MONTO,
        TRUE ~ 0
      ),
      bonogaschof = as.double(bonogaschof),
      bonogashog = case_when(
        is.na(GOBIERNO_NAC) ~ 0,
        is.na(GOB_BONOGAS_HOGARES_MONTO) ~ 0,
        GOBIERNO_NAC == 1 ~ GOB_BONOGAS_HOGARES_MONTO,
        TRUE ~ 0
      ),
      bonogashog = as.double(bonogashog),
      protvejez = case_when(
        is.na(GOBIERNO_NAC) ~ 0,
        is.na(GOB_PROTECCION_VEJEZ_MONTO) ~ 0,
        GOBIERNO_NAC == 1 ~ GOB_PROTECCION_VEJEZ_MONTO,
        TRUE ~ 0
      ),
      protvejez = as.double(protvejez),
      bonoest = case_when(
        is.na(GOBIERNO_NAC) ~ 0,
        is.na(GOB_BONO_ESTUDIANTE_PROG_MONTO) ~ 0,
        GOBIERNO_NAC == 1 ~ GOB_BONO_ESTUDIANTE_PROG_MONTO,
        TRUE ~ 0
      ),
      bonoest = as.double(bonoest),
      incedusup = case_when(
        is.na(GOBIERNO_NAC) ~ 0,
        is.na(GOB_INC_EDUCACION_SUP_MONTO) ~ 0,
        GOBIERNO_NAC == 1 ~ GOB_INC_EDUCACION_SUP_MONTO,
        TRUE ~ 0
      ),
      incedusup = as.double(incedusup),
      incpolpre = case_when(
        is.na(GOBIERNO_NAC) ~ 0,
        is.na(GOB_INC_POLICIA_PREV_MONTO) ~ 0,
        GOBIERNO_NAC == 1 ~ GOB_INC_POLICIA_PREV_MONTO,
        TRUE ~ 0
      ),
      incpolpre = as.double(incpolpre),
      incmarina = case_when(
        is.na(GOBIERNO_NAC) ~ 0,
        is.na(GOB_INC_MARINA_GUERRA_MONTO) ~ 0,
        GOBIERNO_NAC == 1 ~ GOB_INC_MARINA_GUERRA_MONTO,
        TRUE ~ 0
      ),
      incmarina = as.double(incmarina),
      ingquedate = case_when(
        is.na(GOBIERNO_NAC) ~ 0,
        is.na(GOB_QUEDATE_EN_CASA) ~ 0,
        GOBIERNO_NAC == 1 ~ as.numeric(GOB_QUEDATE_EN_CASA),
        TRUE ~ 0),
      ingquedate=as.double(ingquedate),
      ingfase = case_when(
        is.na(GOBIERNO_NAC) ~ 0,
        is.na(GOB_FONDO_ASISTENCIA_FASE) ~ 0,
        GOBIERNO_NAC == 1 ~ as.numeric(GOB_FONDO_ASISTENCIA_FASE),
        TRUE ~ 0),
      ingfase=as.double(ingfase),
      ingpati = case_when(
        is.na(GOBIERNO_NAC) ~ 0,
        is.na(GOB_PROGRAMA_PATI) ~ 0,
        GOBIERNO_NAC == 1 ~ as.numeric(GOB_PROGRAMA_PATI),
        TRUE ~ 0),
      ingpati=as.double(ingpati),

      superate = case_when(
        is.na(GOBIERNO_NAC) ~ 0,
        is.na(SUPERATE) ~ 0,
        GOBIERNO_NAC==1 ~ as.numeric(SUPERATE),
        TRUE ~ 0),
      superate = as.double(superate),
      ing_transferencias_sociales = comeresprimero + asistescolar + bonoluzhog + bonogaschof + bonogashog
      + protvejez + bonoest + incedusup + incpolpre + incmarina + ingquedate + ingfase + ingpati + superate
    ) %>%
     dplyr::select(-c(comeresprimero, asistescolar, bonoluzhog, bonogaschof, bonogashog,
                      protvejez, bonoest, incedusup, incpolpre, incmarina, ingquedate, ingfase, ingpati, superate))

  if(deflactar){
  # Ingreso por Transferencias Sociales deflactado----
  tbl <- tbl %>%
    left_join(ipc_2020, by = c("PERIODO"), copy = TRUE) %>%
    mutate(
      ing_transferencias_sociales_def = IPCcentral / IPCanterior * ing_transferencias_sociales,
      ing_transferencias_sociales_def = as.double(ing_transferencias_sociales_def)
    ) %>%
    select(-dplyr::starts_with("IPC"))
  }
  tbl
}

#' Title
#'
#' @param tbl
#' @param deflactar
#' @param .keep
#' @param .reuse
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_monetario_ext <- function(tbl, deflactar = TRUE, .keep = FALSE, .reuse = FALSE) {
  tbl <- tbl %>%
    dplyr::mutate(dplyr::across(dplyr::contains("_EXT_MONEDA"), as.character))
  ingresos <- c(
    "ing_pension_ext",
    "ing_intereses_dividendos_ext",
    "ing_alquileres_renta_ext",
    "ing_regalos_ext",
    "ing_remesas_ext"
  )

  if (is.logical(.reuse)) {
    if (.reuse) {
      for (ingreso in ingresos) {
        if (ingreso %in% dplyr::tbl_vars(tbl)) {
          ingresos <- ingresos[ingresos != ingreso]
        }
      }
    }
  } else {
    for (ingreso in .reuse) {
      ingresos <- ingresos[ingresos != ingreso]
    }
  }

  cli::cli_progress_bar("Ingresos monetarios del exterior", total = length(ingresos))
  for (ingreso in ingresos) {
    tbl <- get(paste0("ftc_", ingreso))(tbl)
    cli::cli_progress_update()
  }
  cli::cli_progress_done()


  # ing_monetario_ext = monetario exerior----
  tbl <- tbl %>% mutate(
    mext_1 = ing_pension_ext + ing_intereses_dividendos_ext + ing_alquileres_renta_ext + ing_regalos_ext,
    mext_2 = ing_remesas_ext,
    ing_monetario_ext = mext_1 + mext_2,
    mext_1 = as.double(mext_1),
    mext_2 = as.double(mext_2),
    ing_monetario_ext = as.double(ing_monetario_ext)
  )
  if(deflactar){

  # Ingreso monetario del exterior total deflactado----
  # Ingreso monetario del exterior (mext_1) deflactado
  # Ingreso monetario del exterior por remesas (mext_2) deflactado
  # Ingreso monetario del exterior total deflactado
  tbl <- tbl %>%
    left_join(ipc_2020, by = c("PERIODO"), copy = TRUE) %>%
    mutate(
      mext_1_def = IPCcentral / IPCanterior * mext_1,
      mext_1_def = as.double(mext_1_def),
      mext_2_def = IPCcentral / IPCprom * mext_2,
      mext_2_def = as.double(mext_2_def),
      ing_monetario_ext_def = mext_1_def + mext_2_def,
      ing_monetario_ext_def = as.double(ing_monetario_ext_def)
    ) %>%
    select(-dplyr::starts_with("IPC"))
  }



  if (is.logical(.keep)) {
    if (.keep) {
      ingresos <- c()
    }
  } else {
    for (ingreso in .keep) {
      ingresos <- ingresos[ingresos != ingreso]
    }
  }

  tbl %>%
    dplyr::select(-dplyr::all_of(ingresos)) %>%
    dplyr::select(-c(mext_1, mext_2, mext_1_def, mext_2_def))
}

#' Title
#'
#' @param tbl
#' @param .keep
#' @param .reuse
#'
#' @return
#' @export
#'
#' @examples
ftc_ing_total_pobreza <- function(tbl, .keep = FALSE, .reuse = FALSE) {
  ingresos <- c(
    "ing_alquiler_imputado",
    "ing_laboral_monetario",
    "ing_no_monetario_laboral",
    "ing_monetario_no_laboral",
    "ing_no_monetario_no_laboral",
    "ing_transferencias_sociales",
    "ing_monetario_ext"
  )

  pkr <- c(
    "ing_laboral_monetario",
    "ing_no_monetario_laboral",
    "ing_monetario_no_laboral",
    "ing_no_monetario_no_laboral",
    "ing_monetario_ext"
  )

  if (is.logical(.reuse)) {
    if (.reuse) {
      for (ingreso in ingresos) {
        if (ingreso %in% dplyr::tbl_vars(tbl)) {
          ingresos <- ingresos[ingresos != ingreso]
        }
      }
    }
  } else {
    for (ingreso in .reuse) {
      ingresos <- ingresos[ingresos != ingreso]
    }
  }

  cli::cli_progress_bar("Agrupando y deflactando ingresos", total = length(ingresos))
  for (ingreso in ingresos) {
    if(ingreso %in% pkr){
    tbl <- get(paste0("ftc_", ingreso))(tbl, .keep = .keep, .reuse = .reuse)
    } else {
      tbl <- get(paste0("ftc_", ingreso))(tbl)
    }
    cli::cli_progress_update()
  }
  cli::cli_progress_done()



  # Ingreso total deflactado----
  # Excluye alquiler imputado porque no se debe agregar
  # Agregación de la variable Ingreso total deflactado
  # Variable ingreso total más alquiler imputado
  tbl <- tbl %>%
    mutate(
      ing_total_pobreza = ing_laboral_monetario + ing_no_monetario_laboral + ing_monetario_no_laboral + ing_no_monetario_no_laboral + ing_transferencias_sociales + ing_monetario_ext,
      ing_total_pobreza = as.double(ing_total_pobreza)
    ) %>%
    left_join(ipc_2020, by = c("PERIODO"), copy = TRUE) %>%
    mutate(
      ing_total_pobreza_def = ing_laboral_monetario_def + ing_no_monetario_laboral_def + ing_monetario_no_laboral_def + ing_no_monetario_no_laboral_def + ing_transferencias_sociales_def + ing_monetario_ext_def,
      ing_total_pobreza_def = as.double(ing_total_pobreza_def)
    ) %>%
    group_by(TRIMESTRE, VIVIENDA, HOGAR) %>%
    mutate(
      ingresototal_sum = sum(ing_total_pobreza),
      ingtotaldeflactado_sum = sum(ing_total_pobreza_def)
    ) %>%
    dplyr::ungroup() %>%
    mutate(
      ing_total_pobreza = ingresototal_sum + ing_alquiler_imputado,
      ing_total_pobreza = as.double(ing_total_pobreza),
      ing_total_pobreza_def = ingtotaldeflactado_sum + ing_alquiler_imputado_def,
      ing_total_pobreza_def = as.double(ing_total_pobreza_def)
    ) %>%
    select(-dplyr::starts_with("IPC")) %>%
    dplyr::select(-c(ingresototal_sum, ingtotaldeflactado_sum))


  if (is.logical(.keep)) {
    if (.keep) {
      ingresos <- c()
    } else {
      for (ingreso in ingresos) {
        if(paste0(ingreso, "_def") %in% tbl_vars(tbl)){
          ingresos <- append(ingresos, paste0(ingreso, "_def"))
        }
      }
    }
  } else {
    for (ingreso in .keep) {
      ingresos <- ingresos[ingresos != ingreso]
      if(all(paste0(ingreso, "_def") %in% tbl_vars(tbl), !(paste0(ingreso, "_def") %in% .keep))){
        ingresos <- append(ingresos, paste0(ingreso, "_def"))
      }
    }
  }

  tbl %>%
    dplyr::select(-dplyr::all_of(ingresos))

}


#' Ingreso per cápita deflactado (Pobreza Monetaria)
#'
#' @param tbl DataFrame con los datos de la encuesta
#' @param .keep Lista de variables intermedias a mantener en el output.
#'           Vea detalles en la función ftc_pobreza_monetaria.
#' @param .reuse Lista de variables intermedias a reutilizar del input.
#'           Vea detalles en la función ftc_pobreza_monetaria.
#'
#' @return Los datos suministrados en el input con la variable \code{ing_pc_pobreza_def} agregada.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ftc_ing_pc_pobreza_def(encft)
#' }
ftc_ing_pc_pobreza_def <- function(tbl, .keep = FALSE, .reuse = FALSE){
  # Variable del ingreso total per cápita
  tbl %>%
    ftc_ing_total_pobreza(.keep, .reuse) %>%
    group_by(TRIMESTRE, VIVIENDA, HOGAR) %>%
    mutate(
      ing_pc_pobreza_def = ing_total_pobreza_def / n(),
      ing_pc_pobreza_def = as.double(ing_pc_pobreza_def)
    ) %>%
    ungroup()
}



#' Pobreza monetaria por zona de residencia
#'   `r lifecycle::badge("experimental")`
#'
#' @param tbl Un data.frame con los datos de la encuesta
#' @param .keep Lista de variables intermedias a mantener en el output.
#'           Vea detalles para más información.
#' @param .reuse Lista de variables intermedias a reutilizar del input.
#'           Vea detalles para más información.
#'
#' @return los datos del input tbl más las variables:
#'     \itemize{
#'      \item ing_pc_pobreza_def: Ingreso per cápita deflactado (Pobreza Monetaria)
#'      \item ing_total_pobreza_def: Ingreso total deflactado (Pobreza Monetaria)
#'      \item ing_total_pobreza: Ingreso total (Pobreza Monetaria)
#'      \item pobreza_monetaria: Pobreza monetaria
#'    }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tbl <- ftc_pobreza_zona(tbl, conn)
#' }
#'
ftc_pobreza_monetaria <- function(tbl, .keep = FALSE, .reuse = FALSE) {

  tbl <- tbl %>%
    #left_join(tipo_cambio, by = "PERIODO") %>%
    ftc_ing_pc_pobreza_def(.keep, .reuse) %>%
    left_join(lineas_pobreza, by = c("TRIMESTRE", "ZONA"), copy = TRUE) %>%
    mutate(
      pobreza_monetaria = case_when(
        ing_pc_pobreza_def < lpextrema ~ 1,
        ing_pc_pobreza_def < lpgeneral ~ 2,
        TRUE ~ 3
      )
    ) %>%
    dplyr::select(-c(lpextrema, lpgeneral))

  tryCatch({
  tbl <- tbl %>%
     dplyr::select(-dplyr::all_of(names(tipo_cambio)[names(tipo_cambio) != "PERIODO"]))
  }, error = function(e){})
  tbl
}
