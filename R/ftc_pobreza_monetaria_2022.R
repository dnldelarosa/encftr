ftc_pm22_row_sums <- function(tbl, cols) {
  cols <- cols[cols %in% names(tbl)]
  if (!length(cols)) {
    return(rep(0, nrow(tbl)))
  }

  rowSums(dplyr::select(tbl, dplyr::all_of(cols)), na.rm = TRUE)
}

ftc_pm22_normalize_currency <- function(x) {
  out <- toupper(trimws(as.character(x)))
  out[out %in% c("", "NA")] <- NA_character_

  numeric_codes <- c(
    "1" = "DOP", "2" = "EUR", "3" = "USD", "4" = "REAL", "5" = "CAD",
    "6" = "CHF", "7" = "CNY", "8" = "DEG", "9" = "DKK", "10" = "GBP",
    "11" = "LESC", "12" = "JPY", "13" = "NOK", "14" = "SEK",
    "15" = "VEF", "16" = "ARS"
  )

  translated <- unname(numeric_codes[out])
  out[!is.na(translated)] <- translated[!is.na(translated)]
  out[out == "BRL"] <- "REAL"

  out
}

ftc_pm22_macro_region <- function(grupo_region, id_provincia) {
  out <- rep(NA_character_, length(id_provincia))
  grupo_region <- as.character(grupo_region)
  provincia <- suppressWarnings(as.integer(id_provincia))

  out[grupo_region == "Gran Santo Domingo"] <- "Ozama"
  out[grupo_region == "Norte o Cibao"] <- "Norte"
  out[grupo_region == "Sur"] <- "Sur"
  out[grupo_region == "Este"] <- "Este"

  out[is.na(out) & provincia %in% c(1L, 32L)] <- "Ozama"
  out[is.na(out) & provincia %in% c(8L, 11L, 12L, 23L, 29L, 30L)] <- "Este"
  out[is.na(out) & provincia %in% c(2L, 3L, 4L, 7L, 10L, 16L, 17L, 21L, 22L, 31L)] <- "Sur"
  out[is.na(out) & provincia %in% c(5L, 6L, 9L, 13L, 14L, 15L, 18L, 19L, 20L, 24L, 25L, 26L, 27L, 28L)] <- "Norte"

  out
}

ftc_pm22_build_tipo_cambio_lookup <- function() {
  tdc <- tipo_cambio

  tdc_std <- dplyr::transmute(
    tdc,
    PERIODO = as.numeric(PERIODO),
    DOP = 1,
    EUR = as.numeric(EUR),
    USD = as.numeric(USD),
    REAL = as.numeric(BRL),
    CAD = as.numeric(CAD),
    CHF = as.numeric(CHF),
    CNY = as.numeric(CNY),
    DEG = as.numeric(DEG),
    DKK = as.numeric(DKK),
    GBP = as.numeric(GBP),
    LESC = as.numeric(LESC),
    JPY = as.numeric(JPY),
    NOK = as.numeric(NOK),
    SEK = as.numeric(SEK),
    VEF = as.numeric(VEF),
    ARS = as.numeric(ARS)
  )

  ars_patch <- c(
    `201611` = 3.07,
    `201612` = 2.92,
    `201701` = 2.94,
    `201702` = 3.01,
    `201703` = 3.03,
    `201704` = 3.04
  )

  patch_idx <- match(as.numeric(names(ars_patch)), tdc_std$PERIODO)
  tdc_std$ARS[patch_idx[!is.na(patch_idx)]] <- unname(ars_patch[!is.na(patch_idx)])

  tdc_std <- tdc_std %>%
    dplyr::mutate(
      ano = floor(PERIODO / 100),
      mes = PERIODO %% 100,
      trimestre = dplyr::case_when(
        mes <= 3 ~ 1,
        mes <= 6 ~ 2,
        mes <= 9 ~ 3,
        TRUE ~ 4
      )
    )

  moneda_levels <- c(
    "DOP", "EUR", "USD", "REAL", "CAD", "CHF", "CNY", "DEG",
    "DKK", "GBP", "LESC", "JPY", "NOK", "SEK", "VEF", "ARS"
  )

  tdc_long <- tidyr::pivot_longer(
    tdc_std,
    cols = dplyr::all_of(moneda_levels),
    names_to = "moneda",
    values_to = "tasa"
  ) %>%
    dplyr::mutate(moneda = factor(moneda, levels = moneda_levels)) %>%
    dplyr::arrange(PERIODO, moneda) %>%
    dplyr::mutate(
      tasa_a_pesos_mes6 = dplyr::lag(tasa, 16),
      tasa_a_pesos_mes5 = dplyr::lag(tasa_a_pesos_mes6, 16),
      tasa_a_pesos_mes4 = dplyr::lag(tasa_a_pesos_mes5, 16),
      tasa_a_pesos_mes3 = dplyr::lag(tasa_a_pesos_mes4, 16),
      tasa_a_pesos_mes2 = dplyr::lag(tasa_a_pesos_mes3, 16),
      tasa_a_pesos_mes1 = dplyr::lag(tasa_a_pesos_mes2, 16)
    ) %>%
    dplyr::mutate(
      tasa_a_pesos_promedio = rowMeans(
        dplyr::select(
          .,
          tasa_a_pesos_mes1,
          tasa_a_pesos_mes2,
          tasa_a_pesos_mes3,
          tasa_a_pesos_mes4,
          tasa_a_pesos_mes5,
          tasa_a_pesos_mes6
        ),
        na.rm = TRUE
      )
    ) %>%
    dplyr::transmute(
      periodo = PERIODO,
      ano = ano,
      trimestre = trimestre,
      mes = mes,
      moneda = as.character(moneda),
      tasa_a_pesos_mes1 = tasa_a_pesos_mes1,
      tasa_a_pesos_mes2 = tasa_a_pesos_mes2,
      tasa_a_pesos_mes3 = tasa_a_pesos_mes3,
      tasa_a_pesos_mes4 = tasa_a_pesos_mes4,
      tasa_a_pesos_mes5 = tasa_a_pesos_mes5,
      tasa_a_pesos_mes6 = tasa_a_pesos_mes6,
      tasa_a_pesos_promedio = tasa_a_pesos_promedio
    )

  tdc_long
}

ftc_pm22_autocoerce_numeric <- function(encft) {
  protected <- c(
    "grupo_region",
    grep("_moneda$", names(encft), value = TRUE)
  )

  for (col in setdiff(names(encft), protected)) {
    if (!is.character(encft[[col]])) {
      next
    }

    values <- trimws(encft[[col]])
    values[values == ""] <- NA_character_
    numeric_like <- is.na(values) | grepl("^[-+]?[0-9]*\\.?[0-9]+$", values)

    if (all(numeric_like)) {
      encft[[col]] <- suppressWarnings(as.numeric(values))
    }
  }

  encft
}

ftc_pm22_ensure_columns <- function(encft) {
  numeric_zero <- c(
    "remesas_nac_ano_monto",
    "superate",
    "quedate_en_casa",
    "fase",
    "regalia_princ_imp_monto",
    "regalia_pension_imp_monto",
    "ayuda_empresas_nac",
    "ayuda_empresas_nac_monto",
    "ayuda_empresas_nac_ano_monto",
    "otros_ingresos_nac_monto",
    "otros_ingresos_nac_ano_monto",
    "alimentos_escuela_nac_monto",
    "crianza_no_remun_monto",
    "crianza_no_remun_porc",
    "otros_ingresos_ext",
    "otros_ingresos_ext_monto"
  )

  numeric_na <- c(
    "remesas_nac_ano",
    "otros_ingresos_ext_moneda",
    "grupo_region"
  )

  alias_map <- c(
    superate = "gob_programa_superate",
    quedate_en_casa = "gob_quedate_en_casa",
    fase = "gob_fondo_asistencia_fase",
    ayuda_especie_nac_ano_monto = "ayuda_especie_nac_anio"
  )

  for (alias in names(alias_map)) {
    source_name <- alias_map[[alias]]
    if (!(alias %in% names(encft)) && source_name %in% names(encft)) {
      encft[[alias]] <- encft[[source_name]]
    }
  }

  for (name in numeric_zero) {
    if (!(name %in% names(encft))) {
      encft[[name]] <- 0
    }
  }

  for (name in numeric_na) {
    if (!(name %in% names(encft))) {
      encft[[name]] <- NA_real_
    }
  }

  encft
}

ftc_pm22_join_base_currency_rates <- function(encft, tdc_lookup) {
  rate_sources <- c(
    "sueldo_bruto_ap",
    "ingreso_actividad_in",
    "ingreso_actividad_is",
    "regalos_ext",
    "otros_ingresos_ext",
    "alquiler_ext",
    "interes_ext",
    "pension_ext"
  )

  for (prefix in rate_sources) {
    moneda_col <- paste0(prefix, "_moneda")
    tasa_col <- paste0(prefix, "_tasa")

    if (!(moneda_col %in% names(encft))) {
      next
    }

    lookup <- dplyr::transmute(
      tdc_lookup,
      periodo = periodo,
      !!moneda_col := moneda,
      !!tasa_col := tasa_a_pesos_mes6
    )

    encft <- dplyr::left_join(encft, lookup, by = c("periodo", moneda_col))
  }

  encft
}

ftc_pm22_join_remittance_rates <- function(encft, tdc_lookup) {
  for (bloque in 1:3) {
    for (mes_idx in 1:6) {
      moneda_col <- paste0("mes", mes_idx, "_", bloque, "_ext_moneda")
      tasa_col <- paste0("mes", mes_idx, "_", bloque, "_ext_tasa")
      tasa_src <- paste0("tasa_a_pesos_mes", mes_idx)

      if (!(moneda_col %in% names(encft))) {
        next
      }

      lookup <- dplyr::transmute(
        tdc_lookup,
        periodo = periodo,
        !!moneda_col := moneda,
        !!tasa_col := .data[[tasa_src]]
      )

      encft <- dplyr::left_join(encft, lookup, by = c("periodo", moneda_col))
    }
  }

  encft
}

ftc_pm22_build_outputs <- function(encft, original_names, .keep = FALSE) {
  outputs <- dplyr::transmute(
    encft,
    .encftr_rowid,
    macro_region_pobreza = macro_region,
    ing_total_pobreza_def = Ihog_ENCFT_a,
    ing_pc_pobreza_def = IPCm_ENCFT,
    linea_pobreza = linea_pob,
    linea_pobreza_extrema = linea_ind,
    pobreza_monetaria = dplyr::case_when(
      IPCm_ENCFT < linea_ind ~ 1L,
      IPCm_ENCFT < linea_pob ~ 2L,
      TRUE ~ 3L
    )
  ) %>%
    dplyr::mutate(
      pobre = pobreza_monetaria < 3L,
      indigente = pobreza_monetaria == 1L
    )

  if (isTRUE(.keep)) {
    candidate_cols <- setdiff(names(encft), c(original_names, ".encftr_rowid"))
    extras <- dplyr::select(encft, dplyr::all_of(c(".encftr_rowid", candidate_cols)))
    outputs <- dplyr::left_join(outputs, extras, by = ".encftr_rowid")
  } else if (is.character(.keep)) {
    keep_cols <- intersect(.keep, setdiff(names(encft), original_names))
    if (length(keep_cols)) {
      extras <- dplyr::select(encft, dplyr::all_of(c(".encftr_rowid", keep_cols)))
      outputs <- dplyr::left_join(outputs, extras, by = ".encftr_rowid")
    }
  }

  outputs
}

ftc_pobreza_monetaria_2022_impl <- function(tbl, .keep = FALSE, .reuse = FALSE) {
  rm(.reuse)

  required_cols <- c("PERIODO", "TRIMESTRE", "VIVIENDA", "HOGAR")
  missing_required <- setdiff(required_cols, names(tbl))
  if (length(missing_required)) {
    stop(
      "Faltan columnas requeridas para metodología 2022: ",
      paste(missing_required, collapse = ", "),
      call. = FALSE
    )
  }

  tbl_out <- tbl
  tbl_out$.encftr_rowid <- seq_len(nrow(tbl_out))
  encft <- tbl_out
  encft$.encftr_rowid <- seq_len(nrow(encft))
  original_names <- tolower(names(encft))
  names(encft) <- tolower(names(encft))
  encft <- ftc_pm22_ensure_columns(encft)
  encft <- ftc_pm22_autocoerce_numeric(encft)

  encft$personas <- 1
  encft <- encft %>%
    dplyr::group_by(trimestre, vivienda, hogar) %>%
    dplyr::mutate(miembros = sum(personas)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(periodo, vivienda, hogar, miembros)

  currency_cols <- c(
    "sueldo_bruto_ap_moneda",
    "ingreso_actividad_in_moneda",
    "ingreso_actividad_is_moneda",
    "regalos_ext_moneda",
    "otros_ingresos_ext_moneda",
    "alquiler_ext_moneda",
    "interes_ext_moneda",
    "pension_ext_moneda",
    outer(1:6, 1:3, function(i, j) paste0("mes", i, "_", j, "_ext_moneda"))
  )
  currency_cols <- intersect(as.vector(currency_cols), names(encft))
  for (col in currency_cols) {
    encft[[col]] <- ftc_pm22_normalize_currency(encft[[col]])
  }

  tdc_lookup <- ftc_pm22_build_tipo_cambio_lookup()
  encft <- ftc_pm22_join_base_currency_rates(encft, tdc_lookup)
  encft <- ftc_pm22_join_remittance_rates(encft, tdc_lookup)

  encft$p_s4b42 <- dplyr::case_when(
    encft$tiempo_recibe_pago_ap == 4 ~ encft$sueldo_bruto_ap_monto * encft$sueldo_bruto_ap_tasa,
    encft$tiempo_recibe_pago_ap == 3 ~ encft$sueldo_bruto_ap_monto * encft$sueldo_bruto_ap_tasa * 2,
    encft$tiempo_recibe_pago_ap == 2 ~ encft$sueldo_bruto_ap_monto * encft$sueldo_bruto_ap_tasa * 4.3,
    encft$tiempo_recibe_pago_ap == 1 ~ encft$sueldo_bruto_ap_monto * encft$tiempo_recibe_pago_dias_ap * encft$sueldo_bruto_ap_tasa * 4.3,
    TRUE ~ NA_real_
  )
  encft$salario_princ_imp_monto1 <- ifelse(encft$sueldo_bruto_ap == 2, encft$salario_princ_imp_monto, NA_real_)
  encft$p_s4b42_1 <- ftc_pm22_row_sums(encft, c("salario_princ_imp_monto1", "p_s4b42"))

  encft$p_s4b43_1 <- ifelse(encft$comisiones_ap == 1, encft$comisiones_ap_monto, NA_real_)
  encft$p_s4b43_2 <- ifelse(encft$propinas_ap == 1, encft$propinas_ap_monto, NA_real_)
  encft$p_s4b43_3 <- ifelse(encft$horas_extra_ap == 1, encft$horas_extra_ap_monto, NA_real_)
  encft$p_s4b43_99 <- ifelse(encft$otros_pagos_ap == 1, encft$otros_pagos_ap_monto, NA_real_)
  encft$p_s4b44_1 <- ifelse(encft$vacaciones_ap == 1, encft$vacaciones_ap_monto / 12, NA_real_)
  encft$p_s4b44_5 <- ifelse(encft$dividendos_ap == 1, encft$dividendos_ap_monto / 12, NA_real_)
  encft$p_s4b44_2 <- ifelse(encft$bonificacion_ap == 1, encft$bonificacion_ap_monto / 12, NA_real_)
  encft$p_s4b44_3 <- ifelse(
    encft$regalia_ap == 1,
    encft$regalia_ap_monto / 12,
    ifelse(encft$regalia_ap == 2, encft$regalia_princ_imp_monto / 12, NA_real_)
  )
  encft$p_s4b44_7 <- ifelse(encft$utilidad_empresarial_ap == 1, encft$utilidad_empresarial_ap_monto / 12, NA_real_)
  encft$p_s4b44_6 <- ifelse(encft$beneficios_marginales_ap == 1, encft$beneficios_marginales_ap_monto / 12, NA_real_)
  encft$p_s4b44_4 <- ifelse(encft$incentivo_antiguedad_ap == 1, encft$incentivo_antiguedad_ap_monto / 12, NA_real_)
  encft$p_s4b44_99 <- ifelse(encft$otros_beneficios_ap == 1, encft$otros_beneficios_ap_monto / 12, NA_real_)

  encft$p_s4b45_1 <- ifelse(!is.na(encft$alimentacion_especie_ap_monto), encft$alimentacion_especie_ap_monto, NA_real_)
  encft$p_s4b45_2 <- ifelse(!is.na(encft$vivienda_especie_ap_monto), encft$vivienda_especie_ap_monto, NA_real_)
  encft$p_s4b45_3 <- ifelse(!is.na(encft$transporte_especie_ap_monto), encft$transporte_especie_ap_monto, NA_real_)
  encft$p_s4b45_4 <- ifelse(!is.na(encft$gasolina_especie_ap_monto), encft$gasolina_especie_ap_monto, NA_real_)
  encft$p_s4b45_5 <- ifelse(!is.na(encft$celular_especie_ap_monto), encft$celular_especie_ap_monto, NA_real_)
  encft$p_s4b45_99 <- ifelse(!is.na(encft$otros_especie_ap_monto), encft$otros_especie_ap_monto, NA_real_)

  encft$p_s4b71 <- ifelse(encft$sueldo_bruto_as == 1, encft$sueldo_bruto_as_monto, encft$salario_secun_imp_monto)
  encft$p_s4b72 <- ifelse(encft$otros_pago_as == 1, encft$otros_pago_as_monto, NA_real_)
  encft$p_s4b73 <- ifelse(encft$otros_beneficios_as == 1, encft$otros_beneficios_as_monto / 12, NA_real_)
  encft$p_s4b74 <- ifelse(encft$pago_en_especie_as == 1, encft$pago_en_especie_as_monto, NA_real_)

  encft$p_s4b53_1 <- dplyr::case_when(
    encft$ingreso_actividad_in_periodo == 4 ~ encft$ingreso_actividad_in_monto * encft$ingreso_actividad_in_tasa,
    encft$ingreso_actividad_in_periodo == 3 ~ encft$ingreso_actividad_in_monto * encft$ingreso_actividad_in_tasa * 2,
    encft$ingreso_actividad_in_periodo == 2 ~ encft$ingreso_actividad_in_monto * encft$ingreso_actividad_in_tasa * 4.3,
    encft$ingreso_actividad_in_periodo == 1 ~ encft$ingreso_actividad_in_monto * encft$ingreso_actividad_in_dias * encft$ingreso_actividad_in_tasa * 4.3,
    TRUE ~ NA_real_
  )
  encft$ganancia_princ_imp_monto1 <- ifelse(encft$ingreso_actividad_in == 2, encft$ganancia_princ_imp_monto, 0)
  encft$p_s4b52 <- ifelse(encft$ganancia_in_productor == 1, encft$ganancia_in_productor_monto / 6, 0)
  encft$ganancia_princ_imp_monto2 <- ifelse(encft$ganancia_in_productor == 2, encft$ganancia_princ_imp_monto, 0)
  encft$p_s4b5_2a3 <- ftc_pm22_row_sums(encft, c("p_s4b52", "p_s4b53_1", "ganancia_princ_imp_monto1", "ganancia_princ_imp_monto2"))
  encft$p_s4b55 <- ifelse(encft$pago_especies_in == 1, encft$pago_especies_in_monto, NA_real_)
  encft$p_s4b54 <- ifelse(encft$consumio_bienes_in == 1, encft$consumio_bienes_in_monto, NA_real_)

  encft$p_s4b83 <- dplyr::case_when(
    encft$ingreso_actividad_is_periodo == 4 ~ encft$ingreso_actividad_is_monto * encft$ingreso_actividad_is_tasa,
    encft$ingreso_actividad_is_periodo == 3 ~ encft$ingreso_actividad_is_monto * encft$ingreso_actividad_is_tasa * 2,
    encft$ingreso_actividad_is_periodo == 2 ~ encft$ingreso_actividad_is_monto * encft$ingreso_actividad_is_tasa * 4.3,
    encft$ingreso_actividad_is_periodo == 1 ~ encft$ingreso_actividad_is_monto * encft$ingreso_actividad_is_tasa * encft$ingreso_actividad_is_dias * 4.3,
    TRUE ~ NA_real_
  )
  encft$ganancia_secun_imp_monto1 <- ifelse(encft$ingreso_actividad_is == 2, encft$ganancia_secun_imp_monto, 0)
  encft$p_s4b82 <- ifelse(encft$ganancia_is_productor == 1, encft$ganancia_is_productor_monto / 6, NA_real_)
  encft$ganancia_secun_imp_monto2 <- ifelse(encft$ganancia_is_productor == 2, encft$ganancia_secun_imp_monto, NA_real_)
  encft$p_4b8_2a3 <- ftc_pm22_row_sums(encft, c("p_s4b83", "p_s4b82", "ganancia_secun_imp_monto1", "ganancia_secun_imp_monto2"))
  encft$p_s4b84 <- ifelse(encft$consumio_bienes_is == 1, encft$consumio_bienes_is_monto, NA_real_)
  encft$p_s4b85 <- ifelse(encft$pago_especies_is == 1, encft$pago_especies_is_monto, NA_real_)
  encft$p_s4b91 <- ifelse(encft$ingresos_otros_trabajos == 1, encft$ingresos_otros_trabajos_monto, NA_real_)

  encft$p_s4d11_2 <- ifelse(encft$intereses_nac == 1, encft$intereses_nac_monto, NA_real_)
  encft$p_s4d12_1 <- encft$intereses_nac_ano_monto / 12
  encft$p_s4d11_1 <- ifelse(encft$pension_nac == 1, encft$pension_nac_monto, NA_real_)
  encft$p_s4d11_1 <- ifelse(encft$pension_nac == 3, encft$pension_imp_monto, encft$p_s4d11_1)
  encft$p_s4d12_6 <- ifelse(encft$regalia_pension_nac_ano == 1, encft$regalia_pension_nac_ano_monto / 12, NA_real_)
  encft$p_s4d12_6 <- ifelse(encft$pension_nac == 3, encft$regalia_pension_imp_monto / 12, encft$p_s4d12_6)
  encft$p_s4d11_3 <- ifelse(encft$alquiler_nac == 1, encft$alquiler_nac_monto, NA_real_)
  encft$p_s4d12_2 <- encft$alquiler_nac_ano_monto / 12
  encft$p_s4d11_4 <- ifelse(encft$remesas_nac == 1, encft$remesas_nac_monto, NA_real_)
  encft$p_s4d12_3 <- encft$remesas_nac_ano_monto / 12
  encft$p_s4d11_6 <- encft$alimentos_escuela_nac_monto
  encft$p_s4d11_8 <- ifelse(encft$ayuda_empresas_nac == 1, encft$ayuda_empresas_nac_monto, NA_real_)
  encft$p_s4d12_4 <- encft$ayuda_empresas_nac_ano_monto / 12
  encft$p_s4d11_99 <- encft$otros_ingresos_nac_monto
  encft$p_s4d12_99 <- encft$otros_ingresos_nac_ano_monto / 12
  encft$p_s2_6 <- encft$monto_alquilaria_vivienda_mes
  encft$p_s4d11_5 <- ifelse(encft$ayuda_especie_nac == 1, encft$ayuda_especie_nac_monto, 0)
  encft$p_s4d12_5 <- encft$ayuda_especie_nac_ano_monto / 12

  encft$p_s4d11_71 <- encft$gob_comer_primero_monto
  encft$p_s4d11_72 <- encft$gob_inc_asis_escolar_monto
  encft$p_s4d11_73 <- encft$gob_bono_luz_monto
  encft$p_s4d11_74 <- encft$gob_bonogas_choferes_monto
  encft$p_s4d11_75 <- encft$gob_bonogas_hogares_monto
  encft$p_s4d11_76 <- encft$gob_proteccion_vejez_monto
  encft$p_s4d11_77 <- encft$gob_bono_estudiante_prog_monto
  encft$p_s4d11_78 <- encft$gob_inc_educacion_sup_monto
  encft$p_s4d11_79 <- encft$gob_inc_policia_prev_monto
  encft$p_s4d11_710 <- encft$gob_inc_marina_guerra_monto
  encft$p_s4d11_711 <- encft$superate
  encft$p_s4d11_712 <- encft$quedate_en_casa
  encft$p_s4d11_713 <- encft$fase
  encft$p_s4d11_714 <- encft$gob_programa_pati
  encft$p_s4d11_7 <- ftc_pm22_row_sums(
    encft,
    c(
      "p_s4d11_71", "p_s4d11_72", "p_s4d11_73", "p_s4d11_74", "p_s4d11_75",
      "p_s4d11_76", "p_s4d11_77", "p_s4d11_78", "p_s4d11_79", "p_s4d11_710",
      "p_s4d11_711", "p_s4d11_712", "p_s4d11_713", "p_s4d11_714"
    )
  )

  encft$p_s4d13_1 <- encft$consumo_cultivo_monto * (encft$cultivo_no_remun_porc / 100)
  encft$p_s4d13_2 <- encft$crianza_no_remun_monto * (encft$crianza_no_remun_porc / 100)
  encft$p_s4d13_3 <- encft$pesca_no_remun_monto * (encft$pesca_no_remun_porc / 100)
  encft$p_s4d13_4 <- encft$alimentos_no_remun_monto * (encft$alimentos_no_remun_porc / 100)

  encft$pension_ext_tasa <- ifelse(encft$pension_ext == 1 & is.na(encft$pension_ext_tasa), 1, encft$pension_ext_tasa)
  encft$interes_ext_tasa <- ifelse(encft$interes_ext == 1 & is.na(encft$interes_ext_tasa), 1, encft$interes_ext_tasa)
  encft$alquiler_ext_tasa <- ifelse(encft$alquiler_ext == 1 & is.na(encft$alquiler_ext_tasa), 1, encft$alquiler_ext_tasa)
  encft$regalos_ext_tasa <- ifelse(encft$regalos_ext == 1 & is.na(encft$regalos_ext_tasa), 1, encft$regalos_ext_tasa)
  encft$otros_ingresos_ext_tasa <- ifelse(encft$otros_ingresos_ext == 1 & is.na(encft$otros_ingresos_ext_tasa), 1, encft$otros_ingresos_ext_tasa)

  encft$p_s4d21_1 <- ifelse(encft$pension_ext == 1, encft$pension_ext_monto * encft$pension_ext_tasa, NA_real_)
  encft$p_s4d21_2 <- ifelse(encft$interes_ext == 1, encft$interes_ext_monto * encft$interes_ext_tasa, NA_real_)
  encft$p_s4d21_3 <- ifelse(encft$alquiler_ext == 1, encft$alquiler_ext_monto * encft$alquiler_ext_tasa, NA_real_)
  encft$p_s4d21_4 <- ifelse(encft$regalos_ext == 1, encft$regalos_ext_monto * encft$regalos_ext_tasa, NA_real_)

  for (bloque in 1:3) {
    for (mes_idx in 1:6) {
      monto_col <- paste0("mes", mes_idx, "_", bloque, "_ext_monto")
      moneda_col <- paste0("mes", mes_idx, "_", bloque, "_ext_moneda")
      tasa_col <- paste0("mes", mes_idx, "_", bloque, "_ext_tasa")
      rem_col <- paste0("remp", mes_idx, "_", bloque)

      encft[[rem_col]] <- ifelse(
        !is.na(encft[[monto_col]]) & encft[[monto_col]] != 0,
        encft[[monto_col]] * encft[[tasa_col]],
        NA_real_
      )
    }
  }

  encft$p06_rem1 <- ftc_pm22_row_sums(encft, paste0("remp", 1:6, "_1"))
  encft$p06_rem2 <- ftc_pm22_row_sums(encft, paste0("remp", 1:6, "_2"))
  encft$p06_rem3 <- ftc_pm22_row_sums(encft, paste0("remp", 1:6, "_3"))
  encft$p06_rem <- ftc_pm22_row_sums(encft, c("p06_rem1", "p06_rem2", "p06_rem3"))
  encft$p06_rem[is.na(encft$p06_rem)] <- 0
  encft$p_s4d22 <- encft$p06_rem / 6

  encft$In_lab_mon_op <- ftc_pm22_row_sums(encft, c("p_s4b42_1", "p_s4b43_1", "p_s4b43_2", "p_s4b43_3", "p_s4b44_1", "p_s4b44_2", "p_s4b44_3", "p_s4b44_5", "p_s4b44_6", "p_s4b44_7"))
  encft$In_lab_mon_os <- ftc_pm22_row_sums(encft, c("p_s4b71", "p_s4b72", "p_s4b73"))
  encft$In_lab_mon_in_op <- ftc_pm22_row_sums(encft, c("p_s4b5_2a3"))
  encft$In_lab_mon_in_os <- ftc_pm22_row_sums(encft, c("p_4b8_2a3"))
  encft$In_lab_esp_op <- ftc_pm22_row_sums(encft, c("p_s4b45_1", "p_s4b45_2", "p_s4b45_3", "p_s4b45_4", "p_s4b45_5", "p_s4b45_99"))
  encft$In_lab_esp_os <- ftc_pm22_row_sums(encft, c("p_s4b74"))
  encft$In_lab_esp_in_op <- ftc_pm22_row_sums(encft, c("p_s4b55", "p_s4b54"))
  encft$In_lab_esp_in_os <- ftc_pm22_row_sums(encft, c("p_s4b84", "p_s4b85"))
  encft$In_lab_otros_trab <- ftc_pm22_row_sums(encft, c("p_s4b91"))
  encft$Ilab_mon <- ftc_pm22_row_sums(encft, c("In_lab_mon_op", "In_lab_mon_os", "In_lab_mon_in_op", "In_lab_mon_in_os"))
  encft$Ilab_esp <- ftc_pm22_row_sums(encft, c("In_lab_esp_op", "In_lab_esp_os", "In_lab_esp_in_op", "In_lab_esp_in_os"))
  encft$Ilab_tot <- ftc_pm22_row_sums(encft, c("Ilab_mon", "Ilab_esp", "In_lab_otros_trab"))
  encft$Inlab_nac_TrN_mon <- ftc_pm22_row_sums(encft, c("p_s4d11_1", "p_s4d11_2", "p_s4d11_3", "p_s4d11_4", "p_s4d11_6", "p_s4d11_7", "p_s4d12_1", "p_s4d12_2", "p_s4d12_3", "p_s4d12_6"))
  encft$Inlab_nac_esp_a <- ftc_pm22_row_sums(encft, c("p_s4d11_5", "p_s4d12_5"))
  encft$Inlab_nac_esp <- ftc_pm22_row_sums(encft, c("Inlab_nac_esp_a"))
  encft$Inla_trE <- ftc_pm22_row_sums(encft, c("p_s4d21_1", "p_s4d21_2", "p_s4d21_3", "p_s4d22", "p_s4d21_4"))
  encft$Itotal <- ftc_pm22_row_sums(encft, c("Ilab_tot", "Inlab_nac_esp", "Inlab_nac_TrN_mon", "Inla_trE", "p_s2_6"))

  encft <- dplyr::left_join(encft, ipc_pobreza_monetaria_2022, by = "periodo")
  encft$macro_region <- ftc_pm22_macro_region(encft$grupo_region, encft$id_provincia)

  region_vars <- c(
    "p_s2_6", "p_s4b42", "p_s4b42_1", "p_s4b43_1", "p_s4b43_2", "p_s4b43_3", "p_s4b43_99",
    "p_s4b45_1", "p_s4b45_2", "p_s4b45_3", "p_s4b45_4", "p_s4b45_5", "p_s4b45_99",
    "p_s4b71", "p_s4b72", "p_s4b74", "p_s4b53_1", "p_s4b5_2a3", "p_s4b55", "p_s4b54",
    "p_s4b83", "p_4b8_2a3", "p_s4b84", "p_s4b85", "p_s4b91", "p_s4d11_2", "p_s4d11_1",
    "p_s4d11_3", "p_s4d11_4", "p_s4d11_6", "p_s4d11_8", "p_s4d11_99", "p_s4d11_5",
    "p_s4d11_71", "p_s4d11_72", "p_s4d11_73", "p_s4d11_74", "p_s4d11_75", "p_s4d11_76",
    "p_s4d11_77", "p_s4d11_78", "p_s4d11_79", "p_s4d11_710", "p_s4d11_711", "p_s4d11_7",
    "p_s4d21_1", "p_s4d21_2", "p_s4d21_3", "p_s4d21_4", "salario_princ_imp_monto",
    "ganancia_princ_imp_monto", "ganancia_princ_imp_monto1", "ganancia_secun_imp_monto",
    "ganancia_secun_imp_monto1", "p_s4d13_1", "p_s4d13_2", "p_s4d13_3", "p_s4d13_4", "p_s4b73"
  )
  moving_vars <- c("p_s4b52", "ganancia_princ_imp_monto2", "p_s4b82", "ganancia_secun_imp_monto2", "p_s4d22")
  no_deflate_vars <- c(paste0("p_s4b44_", 1:7), "p_s4b44_99", "p_s4d12_1", "p_s4d12_6", "p_s4d12_2", "p_s4d12_3", "p_s4d12_99", "p_s4d12_4", "p_s4d12_5")

  for (var in intersect(region_vars, names(encft))) {
    out_col <- paste0("d_", var)
    encft[[out_col]] <- dplyr::case_when(
      encft$macro_region == "Ozama" ~ encft[[var]] * encft$deflactor_regionozama,
      encft$macro_region == "Este" ~ encft[[var]] * encft$deflactor_regioneste,
      encft$macro_region == "Norte" ~ encft[[var]] * encft$deflactor_regionnorte,
      encft$macro_region == "Sur" ~ encft[[var]] * encft$deflactor_regionsur,
      TRUE ~ encft[[var]]
    )
  }

  for (var in intersect(moving_vars, names(encft))) {
    out_col <- paste0("d_", var)
    encft[[out_col]] <- dplyr::case_when(
      encft$macro_region == "Ozama" ~ encft[[var]] * encft$deflactor_movil_regionozama,
      encft$macro_region == "Este" ~ encft[[var]] * encft$deflactor_movil_regioneste,
      encft$macro_region == "Norte" ~ encft[[var]] * encft$deflactor_movil_regionnorte,
      encft$macro_region == "Sur" ~ encft[[var]] * encft$deflactor_movil_regionsur,
      TRUE ~ encft[[var]]
    )
  }

  for (var in intersect(no_deflate_vars, names(encft))) {
    encft[[paste0("d_", var)]] <- encft[[var]]
  }

  encft$ID_lab_mon_op <- ftc_pm22_row_sums(encft, c("d_p_s4b42_1", "d_p_s4b43_1", "d_p_s4b43_2", "d_p_s4b43_3", "d_p_s4b44_1", "d_p_s4b44_2", "d_p_s4b44_3", "d_p_s4b44_5", "d_p_s4b44_6", "d_p_s4b44_7"))
  encft$ID_lab_mon_os <- ftc_pm22_row_sums(encft, c("d_p_s4b71", "d_p_s4b72", "d_p_s4b73"))
  encft$ID_lab_mon_in_op <- ftc_pm22_row_sums(encft, c("d_p_s4b52", "d_p_s4b53_1", "d_ganancia_princ_imp_monto1", "d_ganancia_princ_imp_monto2"))
  encft$ID_lab_mon_in_os <- ftc_pm22_row_sums(encft, c("d_p_s4b83", "d_p_s4b82", "d_ganancia_secun_imp_monto1", "d_ganancia_secun_imp_monto2"))
  encft$ID_lab_esp_op <- ftc_pm22_row_sums(encft, c("d_p_s4b45_1", "d_p_s4b45_2", "d_p_s4b45_3", "d_p_s4b45_4", "d_p_s4b45_5", "d_p_s4b45_99"))
  encft$ID_lab_esp_os <- ftc_pm22_row_sums(encft, c("d_p_s4b74"))
  encft$ID_lab_esp_in_op <- ftc_pm22_row_sums(encft, c("d_p_s4b55", "d_p_s4b54"))
  encft$ID_lab_esp_in_os <- ftc_pm22_row_sums(encft, c("d_p_s4b84", "d_p_s4b85"))
  encft$IDlab_mon <- ftc_pm22_row_sums(encft, c("ID_lab_mon_op", "ID_lab_mon_os", "ID_lab_mon_in_op", "ID_lab_mon_in_os"))
  encft$IDlab_esp <- ftc_pm22_row_sums(encft, c("ID_lab_esp_op", "ID_lab_esp_os", "ID_lab_esp_in_op", "ID_lab_esp_in_os"))
  encft$IDlab_tot <- ftc_pm22_row_sums(encft, c("IDlab_mon", "IDlab_esp"))
  encft$IDnlab_nac_TrN_mon <- ftc_pm22_row_sums(encft, c("d_p_s4d11_1", "d_p_s4d11_2", "d_p_s4d11_3", "d_p_s4d11_4", "d_p_s4d11_6", "d_p_s4d11_7", "d_p_s4d12_1", "d_p_s4d12_2", "d_p_s4d12_3", "d_p_s4d12_6"))
  encft$IDnlab_nac_esp_a <- ftc_pm22_row_sums(encft, c("d_p_s4d11_5", "d_p_s4d12_5"))
  encft$IDnlab_nac_esp <- ftc_pm22_row_sums(encft, c("IDnlab_nac_esp_a"))
  encft$IDnla_trE <- ftc_pm22_row_sums(encft, c("d_p_s4d21_1", "d_p_s4d21_2", "d_p_s4d21_3", "d_p_s4d22"))

  encft <- encft %>%
    dplyr::group_by(trimestre, periodo, vivienda, hogar) %>%
    dplyr::mutate(miembros = sum(personas)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(trimestre, vivienda, hogar) %>%
    dplyr::mutate(d_p_s2_6h = sum(d_p_s2_6, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      d_p_s2_6m = d_p_s2_6h / miembros,
      ID_total = ftc_pm22_row_sums(., c("IDlab_tot", "IDnlab_nac_esp", "IDnlab_nac_TrN_mon", "IDnla_trE")),
      ID_total_a = ftc_pm22_row_sums(., c("IDlab_tot", "IDnlab_nac_esp", "IDnlab_nac_TrN_mon", "IDnla_trE", "d_p_s2_6m"))
    ) %>%
    dplyr::group_by(trimestre, vivienda, hogar) %>%
    dplyr::mutate(Ihog_ENCFT = sum(ID_total, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Ihog_ENCFT_a = ftc_pm22_row_sums(., c("Ihog_ENCFT", "d_p_s2_6")),
      IPCm_ENCFT = Ihog_ENCFT_a / miembros
    )

  encft <- dplyr::left_join(encft, lineas_pobreza_monetaria_2022, by = "periodo") %>%
    dplyr::mutate(
      linea_pob = dplyr::case_when(
        macro_region == "Ozama" ~ linea_general_regionozama,
        macro_region == "Este" ~ linea_general_regioneste,
        macro_region == "Sur" ~ linea_general_regionsur,
        macro_region == "Norte" ~ linea_general_regionnorte,
        TRUE ~ NA_real_
      ),
      linea_ind = dplyr::case_when(
        macro_region == "Ozama" ~ linea_extrema_regionozama,
        macro_region == "Este" ~ linea_extrema_regioneste,
        macro_region == "Sur" ~ linea_extrema_regionsur,
        macro_region == "Norte" ~ linea_extrema_regionnorte,
        TRUE ~ NA_real_
      )
    )

  outputs <- ftc_pm22_build_outputs(encft, original_names = original_names, .keep = .keep)
  dplyr::left_join(tbl_out, outputs, by = ".encftr_rowid") %>%
    dplyr::select(-.encftr_rowid)
}

#' Pobreza monetaria con metodologia 2012
#' `r lifecycle::badge("stable")`
#'
#' @param tbl Un data.frame con los datos de la encuesta.
#' @param .keep Lista de variables intermedias a mantener en el output.
#' @param .reuse Lista de variables intermedias a reutilizar del input.
#'
#' @return Los datos del input mas las variables:
#' \itemize{
#'   \item `ing_pc_pobreza_def`
#'   \item `ing_total_pobreza_def`
#'   \item `ing_total_pobreza`
#'   \item `pobreza_monetaria`
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' encft <- ftc_pobreza_monetaria_2012(encft)
#' }
ftc_pobreza_monetaria_2012 <- function(tbl, .keep = FALSE, .reuse = FALSE) {
  ftc_pobreza_monetaria_2012_impl(tbl, .keep = .keep, .reuse = .reuse)
}

#' Pobreza monetaria con metodologia 2022
#' `r lifecycle::badge("stable")`
#'
#' @param tbl Un data.frame con los datos de la encuesta. Debe incluir las
#'   variables necesarias para la construccion de ingresos y la macroregion.
#' @param .keep Variables intermedias a mantener en el output. Puede ser
#'   `FALSE`, `TRUE` o un vector de nombres de variables internas.
#' @param .reuse Se acepta por compatibilidad de API. La implementacion 2022
#'   recalcula internamente sus agregados.
#'
#' @return Los datos del input mas las variables:
#' \itemize{
#'   \item `macro_region_pobreza`
#'   \item `ing_total_pobreza_def`
#'   \item `ing_pc_pobreza_def`
#'   \item `linea_pobreza`
#'   \item `linea_pobreza_extrema`
#'   \item `pobreza_monetaria`
#'   \item `pobre`
#'   \item `indigente`
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' encft <- ftc_pobreza_monetaria_2022(encft)
#' }
ftc_pobreza_monetaria_2022 <- function(tbl, .keep = FALSE, .reuse = FALSE) {
  ftc_pobreza_monetaria_2022_impl(tbl, .keep = .keep, .reuse = .reuse)
}

#' Wrapper deprecated para pobreza monetaria
#' `r lifecycle::badge("deprecated")`
#'
#' Conserva la metodologia 2012 para no romper codigo existente. El uso
#' recomendado es llamar explicitamente a `ftc_pobreza_monetaria_2012()` o
#' `ftc_pobreza_monetaria_2022()`.
#'
#' @param tbl Un data.frame con los datos de la encuesta.
#' @param .keep Lista de variables intermedias a mantener en el output.
#' @param .reuse Lista de variables intermedias a reutilizar del input.
#'
#' @return Los datos del input mas las variables:
#' \itemize{
#'   \item `ing_pc_pobreza_def`
#'   \item `ing_total_pobreza_def`
#'   \item `ing_total_pobreza`
#'   \item `pobreza_monetaria`
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' encft <- ftc_pobreza_monetaria(encft)
#' }
ftc_pobreza_monetaria <- function(tbl, .keep = FALSE, .reuse = FALSE) {
  lifecycle::deprecate_warn(
    "0.9.0",
    "ftc_pobreza_monetaria()",
    details = paste(
      "Use `ftc_pobreza_monetaria_2012()`, `ftc_pobreza_monetaria_2022()`",
      "directamente.",
      "Este wrapper se mantiene solo por compatibilidad hacia atrás."
    )
  )

  ftc_pobreza_monetaria_2012(tbl, .keep = .keep, .reuse = .reuse)
}
