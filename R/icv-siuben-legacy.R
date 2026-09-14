# Historical ICV engine migrated from encftr0 0.0.2.9002.
# Numeric coefficients, recode precedence and missing-score rules are unchanged.
# Group keys include the supplied year; the public wrapper validates input.
# Source SHA256: 7debf61a4fafaa7468826582d01a93d849d568bd05d45095030ea616f149d0a2

encft_icv_legacy_compute_icv_siuben <- function(tbl) {
    tbl <- encft_icv_legacy_compute_zona_geografica(tbl)
    tbl <- encft_icv_legacy_compute_equiv1(tbl)
    tbl <- encft_icv_legacy_compute_pisov1(tbl)
    tbl <- encft_icv_legacy_compute_paredv1(tbl)
    tbl <- encft_icv_legacy_compute_techv1(tbl)
    tbl <- encft_icv_legacy_compute_aguav1(tbl)
    tbl <- encft_icv_legacy_compute_escoj1(tbl)
    tbl <- encft_icv_legacy_compute_escov1(tbl)
    tbl <- encft_icv_legacy_compute_alfav1(tbl)
    tbl <- encft_icv_legacy_compute_luzv1(tbl)
    tbl <- encft_icv_legacy_compute_saniv1(tbl)
    tbl <- encft_icv_legacy_compute_cociv1(tbl)
    tbl <- encft_icv_legacy_compute_hacina1(tbl)
    tbl <- encft_icv_legacy_compute_pninme1(tbl)
    tbl <- encft_icv_legacy_compute_asiscv1(tbl)
    tbl <- encft_icv_legacy_compute_basuv1(tbl)
    tbl <- encft_icv_legacy_compute_tipo_vivienda(tbl)
    tbl <- encft_icv_legacy_compute_provin1(tbl)
    tbl %>%
      dplyr::mutate(
        equiv1  = ifelse(is.na(equiv1 ), 0, equiv1 ),
        pisov1  = ifelse(is.na(pisov1 ), 0, pisov1 ),
        paredv1 = ifelse(is.na(paredv1), 0, paredv1),
        techv1  = ifelse(is.na(techv1 ), 0, techv1 ),
        aguav1  = ifelse(is.na(aguav1 ), 0, aguav1 ),
        escoj1  = ifelse(is.na(escoj1 ), 0, escoj1 ),
        escov1  = ifelse(is.na(escov1 ), 0, escov1 ),
        alfav1  = ifelse(is.na(alfav1 ), 0, alfav1 ),
        luzv1   = ifelse(is.na(luzv1  ), 0, luzv1  ),
        saniv1  = ifelse(is.na(saniv1 ), 0, saniv1 ),
        cociv1  = ifelse(is.na(cociv1 ), 0, cociv1 ),
        hacina1 = ifelse(is.na(hacina1), 0, hacina1),
        pninme1 = ifelse(is.na(pninme1), 0, pninme1),
        asiscv1 = ifelse(is.na(asiscv1), 0, asiscv1),
        icv_nuevo = dplyr::case_when(
          zona_geografica == 3 ~
            equiv1 +
            pisov1 +
            paredv1 +
            techv1 +
            aguav1 +
            escoj1 +
            escov1 +
            alfav1 +
            luzv1 +
            saniv1 +
            cociv1 +
            hacina1 +
            pninme1 +
            asiscv1,
          zona_geografica == 1 ~
            saniv1+
            techv1+
            pisov1+
            paredv1+
            equiv1+
            aguav1+
            provin1+
            escoj1+
            escov1+
            alfav1+
            cociv1+
            luzv1+
            basuv1+
            tipov1+
            hacina1,
          zona_geografica == 2 ~
            equiv1+
            cociv1+
            luzv1+
            provin1+
            aguav1+
            basuv1+
            escoj1+
            escov1+
            alfav1+
            paredv1+
            saniv1+
            pisov1+
            techv1+
            tipov1+
            hacina1
        ),
        icv_met = dplyr::case_when(
          zona_geografica == 3 & icv_nuevo <= 73.8018 ~ 1,
          zona_geografica == 3 & icv_nuevo > 73.8018 & icv_nuevo <= 84.9197 ~ 2,
          zona_geografica == 3 & icv_nuevo > 84.9197 & icv_nuevo <= 91.7283 ~ 3,
          zona_geografica == 3 & icv_nuevo > 91.7283 ~ 4
        ),
        icv_urbana = dplyr::case_when(
          zona_geografica == 1 & icv_nuevo <= 55.7844 ~ 1,
          zona_geografica == 1 & icv_nuevo > 55.7844 & icv_nuevo <= 69.3947 ~ 2,
          zona_geografica == 1 & icv_nuevo > 69.3947 & icv_nuevo <= 79.1885 ~ 3,
          zona_geografica == 1 & icv_nuevo > 79.1885 ~ 4
        ),
        icv_rural = dplyr::case_when(
          zona_geografica == 2 & icv_nuevo <= 49.0373 ~ 1,
          zona_geografica == 2 & icv_nuevo > 49.0373 & icv_nuevo <= 63.9496 ~ 2,
          zona_geografica == 2 & icv_nuevo > 63.9496 & icv_nuevo <= 76.3166 ~ 3,
          zona_geografica == 2 & icv_nuevo > 76.3166 ~ 4
        ),
        icv_global = dplyr::case_when(
          zona_geografica == 3 ~ icv_met,
          zona_geografica == 1 ~ icv_urbana,
          zona_geografica == 2 ~ icv_rural
        )
      ) %>%
      dplyr::select(
        -c(
           "icv_met",
           "icv_urbana",
           "icv_rural")
      )
}

################# ZONA GEOGRAFICA#######################
encft_icv_legacy_compute_zona_geografica <- function(tbl) {
  tbl %>%  dplyr::mutate(
    zona_geografica = dplyr::case_when(
      ZONA > 1 ~ 2,
      ID_PROVINCIA == 1 ~ 3,
      ID_PROVINCIA == 32 & ZONA == 1 ~ 3,
      ID_MUNICIPIO == 2501 & ZONA == 1 ~ 3,
      TRUE ~ 1
    )
  )
}

################# ANOS ESTU##############################
encft_icv_legacy_compute_anos_estu <- function(tbl) {
  tbl %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(ftc_household_keys(tbl, dwelling = TRUE)))) %>%
    dplyr::mutate(
      anos_estu = dplyr::case_when(
        EDAD >= 5 & NIVEL_ULTIMO_ANO_APROBADO == 2 & ULTIMO_ANO_APROBADO <= 8 ~ ULTIMO_ANO_APROBADO,
        EDAD >= 5 & NIVEL_ULTIMO_ANO_APROBADO %in% c(3, 4) & ULTIMO_ANO_APROBADO <= 4 ~ ULTIMO_ANO_APROBADO + 8,
        EDAD >= 5 & NIVEL_ULTIMO_ANO_APROBADO == 5 & ULTIMO_ANO_APROBADO <= 6 ~ ULTIMO_ANO_APROBADO + 12,
        EDAD >= 5 & NIVEL_ULTIMO_ANO_APROBADO %in% c(6, 7) & ULTIMO_ANO_APROBADO <= 4 ~ ULTIMO_ANO_APROBADO + 16,
      )
    )
}


################################ GRADO EDU###########################
encft_icv_legacy_compute_grado_edu <- function(tbl) {
  tbl %>%
    dplyr::mutate(
      grado_edu = ULTIMO_ANO_APROBADO
    )
}

############################ NIVEL EDU###############################
encft_icv_legacy_compute_nivel_edu <- function(tbl) {
  tbl %>%
    dplyr::mutate(
      nivel_edu = dplyr::case_when(
        EDAD <= 2 ~ 8,
        dplyr::between(NIVEL_ULTIMO_ANO_APROBADO, 6, 8) ~ 6,
        NIVEL_ULTIMO_ANO_APROBADO %in% c(9, 10) ~ 0,
        NIVEL_ULTIMO_ANO_APROBADO >= 1 ~ NIVEL_ULTIMO_ANO_APROBADO
      )
    )
}

# Puedo hacer que esta sea solo un parametro de edad de la principal
encft_icv_legacy_compute_nivel_edu15 <- function(tbl) {
  tbl %>% encft_icv_legacy_compute_nivel_edu() %>%
    dplyr::mutate(
      nivel_edu15 = dplyr::case_when(
        EDAD >= 15 ~ nivel_edu
      )
    )
}


################################## ANALFABETISMO###################3
encft_icv_legacy_compute_analfabeta <- function(tbl) {
  tbl %>%
    dplyr::mutate(
      analfabeta = dplyr::case_when(
        EDAD > 5 & SABE_LEER_ESCRIBIR == 1 ~ 0,
        EDAD > 5 & SABE_LEER_ESCRIBIR == 2 ~ 1
      )
    )
}

# Puedo hacer que esta funcion sea un argumento de edad para la anterior y así
# calculo ambas con una sola funcion
encft_icv_legacy_compute_analfabeta15 <- function(tbl) {
  tbl %>%  encft_icv_legacy_compute_analfabeta() %>%
    dplyr::mutate(
      analfabeta15 = dplyr::case_when(
        EDAD >= 15 ~ analfabeta
      )
    )
}


########################## AÑOS EDUCACION#######################
encft_icv_legacy_compute_anos_edu <- function(tbl) {
  tbl %>%
    encft_icv_legacy_compute_nivel_edu() %>%
    encft_icv_legacy_compute_grado_edu() %>%
    dplyr::mutate(
      anos_edu = dplyr::case_when(
        EDAD <= 2 ~ 0,
        nivel_edu <= 1 ~ 0,
        nivel_edu == 2 ~ grado_edu,
        nivel_edu %in% c(3, 4) ~ 8 + grado_edu,
        nivel_edu == 5 ~ 12 + grado_edu,
        nivel_edu == 6 ~ 16 + grado_edu
      )
    )
}

encft_icv_legacy_compute_anos_edu15 <- function(tbl) {
  tbl %>%
    encft_icv_legacy_compute_anos_edu() %>%
    dplyr::mutate(
      anos_edu15 = dplyr::case_when(
        EDAD >= 15 ~ anos_edu
      )
    )
}


######################## EQUIPAMIENTO DEL HOGAR###################
encft_icv_legacy_compute_equiv1 <- function(tbl) {
  tbl %>%
    encft_icv_legacy_compute_equivivi() %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(ftc_household_keys(tbl, dwelling = FALSE)))) %>%
    dplyr::mutate(
      equiv1 = dplyr::first(equivivi)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -c(
        "estufa",
        "lavadora",
        "televisor",
        "nevera",
        "basicos",
        "aire_acondicionado",
        "vehiculo_privado",
        "computador",
        "modernos",
        "equipo",
        "equivivi"
      )
    )
}

encft_icv_legacy_compute_equivivi <- function(tbl) {
  tbl <- encft_icv_legacy_compute_zona_geografica(tbl)
  tbl <- encft_icv_legacy_compute_equipo(tbl)
  tbl %>%
    dplyr::mutate(
      equivivi = dplyr::case_when(
        zona_geografica %in% c(1, 3) & equipo %in% c(1, 2) ~ 0,
        zona_geografica == 3 & equipo == 3 ~ 0.6893,
        zona_geografica == 3 & equipo == 4 ~ 2.2423,
        zona_geografica == 3 & equipo == 5 ~ 3.1323,
        zona_geografica == 1 & equipo == 3 ~ 1.0607,
        zona_geografica == 1 & equipo == 4 ~ 2.1915,
        zona_geografica == 1 & equipo == 5 ~ 3.0301,
        zona_geografica == 2 & equipo == 1 ~ 0,
        zona_geografica == 2 & equipo == 2 ~ 2.0455,
        zona_geografica == 2 & equipo == 3 ~ 3.6033,
        zona_geografica == 2 & equipo == 4 ~ 5.0378,
        zona_geografica == 2 & equipo == 5 ~ 6.5224
      )
    )
}

encft_icv_legacy_compute_equipo <- function(tbl) {
  tbl <- encft_icv_legacy_compute_basicos(tbl)
  tbl <- encft_icv_legacy_compute_modernos(tbl)
  tbl %>%
    dplyr::mutate(
      equipo = dplyr::case_when(
        basicos == 0 ~ 1,
        basicos >= 1 & basicos < 4 ~ 2,
        basicos == 4 & modernos == 0 ~ 3,
        basicos == 4 & modernos >= 1 & modernos < 3 ~ 4,
        modernos == 3 ~ 5,
        TRUE ~ 0
      )
    )
}

encft_icv_legacy_compute_basicos <- function(tbl) {
  tbl <- encft_icv_legacy_compute_estufa(tbl)
  tbl <- encft_icv_legacy_compute_lavadora(tbl)
  tbl <- encft_icv_legacy_compute_televisor(tbl)
  tbl <- encft_icv_legacy_compute_nevera(tbl)
  tbl %>%
    dplyr::mutate(
      basicos = estufa + lavadora + televisor + nevera,
      basicos = ifelse(is.na(basicos), 0, basicos)
    )
}

encft_icv_legacy_compute_modernos <- function(tbl) {
  tbl <- encft_icv_legacy_compute_aire_acondicionado(tbl)
  tbl <- encft_icv_legacy_compute_vehiculo_privado(tbl)
  tbl <- encft_icv_legacy_compute_computador(tbl)
  tbl %>%
    dplyr::mutate(
      modernos = aire_acondicionado + vehiculo_privado + computador,
      modernos = ifelse(is.na(modernos), 0, modernos)
    )
}

encft_icv_legacy_compute_estufa <- function(tbl) {
  tbl %>%
    dplyr::mutate(
      estufa = dplyr::case_when(
        ESTUFA == 1 ~ 1,
        TRUE ~ 0
      )
    )
}

encft_icv_legacy_compute_nevera <- function(tbl) {
  tbl %>%
    dplyr::mutate(
      nevera = dplyr::case_when(
        REFRIGERADOR == 1 ~ 1,
        TRUE ~ 0
      )
    )
}

encft_icv_legacy_compute_televisor <- function(tbl) {
  tbl %>%
    dplyr::mutate(
      televisor = dplyr::case_when(
        TELEVISOR == 1 ~ 1,
        TRUE ~ 0
      )
    )
}

encft_icv_legacy_compute_lavadora <- function(tbl) {
  tbl %>%
    dplyr::mutate(
      lavadora = dplyr::case_when(
        LAVADORA == 1 ~ 1,
        TRUE ~ 0
      )
    )
}

encft_icv_legacy_compute_vehiculo_privado <- function(tbl) {
  tbl %>%
    dplyr::mutate(
      vehiculo_privado = dplyr::case_when(
        AUTOMOVIL == 1 ~ 1,
        TRUE ~ 0
      )
    )
}

encft_icv_legacy_compute_aire_acondicionado <- function(tbl) {
  tbl %>%
    dplyr::mutate(
      aire_acondicionado = dplyr::case_when(
        AIRE_ACONDICIONADO == 1 ~ 1,
        TRUE ~ 0
      )
    )
}

encft_icv_legacy_compute_computador <- function(tbl) {
  tbl %>%
    dplyr::mutate(
      computador = dplyr::case_when(
        COMPUTADOR == 1 ~ 1,
        TRUE ~ 0
      )
    )
}


############################# PISO DE LA VIVIENDA##################
encft_icv_legacy_compute_pisov1 <- function(tbl) {
  tbl <- encft_icv_legacy_compute_zona_geografica(tbl)
  tbl %>%
    dplyr::mutate(
      pisov1 = dplyr::case_when(
        zona_geografica == 3 & MATERIAL_PISO %in% c(1, 4, 5, 7, 99) ~ 2.2537,
        zona_geografica == 3 & MATERIAL_PISO %in% c(2, 3, 6, 8) ~ 4.7183,
        zona_geografica == 3 & MATERIAL_PISO == 9 ~ 0,
        zona_geografica == 1 & MATERIAL_PISO %in% c(1, 4, 5, 7, 99) ~ 3.2285,
        zona_geografica == 1 & MATERIAL_PISO %in% c(2, 3, 6, 8) ~ 5.3893,
        zona_geografica == 1 & MATERIAL_PISO == 9 ~ 0,
        zona_geografica == 2 & MATERIAL_PISO %in% c(1, 4, 5, 7, 99) ~ 4.5907,
        zona_geografica == 2 & MATERIAL_PISO %in% c(2, 3, 6, 8) ~ 8.4426,
        zona_geografica == 2 & MATERIAL_PISO == 9 ~ 0
      )
    )
}


################### PARED DE LA VIVIENDA##########################
encft_icv_legacy_compute_paredv1 <- function(tbl) {
  tbl <- encft_icv_legacy_compute_zona_geografica(tbl)
  tbl <- encft_icv_legacy_recode_material_pared_vivienda(tbl)
  tbl %>%
    dplyr::mutate(
      paredv1 = dplyr::case_when(
        zona_geografica == 3 & MATERIAL_PARED_EXTERIOR2 %in% c(2, 4, 5) ~ 3.6520,
        zona_geografica == 3 & MATERIAL_PARED_EXTERIOR2 %in% c(7, 6, 9) ~ 0.3728,
        zona_geografica == 1 & MATERIAL_PARED_EXTERIOR2 %in% c(2, 4, 5) ~ 3.0066,
        zona_geografica == 1 & MATERIAL_PARED_EXTERIOR2 %in% c(6, 7) ~ 1.0756,
        zona_geografica == 1 & MATERIAL_PARED_EXTERIOR2 == 9 ~ 0.5098,
        zona_geografica == 2 & MATERIAL_PARED_EXTERIOR2 %in% c(2, 4, 5) ~ 4.9485,
        zona_geografica == 2 & MATERIAL_PARED_EXTERIOR2 %in% c(6, 7) ~ 2.3834,
        zona_geografica == 2 & MATERIAL_PARED_EXTERIOR2 == 9 ~ 0.8558,
        TRUE ~ 0
      )
    )
}


###################### TECHO DE LA VIVIENDA#####################################
encft_icv_legacy_compute_techv1 <- function(tbl) {
  tbl <- encft_icv_legacy_compute_zona_geografica(tbl)
  tbl %>%
    dplyr::mutate(
      techv1 = dplyr::case_when(
        zona_geografica == 3 & MATERIAL_TECHO == 2 ~ 3.2812,
        zona_geografica == 3 & MATERIAL_TECHO %in% c(1, 4) ~ 0.6231,
        zona_geografica %in% c(1, 2, 3) & MATERIAL_TECHO %in% c(3, 5, 99) ~ 0,
        zona_geografica == 1 & MATERIAL_TECHO == 2 ~ 4.0718,
        zona_geografica == 1 & MATERIAL_TECHO %in% c(1, 4) ~ 1.8422,
        zona_geografica == 2 & MATERIAL_TECHO == 2 ~ 9.9672,
        zona_geografica == 2 & MATERIAL_TECHO %in% c(1, 4) ~ 5.8366
      )
    )
}


##################### PROVIENE AGUA#############################################
encft_icv_legacy_compute_aguav1 <- function(tbl) {
  tbl <- encft_icv_legacy_compute_zona_geografica(tbl)
  tbl %>%
    dplyr::mutate(
      aguav1 = dplyr::case_when(
        zona_geografica == 3 & DONDE_PROVIENE_AGUA <= 2 ~ 2.7706,
        zona_geografica %in% c(1, 2, 3) & DONDE_PROVIENE_AGUA > 2 ~ 0,
        zona_geografica == 1 & DONDE_PROVIENE_AGUA <= 2 ~ 1.9933,
        zona_geografica == 2 & DONDE_PROVIENE_AGUA <= 2 ~ 2.4485
      )
    )
}

########################### EDUCACION JEFE HOGAR################################
encft_icv_legacy_compute_escoj1 <- function(tbl) {
  tbl %>%
    encft_icv_legacy_compute_zona_geografica() %>%
    encft_icv_legacy_compute_anos_edu() %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(ftc_household_keys(tbl, dwelling = FALSE)))) %>%
    dplyr::mutate(
      edu_jefe = dplyr::case_when(
        zona_geografica == 3 & anos_edu[PARENTESCO == 1] == 0 ~ 0,
        zona_geografica == 3 & anos_edu[PARENTESCO == 1] > 0 & anos_edu[PARENTESCO == 1] < 4 ~ 1.7553,
        zona_geografica == 3 & anos_edu[PARENTESCO == 1] >= 4 & anos_edu[PARENTESCO == 1] < 8 ~ 2.4540,
        zona_geografica == 3 & anos_edu[PARENTESCO == 1] >= 8 & anos_edu[PARENTESCO == 1] < 12 ~ 2.7209,
        zona_geografica == 3 & anos_edu[PARENTESCO == 1] >= 12 & anos_edu[PARENTESCO == 1] < 16 ~ 4.1150,
        zona_geografica == 3 & anos_edu[PARENTESCO == 1] >= 16 ~ 5.2221,
        zona_geografica == 1 & anos_edu[PARENTESCO == 1] == 0 ~ 0,
        zona_geografica == 1 & anos_edu[PARENTESCO == 1] > 0 & anos_edu[PARENTESCO == 1] < 4 ~ 2.1945,
        zona_geografica == 1 & anos_edu[PARENTESCO == 1] >= 4 & anos_edu[PARENTESCO == 1] < 8 ~ 2.7096,
        zona_geografica == 1 & anos_edu[PARENTESCO == 1] >= 8 & anos_edu[PARENTESCO == 1] < 12 ~ 3.3955,
        zona_geografica == 1 & anos_edu[PARENTESCO == 1] >= 12 & anos_edu[PARENTESCO == 1] < 16 ~ 4.2606,
        zona_geografica == 1 & anos_edu[PARENTESCO == 1] >= 16 ~ 5.3853,
        zona_geografica == 2 & anos_edu[PARENTESCO == 1] == 0 ~ 0,
        zona_geografica == 2 & anos_edu[PARENTESCO == 1] > 0 & anos_edu[PARENTESCO == 1] < 4 ~ 2.0880,
        zona_geografica == 2 & anos_edu[PARENTESCO == 1] >= 4 & anos_edu[PARENTESCO == 1] < 8 ~ 2.6880,
        zona_geografica == 2 & anos_edu[PARENTESCO == 1] >= 8 & anos_edu[PARENTESCO == 1] < 12 ~ 3.5683,
        zona_geografica == 2 & anos_edu[PARENTESCO == 1] >= 12 & anos_edu[PARENTESCO == 1] < 16 ~ 4.8542,
        zona_geografica == 2 & anos_edu[PARENTESCO == 1] >= 16 ~ 5.8025
      ),
      escoj1 = dplyr::first(edu_jefe)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -c("nivel_edu", "grado_edu", "anos_edu", "edu_jefe")
    )
}

######################### ESCOLARIDAD PROMEDIO VIVIENDA#########################
encft_icv_legacy_compute_escov1 <- function(tbl) {
  encft_icv_legacy_compute_zona_geografica(tbl) %>%
  encft_icv_legacy_compute_anos_edu() %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(ftc_household_keys(tbl, dwelling = FALSE)))) %>%
    dplyr::mutate(
      escov1 = dplyr::case_when(
        zona_geografica == 3 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) == 0 ~ 0,
        zona_geografica == 3 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) > 0 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) < 4 ~ 1.7781,
        zona_geografica == 3 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) >= 4 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) < 8 ~ 2.5326,
        zona_geografica == 3 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) >= 8 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) < 12 ~ 3.7199,
        zona_geografica == 3 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) >= 12 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) < 16 ~ 5.3456,
        zona_geografica == 3 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) >= 16 ~ 6.0652,
        zona_geografica == 1 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) == 0 ~ 0,
        zona_geografica == 1 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) > 0 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) < 4 ~ 2.3988,
        zona_geografica == 1 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) >= 4 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) < 8 ~ 3.6848,
        zona_geografica == 1 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) >= 8 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) < 12 ~ 4.7543,
        zona_geografica == 1 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) >= 12 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) < 16 ~ 5.8673,
        zona_geografica == 1 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) >= 16 ~ 6.9653,
        zona_geografica == 2 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) == 0 ~ 0,
        zona_geografica == 2 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) > 0 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) < 4 ~ 2.1855,
        zona_geografica == 2 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) >= 4 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) < 8 ~ 3.4882,
        zona_geografica == 2 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) >= 8 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) < 12 ~ 4.7975,
        zona_geografica == 2 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) >= 12 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) < 16 ~ 6.1395,
        zona_geografica == 2 & mean(anos_edu[EDAD >= 15], na.rm = TRUE) >= 16 ~ 6.5294
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -c('nivel_edu', 'grado_edu', 'anos_edu')
    )
}

############################ ALFABETIZACION DEL HOGAR###########################
encft_icv_legacy_compute_alfav1 <- function(tbl) {
  tbl <- encft_icv_legacy_compute_zona_geografica(tbl)
  tbl %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(ftc_household_keys(tbl, dwelling = FALSE)))) %>%
    dplyr::mutate(
      alfabeta15 = dplyr::case_when(
        EDAD >= 15 & SABE_LEER_ESCRIBIR == 1 ~ 1,
        EDAD >= 15 & SABE_LEER_ESCRIBIR == 2 ~ 0
      ),
      alfa15 = mean(alfabeta15, na.rm = TRUE),
      alfav1 = dplyr::case_when(
        zona_geografica == 3 & alfa15 == 0 ~ 0,
        zona_geografica == 3 & alfa15 > 0 & alfa15 < 0.5 ~ 0.4654,
        zona_geografica == 3 & alfa15 >= 0.5 & alfa15 < 0.7 ~ 1.4556,
        zona_geografica == 3 & alfa15 >= 0.7 & alfa15 < 0.99 ~ 3.4762,
        zona_geografica == 3 & alfa15 >= 0.99 ~ 4.9410,
        zona_geografica == 1 & alfa15 == 0 ~ 0,
        zona_geografica == 1 & alfa15 > 0 & alfa15 < 0.5 ~ 1.1093,
        zona_geografica == 1 & alfa15 >= 0.5 & alfa15 < 0.7 ~ 2.7664,
        zona_geografica == 1 & alfa15 >= 0.7 & alfa15 < 0.99 ~ 3.9275,
        zona_geografica == 1 & alfa15 >= 0.99 ~ 5.2773,
        zona_geografica == 2 & alfa15 == 0 ~ 0,
        zona_geografica == 2 & alfa15 > 0 & alfa15 < 0.5 ~ 1.8660,
        zona_geografica == 2 & alfa15 >= 0.5 & alfa15 < 0.7 ~ 2.5437,
        zona_geografica == 2 & alfa15 >= 0.7 & alfa15 < 0.99 ~ 3.4730,
        zona_geografica == 2 & alfa15 >= 0.99 ~ 4.7718
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -c("alfabeta15", "alfa15")
    )
}


###################### TIPO ALUMBRADO VIVIENDA##################################
encft_icv_legacy_compute_luzv1 <- function(tbl) {
  tbl <- encft_icv_legacy_compute_zona_geografica(tbl)
  tbl %>%
    dplyr::mutate(
      luzv1 = dplyr::case_when(
        zona_geografica == 3 & TIPO_ALUMBRADO %in% c(1, 2, 3) ~ 33.3130,
        zona_geografica == 3 & TIPO_ALUMBRADO >= 4 ~ 0,
        zona_geografica == 1 & TIPO_ALUMBRADO == 1 ~ 17.0658,
        zona_geografica == 1 & TIPO_ALUMBRADO %in% c(2, 3) ~ 17.5573,
        zona_geografica == 1 & TIPO_ALUMBRADO >= 4 ~ 0,
        zona_geografica == 2 & TIPO_ALUMBRADO == 1 ~ 4.7230,
        zona_geografica == 2 & TIPO_ALUMBRADO %in% c(2, 3) ~ 4.7230,
        zona_geografica == 2 & TIPO_ALUMBRADO >= 4 ~ 0
      )
    )
}


#####################ELIMINA BASURA############################################
encft_icv_legacy_compute_basuv1 <- function(tbl){
  encft_icv_legacy_compute_zona_geografica(tbl) %>%
    dplyr::mutate(
      basuv1 = dplyr::case_when(
        zona_geografica == 1 & COMO_ELIMINA_BASURA == 1 ~ 7.0646,
        zona_geografica == 1 & COMO_ELIMINA_BASURA == 2 ~ 10.2385,
        zona_geografica == 1 & COMO_ELIMINA_BASURA == 3 ~ 2.2588,
        zona_geografica == 1 & COMO_ELIMINA_BASURA == 4 ~ 0.0000,
        zona_geografica == 1 & COMO_ELIMINA_BASURA >= 5 ~ 2.2588,
        zona_geografica == 2 & COMO_ELIMINA_BASURA == 1 ~ 2.8028,
        zona_geografica == 2 & COMO_ELIMINA_BASURA == 2 ~ 1.6266,
        zona_geografica == 2 & COMO_ELIMINA_BASURA == 3 ~ 0.0000,
        zona_geografica == 2 & COMO_ELIMINA_BASURA == 4 ~ 0.6206,
        zona_geografica == 2 & COMO_ELIMINA_BASURA >= 5 ~ 0.0000
      )
    )
}

#####################TIPO DE VIVIENDA###########################################
encft_icv_legacy_compute_tipo_vivienda <- function(tbl){
  encft_icv_legacy_compute_zona_geografica(tbl) %>%
    dplyr::mutate(
      tipov1 = dplyr::case_when(
        zona_geografica == 1 & TIPO_VIVIENDA <= 3 ~ 7.0670,
        zona_geografica == 1 & TIPO_VIVIENDA %in% c(4, 5) ~ 16.3331,
        zona_geografica == 1 & TIPO_VIVIENDA %in% c(7, 8) ~ 0,
        zona_geografica == 1 & TIPO_VIVIENDA %in% c(6, 99) ~ 12.7737,
        zona_geografica == 2 & TIPO_VIVIENDA <= 3 ~ 10.9493,
        zona_geografica == 2 & TIPO_VIVIENDA %in% c(4, 5) ~ 23.1710,
        zona_geografica == 2 & TIPO_VIVIENDA %in% c(7, 8) ~ 0,
        zona_geografica == 2 & TIPO_VIVIENDA %in% c(6, 99) ~ 20.0042
      )
    )
}

#################### SANITARIO VIVIENDA#########################################
encft_icv_legacy_compute_saniv1 <- function(tbl) {
  tbl <- encft_icv_legacy_compute_zona_geografica(tbl)
  tbl %>%
    dplyr::mutate(
      saniv1 = dplyr::case_when(
        zona_geografica == 3 & TIPO_SANITARIO %in% c(3, 4, 5) ~ 0,
        zona_geografica == 3 & TIPO_SANITARIO %in% c(1, 2) ~ 5.2669,
        zona_geografica == 1 & TIPO_SANITARIO == 5 ~ 0,
        zona_geografica == 1 & TIPO_SANITARIO %in% c(3, 4) ~ 1.1029,
        zona_geografica == 1 & TIPO_SANITARIO %in% c(1, 2) ~ 3.3748,
        zona_geografica == 2 & TIPO_SANITARIO == 5 ~ 0,
        zona_geografica == 2 & TIPO_SANITARIO %in% c(3, 4) ~ 2.1663,
        zona_geografica == 2 & TIPO_SANITARIO %in% c(1, 2) ~ 5.1403
      )
    )
}

###################### COMBUSTIBLE COCINA#######################################
encft_icv_legacy_compute_cociv1 <- function(tbl) {
  tbl <- encft_icv_legacy_compute_zona_geografica(tbl)
  tbl %>%
    dplyr::mutate(
      cociv1 = dplyr::case_when(
        zona_geografica == 3 & COMBUSTIBLE_PARA_COCINAR <= 3 ~ 11.2050,
        zona_geografica == 3 & COMBUSTIBLE_PARA_COCINAR %in% c(4, 5, 99) ~ 0,
        zona_geografica == 3 & COMBUSTIBLE_PARA_COCINAR == 6 ~ 4.9538,
        zona_geografica == 1 & COMBUSTIBLE_PARA_COCINAR <= 3 ~ 8.4014,
        zona_geografica == 1 & COMBUSTIBLE_PARA_COCINAR %in% c(4, 5, 99) ~ 0,
        zona_geografica == 1 & COMBUSTIBLE_PARA_COCINAR == 6 ~ 3.2617,
        zona_geografica == 2 & COMBUSTIBLE_PARA_COCINAR <= 3 ~ 3.4012,
        zona_geografica == 2 & COMBUSTIBLE_PARA_COCINAR %in% c(4, 5, 99) ~ 0,
        zona_geografica == 2 & COMBUSTIBLE_PARA_COCINAR == 6 ~ 0.6577
      )
    )
}

##################### HACINAMIENTO#################################
encft_icv_legacy_compute_hacina1 <- function(tbl) {
  tbl <- encft_icv_legacy_compute_zona_geografica(tbl)
  tbl <- tbl %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(ftc_household_keys(tbl, dwelling = TRUE)))) %>%
    dplyr::mutate(
      hacina1 = dplyr::case_when(
        CANT_DORMITORIOS_VIVIENDA == 0 ~ 0,
        zona_geografica == 3 & n() / CANT_DORMITORIOS_VIVIENDA >= 4 ~ 0,
        zona_geografica == 3 & n() / CANT_DORMITORIOS_VIVIENDA >= 2.5 & n() / CANT_DORMITORIOS_VIVIENDA < 4 ~ 1.5135,
        zona_geografica == 3 & n() / CANT_DORMITORIOS_VIVIENDA >= 2 & n() / CANT_DORMITORIOS_VIVIENDA < 2.5 ~ 2.5462,
        zona_geografica == 3 & n() / CANT_DORMITORIOS_VIVIENDA < 2 ~ 3.8638,
        zona_geografica == 1 & n() / CANT_DORMITORIOS_VIVIENDA >= 4 ~ 0,
        zona_geografica == 1 & n() / CANT_DORMITORIOS_VIVIENDA >= 2.5 & n() / CANT_DORMITORIOS_VIVIENDA < 4 ~ 3.4806,
        zona_geografica == 1 & n() / CANT_DORMITORIOS_VIVIENDA >= 2 & n() / CANT_DORMITORIOS_VIVIENDA < 2.5 ~ 4.9446,
        zona_geografica == 1 & n() / CANT_DORMITORIOS_VIVIENDA < 2 ~ 6.2249,
        zona_geografica == 2 & n() / CANT_DORMITORIOS_VIVIENDA >= 4 ~ 0,
        zona_geografica == 2 & n() / CANT_DORMITORIOS_VIVIENDA >= 2.5 & n() / CANT_DORMITORIOS_VIVIENDA < 4 ~ 5.3100,
        zona_geografica == 2 & n() / CANT_DORMITORIOS_VIVIENDA >= 2 & n() / CANT_DORMITORIOS_VIVIENDA < 2.5 ~ 5.8757,
        zona_geografica == 2 & n() / CANT_DORMITORIOS_VIVIENDA < 2 ~ 6.5229
      )
    ) %>%
    dplyr::ungroup()
  tbl
}

#################### Proporción menores de 5 años###############################
encft_icv_legacy_compute_pninme1 <- function(tbl) {
  tbl <- encft_icv_legacy_compute_zona_geografica(tbl)
  tbl %>%
    dplyr::mutate(
      menor5 = dplyr::case_when(
        zona_geografica == 3 & EDAD <= 5 ~ 1,
        zona_geografica == 3 & EDAD >  5 ~ 0
      )
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(ftc_household_keys(tbl, dwelling = FALSE)))) %>%
    dplyr::mutate(
      pninme1 = dplyr::case_when(
        zona_geografica == 3 & sum(menor5, na.rm = TRUE) / CANTIDAD_MIEMBROS_HOGAR == 0 ~ 2.6054,
        zona_geografica == 3 & sum(menor5, na.rm = TRUE) / CANTIDAD_MIEMBROS_HOGAR > 0 ~ 0
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -'menor5'
    )
}

################### ASISTENCIA ESCOLAR##########################################
encft_icv_legacy_compute_asiscv1 <- function(tbl) {
  tbl <- encft_icv_legacy_compute_zona_geografica(tbl)
  tbl %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(ftc_household_keys(tbl, dwelling = FALSE)))) %>%
    dplyr::mutate(
      E614 = dplyr::case_when(
        zona_geografica == 3 & dplyr::between(EDAD, 6, 14) ~ 1,
        zona_geografica == 3 & !dplyr::between(EDAD, 6, 14) ~ 0
      ),
      inscrito = dplyr::case_when(
        EDAD > 2 & dplyr::between(NIVEL_SE_MATRICULO, 1, 8) ~ 1,
        EDAD > 2 & NIVEL_SE_MATRICULO == 9 & dplyr::between(MES, 6, 8) & PORQUE_NO_ESTUDIA == 1 ~ 1,
        EDAD > 2 ~ 0
      ),
      AS614 = dplyr::case_when(
        zona_geografica == 3 & E614 == 1 & inscrito == 1 ~ 1
      ),
      AS614HOGAR = dplyr::case_when(
        zona_geografica == 3 ~ sum(AS614, na.rm = TRUE)
      ),
      E614HOGAR = sum(E614),
      asiscv1 = dplyr::case_when(
        zona_geografica == 3 & E614HOGAR > 0 & AS614HOGAR == 0 ~ 0,
        zona_geografica == 3 & E614HOGAR > 0 & (AS614HOGAR < E614HOGAR & AS614HOGAR > 0) ~ 8.7221,
        zona_geografica == 3 & E614HOGAR > 0 & (AS614HOGAR == E614HOGAR) ~ 8.9740,
        zona_geografica == 3 & E614HOGAR == 0 ~ 9.9633
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -c("E614",
         "inscrito",
         "AS614",
         "AS614HOGAR",
         "E614HOGAR")
    )
}

####################### PROVINCIA ############################################
encft_icv_legacy_compute_provin1 <- function(tbl){
  encft_icv_legacy_compute_zona_geografica(tbl) %>%
    dplyr::mutate(
      provin1 = dplyr::case_when(
        zona_geografica == 1 & ID_PROVINCIA == 2  ~ 1.0049,
        zona_geografica == 1 & ID_PROVINCIA == 3  ~ 0.4704,
        zona_geografica == 1 & ID_PROVINCIA == 4  ~ 0.8853,
        zona_geografica == 1 & ID_PROVINCIA == 5  ~ 1.1236,
        zona_geografica == 1 & ID_PROVINCIA == 6  ~ 2.1117,
        zona_geografica == 1 & ID_PROVINCIA == 7  ~ 0.4642,
        zona_geografica == 1 & ID_PROVINCIA == 8  ~ 1.4636,
        zona_geografica == 1 & ID_PROVINCIA == 9  ~ 2.1426,
        zona_geografica == 1 & ID_PROVINCIA == 10 ~ 0.0629,
        zona_geografica == 1 & ID_PROVINCIA == 11 ~ 2.2711,
        zona_geografica == 1 & ID_PROVINCIA == 12 ~ 2.0400,
        zona_geografica == 1 & ID_PROVINCIA == 13 ~ 2.7281,
        zona_geografica == 1 & ID_PROVINCIA == 14 ~ 1.9953,
        zona_geografica == 1 & ID_PROVINCIA == 15 ~ 0.5682,
        zona_geografica == 1 & ID_PROVINCIA == 16 ~ 0.0000,
        zona_geografica == 1 & ID_PROVINCIA == 17 ~ 1.5429,
        zona_geografica == 1 & ID_PROVINCIA == 18 ~ 2.4547,
        zona_geografica == 1 & ID_PROVINCIA == 19 ~ 2.1620,
        zona_geografica == 1 & ID_PROVINCIA == 20 ~ 2.3363,
        zona_geografica == 1 & ID_PROVINCIA == 21 ~ 2.0712,
        zona_geografica == 1 & ID_PROVINCIA == 22 ~ 1.4347,
        zona_geografica == 1 & ID_PROVINCIA == 23 ~ 2.1183,
        zona_geografica == 1 & ID_PROVINCIA == 24 ~ 1.7771,
        zona_geografica == 1 & ID_PROVINCIA == 25 ~ 1.8460,
        zona_geografica == 1 & ID_PROVINCIA == 26 ~ 1.8382,
        zona_geografica == 1 & ID_PROVINCIA == 27 ~ 0.9014,
        zona_geografica == 1 & ID_PROVINCIA == 28 ~ 2.7509,
        zona_geografica == 1 & ID_PROVINCIA == 29 ~ 0.8212,
        zona_geografica == 1 & ID_PROVINCIA == 30 ~ 1.1808,
        zona_geografica == 1 & ID_PROVINCIA == 31 ~ 1.1775,
        zona_geografica == 2 & ID_PROVINCIA == 2  ~ 2.3445,
        zona_geografica == 2 & ID_PROVINCIA == 3  ~ 0.5522,
        zona_geografica == 2 & ID_PROVINCIA == 4  ~ 1.1805,
        zona_geografica == 2 & ID_PROVINCIA == 5  ~ 2.2416,
        zona_geografica == 2 & ID_PROVINCIA == 6  ~ 3.0888,
        zona_geografica == 2 & ID_PROVINCIA == 7  ~ 0.0667,
        zona_geografica == 2 & ID_PROVINCIA == 8  ~ 0.0000,
        zona_geografica == 2 & ID_PROVINCIA == 9  ~ 3.2083,
        zona_geografica == 2 & ID_PROVINCIA == 10 ~ 1.2629,
        zona_geografica == 2 & ID_PROVINCIA == 11 ~ 2.1869,
        zona_geografica == 2 & ID_PROVINCIA == 12 ~ 1.3054,
        zona_geografica == 2 & ID_PROVINCIA == 13 ~ 3.9664,
        zona_geografica == 2 & ID_PROVINCIA == 14 ~ 3.2458,
        zona_geografica == 2 & ID_PROVINCIA == 15 ~ 1.8245,
        zona_geografica == 2 & ID_PROVINCIA == 16 ~ 0.5143,
        zona_geografica == 2 & ID_PROVINCIA == 17 ~ 3.4921,
        zona_geografica == 2 & ID_PROVINCIA == 18 ~ 2.9998,
        zona_geografica == 2 & ID_PROVINCIA == 19 ~ 3.2481,
        zona_geografica == 2 & ID_PROVINCIA == 20 ~ 3.2967,
        zona_geografica == 2 & ID_PROVINCIA == 21 ~ 3.3493,
        zona_geografica == 2 & ID_PROVINCIA == 22 ~ 1.5091,
        zona_geografica == 2 & ID_PROVINCIA == 23 ~ 1.9601,
        zona_geografica == 2 & ID_PROVINCIA == 24 ~ 3.1140,
        zona_geografica == 2 & ID_PROVINCIA == 25 ~ 4.8060,
        zona_geografica == 2 & ID_PROVINCIA == 26 ~ 2.3673,
        zona_geografica == 2 & ID_PROVINCIA == 27 ~ 1.1868,
        zona_geografica == 2 & ID_PROVINCIA == 28 ~ 4.7285,
        zona_geografica == 2 & ID_PROVINCIA == 29 ~ 1.2119,
        zona_geografica == 2 & ID_PROVINCIA == 30 ~ 0.2984,
        zona_geografica == 2 & ID_PROVINCIA == 31 ~ 0.7792,
        zona_geografica == 2 & ID_PROVINCIA == 32 ~ 4.1690
      )
    )
}

####################### RECODIFICAR MATERIAL PARED VIVIENDA#####################
encft_icv_legacy_recode_material_pared_vivienda <- function(tbl) {
  tbl <- tbl %>%
    dplyr::mutate(
      MATERIAL_PARED_EXTERIOR2 = dplyr::case_when(
        MATERIAL_PARED_EXTERIOR_ESP == "ARENA CEMENTO ALAMBRE Y MADERA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "ARENA Y VARILLA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "ARENA, CEMENTO, ALAMABRE Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "ARENA, CEMENTO, ALAMBRE Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "ARENA, VARILA, CEMENTO Y ALAMBRE" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "ARENA, VARILLA, CEMENTO Y ALAMBRE" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "BACIADO DE CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "BACIADO DE CEMENTO Y ARENAS" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "BACIADO DE CEMENTO, ARENA Y VARILLA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "BACIADO DE CEMENTO, ARENA, VARILLA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "BACIADO DE CEMENTO, VARILLA Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "BAIADO DE CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "BASIADO Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "BLOCK MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "BVACIADO DE CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "CEEMNTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "CEMENTO ARENA SIN VARILLA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "CEMENTO SIN VARILLA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "CEMENTO, ARENA, ALAMBRE Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "CEMENTO, VARILLA, ALAMBRE, ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "COCRETO VACIADO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "CONCRETO BACIADO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "CONCRETO DE ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "CONCRETO NACIADO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "CONCRETO VACIADO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "CONCRETO VACIADO DE ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "CONCRETO VACIANDO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "CONCRETO VARIADO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "CONCRETO VASIADO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "CONCRETRO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "MEZCLA DE CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACADO DE CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACAIDO DE ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACAIDO DE CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACEADI DE CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACEADO BLOCK Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACEADO CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACEADO CEMENTO Y ARENA MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACEADO CEMENTO Y VARILLA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACEADO CEMENTO YARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACEADO CEMETO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACEADO CEMNTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACEADO CONCRETO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACEADO DE ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACEADO DE CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACEADO DE CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACEADO DE CEMENTO Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACEADO DE CEMENTO Y VARILLA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACEADO DE CEMETO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACEADO DE CONCRETO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACEDO CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACEDO DE CEMNTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIA DE ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADIO DE ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ALAMBRE CEMENTO ARENA, MADERO" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ALAMBRE Y CONCRETO, MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ALAMBRE, Y CONCRETO Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA ALAMBRE VARILLA CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA ALAMBRE Y VARILLA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA ALAMBRE Y VARILLA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA CEMENTO VARILLA Y ALAMBRE" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA CEMENTO Y VARILLA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA VARILLA ALAMBRE Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA VARILLA CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA VARILLA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA VARRILA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA VASILLA CEMENTE" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA Y CEMENTO, VARRILLA, ALMABRE" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA Y CMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA, CEMENTO," ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA, VARILLA Y ALAMBRE" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA, VARILLA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA, VARILLA, CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO CAMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO CEMENTO ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO CEMENTO ARENA ALAMBRE Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO CEMENTO Y ARE4NA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO CEMENTO, ALAMBRE, MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO CEMENTO, ARENA, ALAMBRE Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO CON CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO CON CEMENTO SIN VARILLAS" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO CONCRETO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO CONCRETO Y ALAMBRE MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO CONCRETO Y MADERO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ALAMBRE CON CEMENTO Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ALAMBRE VARILLA Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ALAMBRE Y CONCRETO Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ALAMBRES Y CONCRETOS" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE AREAN Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA ALAMBRA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA ALAMBRADO Y VARILLA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA ALAMBRE Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA ALAMBRE Y VARILLA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA CON CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA VARILLA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA Y ALAMBRE" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA Y ALAMBRE Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA Y ALAMBRES" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA Y CEMENO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA Y CEMENTA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA Y CEMENTE" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA Y CEMENTO ALAMBRE Y VARILLA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA Y CEMENTO SORRILLA Y ALAMBRE" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA Y CEMENTO Y VARILLA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA Y CEMENTO," ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA Y CEMENTOC" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA Y CMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA Y DE CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA Y VARILLA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA YCEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA, ALAMBRE VARILLA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA, ALAMBRE Y VARILLA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA, BARILLA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA, CEMENTO Y BERILLAS" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA, CEMENTO Y VARILLA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA, CEMENTO, ALAMBRE" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA, CEMENTO, ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA, CEMENTO, VARILLA, ALAMBRE" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA, LAMBRE Y VARILLA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA, VARILLA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA, VARILLA, ALAMBRE Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA,VARRILLA Y ALAMBRE" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA, ALAMBRE, CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENAY ALAMBRE" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARGRO, GRAVA Y VARILLA CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ATRENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CAMENTO Y ARADO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENETO Y VARILLA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTIO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO , ALAMBRE Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO ARENA ALAMBRE Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO AY ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO TY ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO Y AREANA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO Y ARENA Y VARILLA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO Y ARENAS" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO Y MADERA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO Y VARILLA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO, ALAMBRE Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO, ALAMBRE, ARENA Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO, ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO, ARENA Y BARILLA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO, ARENA Y BARILLAS" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO, ARENA Y VARILLA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO, ARENA, ALAMBRE Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO, ARENA, BARILLAS" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO, ARENA, CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO, VARILLA, CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTYO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMNETO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMNETO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CONCRETO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CONCRETO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CONCRETO, ALAMBRE Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE DE CONCRETO MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE MEZCLA CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE MEZCLA DE CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE MEZCLA DE CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE MEZCLA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE MEZLCA DE CEMENTOS" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE TABLA Y PALMA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE VARILLA, ARENA Y ALAMBRE" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO EN CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO EN CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO EN CEMENTO Y VARILLA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO MEZCLA CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO MEZCLA DE ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO MEZCLA DE CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO MEZCLA DE CEMENTO ARMADO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO MEZCLA DE CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO MEZLCA ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO Y CEMENTO DE ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO Y CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO Y MADERA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO Y TABLA DE PALMA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO, ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO, ARENA, VARILLA Y ALAMBRE" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO, CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO, CEMENTO Y ARENAS" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO, CEMENTO, ARENA Y VARILLA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO, CEMENTO, Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO, MEZCLA DE CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADODE ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADOR DE ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADOR DE MEZCLA CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADOS DE ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADOS EN CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIANDO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIANDO DE ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIAO MEZCLA DE CEENTI Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIJA BUENA CEMENTO Y MADERA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIOADO DE ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIOADO DE CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIODO DE ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIODO DE CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VAIADO DE CEMENTOS" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VARILLA CEMENTO ALAMBRE Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VARILLA,CEMENTO, ARENA Y ALAMBRE" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VECIADO DE ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VIACIADO DE ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "B LOCK Y TABLA DE PALMA" ~ 9,
        MATERIAL_PARED_EXTERIOR_ESP == "BLCOK Y PALMA" ~ 9,
        MATERIAL_PARED_EXTERIOR_ESP == "BLOCK PALMA" ~ 9,
        MATERIAL_PARED_EXTERIOR_ESP == "BLOCK Y PALMA" ~ 9,
        MATERIAL_PARED_EXTERIOR_ESP == "BLOCK Y DALMA" ~ 9,
        MATERIAL_PARED_EXTERIOR_ESP == "BLOCK Y PALMA" ~ 9,
        MATERIAL_PARED_EXTERIOR_ESP == "BLOCK Y PALMA," ~ 9,
        MATERIAL_PARED_EXTERIOR_ESP == "BLOCK Y PALMAS" ~ 9,
        MATERIAL_PARED_EXTERIOR_ESP == "BLOCK Y PLAMA" ~ 9,
        MATERIAL_PARED_EXTERIOR_ESP == "BLOCK Y TABLA" ~ 9,
        MATERIAL_PARED_EXTERIOR_ESP == "BLOCK Y TABLA DE PALMA" ~ 9,
        MATERIAL_PARED_EXTERIOR_ESP == "BLOCK Y TABLA DE PLAMA" ~ 9,
        MATERIAL_PARED_EXTERIOR_ESP == "BLOCK Y TABLA PALMA" ~ 9,
        MATERIAL_PARED_EXTERIOR_ESP == "CEMENTO Y PLAYWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MAJA REBESTIDA DE CEMENTO Y PLYWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MAJA REBESTIDO DE CEMENTO PLYWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MATA REBESTIDA DE CEMENTO Y PLYWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MATA REBISTIDORA DE CEMENTO Y PLYWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MATA REVESTIDA CON CEMENTO Y PLYWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MAYA CON CEMENTO Y PLYWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MAYA DE REBESTIDO DE CEMENTO Y PLYWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MAYA REBESTIBLE DE CEMENTO Y PLYWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MAYA REBESTIDA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MAYA REBESTIDA CON CEMENTO Y PLYWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MAYA REBESTIDA DE CEMENTO	" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MAYA REBESTIDA DE CEMENTO PLYWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MAYA REBESTIDA DE CEMENTO Y PLAYWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MAYA REBESTIDA DE CEMENTO Y PLEYWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MAYA REBESTIDA DE CEMENTO Y PLYWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MAYA REBESTIDAS DE CEMENTO Y PLYWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MAYA REBESTIDO DE CEMENTO Y PLYWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MAYA REVERCIBLE Y PLYWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MAYA REVESTIBLE DE CEMENTO Y PLEWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MAYA REVESTIBLE DE CEMENTO Y PLYWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MAYA REVESTIDA CEMENTO Y PLEWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MAYA REVESTIDA DE CEMENTO Y PLAYWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MAYA REVESTIDA DE CEMENTO Y PLYWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MAYA REVESTIDA POR CEMENTO Y PLAYWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MAYA REVUELTA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "MAYOR REVESTIDO CEMENTO Y PLAYWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "REBESTIDA DE CEMENTO Y PLYWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VARILLA, ARENA, CEMENTO, ALAMBRE, MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VECIADO DE CEMENETO Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUERO" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETA" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETA PRE FABRICACION" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETA PRE FABRICADA" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETA PRE FABRICADAS" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETA PREFABRICACDA" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETA PREFABRICADA" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETA PREFABRICADAS" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETA, PRE-FABRICADAS" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETAQR PREFABRICADA" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETAR PREDABRICADA" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETAR PREFABRICACDA" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETAR PREFABRICADA" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETAS" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETAS PRE - FABRICADAS" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETAS PRE FABRICACION" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETAS PRE FABRICADA" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETAS PRE FABRICADAS" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETAS PRE FABRICADO" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETAS PRE- FABRICAADA" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETAS PREDABRICADAS" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETAS PREFABRICACDAS" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETAS PREFABRICADA" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETAS PREFABRICADAS" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETAS RPEFABRICADAS" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETAS, PRE FABRICADAS" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETES" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETES PRE FABRICADA" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PLAUQETAR PREFABRICADA" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PRE FABRICADA PLAQUETAS" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PRE FABRICADAS PLAQUETAS" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PRE FABRICADO PLAQUETAS" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PRE-FABRICADAS PLAQUETAS" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PRE-FABRICADO PLAQUETAS" ~ 4,
        MATERIAL_PARED_EXTERIOR == 99 & MATERIAL_PARED_EXTERIOR_ESP == "PREFABRICADA PLAQUETAS" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "CEMENTO Y AREN" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "CEMENTO Y PLAYWODD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "CEMENTO Y PLAYWOOD" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETA PREFABRICADA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETAR PREFABRICADA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETAS PREFABRICACION" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETAS PREFABRICADA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETON PREFABRICADO" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACEADO CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO CEMENTO Y ARENA, MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA VARILLA CEMENTO MADERA Y ALMABRE" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA VARILLA MADERA Y CEMENTO" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA Y CEMENTO, VARILLA Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA, CEMENTO Y VARILLA, MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA, CEMENTO, VARILLA Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA, CEMENTO, VARILLA, MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA, VARILLA, CEMENTO Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENRO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO Y ARENA Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO, VARILLA, MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTYO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMNTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE EMENTO Y ARENA," ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DEMENTO ALAMBRE ARENA Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIASDO DE CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VARIADO DE ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VARIADO DE CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA, CEMENTO Y VARILLA, MADERA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA, CEMENTO, VARILLA Y MADERA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "CEMENTO ARENA Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "CEMENTO Y ATRENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "CEMENTOY ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETA PREFABRICADA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETE PREFABRICADA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETON PREFABRICADA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETON PREFABRICADO" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA ALAMBRE CEMENTO MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA CEMENTO ALAMBRE MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA CEMENTO MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA CEMENTO VARILLA MADERA ALAMBRE" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA MADERA VARILLA CEMENTO ALAMBRE" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA Y CEMENTO, VARILLA, MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA, CEMENTO, VARILLA, MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO ARENA, CEMENTO, VARILLA, MADERA, ALAMBRE" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO CEMENTI ARENA ALAMBRE Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO CEMENTO VARILLA MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO CEMENTO Y HARINA , ALAMBRE, MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA Y CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA Y VARILLA ALAMBRE CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE ARENA, CEMENTO, VARILLA, MADERA, ALAMBR" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO ARENA ALAMBRE Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO Y AREN" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO Y VARILLA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTO, ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DE CEMENTRO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DEMENTO ALAMBRE ARENA Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIADO DEMENTO ARENA ALAMBRE Y MADERA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VACIOADO DE CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "VACUADO DE CEMENTO Y ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETAR PREFABRICADA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "PLAQUEDOR PREPARADA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETON PREFABRICADO" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETON PREFABRICADOR" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETON PREPARADO" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETON RELOBRICADO" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "PLOQUETOR PREFABRICADO" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VICIADO DE ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR_ESP == "PLAQUEDOR PREPARADA" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETON PREFABRICADO" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETON PREFABRICADOR" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETON PREPARADO" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "PLAQUETON RELOBRICADO" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "PLOQUETOR PREFABRICADO" ~ 7,
        MATERIAL_PARED_EXTERIOR_ESP == "VICIADO DE ARENA" ~ 4,
        MATERIAL_PARED_EXTERIOR == 14 ~ 4,
        TRUE ~ MATERIAL_PARED_EXTERIOR
      )
    )
  tbl
}
