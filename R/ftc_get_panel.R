ftc_get_panel <- function(encft, .start, .end, all_periods = TRUE, level = c("h", "m"), filter_cases = FALSE, factor_exp = "mean") {
  if (length(level) > 1) {
    level <- level[[1]]
  }

  if (!level %in% c("h", "m")) {
    # error....
  }

  if (all_periods) {
    valid_ids <- encft %>%
      dplyr::filter(dplyr::between(TRIMESTRE, .start, .end))
  } else {
    valid_ids <- encft %>%
      dplyr::filter(TRIMESTRE %in% c(.start, .end))
  }

  key_var <- "ID_HOGAR"

  if (level == "m") {
    key_var <- "ID_PERSONA"
  }

  valid_ids %>%
    pull(TRIMESTRE) %>%
    unique() -> periods

  length(periods) -> n_periods

  encft <- valid_ids %>%
    dplyr::select(TRIMESTRE, dplyr::all_of(key_var)) %>%
    dplyr::distinct() %>%
    dplyr::select(-TRIMESTRE) %>%
    dplyr::count(dplyr::across(dplyr::all_of(key_var))) %>%
    dplyr::filter(n == n_periods) %>%
    dplyr::select(-n) %>%
    dplyr::mutate(panel = 1) %>%
    dplyr::left_join(encft, ., by = dplyr::join_by(!!!key_var)) %>%
    dplyr::mutate(panel = dplyr::if_else(is.na(panel), 0, 1)) %>%
    dplyr::filter(TRIMESTRE %in% periods)

  if (filter_cases) {
    encft <- encft %>%
      dplyr::filter(panel == 1) %>%
      dplyr::select(-panel)
  }

  if (!is.null(factor_exp)) {
    if (factor_exp == "mean") {
      encft <- encft %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(key_var))) %>%
        dplyr::mutate(FACTOR_EXPANSION = mean(FACTOR_EXPANSION)) %>%
        dplyr::ungroup()
    }
  }

  encft
}
