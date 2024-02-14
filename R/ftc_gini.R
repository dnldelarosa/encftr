ftc_summ_gini <- function(tbl, ing_var = "ing_pc_pobreza_def", .wt = "FACTOR_EXPANSION"){
  if(is.null(.wt)){
    tbl %>%
      dplyr::summarise(gini = dineq::gini.wtd(!!as.symbol(ing_var)))
  } else {
    tbl %>%
      dplyr::summarise(gini = dineq::gini.wtd(!!as.symbol(ing_var), !!as.symbol(.wt)))
  }
}
