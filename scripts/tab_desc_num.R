#' @description Unify results for numerical variables
#' @param aux Data frame with survival data
#' @return A tibble with frequency and survival statistics

tab_desc_num <- function(aux, column, k = 4){
  fit <- coxph(data = aux, Surv(aux$tempos, aux$censura)~.y.)
  
  tibble(
    .y. = 'Coeficiente regressao',
    group1 = NA,
    group2 = fit %>% coef %>% exp %>% round(., k) %>% as.character, 
    p = format_sig(summary(fit)[["sctest"]][["pvalue"]]),
    test = 'placeholder'
  )
}