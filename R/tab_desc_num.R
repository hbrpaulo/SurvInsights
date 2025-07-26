#' @description Unify results for numerical variables
#' @param aux Data frame with survival data
#' @param test Label for the statistical test (default "-")
#' @return A tibble with frequency and survival statistics

tab_desc_num <- function(aux, column, k = 4, test = '-'){
  fit <- coxph(data = aux, Surv(aux$tempos, aux$censura)~.y.)
  
  tibble(
    .y. = 'Regression coefficient',
    group1 = NA,
    group2 = fit %>% coef %>% exp %>% round(., k) %>% as.character, 
    p = format_sig(summary(fit)[["sctest"]][["pvalue"]]),
    test = test
  )
}
