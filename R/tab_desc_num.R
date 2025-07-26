#' Cox model summary row
#'
#' @description Generate a descriptive row for a numeric covariate using Cox regression.
#' @param aux Data frame with columns `tempos`, `censura` and the numeric variable `.y.`.
#' @param column Name of the numeric column.
#' @param k Number of decimal places for rounding.
#' @param test Label for the statistical test to display.
#' @return A tibble with the regression coefficient and p-value.
#' @examples
#' tab_desc_num(lung %>% dplyr::select(tempos, censura, .y.=age), 'age')
#' @export

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

