#' Cox model summary row
#'
#' @description Generate a descriptive row for a numeric covariate using Cox regression.
#' @param data Data frame with columns `tempos`, `censura` and the numeric variable `.y.`.
#' @param column Name of the numeric column.
#' @param k Number of decimal places for rounding.
#' @param test Label for the statistical test to display.
#' @importFrom survival coxph Surv
#' @importFrom tibble tibble
#' @return A tibble with the regression coefficient and p-value.
#' @examples
#' tab_desc_num(lung %>% dplyr::select(tempos, censura, .y.=age), 'age')
#' @export

tab_desc_num <- function(data, column, k = 4, test = '-'){
  fit <- coxph(data = data, Surv(data$tempos, data$censura)~.y.)
  
  tibble(
    .y. = 'Regression coefficient',
    frequency_col = NA,
    summary_text = fit %>% coef %>% exp %>% round(., k) %>% as.character,
    p = format_sig(summary(fit)[["sctest"]][["pvalue"]]),
    test = test
  )
}

