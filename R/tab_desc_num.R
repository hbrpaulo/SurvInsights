#' Cox model summary row
#'
#' @description Generate a descriptive row for a numeric covariate using Cox regression.
#' @param data Data frame with time, event and the numeric variable `.y.`.
#' @param column Name of the numeric column.
#' @param k Number of decimal places for rounding.
#' @param test Label for the statistical test to display.
#' @param time_col Name of the time-to-event column.
#' @param event_col Name of the event indicator column.
#' @importFrom survival coxph Surv
#' @importFrom tibble tibble
#' @return A tibble with the regression coefficient and p-value.
#' @examples
#' tab_desc_num(lung %>% dplyr::select(tempos, censura, .y.=age), 'age')
#' df2 <- lung %>% dplyr::select(t = time, e = status, .y. = age)
#' tab_desc_num(df2, 'age', time_col = 't', event_col = 'e')
#' @export

tab_desc_num <- function(data, column, k = 4, test = '-',
                         time_col = "tempos", event_col = "censura"){
  fit <- coxph(data = data, Surv(data[[time_col]], data[[event_col]])~.y.)
  
  tibble(
    .y. = 'Regression coefficient',
    frequency_col = NA,
    summary_text = fit %>% coef %>% exp %>% round(., k) %>% as.character,
    p = format_sig(summary(fit)[["sctest"]][["pvalue"]]),
    test = test
  )
}

