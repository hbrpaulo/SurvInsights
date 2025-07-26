#' Descriptive summary for numeric variables
#'
#' Fits a Cox model to `.y.` and returns the exponentiated coefficient.
#'
#' @param aux Data frame with `tempos`, `censura` and `.y.`.
#' @param column Name of the numeric column.
#' @param k Number of decimal places for rounding.
#' @return Tibble with coefficient and p-value.
#'
#' @examples
#' df <- tibble::tibble(tempos = c(5,10,7),
#'                      censura = c(1,0,1),
#'                      age = c(70,45,63))
#' tab_desc_num(dplyr::select(df, tempos, censura, .y. = age), 'age')
 tab_desc_num <- function(aux, column, k = 4){
   fit <- coxph(data = aux, Surv(aux$tempos, aux$censura)~.y.)

   tibble(
     .y. = 'Regression coefficient',
     group1 = NA,
     group2 = fit %>% coef %>% exp %>% round(., k) %>% as.character,
     p = format_sig(summary(fit)[["sctest"]][["pvalue"]]),
     test = 'placeholder'
   )
 }
