#' Mean survival by group
#'
#' @description Compute mean survival time and standard error for each group in a dataset.
#' @param data Data frame with columns `tempos`, `censura` and grouping variable `.y.`.
#' @param k Number of decimal places for rounding.
#' @importFrom tibble tibble
#' @importFrom dplyr mutate across select
#' @importFrom stringr str_remove
#' @importFrom survival Surv survfit
#' @return A tibble with group labels and a formatted summary string.
#' @examples
#' df <- tibble::tibble(tempos = c(1,2), censura = c(1,0), .y. = c('A','B'))
#' msdr_y(df)
#' @export
msdr_y <- function(data, k = 2){

  if(length(unique(data$.y.)) < 2){
    fit <- survfit(data = data,
                   Surv(data$tempos, data$censura)~.y.)
    fit_sum <- data.frame(t(summary(fit)$table))
    tibble(.y. = data$.y.[1],
           media = fit_sum$rmean,
           sd = fit_sum$se.rmean,
           li = fit_sum$X0.95LCL,
           ls = fit_sum$X0.95UCL) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, digits = k))) %>%
      mutate(summary_text = format_msdr(media, sd, li, ls)) %>%
      select(.y., summary_text)
  }else{
    fit <- survfit(data = data,
                   Surv(data$tempos, data$censura)~.y.)
    fit_sum <- data.frame(summary(fit)$table)

    tibble(.y. = str_remove(rownames(fit_sum), '.y.='),
           media = fit_sum$rmean,
           sd = fit_sum$se.rmean.,
           li = fit_sum$X0.95LCL,
           ls = fit_sum$X0.95UCL) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, digits = k))) %>%
      mutate(summary_text = format_msdr(media, sd, li, ls)) %>%
      select(.y., summary_text)
  }
}

