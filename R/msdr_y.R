#' Mean survival by group
#'
#' @description Compute mean survival time and standard error for each group in a dataset.
#' @param aux Data frame with columns `tempos`, `censura` and grouping variable `.y.`.
#' @param k Number of decimal places for rounding.
#' @return A tibble with group labels and a formatted summary string.
#' @examples
#' df <- tibble::tibble(tempos = c(1,2), censura = c(1,0), .y. = c('A','B'))
#' msdr_y(df)
#' @export
msdr_y <- function(aux, k = 2){

  if(length(unique(aux$.y.)) < 2){
    fit <- survfit(data = aux,
                   Surv(aux$tempos, aux$censura)~.y.)
    fit_sum <- data.frame(t(summary(fit)$table))
    tibble(.y. = aux$.y.[1],
           media = fit_sum$rmean,
           sd = fit_sum$se.rmean,
           li = fit_sum$X0.95LCL,
           ls = fit_sum$X0.95UCL) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), round, digits = k)) %>%
      mutate(group2 = paste0(media, '\u00B1', sd,' (',
                             li, '~', ls, ')')) %>%
      select(.y., group2)
  }else{
    fit <- survfit(data = aux,
                   Surv(aux$tempos, aux$censura)~.y.)
    fit_sum <- data.frame(summary(fit)$table)

    tibble(.y. = str_remove(rownames(fit_sum), '.y.='),
           media = fit_sum$rmean,
           sd = fit_sum$se.rmean.,
           li = fit_sum$X0.95LCL,
           ls = fit_sum$X0.95UCL) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), round, digits = k)) %>%
      mutate(group2 = paste0(media, '\u00B1', sd,' (',
                             li, '~', ls, ')')) %>%
      select(.y., group2)
  }
}

