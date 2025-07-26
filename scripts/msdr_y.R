#' Mean survival time by group
#'
#' Computes the restricted mean survival time for each level of `.y.`.
#'
#' @param aux Data frame with columns `tempos`, `censura` and `.y.`.
#' @param k Number of decimal places for rounding.
#' @return Tibble with formatted mean survival time per group.
#'
#' @examples
#' df <- tibble::tibble(tempos = c(5, 10, 7),
#'                      censura = c(1, 0, 1),
#'                      group = c('A', 'B', 'A'))
#' msdr_y(dplyr::select(df, tempos, censura, .y. = group))
msdr_y <- function(aux, k = 2){

  if(length(unique(aux$.y.))<2){
    fit <- survfit(data = aux,
                   Surv(aux$tempos, aux$censura)~.y.)
    fit_sum <- data.frame(t(summary(fit)$table))
    tibble(.y. = aux$.y.[1],
           media = fit_sum$rmean,
           sd = fit_sum$se.rmean,
           li = fit_sum$X0.95LCL,
           ls = fit_sum$X0.95UCL) %>%
      mutate_if(is.numeric, round, digits = k) %>%
      mutate(group2 = paste0(media, '±', sd,' (',
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
      mutate_if(is.numeric, round, digits = k) %>%
      mutate(group2 = paste0(media, '±', sd,' (',
                             li, '~', ls, ')')) %>%
      select(.y., group2)
  }
}
