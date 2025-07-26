#' @description: 'Calculates the mean survival time and its standard error 
#' for each group in a survival analysis dataset.

#' @param aux A data frame containing survival data with columns for time,
#' censoring, and a grouping variable (.y.).
#' @param k Number of decimal places to round the results (default is 2).
#' @return A tibble with the group identifier, mean survival time, standard error,
#' lower and upper confidence limits, and a formatted string for the group.

#' @examples
#' # Assuming 'df' is a data frame with survival data
#' msdr_y(df, k = 2)
#' rmean plus and minus se_rmean (0.95LCL ~ 0.95UCL)

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
      mutate_if(is.numeric, round, digits = k) %>% 
      mutate(group2 = paste0(media, '\u00B1', sd,' (',
                             li, '~', ls, ')')) %>% 
      select(.y., group2)
  }
}
