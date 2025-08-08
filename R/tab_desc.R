#' Descriptive statistics for one column
#'
#' @description Summarise a variable with counts and survival metrics.
#' @param df Data frame containing `tempos`, `censura` and the variable.
#' @param column Name of the column to be analysed.
#' @importFrom dplyr select mutate ungroup all_of
#' @importFrom tibble add_row
#' @importFrom stringr str_replace_all str_to_title str_trim str_sub
#' @importFrom survival Surv survdiff
#' @return A tibble with frequency and survival information formatted for tables.
#' @examples
#' tab_desc(lung, "sex")
#' @export

tab_desc <- function(df, column){
  data <- df %>% select(tempos, censura, .y. = all_of(column))
  
  # separate execution based on variable type
  if(class(data$.y.) %in% c('integer', 'numeric')){
    
    # numeric variable ----
    
    data <- tab_desc_num(data, column) %>%
      mutate(highlight = 'J2') %>% ungroup %>%
      # add dividing line to organize the table and highlight the
      # name of the variable being analysed
      add_row(.y. = paste0('[', str_replace_all(stringr::str_to_title(column),
                                                '_', ' '), ']'),
              # variable highlight serves for styling the table via
              # kableExtra::row_spec, for instance to mark numeric or
              # categorical variables with different colors
              highlight = 'J1',
              ., .before = 1, frequency_col = msdr(data$.y.))
  }else{
    
    # categorical variable ----
    # show which categorical levels exist for the variable

    exemplo_niveis <- paste('Levels:', paste(sort(unique(data$.y.)),
                                             collapse = ', '))
    # limit length of the string to avoid breaking the table layout
    if(nchar(exemplo_niveis) > 20){
      exemplo_niveis <- paste0(str_trim(str_sub(exemplo_niveis, end = 20)), '...')}

    try_error <- class(try(
      survdiff(data = data, Surv(data$tempos, data$censura)~.y.)[["pvalue"]],
      silent = TRUE))
    if(try_error=='try-error'){
      p_value <- 1}else{
        p_value <- survdiff(data = data,Surv(data$tempos, data$censura)~.y.)[["pvalue"]]}
    
    # add dividing line to organize the table
    data <- tab_desc_fac(data) %>%
      mutate(highlight = 'J2') %>% ungroup %>%
      add_row(.y. = paste0('[', str_replace_all(stringr::str_to_title(column),
                                                '_', ' '), ']'),
              ., .before = 1, highlight = 'J1',
              frequency_col = exemplo_niveis,
              p = as.character(format_sig(p_value)),
              test = '(Logrank)')
  }
  data %>% add_row(.after = nrow(data), highlight = '.')
}

