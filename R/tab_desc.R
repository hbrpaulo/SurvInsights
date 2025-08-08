#' Descriptive statistics for one column
#'
#' @description Summarise a variable with counts and survival metrics.
#' @param df Data frame containing time, event and the target variable.
#' @param column Name of the column to be analysed.
#' @param time_col Name of the time-to-event column.
#' @param event_col Name of the event indicator column.
#' @importFrom dplyr select mutate ungroup all_of
#' @importFrom tibble add_row
#' @importFrom stringr str_replace_all str_to_title str_trim str_sub
#' @importFrom survival Surv survdiff
#' @return A tibble with frequency and survival information formatted for tables.
#' @examples
#' tab_desc(lung, "sex")
#' df2 <- dplyr::rename(lung, tempo = time, evento = status)
#' tab_desc(df2, "sex", time_col = "tempo", event_col = "evento")
#' @export

tab_desc <- function(df, column, time_col = "tempos", event_col = "censura"){
  time_col_sym <- rlang::sym(time_col)
  event_col_sym <- rlang::sym(event_col)
  data <- df %>% select({{time_col_sym}}, {{event_col_sym}}, .y. = all_of(column))
  
  # separate execution based on variable type
  if(class(data$.y.) %in% c('integer', 'numeric')){
    
    # numeric variable ----
    
    data <- tab_desc_num(data, column, time_col = time_col, event_col = event_col) %>%
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
      survdiff(data = data,
               Surv(data[[time_col]], data[[event_col]])~.y.)[["pvalue"]],
      silent = TRUE))
    if(try_error=='try-error'){
      p_value <- 1}else{
        p_value <- survdiff(data = data,
                            Surv(data[[time_col]], data[[event_col]])~.y.)[["pvalue"]]}
    
    # add dividing line to organize the table
    data <- tab_desc_fac(data, time_col = time_col, event_col = event_col) %>%
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

