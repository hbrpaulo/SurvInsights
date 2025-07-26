#' @description Unify results for both numeric and categorical variables
#' @param aux Data frame with survival data
#' @param column Name of the column to be analyzed
#' @return A tibble with frequency and survival statistics
#' @examples
#' tab_desc(lung, 'sex')

tab_desc <- function(df, column){
  aux <- df %>% select(tempos, censura, .y. = all_of(column))
  
  # separate execution based on variable type
  if(class(aux$.y.) %in% c('integer', 'numeric')){
    
    # numeric variable ----
    
    aux <- tab_desc_num(aux, column) %>%
      mutate(highlight = 'J2') %>% ungroup %>%
      # add dividing line to organize the table and highlight the
      # name of the variable being analysed
      add_row(.y. = paste0('[', str_replace_all(stringr::str_to_title(column),
                                                '_', ' '), ']'),
              # variable highlight serves for styling the table via
              # kableExtra::row_spec, for instance to mark numeric or
              # categorical variables with different colors
              highlight = 'J1',
              ., .before = 1, group1 = msdr(aux$.y.))
  }else{
    
    # categorical variable ----
    # show which categorical levels exist for the variable

    exemplo_niveis <- paste('Levels:', paste(sort(unique(aux$.y.)),
                                             collapse = ', '))
    # limit length of the string to avoid breaking the table layout
    if(nchar(exemplo_niveis) > 20){
      exemplo_niveis <- paste0(str_trim(str_sub(exemplo_niveis, end = 20)), '...')}

    try_error <- class(try(
      survdiff(data = aux, Surv(aux$tempos, aux$censura)~.y.)[["pvalue"]],
      silent = TRUE))
    if(try_error=='try-error'){
      p_value <- 1}else{
        p_value <- survdiff(data = aux,Surv(aux$tempos, aux$censura)~.y.)[["pvalue"]]}
    
    # adiciona linha divisoria para organizacao da tabela
    aux <- tab_desc_fac(aux) %>%
      mutate(highlight = 'J2') %>% ungroup %>%
      add_row(.y. = paste0('[', str_replace_all(stringr::str_to_title(column),
                                                '_', ' '), ']'),
              ., .before = 1, highlight = 'J1',
              group1 = exemplo_niveis,
              p = as.character(format_sig(p_value)),
              test = '(Logrank)')
  }
  aux %>% add_row(.after = nrow(aux), highlight = '.')
}
