#' @description Unify results for both numeric and categorical variables
#' @param aux Data frame with survival data
#' @param column Name of the column to be analyzed
#' @return A tibble with frequency and survival statistics

tab_desc <- function(df, column){
  aux <- df %>% select(tempos, censura, .y. = all_of(column))
  
  # separar execucao entre var numericas e demais
  if(class(aux$.y.) %in% c('integer', 'numeric')){
    
    # var numerica ----
    
    aux <- tab_desc_num(aux, column) %>%
      mutate(highlight = 'J2') %>% ungroup %>%
      # adiciona linha divisoria para organizacao da tabela
      # destacando o nome da variavel que esta sendo analisada
      add_row(.y. = paste0('[', str_replace_all(stringr::str_to_title(column),
                                                '_', ' '), ']'),
              # variable highlight serves for styling the table via
              # kableExtra::row_spec, for instance to mark numeric or
              # categorical variables with different colors
              highlight = 'J1',
              ., .before = 1, group1 = msdr(aux$.y.))
  }else{
    
    # var categorica ----
    
    # mostrar quais niveis categoricas existentem para a var cat
    example_levels <- paste('Niveis:', paste(sort(unique(aux$.y.)),
                                             collapse = ', '))
    # limita o tamanho do vetor para nao desconfigurar a tabela
    if(str_count(example_levels)>10){
      example_levels <- paste0(str_trim(str_sub(example_levels,  end = 20)), '...')}

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
              group1 = example_levels,
              p = as.character(format_sig(p_value)),
              test = '(Logrank)')
  }
  aux %>% add_row(.after = nrow(aux), highlight = '.')
}
