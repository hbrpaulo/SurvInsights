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
      mutate(coloracao = 'J2') %>% ungroup %>%
      # adiciona linha divisoria para organizacao da tabela
      # destacando o nome da variavel que esta sendo analisada
      add_row(.y. = paste0('[', str_replace_all(stringr::str_to_title(column),
                                                '_', ' '), ']'),
              # variavel coloracao servira para adicionar estetica a tabela atraves do 
              # comando row_spec, por exemplo para destacar por cores se a column se trata
              # de variaveis numericas, categoricas, etc
              coloracao = 'J1',
              ., .before = 1, group1 = msdr(aux$.y.))
  }else{
    
    # var categorica ----
    
    # mostrar quais niveis categoricas existentem para a var cat
    exemplo_niveis <- paste('Levels:', paste(sort(unique(aux$.y.)),
                                             collapse = ', '))
    # limita o tamanho do vetor para nao desconfigurar a tabela
    if(str_count(exemplo_niveis)>10){
      exemplo_niveis <- paste0(str_trim(str_sub(exemplo_niveis,  end = 20)), '...')}
    
    se_erro <- class(try(
      survdiff(data = aux, Surv(aux$tempos, aux$censura)~.y.)[["pvalue"]],
      silent = TRUE))
    if(se_erro=='try-error'){
      p_value <- 1}else{
        p_value <- survdiff(data = aux,Surv(aux$tempos, aux$censura)~.y.)[["pvalue"]]}
    
    # adiciona linha divisoria para organizacao da tabela
    aux <- tab_desc_fac(aux) %>% 
      mutate(coloracao = 'J2') %>% ungroup %>%
      add_row(.y. = paste0('[', str_replace_all(stringr::str_to_title(column),
                                                '_', ' '), ']'),
              ., .before = 1, coloracao = 'J1',
              group1 = exemplo_niveis,
              p = as.character(format_sig(p_value)),
              test = '(Logrank)')
  }
  aux %>% add_row(.after = nrow(aux), coloracao = '.')
}
