#' Unified descriptive table
#'
#' Produces a summary for either numeric or categorical variables
#' in a survival data set.
#'
#' @param df Data frame with columns `tempos` and `censura`.
#' @param column Name of the variable to analyse.
#' @return Tibble ready for display with kableExtra.
#'
#' @examples
#' df <- tibble::tibble(
#'   tempos = c(5,10,7),
#'   censura = c(1,0,1),
#'   age = c(70,45,63),
#'   group = c('A','B','A'))
#' tab_desc(df, 'group')
 tab_desc <- function(df, column){
   aux <- df %>% select(tempos, censura, .y. = all_of(column))

   if(class(aux$.y.) %in% c('integer', 'numeric')){

     aux <- tab_desc_num(aux, column) %>%
       mutate(highlight = 'J2') %>% ungroup %>%
       add_row(
         .y. = paste0('[', str_replace_all(stringr::str_to_title(column), '_', ' '), ']'),
         highlight = 'J1',
         ., .before = 1, group1 = msdr(aux$.y.)
       )
   }else{
     example_levels <- paste('Levels:', paste(sort(unique(aux$.y.)), collapse = ', '))
     if(str_count(example_levels)>10){
       example_levels <- paste0(str_trim(str_sub(example_levels, end = 20)), '...')
     }
     try_error <- class(try(survdiff(data = aux, Surv(aux$tempos, aux$censura)~.y.)[["pvalue"]], silent = TRUE))
     if(try_error=='try-error'){
       p_value <- 1
     }else{
       p_value <- survdiff(data = aux,Surv(aux$tempos, aux$censura)~.y.)[["pvalue"]]
     }
     aux <- tab_desc_fac(aux) %>%
       mutate(highlight = 'J2') %>% ungroup %>%
       add_row(
         .y. = paste0('[', str_replace_all(stringr::str_to_title(column), '_', ' '), ']'),
         ., .before = 1, highlight = 'J1',
         group1 = example_levels,
         p = as.character(format_sig(p_value)),
         test = '(Logrank)'
       )
   }
   aux %>% add_row(.after = nrow(aux), highlight = '.')
 }
