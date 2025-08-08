#' Frequency table
#'
#' @description Create a simple frequency table for the column `.y.` of a data frame.
#' @param aux Data frame containing a `.y.` column.
#' @importFrom dplyr select mutate group_by n distinct if_else arrange ungroup
#' @importFrom stringr str_replace_all fixed
#' @return Tibble with counts and percentages for each level.
#' @examples
#' tab_freq(tibble::tibble(.y. = c('A','B','A')))
#' @export

tab_freq <- function(aux){
  # aux <- df %>%
  #   select(tempos, censura, .y. = all_of(column))
  aux %>% 
    select(.y.) %>% 
    mutate(n_total = n()) %>%
    group_by(.y.) %>%
    mutate(ni = n()) %>%
    distinct %>%
    mutate(group1 = paste0(ni, ' (', round(ni/n_total*100, 2), '%)'),
           group1 = str_replace_all(group1, pattern = fixed('.'),replacement = ','),
           .y. = if_else(is.na(.y.), 'xNA', .y.),
           p = ' ',
           test = ' ') %>% 
    select(-n_total, -ni) %>%
    arrange(.y.) %>% 
    ungroup
}

