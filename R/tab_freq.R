#' Frequency table
#'
#' @description Create a simple frequency table for the column `.y.` of a data frame.
#' @param data Data frame containing a `.y.` column.
#' @importFrom dplyr select mutate group_by n distinct if_else arrange ungroup
#' @importFrom stringr str_replace_all fixed
#' @return Tibble with counts and percentages for each level.
#' @examples
#' tab_freq(tibble::tibble(.y. = c('A','B','A')))
#' @export

tab_freq <- function(data){
  # data <- df %>%
  #   select(tempos, censura, .y. = all_of(column))
  data %>%
    select(.y.) %>%
    mutate(n_total = n()) %>%
    group_by(.y.) %>%
    mutate(ni = n()) %>%
    distinct %>%
    mutate(frequency_col = paste0(ni, ' (', round(ni/n_total*100, 2), '%)'),
           frequency_col = str_replace_all(frequency_col, pattern = fixed('.'),replacement = ','),
           .y. = if_else(is.na(.y.), 'xNA', .y.),
           p = ' ',
           test = ' ') %>%
    select(-n_total, -ni) %>%
    arrange(.y.) %>% 
    ungroup
}

