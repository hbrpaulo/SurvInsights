#' @description This function creates a frequency table for a specified column in a data frame.
#' @param aux A data frame containing the column to be analyzed
#' @param .y. The name of the column to be analyzed (as a string)
#' @return A data frame with the frequency of each unique value in the specified column, including the percentage of total occurrences.
#' @examples
# df <- data.frame(.y. = c('short', 'medium', 'long', 'short', 'medium'),
#                  aux = c('yes', 'no', 'yes', 'no', 'yes'))
# tab_freq(df)

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
