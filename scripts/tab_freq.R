#' Frequency table for a categorical column
#'
#' Calculates counts and percentages of each distinct value in `.y.`.
#'
#' @param aux Data frame containing the column `.y.`.
#' @return Data frame with absolute and relative frequencies.
#'
#' @examples
#' df <- data.frame(.y. = c('A', 'B', 'A', 'C'))
#' tab_freq(df)
 tab_freq <- function(aux){
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
