#' Descriptive summary for a factor
#'
#' Combines frequency counts with mean survival time.
#'
#' @param aux Data frame with `tempos`, `censura` and `.y.`.
#' @return Tibble with counts and restricted mean survival time by level.
#'
#' @examples
#' df <- tibble::tibble(tempos = c(5,10,7),
#'                      censura = c(1,0,1),
#'                      group = c('A','B','A'))
#' tab_desc_fac(dplyr::select(df, tempos, censura, .y. = group))
 tab_desc_fac <- function(aux){
   ungroup(full_join(tab_freq(aux),
                     msdr_y(aux), by = join_by(.y.)))
 }
