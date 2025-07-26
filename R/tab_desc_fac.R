#' Combine frequency and survival summaries for factors
#'
#' @description Helper for `tab_desc` used when the analysed variable is categorical.
#' @param aux Data frame with columns `tempos`, `censura` and `.y.`.
#' @return Tibble with counts and mean survival for each level.
#' @examples
#' tab_desc_fac(tibble::tibble(tempos=1:2, censura=c(1,0), .y.=c('A','B')))
#' @export
 
tab_desc_fac <- function(aux){
  ungroup(full_join(tab_freq(aux), 
                    msdr_y(aux), by = join_by(.y.)))
}

