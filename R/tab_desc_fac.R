#' Combine frequency and survival summaries for factors
#'
#' @description Helper for `tab_desc` used when the analysed variable is categorical.
#' @param data Data frame with columns `tempos`, `censura` and `.y.`.
#' @importFrom dplyr ungroup full_join join_by
#' @return Tibble with counts and mean survival for each level.
#' @examples
#' tab_desc_fac(tibble::tibble(tempos=1:2, censura=c(1,0), .y.=c('A','B')))
#' @export
 
tab_desc_fac <- function(data){
  ungroup(full_join(tab_freq(data),
                    msdr_y(data), by = join_by(.y.)))
}

