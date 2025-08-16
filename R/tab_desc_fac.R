#' Combine frequency and survival summaries for factors
#'
#' @description Helper for `tab_desc` used when the analysed variable is categorical.
#' @param data Data frame with time, event and `.y.`.
#' @param time_col Name of the time-to-event column.
#' @param event_col Name of the event indicator column.
#' @importFrom dplyr ungroup full_join join_by
#' @return Tibble with counts and mean survival for each level.
#' @examples
#' tab_desc_fac(tibble::tibble(tempos=1:2, censura=c(1,0), .y.=c('A','B')))
#' df2 <- tibble::tibble(tempo=1:2, evento=c(1,0), .y.=c('A','B'))
#' tab_desc_fac(df2, time_col = "tempo", event_col = "evento")
#' @export
 
tab_desc_fac <- function(data, time_col = "tempos", event_col = "censura"){
  ungroup(full_join(tab_freq(data),
                    msdr_y(data, time_col = time_col, event_col = event_col),
                    by = join_by(.y.)))
}

