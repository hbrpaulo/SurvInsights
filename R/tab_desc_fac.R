#' @description Unify results for categorical variables
#' @param aux Data frame with survival data
#' @return A tibble with frequency and survival statistics
 
tab_desc_fac <- function(aux){
  ungroup(full_join(tab_freq(aux), 
                    msdr_y(aux), by = join_by(.y.)))
}