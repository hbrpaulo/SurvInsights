#' Compute descriptive metrics for multiple variables
#'
#' @param aux Data frame containing `tempos`, `censura` and variables of interest.
#' @param cols Character vector with column names to summarise.
#'
#' @return Named list of tibbles with metrics for each variable.
#' @export
#'
#' @examples
#' compute_variable_metrics(lung, c("age", "sex"))
compute_variable_metrics <- function(aux, cols) {
  setNames(lapply(cols, function(col) tab_desc(aux, col)), cols)
}

