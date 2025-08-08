#' Assemble variable metrics into a single table
#'
#' @param metrics_list Named list of tibbles created by `compute_variable_metrics()`.
#'
#' @return Tibble combining all metrics with a `variable` column identifying the source.
#' @export
#'
#' @examples
#' metrics <- compute_variable_metrics(lung, c("age", "sex"))
#' assemble_metrics_table(metrics)
assemble_metrics_table <- function(metrics_list) {
  dplyr::bind_rows(metrics_list, .id = "variable")
}

