#' Numeric summary string
#'
#' @description Format the mean, standard deviation and range of a numeric vector.
#' @param x Numeric vector to summarise.
#' @param k Number of decimal places to round to.
#' @return A character string in the form "mean±sd (min~max)".
#' @examples
#' msdr(c(1, 2, 3))
#' @export
msdr <- function(x, k = 2) {
  x <- as.numeric(na.omit(x))
  paste0(
    round(mean(x), k), "±",
    round(sd1(x), k), " (",
    paste0(round(min(x), k), '~', round(max(x), k), ")")
  )
}

#' Format p-values with significance stars
#'
#' @description Add significance stars to a numeric value according to provided thresholds.
#' @param x Numeric p-value.
#' @param k Number of decimal places to round.
#' @param thresholds Numeric vector of significance limits.
#' @param stars Character vector of symbols corresponding to each threshold.
#' @return Character string of the rounded p-value and stars.
#' @examples
#' format_sig(0.02)
#' @export
format_sig <- function(x, k = 3, thresholds = c(0.001, 0.05, 0.1),
                       stars = c("***", "**", "*", "")) {
  if (x < 0) message('Negative values should not be used in this function')
  x <- format(round(x, digits = k), scientific = FALSE)
  star <- stars[findInterval(as.numeric(x), thresholds, left.open = TRUE) + 1]
  paste0(x, star)
}

