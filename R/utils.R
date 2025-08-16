#' Build a formatted summary string
#'
#' @description Internal helper to compose "mean\u00B1sd (li~ls)" strings.
#' @param mean Mean value.
#' @param sd Standard deviation.
#' @param li Lower limit of the range.
#' @param ls Upper limit of the range.
#' @return Character string with the formatted summary.
#' @noRd
format_msdr <- function(mean, sd, li, ls) {
  paste0(mean, '\u00B1', sd, ' (', li, '~', ls, ')')
}

#' Numeric summary string
#'
#' @description Format the mean, standard deviation and range of a numeric vector.
#' @param x Numeric vector to summarise.
#' @param k Number of decimal places to round to.
#' @return A character string in the form "mean plus and minus sd (min~max)".
#' @examples
#' msdr(c(1, 2, 3))
#' @export
msdr <- function(x, k = 2) {
  x <- as.numeric(na.omit(x))
  format_msdr(
    round(mean(x), k),
    round(sd1(x), k),
    round(min(x), k),
    round(max(x), k)
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
  x <- as.numeric(format(round(x, digits = k), scientific = FALSE))
  star <- stars[findInterval(as.numeric(x), thresholds, left.open = TRUE) + 1]
  text <- ifelse(x!=0, paste0(x, star), '~0***')
}

