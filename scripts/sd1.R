#' Calculate standard deviation with a length check
#'
#' Returns `0` when the input has fewer than two observations.
#'
#' @param x Numeric vector.
#' @return Standard deviation of `x` or `0`.
#' @examples
#' sd1(c(1, 2, 3))
#' sd1(1)  # returns 0
sd1 <- function(x){
  ifelse(length(x) < 2, 0, sd(x))
}
