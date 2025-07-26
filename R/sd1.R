#' Safe standard deviation
#'
#' @description Compute the standard deviation of a numeric vector returning 0 when length is less than two.
#' @param x Numeric vector.
#' @return Numeric standard deviation or 0.
#' @examples
#' sd1(c(1, 2, 3))
#' @export
sd1 <- function(x){
  ifelse(length(x) < 2, 0, sd(x))
}

