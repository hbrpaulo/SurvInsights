msdr <- function(x, k = 2) {
  x <- as.numeric(na.omit(x))
  paste0(
    round(mean(x), k), "±",
    round(sd1(x), k), " (",
    paste0(round(min(x), k), '~', round(max(x), k), ")")
  )
}

format_sig <- function(x, k = 3, thresholds = c(0.001, 0.05, 0.1),
                       stars = c("***", "**", "*", "")) {
  if (x < 0) message('Negative values should not be used in this function')
  x <- format(round(x, digits = k), scientific = FALSE)
  star <- stars[findInterval(as.numeric(x), thresholds, left.open = TRUE) + 1]
  paste0(x, star)
}
