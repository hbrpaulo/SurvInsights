library(SurvInsights)
library(dplyr)

test_that("column_classifier identifies basic column types", {
  df <- data.frame(
    num = c(1, 2, 3),
    fac = factor(c("a", "b", "a")),
    char = c("x", "y", "z")
  )

  result <- column_classifier(df)

  expected <- data.frame(
    column = c("char", "fac", "num"),
    type = c("character", "factor", "numeric"),
    stringsAsFactors = FALSE
  )

  expect_equal(result, expected)
})
