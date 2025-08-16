library(SurvInsights)

test_df <- tibble::tibble(.y. = c('A','B','A',NA))

res <- tab_freq(test_df)

expected <- tibble::tibble(
  .y. = c('A','B','xNA'),
  frequency_col = c('2 (50%)','1 (25%)','1 (25%)'),
  p = ' ',
  test = ' '
)

test_that("tab_freq computes counts and percentages", {
  expect_equal(res, expected)
})
