library(SurvInsights)

test_that('complete_tab returns knitr_kable with expected columns', {
  df <- tibble::tibble(
    tempos = c(1, 2),
    censura = c(1, 0),
    age = c(60, 70)
  )

  res <- complete_tab(df)
  expect_s3_class(res, 'knitr_kable')
  expect_equal(attr(res, 'kable_meta')$col_names,
               c('.y.', 'group1', 'group2', 'p', 'test', 'highlight'))
})
