library(SurvInsights)

test_that('complete_tab returns knitr_kable with expected columns', {
  df <- tibble::tibble(
    tempos = c(1, 2),
    censura = c(1, 0),
    age = c(60, 70)
  )
  df2 <- dplyr::rename(df, tempo = tempos, evento = censura)

  res <- complete_tab(df)
  res2 <- complete_tab(df2, time_col = 'tempo', event_col = 'evento')
  expect_s3_class(res, 'knitr_kable')
  expect_s3_class(res2, 'knitr_kable')
  expect_equal(attr(res, 'kable_meta')$col_names,
               c('.y.', 'group1', 'group2', 'p', 'test', 'highlight'))
  expect_equal(attr(res2, 'kable_meta')$col_names,
               c('.y.', 'group1', 'group2', 'p', 'test', 'highlight'))
})
