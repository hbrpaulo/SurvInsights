library(SurvInsights)

test_df <- tibble::tibble(
  tempos = c(5, 10, 7, 12, 20, 3),
  censura = c(1, 0, 1, 1, 0, 1),
  age = c(70, 45, 63, 50, 80, 58),
  group = c('A', 'B', 'A', 'B', 'A', 'B')
)

metrics <- compute_variable_metrics(test_df, c('age', 'group'))

tbl <- assemble_metrics_table(metrics)

test_that('assemble_metrics_table binds metrics into a tibble', {
  expect_s3_class(tbl, 'tbl_df')
  expect_true('variable' %in% names(tbl))
  expect_equal(sort(unique(tbl$variable)), c('age', 'group'))
})

