library(SurvInsights)

test_df <- tibble::tibble(
  tempos = c(5, 10, 7, 12, 20, 3),
  censura = c(1, 0, 1, 1, 0, 1),
  age = c(70, 45, 63, 50, 80, 58),
  group = c('A', 'B', 'A', 'B', 'A', 'B')
)

res <- compute_variable_metrics(test_df, c('age', 'group'))

test_that('compute_variable_metrics returns a named list of tibbles', {
  expect_named(res, c('age', 'group'))
  expect_true(all(vapply(res, tibble::is_tibble, logical(1))))
})

