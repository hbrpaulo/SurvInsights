library(SurvInsights)

test_df <- tibble::tibble(
  tempos = c(5,10,7,12,20,3),
  censura = c(1,0,1,1,0,1),
  age = c(70,45,63,50,80,58),
  group = c('A','B','A','B','A','B')
)

num_res <- tab_desc(test_df, 'age')
cat_res <- tab_desc(test_df, 'group')


test_that('tab_desc works for numeric variables', {
  expect_true('[Age]' %in% num_res$.y.)
  expect_true('Regression coefficient' %in% num_res$.y.)
})


test_that('tab_desc works for categorical variables', {
  expect_true('[Group]' %in% cat_res$.y.)
  expect_true(all(sort(unique(test_df$group)) %in% cat_res$.y.))
})
