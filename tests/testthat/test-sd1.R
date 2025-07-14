library(SurvInsights)

test_that("sd1 computes sd or zero", {
  expect_equal(sd1(c(1, 2, 3, 4)), sd(c(1, 2, 3, 4)))
  expect_equal(sd1(1), 0)
})
