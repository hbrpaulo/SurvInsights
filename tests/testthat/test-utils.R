library(SurvInsights)

test_that("msdr formats numeric summary correctly", {
  expect_equal(msdr(c(1,2,3)), "2\u00B11 (1~3)")
})

test_that("format_sig adds significance stars", {
  expect_equal(format_sig(0.0007), "0.001***")
  expect_equal(format_sig(0.02), "0.02**")
  expect_equal(format_sig(0.06), "0.06*")
  expect_equal(format_sig(0.2), "0.2")
})
