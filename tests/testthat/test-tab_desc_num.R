library(SurvInsights)

sample_df <- tibble::tibble(
  tempos = c(5,10,7,12,20,3),
  censura = c(1,0,1,1,0,1),
  age = c(70,45,63,50,80,58)
)
aux <- sample_df %>% dplyr::select(tempos, censura, .y.=age)

res <- tab_desc_num(aux, 'age', test = 'Cox PH')
fit <- survival::coxph(data = aux, survival::Surv(aux$tempos, aux$censura)~.y.)
expected <- tibble::tibble(
  .y. = 'Regression coefficient',
  frequency_col = NA,
  summary_text = as.character(round(exp(coef(fit)), 4)),
  p = format_sig(summary(fit)[["sctest"]][["pvalue"]]),
  test = 'Cox PH'
)

test_that("tab_desc_num fits cox model", {
  expect_equal(res, expected)
})
