library(SurvInsights)

sample_df <- tibble::tibble(
  tempos = c(5, 10, 7, 12, 20, 3),
  censura = c(1, 0, 1, 1, 0, 1),
  group = c('A', 'B', 'A', 'B', 'A', 'B')
)

res <- msdr_y(sample_df %>% dplyr::select(tempos, censura, .y. = group), k = 2)

fit <- survival::survfit(survival::Surv(tempos, censura) ~ group, data = sample_df)
fit_sum <- data.frame(summary(fit)$table)
expected <- tibble::tibble(
  .y. = c('A','B'),
  group2 = c(
    paste0(round(fit_sum$rmean[1],2),'±',round(fit_sum$se.rmean[1],2),' (',round(fit_sum$X0.95LCL[1],2),'~',round(fit_sum$X0.95UCL[1],2),')'),
    paste0(round(fit_sum$rmean[2],2),'±',round(fit_sum$se.rmean[2],2),' (',round(fit_sum$X0.95LCL[2],2),'~',round(fit_sum$X0.95UCL[2],2),')')
  )
)


test_that("msdr_y summarises groups correctly", {
  expect_equal(res$.y., expected$.y.)
  expect_equal(res$group2, expected$group2)
})
