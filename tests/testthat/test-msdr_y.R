library(SurvInsights)
library(survival)
library(tidyverse)

sample_df <- tibble::tibble(
  tempos = c(5, 10, 7, 12, 20, 3),
  censura = c(1, 0, 1, 1, 0, 1),
  group = c('A', 'B', 'A', 'B', 'A', 'B')
)

sample_df2 <- sample_df %>% dplyr::rename(tempo = tempos, evento = censura)

res <- msdr_y(sample_df %>% dplyr::select(tempos, censura, .y. = group), k = 2)
res2 <- msdr_y(sample_df2 %>% dplyr::select(tempo, evento, .y. = group),
               k = 2, time_col = 'tempo', event_col = 'evento')

fit <- survival::survfit(survival::Surv(tempos, censura) ~ group, data = sample_df)
fit_sum <- data.frame(summary(fit)$table)
expected <- tibble::tibble(
  .y. = c('A','B'),
  summary_text = c(
    SurvInsights:::format_msdr(
      round(fit_sum$rmean[1], 2),
      round(fit_sum$se.rmean[1], 2),
      round(fit_sum$X0.95LCL[1], 2),
      round(fit_sum$X0.95UCL[1], 2)
    ),
    SurvInsights:::format_msdr(
      round(fit_sum$rmean[2], 2),
      round(fit_sum$se.rmean[2], 2),
      round(fit_sum$X0.95LCL[2], 2),
      round(fit_sum$X0.95UCL[2], 2)
    )
  )
)


test_that("msdr_y summarises groups correctly", {
  expect_equal(res$.y., expected$.y.)
  expect_equal(res$summary_text, expected$summary_text)
})

test_that("msdr_y accepts custom column names", {
  expect_equal(res2$.y., expected$.y.)
  expect_equal(res2$summary_text, expected$summary_text)
})
