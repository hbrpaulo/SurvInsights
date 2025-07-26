library(SurvInsights)

sample_df <- tibble::tibble(
  tempos = c(5,10,7,12,20,3),
  censura = c(1,0,1,1,0,1),
  group = c('A','B','A','B','A','B')
)
aux <- sample_df %>% dplyr::select(tempos, censura, .y.=group)

res <- tab_desc_fac(aux)
expected <- dplyr::ungroup(dplyr::full_join(tab_freq(aux), msdr_y(aux), by = dplyr::join_by(.y.)))

test_that("tab_desc_fac combines freq and msdr_y", {
  expect_equal(res, expected)
})
