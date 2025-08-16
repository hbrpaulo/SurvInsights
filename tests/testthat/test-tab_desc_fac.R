library(SurvInsights)

sample_df <- tibble::tibble(
  tempos = c(5,10,7,12,20,3),
  censura = c(1,0,1,1,0,1),
  group = c('A','B','A','B','A','B')
)
aux <- sample_df %>% dplyr::select(tempos, censura, .y.=group)
sample_df2 <- sample_df %>% dplyr::rename(tempo = tempos, evento = censura)
aux2 <- sample_df2 %>% dplyr::select(tempo, evento, .y.=group)

res <- tab_desc_fac(aux)
expected <- dplyr::ungroup(dplyr::full_join(tab_freq(aux), msdr_y(aux), by = dplyr::join_by(.y.)))
res2 <- tab_desc_fac(aux2, time_col = 'tempo', event_col = 'evento')
expected2 <- dplyr::ungroup(dplyr::full_join(tab_freq(aux2),
                                             msdr_y(aux2, time_col = 'tempo', event_col = 'evento'),
                                             by = dplyr::join_by(.y.)))

test_that("tab_desc_fac combines freq and msdr_y", {
  expect_equal(res, expected)
})

test_that("tab_desc_fac handles custom columns", {
  expect_equal(res2, expected2)
})
