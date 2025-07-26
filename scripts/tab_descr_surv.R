#' Script to build a descriptive survival table for all variables in ``df``.
#'
#' This example orders the variables by type (numeric first) and then applies
#' ``tab_desc`` to each of them. The resulting table has clickable references
#' for the tests used.

library(dplyr)
library(tibble)

## Collect all covariate names except the censoring indicator
vars <- sort(setdiff(names(df), c("censura", "tempos")))

## Order variables so that numeric ones appear first
get_class <- function(col) class(dplyr::pull(df, col))
vars <- tibble(variable = vars, class = sapply(vars, get_class)) %>%
  arrange(class) %>%
  pull(variable)

## Example loop for visual inspection of each variable
# for (v in vars) {
#   print(v)
#   tab_desc(df, v) %>% print()
#   Sys.sleep(0.1)
# }

link_logrank <- "https://en.wikipedia.org/wiki/Logrank_test"
results_table <- do.call(rbind, lapply(vars, function(x) tab_desc(df, x))) %>%
  mutate(test = case_when(
    test == "(Logrank)" ~ kableExtra::cell_spec(test, link = link_logrank),
    TRUE ~ test
  ))

## TODO:
# - Add p-values
# - Apply styling
# - Consider handling censoring summary

## General notes
# The log-rank formula fails when variable names contain spaces:
# survdiff(data = df, Surv(df$tempos, df$censura) ~ `binary variable`)
# A workaround is janitor::clean_names()
