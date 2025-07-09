#!/usr/bin/env Rscript

# Test script for all functions in SurvInsights

library(dplyr)
library(stringr)
library(tibble)
library(survival)

# Load package functions
source('scripts/sd1.R')
source('scripts/msdr_y.R')
source('scripts/tab_freq.R')
source('scripts/tab_desc_fac.R')
source('scripts/tab_desc_num.R')
source('scripts/tab_desc.R')

# Example dataset

test_df <- tibble(
  tempos = c(5, 10, 7, 12, 20, 3),
  censura = c(1, 0, 1, 1, 0, 1),
  age = c(70, 45, 63, 50, 80, 58),
  group = c('A', 'B', 'A', 'B', 'A', 'B')
)

cat('Testing sd1:\n')
print(sd1(test_df$age))

cat('\nTesting msdr_y:\n')
print(msdr_y(test_df %>% select(tempos, censura, .y. = group)))

cat('\nTesting tab_freq:\n')
print(tab_freq(test_df %>% select(.y. = group)))

cat('\nTesting tab_desc_num:\n')
print(tab_desc_num(test_df %>% select(tempos, censura, .y. = age), 'age'))

cat('\nTesting tab_desc_fac:\n')
print(tab_desc_fac(test_df %>% select(tempos, censura, .y. = group)))

cat('\nTesting tab_desc for numeric variable:\n')
print(tab_desc(test_df, 'age'))

cat('\nTesting tab_desc for categorical variable:\n')
print(tab_desc(test_df, 'group'))
