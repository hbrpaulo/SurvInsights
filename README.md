# SurvInsights

**SurvInsights** is an R package designed to generate smart descriptive tables tailored for survival analysis.

## Features

- Tables for numeric and categorical covariates,
- Survival summaries via Kaplan-Meier, Cox regression, etc.,
- Descriptive statistics with means, standard errors, confidence intervals, and p-values.

## Installation

```r
# Install from GitHub (coming ~not so~ soon)
# devtools::install_github("hbrpaulo/SurvInsights")
```

## Quick Start

```r
library(SurvInsights)
library(survival)
library(dplyr)

df <- survival::lung |>
  select(tempos = time, censura = status, age, sex)

# default column names
tab_desc(df, "age")
#> # A tibble: 3 × 5
#>   .y.                    frequency_col summary_text p     test
#>   <chr>                  <chr>         <chr>        <chr> <chr>
#> 1 [Age]                  62.0          NA           NA    NA
#> 2 Regression coefficient NA            1.01         0.45  (Cox PH)
#> 3 .                      NA            NA           NA    NA

complete_tab(df)
#> Renders a scrolling HTML table summarising all columns

# using alternative column names
df_alt <- survival::lung |> select(tempo = time, evento = status, age, sex)
tab_desc(df_alt, "age", time_col = "tempo", event_col = "evento")
```

## Sample datasets

The `input/ics_archive/` folder contains excerpts from the dataset used in
the study *"Survival prediction of patients with sepsis from age, sex, and
septic episode number alone"* (doi:[10.1038/s41598-020-73558-3](https://doi.org/10.1038/s41598-020-73558-3)).
Only a few variables are kept to keep the files small and suitable for
examples.

## Reproducing the example analysis

1. Install the required packages:

   ```r
   install.packages(c("kableExtra", "tidyverse", "survival"))
   ```

2. Source the helper scripts and render the R Markdown document:

   ```r
   rmarkdown::render("usage_examples/legacy/skeleton.Rmd")
   ```

   The rendered HTML will contain the descriptive tables shown in the
   examples.

These tables provide quick summaries of both numeric and categorical
variables. Survival estimates are reported alongside descriptive
statistics so you can immediately assess important trends. The helper
functions `tab_desc_num()`, `tab_desc_fac()`, and `tab_freq()` generate
the results, which are formatted for publication with `kableExtra`.

