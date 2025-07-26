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

## Sample datasets

The `data/ics_archive/` folder contains excerpts from the dataset used in
the study *"Survival prediction of patients with sepsis from age, sex, and
septic episode number alone"* (doi:[10.1038/s41598-020-73558-3](https://doi.org/10.1038/s41598-020-73558-3)).
Only a few variables are kept to keep the files small and suitable for
examples.
### Example CSV files

Minimal clinical records stored in `data/ics_archive/` are shipped solely to illustrate the package functions. They contain the variables `age_years`, `sex_0male_1female`, `episode_number` and `hospital_outcome_1alive_0dead`. See [data/ics_archive/README.md](data/ics_archive/README.md) for more details.


## Reproducing the example analysis

1. Install the required packages:

   ```r
   install.packages(c("kableExtra", "tidyverse", "survival"))
   ```

2. Source the helper scripts and render the R Markdown document:

   ```r
   rmarkdown::render("skeleton.Rmd")
   ```

   The rendered HTML will contain the descriptive tables shown in the
   examples.