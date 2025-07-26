The CSV files in this directory are excerpts of the "Sepsis Survival Minimal Clinical Records" dataset from the [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Sepsis+Survival+Minimal+Clinical+Records). These cohorts were used in the article *"Survival prediction of patients with sepsis from age, sex, and septic episode number alone"* (Sci Rep 2020, doi:10.1038/s41598-020-73558-3).

- **Primary cohort:** 110\,204 admissions from Norway.
- **Study cohort:** 19\,051 admissions (a subset of the primary cohort).
- **Validation cohort:** 137 cases from South Korea.

Each file includes only the following variables:

- `age_years`
- `sex_0male_1female`
- `episode_number`
- `hospital_outcome_1alive_0dead`
