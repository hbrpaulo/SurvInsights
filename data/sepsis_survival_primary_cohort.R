#' Primary sepsis survival cohort
#'
#' Excerpt from the dataset used in the study
#' "Survival prediction of patients with sepsis from age, sex, and septic
#' episode number alone" (Scientific Reports 2020). Each row represents one
#' hospital episode with patient demographics and outcome.
#'
#' @format A tibble with 110,204 rows and 4 variables:
#' \describe{
#'   \item{age_years}{Age of the patient in years}
#'   \item{sex_0male_1female}{Patient sex (0 = male, 1 = female)}
#'   \item{episode_number}{Ordinal number of the septic episode}
#'   \item{hospital_outcome_1alive_0dead}{Hospital outcome (1 = alive, 0 = dead)}
#' }
#' @source <https://doi.org/10.1038/s41598-020-73558-3>
"sepsis_survival_primary_cohort"
