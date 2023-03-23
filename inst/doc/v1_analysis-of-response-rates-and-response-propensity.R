## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE, message=FALSE, warning=FALSE--------------------------
suppressPackageStartupMessages({
  library(nrba)
  library(survey)
  library(dplyr)
})

## ---- echo=FALSE--------------------------------------------------------------
involvement_survey_srs |>
  mutate(RESPONSE_STATUS = case_when(
    RESPONSE_STATUS == "Respondent" ~ "1 (Respondent)",
    RESPONSE_STATUS == "Nonrespondent" ~ "2 (Nonrespondent)",
    RESPONSE_STATUS == "Ineligible" ~ "3 (Ineligible)",
    RESPONSE_STATUS == "Unknown" ~ "4 (Unknown Eligibility)"
  )) |>
  select(UNIQUE_ID, RESPONSE_STATUS) |>
  group_by(RESPONSE_STATUS) |>
  sample_n(size = 2) |>
  ungroup() |>
  sample_n(size = 8, replace = FALSE) |>
  knitr::kable()

## -----------------------------------------------------------------------------
# Load example data
data('involvement_survey_srs', package = "nrba")

# Calculate overall response rates for the survey
calculate_response_rates(
  data = involvement_survey_srs,
  status = "RESPONSE_STATUS",
  status_codes = c(
    'ER' = 'Respondent',
    'EN' = 'Nonrespondent',
    'IE' = 'Ineligible',
    'UE' = 'Unknown'
  ),
  rr_formula = 'RR1'
)

## -----------------------------------------------------------------------------
# Load example data
data('involvement_survey_str2s', package = "nrba")

# Calculate overall response rates for the survey
calculate_response_rates(
  data = involvement_survey_str2s,
  weights = "BASE_WEIGHT",
  status = "RESPONSE_STATUS",
  status_codes = c(
    'ER' = 'Respondent',
    'EN' = 'Nonrespondent',
    'IE' = 'Ineligible',
    'UE' = 'Unknown'
  ),
  rr_formula = 'RR1'
)

## -----------------------------------------------------------------------------
library(dplyr)

involvement_survey_srs |>
  group_by(STUDENT_RACE) |>
  calculate_response_rates(
    status = "RESPONSE_STATUS",
    status_codes = c(
      'ER' = 'Respondent',
      'EN' = 'Nonrespondent',
      'IE' = 'Ineligible',
      'UE' = 'Unknown'
    ),
    rr_formula = 'RR1'
  )

## ---- echo=FALSE--------------------------------------------------------------
calculate_response_rates(
  data = involvement_survey_srs,
  status = "RESPONSE_STATUS",
  status_codes = c(
    'ER' = 'Respondent',
    'EN' = 'Nonrespondent',
    'IE' = 'Ineligible',
    'UE' = 'Unknown'
  ),
  rr_formula = c('RR1', 'RR3', 'RR5')
)

## -----------------------------------------------------------------------------

involvement_survey_srs |>
  group_by(PARENT_HAS_EMAIL) |>
  calculate_response_rates(
    status = "RESPONSE_STATUS",
    status_codes = c(
      'ER' = 'Respondent',
      'EN' = 'Nonrespondent',
      'IE' = 'Ineligible',
      'UE' = 'Unknown'
    ),
    rr_formula = 'RR3',
    elig_method = "CASRO-subgroup"
  )

## -----------------------------------------------------------------------------
involvement_survey_srs %>%
  mutate(e_by_email = ifelse(PARENT_HAS_EMAIL == 'Has Email', 0.75, 0.25)) %>%
  group_by(PARENT_HAS_EMAIL) %>%
  calculate_response_rates(status = "RESPONSE_STATUS",
                           status_codes = c(
                             'ER' = 'Respondent',
                             'EN' = 'Nonrespondent',
                             'IE' = 'Ineligible',
                             'UE' = 'Unknown'
                           ),
                           rr_formula = "RR3",
                           elig_method = "specified",
                           e = "e_by_email")

## -----------------------------------------------------------------------------
library(survey)

# Create a survey design object with the 'survey' package
involvement_svy <- svydesign(
  data = involvement_survey_str2s,
  weights = ~ BASE_WEIGHT,
  strata =  ~ SCHOOL_DISTRICT,
  ids =     ~ SCHOOL_ID             + UNIQUE_ID, # School ID and Student ID
  fpc =     ~ N_SCHOOLS_IN_DISTRICT + N_STUDENTS_IN_SCHOOL # Population sizes at each sampling stage
)

## -----------------------------------------------------------------------------
chisq_test_ind_response(
  survey_design = involvement_svy,
  # Specify the response status variable
  status = "RESPONSE_STATUS",
  # Specify how to interpret categories of response status variable
  status_codes = c(
    'ER' = 'Respondent',
    'EN' = 'Nonrespondent',
    'IE' = 'Ineligible',
    'UE' = 'Unknown'
  ),
  # Specify variable(s) to use for the Chi-Square test(s)
  aux_vars = c("STUDENT_RACE", "PARENT_HAS_EMAIL")
)

## -----------------------------------------------------------------------------
predict_response_status_via_glm(
  survey_design = involvement_svy,
  status = "RESPONSE_STATUS",
  status_codes = c("ER" = "Respondent",
                   "EN" = "Nonrespondent",
                   "IE" = "Ineligible",
                   "UE" = "Unknown"),
  # Specify models
  model_selection = 'main-effects',
  # Specify predictor variables for the model
  numeric_predictors = c("STUDENT_AGE"),
  categorical_predictors = c("PARENT_HAS_EMAIL", "STUDENT_RACE")
)

