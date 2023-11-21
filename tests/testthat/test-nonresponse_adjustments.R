suppressWarnings({
  suppressPackageStartupMessages({
    library(survey)
    library(nrba)
  })
})

# Load the survey data

data("involvement_survey_str2s", package = 'nrba')
data("involvement_survey_pop", package = 'nrba')

# Create a replicate design

base_weights_design <- svydesign(
  data = involvement_survey_str2s,
  weights = ~ BASE_WEIGHT,
  strata =  ~ SCHOOL_DISTRICT,
  ids =     ~ SCHOOL_ID             + UNIQUE_ID,
  fpc =     ~ N_SCHOOLS_IN_DISTRICT + N_STUDENTS_IN_SCHOOL
) |> as.svrepdesign(type = "mrbbootstrap", replicates = 50)

ue_adjusted_design <- base_weights_design |>
  wt_class_adjust(
    type = "UE",
    status = "RESPONSE_STATUS",
    status_codes = c("ER" = "Respondent",
                     "EN" = "Nonrespondent",
                     "IE" = "Ineligible",
                     "UE" = "Unknown"),
    wt_class = c("SCHOOL_DISTRICT")
  )

nr_adjusted_Design <- ue_adjusted_design |>
  wt_class_adjust(
    type = "NR",
    status = "RESPONSE_STATUS",
    status_codes = c("ER" = "Respondent",
                     "EN" = "Nonrespondent",
                     "IE" = "Ineligible",
                     "UE" = "Unknown"),
    wt_class = c("SCHOOL_DISTRICT")
  )

ue_nr_adjusted_Design <- base_weights_design |>
  wt_class_adjust(
    type = c("UE", "NR"),
    status = "RESPONSE_STATUS",
    status_codes = c("ER" = "Respondent",
                     "EN" = "Nonrespondent",
                     "IE" = "Ineligible",
                     "UE" = "Unknown"),
    wt_class = c("SCHOOL_DISTRICT")
  )

# Basic checks of correctness -----

test_that(
  "Correct results for basic example", {
    expect_equal(
      object = ue_adjusted_design,
      expected = base_weights_design |>
        svrep::redistribute_weights(
          reduce_if = RESPONSE_STATUS == "Unknown",
          increase_if = RESPONSE_STATUS != "Unknown",
          by = "SCHOOL_DISTRICT"
        )
    )
    expect_equal(
      object = nr_adjusted_Design,
      expected = ue_adjusted_design |>
        svrep::redistribute_weights(
          reduce_if = RESPONSE_STATUS == "Nonrespondent",
          increase_if = RESPONSE_STATUS == "Respondent",
          by = "SCHOOL_DISTRICT"
        )
    )
  })

test_that(
  "Works without specifying a grouping variable", {
    expect_equal(
      object = wt_class_adjust(
        survey_design = base_weights_design,
        type = "UE",
        status = "RESPONSE_STATUS",
        status_codes = c("ER" = "Respondent",
                         "EN" = "Nonrespondent",
                         "IE" = "Ineligible",
                         "UE" = "Unknown")
      ),
      expected = base_weights_design |>
        svrep::redistribute_weights(
          reduce_if = RESPONSE_STATUS == "Unknown",
          increase_if = RESPONSE_STATUS != "Unknown",
        )
    )
  })


# Test for two-step adjustment ----

test_that(
  "Correct results for two-step adjustment", {
  expect_equal(object = ue_nr_adjusted_Design,
               expected = nr_adjusted_Design)
  expect_equal(expected = ue_nr_adjusted_Design,
               object = base_weights_design |>
                 wt_class_adjust(
                   type = c("NR", "UE"),
                   status = "RESPONSE_STATUS",
                   status_codes = c("ER" = "Respondent",
                                    "EN" = "Nonrespondent",
                                    "IE" = "Ineligible",
                                    "UE" = "Unknown"),
                   wt_class = c("SCHOOL_DISTRICT")
                 ))
})

# Informative error messages for bad inputs

bad_survey_design <- base_weights_design
bad_survey_design[['variables']][['weight_class']] <- c(NA, rep(1, times = nrow(base_weights_design) - 1))

test_that(
  "Informative error message for missing values in grouping variable", {
  expect_error(
    object = {wt_class_adjust(
      survey_design = bad_survey_design,
      wt_class = "weight_class",
      status = "RESPONSE_STATUS",
      status_codes = c("ER" = "Respondent",
                       "EN" = "Nonrespondent",
                       "IE" = "Ineligible",
                       "UE" = "Unknown"),
      type = "UE"
    )},
    regexp = "should not have any missing values"
  )
})
