suppressWarnings({
  suppressPackageStartupMessages({
    library(survey)
    library(nrba)
  })
})

# Create an example survey design
# with a variable representing number of contact attempts

library(survey)
data(involvement_survey_srs, package = 'nrba')

involvement_survey_srs[['CONTACT_ATTEMPTS']] <- runif(
  n = nrow(involvement_survey_srs),
  min = 1, max = 10
) |> round()

survey_design <- svydesign(weights = ~ BASE_WEIGHT,
                           id = ~ UNIQUE_ID,
                           fpc = ~ N_STUDENTS,
                           data = involvement_survey_srs)

# Cumulative estimates for numeric variable -----
  result <- get_cumulative_estimates(
    survey_design = survey_design |>
      subset(RESPONSE_STATUS == "Respondent") |>
      subset(CONTACT_ATTEMPTS > 7),
    y_var = "STUDENT_AGE",
    y_var_type = "numeric",
    predictor_variable = "CONTACT_ATTEMPTS"
  )

  expected_result <- NULL
  expected_labels <- c(
    "'8'", "'8' to '9'", "'8' to '10'"
  )

  for (i in 0:2) {
    values <- seq(from = 8, to = 8 + i)
    survey_data <- survey_design |>
      subset(RESPONSE_STATUS == "Respondent") |>
      subset(CONTACT_ATTEMPTS %in% values)
    expected_result <- rbind(
      expected_result,
      data.frame(
        'CONTACT_ATTEMPTS' = expected_labels[i + 1],
        'outcome' = "STUDENT_AGE",
        'estimate' = coef(
          svymean(x = ~ STUDENT_AGE, design = survey_data, na.rm = TRUE)
        ) |> unname(),
        'std_error' = SE(
          svymean(x = ~ STUDENT_AGE, design = survey_data, na.rm = TRUE)
        ) |> unname(),
        'respondent_sample_size' = nrow(survey_data)
      )
    )
  }
  rownames(expected_result) <- NULL

  testthat::expect_equal(
    object = result,
    expected = expected_result
  )

# Cumulative estimates from respondents for proportions of categorical variable ----
  result <- get_cumulative_estimates(
    survey_design = survey_design |>
      subset(RESPONSE_STATUS == "Respondent") |>
      subset(CONTACT_ATTEMPTS > 7),
    y_var = "WHETHER_PARENT_AGREES",
    y_var_type = "categorical",
    predictor_variable = "CONTACT_ATTEMPTS"
  )

  expected_labels <- c(
    "'8'", "'8' to '9'", "'8' to '10'"
  )

  expected_result <- NULL
  for (i in 0:2) {
    values <- seq(from = 8, to = 8 + i)
    survey_data <- survey_design |>
      subset(RESPONSE_STATUS == "Respondent") |>
      subset(CONTACT_ATTEMPTS %in% values)
    expected_result <- rbind(
      expected_result,
      data.frame(
        'CONTACT_ATTEMPTS' = rep(expected_labels[i + 1], times = 2),
        'outcome' = rep("WHETHER_PARENT_AGREES", times = 2),
        'outcome_category' = c("AGREE", "DISAGREE"),
        'estimate' = coef(
          svymean(x = ~ WHETHER_PARENT_AGREES, design = survey_data, na.rm = TRUE)
        ) |> unname(),
        'std_error' = SE(
          svymean(x = ~ WHETHER_PARENT_AGREES, design = survey_data, na.rm = TRUE)
        ) |> unname(),
        'respondent_sample_size' = rep(nrow(survey_data), times = 2)
      )
    )
  }
  rownames(expected_result) <- NULL

  testthat::expect_equal(
    object = result,
    expected = expected_result
  )

  ##_ Check that works with `y_var_type` automatically inferred ----
  result <- get_cumulative_estimates(
    survey_design = survey_design |>
      subset(RESPONSE_STATUS == "Respondent") |>
      subset(CONTACT_ATTEMPTS > 7) |>
      transform(WHETHER_PARENT_AGREES = ifelse(
        WHETHER_PARENT_AGREES == "DISAGREE", "1", "0"
      )),
    y_var = "WHETHER_PARENT_AGREES",
    y_var_type = NULL,
    predictor_variable = "CONTACT_ATTEMPTS"
  )

  expected_result[['outcome_category']] <- rep(c('0', '1'), times = 3)

  testthat::expect_equal(
    object = result,
    expected = expected_result
  )

# Character predictor variable ----

  result <- get_cumulative_estimates(
    survey_design = survey_design |>
      subset(RESPONSE_STATUS == "Respondent") |>
      subset(CONTACT_ATTEMPTS <= 4) |>
      transform(CONTACT_ATTEMPTS = letters[CONTACT_ATTEMPTS]),
    y_var = "STUDENT_AGE",
    y_var_type = "numeric",
    predictor_variable = "CONTACT_ATTEMPTS"
  )

  expected_result <- NULL
  for (i in 1:4) {
    values <- letters[seq(from = 1, to = i)]
    expected_text <- paste(
      paste0("'", values[unique(c(1, i))], "'"),
      collapse = " to "
    )
    survey_data <- survey_design |>
      subset(RESPONSE_STATUS == "Respondent") |>
      transform(CONTACT_ATTEMPTS = letters[CONTACT_ATTEMPTS]) |>
      subset(CONTACT_ATTEMPTS %in% values)
    expected_result <- rbind(
      expected_result,
      data.frame(
        'CONTACT_ATTEMPTS' = expected_text,
        'outcome' = "STUDENT_AGE",
        'estimate' = coef(
          svymean(x = ~ STUDENT_AGE, design = survey_data, na.rm = TRUE)
        ) |> unname(),
        'std_error' = SE(
          svymean(x = ~ STUDENT_AGE, design = survey_data, na.rm = TRUE)
        ) |> unname(),
        'respondent_sample_size' = nrow(survey_data)
      )
    )
  }
  rownames(expected_result) <- NULL

  testthat::expect_equal(
    object = result,
    expected = expected_result
  )

# Factor predictor variable ----

  result <- get_cumulative_estimates(
    survey_design = survey_design |>
      subset(RESPONSE_STATUS == "Respondent") |>
      subset(CONTACT_ATTEMPTS <= 4) |>
      transform(CONTACT_ATTEMPTS = letters[CONTACT_ATTEMPTS] |>
                  factor(levels = c("b", "a", "c", "d"))),
    y_var = "STUDENT_AGE",
    y_var_type = "numeric",
    predictor_variable = "CONTACT_ATTEMPTS"
  )

  expected_result <- NULL
  for (i in 1:4) {
    values <- c("b", "a", "c", "d")[seq(from = 1, to = i)]
    expected_text <- paste(
      paste0("'", values[unique(c(1, i))], "'"),
      collapse = " to "
    )
    survey_data <- survey_design |>
      subset(RESPONSE_STATUS == "Respondent") |>
      transform(CONTACT_ATTEMPTS = letters[CONTACT_ATTEMPTS] |>
                  factor(levels = c("b", "a", "c", "d"))) |>
      subset(CONTACT_ATTEMPTS %in% values)
    expected_result <- rbind(
      expected_result,
      data.frame(
        'CONTACT_ATTEMPTS' = expected_text,
        'outcome' = "STUDENT_AGE",
        'estimate' = coef(
          svymean(x = ~ STUDENT_AGE, design = survey_data, na.rm = TRUE)
        ) |> unname(),
        'std_error' = SE(
          svymean(x = ~ STUDENT_AGE, design = survey_data, na.rm = TRUE)
        ) |> unname(),
        'respondent_sample_size' = nrow(survey_data)
      )
    )
  }
  rownames(expected_result) <- NULL

  testthat::expect_equal(
    object = result,
    expected = expected_result
  )

# Missing values in outcome ----

  design_w_missing_data <- survey_design |>
    subset(RESPONSE_STATUS == "Respondent") |>
    subset(CONTACT_ATTEMPTS <= 4)

  design_w_missing_data$variables$STUDENT_AGE[c(1,5,10)] <- NA

  result <- get_cumulative_estimates(
    survey_design = design_w_missing_data,
    y_var = "STUDENT_AGE",
    y_var_type = "numeric",
    predictor_variable = "CONTACT_ATTEMPTS"
  )

  expected_result <- NULL
  for (i in 1:4) {
    values <- seq(from = 1, to = i)
    expected_text <- paste(
      paste0("'", values[unique(c(1, i))], "'"),
      collapse = " to "
    )
    survey_data <- design_w_missing_data |>
      subset(CONTACT_ATTEMPTS %in% values) |>
      subset(!is.na(STUDENT_AGE))
    expected_result <- rbind(
      expected_result,
      data.frame(
        'CONTACT_ATTEMPTS' = expected_text,
        'outcome' = "STUDENT_AGE",
        'estimate' = coef(
          svymean(x = ~ STUDENT_AGE, design = survey_data,
                  na.rm = TRUE)
        ) |> unname(),
        'std_error' = SE(
          svymean(x = ~ STUDENT_AGE, design = survey_data,
                  na.rm = TRUE)
        ) |> unname(),
        'respondent_sample_size' = nrow(survey_data)
      )
    )
  }
  rownames(expected_result) <- NULL

  testthat::expect_equal(
    object = result,
    expected = expected_result
  )

# Missing values in predictor ----

  design_w_missing_data <- survey_design |>
    subset(RESPONSE_STATUS == "Respondent") |>
    subset(CONTACT_ATTEMPTS <= 4)

  design_w_missing_data$variables$CONTACT_ATTEMPTS[c(1,5,10)] <- NA

  result <- get_cumulative_estimates(
    survey_design = design_w_missing_data,
    y_var = "STUDENT_AGE",
    y_var_type = "numeric",
    predictor_variable = "CONTACT_ATTEMPTS"
  )

  expected_result <- NULL
  for (i in 1:4) {
    expected_text <- paste(
      paste0("'", unique(c(1,i)), "'"),
      collapse = " to "
    )
    values <- seq(from = 1, to = i)
    survey_data <- design_w_missing_data |>
      subset(CONTACT_ATTEMPTS %in% values) |>
      subset(!is.na(STUDENT_AGE))
    expected_result <- rbind(
      expected_result,
      data.frame(
        'CONTACT_ATTEMPTS' = expected_text,
        'outcome' = "STUDENT_AGE",
        'estimate' = coef(
          svymean(x = ~ STUDENT_AGE, design = survey_data,
                  na.rm = TRUE)
        ) |> unname(),
        'std_error' = SE(
          svymean(x = ~ STUDENT_AGE, design = survey_data,
                  na.rm = TRUE)
        ) |> unname(),
        'respondent_sample_size' = nrow(survey_data)
      )
    )
  }
  rownames(expected_result) <- NULL

  testthat::expect_equal(
    object = result,
    expected = expected_result
  )
