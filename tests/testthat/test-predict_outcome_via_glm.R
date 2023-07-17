library(survey)

# Create a survey design ----
  data(involvement_survey_str2s, package = 'nrba')

  survey_design <- survey_design <- svydesign(
    data = involvement_survey_str2s |>
      transform(STUDENT_GRADE = factor(
        STUDENT_GRADE, levels = c("K", 1:12)
      )),
    weights = ~ BASE_WEIGHT,
    strata =  ~ SCHOOL_DISTRICT,
    ids =     ~ SCHOOL_ID             + UNIQUE_ID,
    fpc =     ~ N_SCHOOLS_IN_DISTRICT + N_STUDENTS_IN_SCHOOL
  )

# Prepare data for modeling ----
  model_input_data <- survey_design |>
    subset(RESPONSE_STATUS %in% c("Respondent")) |>
    transform(
      PARENT_AGREES = ifelse(WHETHER_PARENT_AGREES == "AGREE", 1, 0)
    ) |>
    transform(
      PARENT_AGREES = ifelse(is.na(WHETHER_PARENT_AGREES), NA_real_, PARENT_AGREES)
    )

# Fit a pre-specified model ----

  ##_ Binary outcome variable ----
  result <- predict_outcome_via_glm(
    survey_design = model_input_data,
    outcome_type = "binary",
    outcome_variable = "WHETHER_PARENT_AGREES",
    outcome_to_predict = "AGREE",
    model_selection = 'main-effects',
    numeric_predictors = c("STUDENT_AGE"),
    categorical_predictors = c("PARENT_HAS_EMAIL", "STUDENT_GRADE")
  )

  expected_model <- svyglm(
    design = model_input_data,
    formula = PARENT_AGREES ~ STUDENT_AGE + PARENT_HAS_EMAIL + STUDENT_GRADE,
    family = quasibinomial(link = 'logit')
  )

  expected_results <- dplyr::bind_cols(
    expected_model |> summary() |> getElement("coefficients") |>
      as.data.frame() |>
      `colnames<-`(c("exp_estimated_coefficient",
                     "exp_se_coefficient",
                     "exp_t_value_coefficient",
                     "exp_p_value_coefficient")) |>
      tibble::rownames_to_column("coefficient"),
    confint(expected_model, level = 0.95) |>
    as.data.frame() |>
    `colnames<-`(c("exp_conf_intrvl_lower",
                   "exp_conf_intrvl_upper"))
  ) |> `rownames<-`(NULL)
  expected_results[['variable']] <- stringr::str_extract(
    expected_results[['coefficient']],
    "PARENT_HAS_EMAIL|STUDENT_AGE|STUDENT_GRADE|\\(Intercept\\)"
  )
  expected_results[['variable_category']] <- stringr::str_remove(
    expected_results[['coefficient']],
    "PARENT_HAS_EMAIL|STUDENT_AGE|STUDENT_GRADE|intercept"
  )
  expected_results[['variable_category']] <- ifelse(
    expected_results[['variable_category']] %in% c("", "(Intercept)"), NA_character_,
    expected_results[['variable_category']]
  )

  compiled_results <- dplyr::full_join(
    x = result,
    y = expected_results,
    by = c("variable", "variable_category")
  )

  testthat::expect_equal(
    object = compiled_results$estimated_coefficient,
    expected = compiled_results$exp_estimated_coefficient
  )
  testthat::expect_equal(
    object = compiled_results$se_coefficient,
    expected = compiled_results$exp_se_coefficient
  )
  testthat::expect_equal(
    object = compiled_results$conf_intrvl_lower,
    expected = compiled_results$exp_conf_intrvl_lower
  )
  testthat::expect_equal(
    object = compiled_results$conf_intrvl_upper,
    expected = compiled_results$exp_conf_intrvl_upper
  )
  testthat::expect_equal(
    object = compiled_results$p_value_coefficient,
    expected = compiled_results$exp_p_value_coefficient
  )
  testthat::expect_equal(
    object = attr(result, 'reference_levels'),
    expected = data.frame(
      variable = c("PARENT_HAS_EMAIL", "STUDENT_GRADE"),
      variable_category = c(
        model_input_data$variables[['PARENT_HAS_EMAIL']] |>
          unique() |> sort() |> head(1),
        model_input_data$variables[['STUDENT_GRADE']] |>
          levels() |> head(1)
      ))
  )

  ##_ Continuous outcome variable ----
  result <- predict_outcome_via_glm(
    survey_design = model_input_data,
    outcome_type = "continuous",
    outcome_variable = "PARENT_AGREES",
    model_selection = 'main-effects',
    numeric_predictors = c("STUDENT_AGE"),
    categorical_predictors = c("PARENT_HAS_EMAIL", "STUDENT_GRADE")
  )

  expected_model <- svyglm(
    design = model_input_data,
    formula = PARENT_AGREES ~ STUDENT_AGE + PARENT_HAS_EMAIL + STUDENT_GRADE,
    family = gaussian()
  )

  expected_results <- dplyr::bind_cols(
    expected_model |> summary() |> getElement("coefficients") |>
      as.data.frame() |>
      `colnames<-`(c("exp_estimated_coefficient",
                     "exp_se_coefficient",
                     "exp_t_value_coefficient",
                     "exp_p_value_coefficient")) |>
      tibble::rownames_to_column("coefficient"),
    confint(expected_model, level = 0.95) |>
      as.data.frame() |>
      `colnames<-`(c("exp_conf_intrvl_lower",
                     "exp_conf_intrvl_upper"))
  ) |> `rownames<-`(NULL)
  expected_results[['variable']] <- stringr::str_extract(
    expected_results[['coefficient']],
    "PARENT_HAS_EMAIL|STUDENT_AGE|STUDENT_GRADE|\\(Intercept\\)"
  )
  expected_results[['variable_category']] <- stringr::str_remove(
    expected_results[['coefficient']],
    "PARENT_HAS_EMAIL|STUDENT_AGE|STUDENT_GRADE|intercept"
  )
  expected_results[['variable_category']] <- ifelse(
    expected_results[['variable_category']] %in% c("", "(Intercept)"), NA_character_,
    expected_results[['variable_category']]
  )

  compiled_results <- dplyr::full_join(
    x = result,
    y = expected_results,
    by = c("variable", "variable_category")
  )

  testthat::expect_equal(
    object = compiled_results$estimated_coefficient,
    expected = compiled_results$exp_estimated_coefficient
  )
  testthat::expect_equal(
    object = compiled_results$se_coefficient,
    expected = compiled_results$exp_se_coefficient
  )
  testthat::expect_equal(
    object = compiled_results$conf_intrvl_lower,
    expected = compiled_results$exp_conf_intrvl_lower
  )
  testthat::expect_equal(
    object = compiled_results$conf_intrvl_upper,
    expected = compiled_results$exp_conf_intrvl_upper
  )
  testthat::expect_equal(
    object = compiled_results$p_value_coefficient,
    expected = compiled_results$exp_p_value_coefficient
  )

# Use stepwise selection ----

  suppressMessages({
    result <- predict_outcome_via_glm(
      survey_design = model_input_data,
      outcome_type = "binary",
      outcome_variable = "WHETHER_PARENT_AGREES",
      outcome_to_predict = "AGREE",
      model_selection = 'stepwise',
      numeric_predictors = c("STUDENT_AGE"),
      categorical_predictors = c("PARENT_HAS_EMAIL", "STUDENT_GRADE", "STUDENT_SEX")
    )

    expected_model <- stepwise_model_selection(
      survey_design = model_input_data,
      outcome_variable = "PARENT_AGREES",
      predictor_variables = c("STUDENT_AGE", "PARENT_HAS_EMAIL", "STUDENT_GRADE", "STUDENT_SEX"),
      model_type = "binary-logistic",
      max_iterations = 5, alpha_enter = 0.45, alpha_remain = 0.45
    )
  })

  expected_results <- dplyr::bind_cols(
    expected_model |> summary() |> getElement("coefficients") |>
      as.data.frame() |>
      `colnames<-`(c("exp_estimated_coefficient",
                     "exp_se_coefficient",
                     "exp_t_value_coefficient",
                     "exp_p_value_coefficient")) |>
      tibble::rownames_to_column("coefficient"),
    confint(expected_model, level = 0.95) |>
      as.data.frame() |>
      `colnames<-`(c("exp_conf_intrvl_lower",
                     "exp_conf_intrvl_upper"))
  ) |> `rownames<-`(NULL)
  expected_results[['variable']] <- stringr::str_extract(
    expected_results[['coefficient']],
    "PARENT_HAS_EMAIL|STUDENT_AGE|STUDENT_GRADE|STUDENT_SEX|\\(Intercept\\)"
  )
  expected_results[['variable_category']] <- stringr::str_remove(
    expected_results[['coefficient']],
    "PARENT_HAS_EMAIL|STUDENT_AGE|STUDENT_GRADE|STUDENT_SEX|intercept"
  )
  expected_results[['variable_category']] <- ifelse(
    expected_results[['variable_category']] %in% c("", "(Intercept)"), NA_character_,
    expected_results[['variable_category']]
  )

  compiled_results <- dplyr::full_join(
    x = result,
    y = expected_results,
    by = c("variable", "variable_category")
  )

  testthat::expect_equal(
    object = compiled_results$estimated_coefficient,
    expected = compiled_results$exp_estimated_coefficient
  )
  testthat::expect_equal(
    object = compiled_results$se_coefficient,
    expected = compiled_results$exp_se_coefficient
  )
  testthat::expect_equal(
    object = compiled_results$conf_intrvl_lower,
    expected = compiled_results$exp_conf_intrvl_lower
  )
  testthat::expect_equal(
    object = compiled_results$conf_intrvl_upper,
    expected = compiled_results$exp_conf_intrvl_upper
  )
  testthat::expect_equal(
    object = compiled_results$p_value_coefficient,
    expected = compiled_results$exp_p_value_coefficient
  )
