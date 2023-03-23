#' @title Fit a logistic regression model to predict response to the survey.
#' @description A logistic regression model is fit to the sample data to
#' predict whether an individual responds to the survey (i.e. is an eligible respondent)
#' rather than a nonrespondent. Ineligible cases and cases with unknown eligibility status
#' are not included in this model. \cr
#'
#' The function returns a summary of the model, including overall tests
#' for each variable of whether that variable improves the model's
#' ability to predict response status in the population of interest (not just in the random sample at hand). \cr
#'
#' This model can be used to identify auxiliary variables associated with response status
#' and compare multiple auxiliary variables in terms of their ability to predict response status.
#'
#' @param survey_design A survey design object created with the `survey` package.
#' @param numeric_predictors A list of names of numeric auxiliary variables to use for predicting response status.
#' @param categorical_predictors A list of names of categorical auxiliary variables to use for predicting response status.
#' @param model_selection A character string specifying how to select a model.
#' The default and recommended method is 'main-effects', which simply includes main effects
#' for each of the predictor variables. \cr
#' The method \code{'stepwise'} can be used to perform stepwise selection of variables for the model.
#' However, stepwise selection invalidates p-values, standard errors, and confidence intervals,
#' which are generally calculated under the assumption that model specification is predetermined.
#' @param status A character string giving the name of the variable representing response/eligibility status.
#' The \code{status} variable should have at most four categories,
#' representing eligible respondents (ER), eligible nonrespondents (EN),
#' known ineligible cases (IE), and cases whose eligibility is unknown (UE).
#' @param status_codes A named vector, with two entries named 'ER' and 'EN'
#' indicating which values of the \code{status} variable represent
#' eligible respondents (ER) and eligible nonrespondents (EN).
#' @param selection_controls Only required if \code{model-selection} isn't set to \code{"main-effects"}.
#' Otherwise, a list of parameters for model selection to pass on to \code{\link{stepwise_model_selection}},
#' with elements \code{alpha_enter}, \code{alpha_remain}, and \code{max_iterations}.
#'
#' @details
#' See Lumley and Scott (2017) for details of how regression models are fit to survey data.
#' For overall tests of variables, a Rao-Scott Likelihood Ratio Test is conducted
#' (see section 4 of Lumley and Scott (2017) for statistical details)
#' using the function \code{regTermTest(method = "LRT", lrt.approximation = "saddlepoint")}
#' from the 'survey' package. \cr
#'
#' If the user specifies \code{model_selection = "stepwise"}, a regression model
#' is selected by adding and removing variables based on the p-value from a
#' likelihood ratio rest. At each stage, a single variable is added to the model if
#' the p-value of the likelihood ratio test from adding the variable is below \code{alpha_enter}
#' and its p-value is less than that of all other variables not already in the model.
#' Next, of the variables already in the model, the variable with the largest p-value
#' is dropped if its p-value is greater than \code{alpha_remain}. This iterative process
#' continues until a maximum number of iterations is reached or until
#' either all variables have been added to the model or there are no unadded variables
#' for which the likelihood ratio test has a p-value below \code{alpha_enter}.
#'
#' @references
#' - Lumley, T., & Scott A. (2017). Fitting Regression Models to Survey Data. Statistical Science 32 (2) 265 - 278. https://doi.org/10.1214/16-STS605
#'
#' @return A data frame summarizing the fitted logistic regression model. \cr
#'
#' Each row in the data frame represents a coefficient in the model.
#' The column \code{variable} describes the underlying variable
#' for the coefficient. For categorical variables, the column \code{variable_category} indicates
#' the particular category of that variable for which a coefficient is estimated. \cr
#'
#' The columns \code{estimated_coefficient}, \code{se_coefficient}, \code{conf_intrvl_lower}, \code{conf_intrvl_upper},
#' and \code{p_value_coefficient} are summary statistics for
#' the estimated coefficient. Note that \code{p_value_coefficient} is based on the Wald t-test for the coefficient. \cr
#'
#' The column \code{variable_level_p_value} gives the p-value of the
#' Rao-Scott Likelihood Ratio Test for including the variable in the model.
#' This likelihood ratio test has its test statistic given by the column
#' \code{LRT_chisq_statistic}, and the reference distribution
#' for this test is a linear combination of \code{p} F-distributions
#' with numerator degrees of freedom given by \code{LRT_df_numerator} and
#' denominator degrees of freedom given by \code{LRT_df_denominator},
#' where \code{p} is the number of coefficients in the model corresponding to
#' the variable being tested.
#'
#' @export
#'
#' @examples
#' library(survey)
#'
#' # Create a survey design ----
#' data(involvement_survey_str2s, package = "nrba")
#'
#' survey_design <- survey_design <- svydesign(
#'   data = involvement_survey_str2s,
#'   weights = ~BASE_WEIGHT,
#'   strata = ~SCHOOL_DISTRICT,
#'   ids = ~ SCHOOL_ID + UNIQUE_ID,
#'   fpc = ~ N_SCHOOLS_IN_DISTRICT + N_STUDENTS_IN_SCHOOL
#' )
#'
#' predict_response_status_via_glm(
#'   survey_design = survey_design,
#'   status = "RESPONSE_STATUS",
#'   status_codes = c(
#'     "ER" = "Respondent",
#'     "EN" = "Nonrespondent",
#'     "IE" = "Ineligible",
#'     "UE" = "Unknown"
#'   ),
#'   model_selection = "main-effects",
#'   numeric_predictors = c("STUDENT_AGE"),
#'   categorical_predictors = c("PARENT_HAS_EMAIL", "STUDENT_GRADE")
#' )
#'
predict_response_status_via_glm <- function(survey_design,
                                            status,
                                            status_codes = c("ER", "EN", "IE", "UE"),
                                            numeric_predictors = NULL,
                                            categorical_predictors = NULL,
                                            model_selection = "main-effects",
                                            selection_controls = list(
                                              alpha_enter = 0.5, alpha_remain = 0.5,
                                              max_iterations = 100L
                                            )) {

  # Parameter checks ----

  ## _ Model selection parameter

  if (!is.character(model_selection) || length(model_selection) != 1 || !model_selection %in% c("main-effects", "stepwise")) {
    stop("`model_selection` must be either 'main-effects' or 'stepwise'")
  }

  ## _ Status variable and codes

  if (missing(status)) {
    stop("A variable name must be supplied to the `status` parameter")
  }

  if (!is.character(status) || length(status) != 1) {
    stop("Must specify a single variable name for `status`")
  }
  if (!status %in% colnames(survey_design)) {
    stop(sprintf(
      "The status variable '%s' does not appear in the supplied data",
      status
    ))
  }

  if (missing(status_codes) || is.null(status_codes)) {
    stop("Must supply status codes to the `status_codes` parameter.")
  }

  if (!((is.vector(status_codes) || is.factor(status_codes)) && !is.list(status_codes)) || is.null(names(status_codes))) {
    stop("Must supply a named vector for `status_codes`.")
  }

  valid_status_code_names <- !any(is.na(names(status_codes)))
  valid_status_code_names <- valid_status_code_names && is.logical(all.equal(sort(names(status_codes)), c("EN", "ER", "IE", "UE")))
  if (length(status_codes) != 4 || !valid_status_code_names) {
    stop("`status_codes` must be a vector with four values, with names: 'ER', 'EN', 'IE', and 'UE'.")
  }

  if (length(unique(status_codes)) != 4) {
    stop("Must use distinct values for the status codes supplied to `status_codes`.")
  }

  valid_status_values <- all(survey_design[["variables"]][[status]] %in% status_codes)
  if (!valid_status_values) {
    stop("The data contains values in the status variable which are not listed in `status_codes`.")
  }

  ## _ Predictor variables ----
  predictor_variables <- sort(union(
    categorical_predictors,
    numeric_predictors
  ))
  missing_predictor_variables <- setdiff(
    predictor_variables,
    colnames(survey_design[["variables"]])
  )
  if (length(missing_predictor_variables) > 0) {
    error_msg <- sprintf(
      "The following variables are missing from the survey data: %s",
      paste(sprintf("`%s`", missing_predictor_variables),
        collapse = ", "
      )
    )
    stop(error_msg)
  }
  if (length(intersect(numeric_predictors, categorical_predictors)) > 0) {
    stop("The list of numeric predictor variables and the list of categorical predictor variables cannot overlap.")
  }

  # Prepare the survey design for modeling ----

  survey_design_for_model <- survey_design

  survey_design_for_model$variables[["_IS_RESPONDENT_"]] <- ifelse(
    survey_design[["variables"]][[status]] %in% status_codes[c("ER")], 1L,
    ifelse(survey_design[["variables"]][[status]] %in% status_codes[c("EN")],
      0L, NA_integer_
    )
  )


  # Select the variables for the model ----

  if (model_selection == "main-effects") {
    model_variables <- predictor_variables
    model_formula <- reformulate(
      response = as.name("_IS_RESPONDENT_"),
      termlabels = model_variables
    )
    final_model <- survey::svyglm(
      formula = model_formula,
      design = survey_design_for_model,
      family = quasibinomial("logit")
    )
  }

  if (model_selection == "stepwise") {
    message("Beginning stepwise model selection.")

    stepwise_model <- stepwise_model_selection(
      survey_design_for_model,
      outcome_variable = "_IS_RESPONDENT_",
      predictor_variables = predictor_variables,
      model_type = "binary-logistic",
      max_iterations = selection_controls[["max_iterations"]],
      alpha_enter = selection_controls[["alpha_enter"]],
      alpha_remain = selection_controls[["alpha_remain"]]
    )

    model_variables <- stepwise_model |>
      getElement("formula") |>
      terms() |>
      attr(which = "term.labels", exact = TRUE)

    rm(stepwise_model)
    gc()

    model_formula <- reformulate(
      response = as.name("_IS_RESPONDENT_"),
      termlabels = model_variables
    )
    final_model <- survey::svyglm(
      formula = model_formula,
      design = survey_design_for_model,
      family = quasibinomial("logit")
    )
  }

  # Make a (partial) ANOVA summary table ----
  model_terms <- attr(final_model$terms, "term.labels")
  anova_table <- NULL
  for (term in model_terms) {
    test_output <- survey::regTermTest(
      model = final_model, test.terms = term,
      method = "LRT", df = NULL
    )
    test_output <- cbind(
      "term" = term,
      as.data.frame(test_output[c("p", "chisq", "df", "ddf")]),
      "DEff" = mean(test_output$lambda)
    )
    test_output <- test_output[, c("term", "p", "chisq", "DEff", "df", "ddf")]
    anova_table <- rbind(anova_table, test_output)
  }

  # Summarize estimated model coefficients ----

  coefficients_table <- broom::tidy(
    final_model,
    conf.int = TRUE, conf.level = 0.95,
    exponentiate = FALSE
  )
  coefficients_table <- coefficients_table[, c(
    "term", "estimate", "std.error",
    "conf.low", "conf.high", "p.value"
  )]
  colnames(coefficients_table) <- c(
    "name_coefficient", "estimated_coefficient", "se_coefficient",
    "conf_intrvl_lower", "conf_intrvl_upper", "p_value_coefficient"
  )

  # Combine model summary into a single table ----

  coef_assignments <- model.matrix(final_model) |> attr("assign")
  coef_names <- model.matrix(final_model) |> colnames()
  model_variables <- final_model$terms |> attr("term.labels")
  variable_coefficient_lookup <- data.frame(
    "term" = model_variables[coef_assignments],
    "name_coefficient" = coef_names[coef_names != "(Intercept)"],
    stringsAsFactors = FALSE
  )

  coefs_table <- dplyr::inner_join(
    x = coefficients_table,
    y = variable_coefficient_lookup,
    by = "name_coefficient"
  )
  combined_summary_table <- dplyr::full_join(
    x = anova_table,
    y = coefs_table,
    by = "term"
  )

  for (i in seq_len(nrow(combined_summary_table))) {
    nchars_to_remove <- combined_summary_table[["term"]][i] |> nchar()
    total_chars <- combined_summary_table[["name_coefficient"]][i] |> nchar()
    if (nchars_to_remove > 0) {
      combined_summary_table[["name_coefficient"]][i] <- substr(
        combined_summary_table[["name_coefficient"]][i],
        start = nchars_to_remove + 1L, stop = total_chars
      )
      if (nchars_to_remove == total_chars) {
        combined_summary_table[["name_coefficient"]][i] <- NA_character_
      }
    }
  }

  # Add rows for the intercept (if applicable)

  intercept_rows <- coefficients_table[coefficients_table[["name_coefficient"]] == "(Intercept)", , drop = FALSE]
  if (nrow(intercept_rows) > 0) {
    intercept_rows[["term"]] <- "(Intercept)"
    intercept_rows[["name_coefficient"]] <- NA_character_
    combined_summary_table <- dplyr::bind_rows(
      intercept_rows,
      combined_summary_table
    ) |>
      dplyr::select(dplyr::one_of(colnames(combined_summary_table)))
  }

  # Rename and subset columns

  colnames(combined_summary_table) <- dplyr::case_when(
    colnames(combined_summary_table) == "term" ~ "variable",
    colnames(combined_summary_table) == "p" ~ "variable_level_p_value",
    colnames(combined_summary_table) == "chisq" ~ "LRT_chisq_statistic",
    colnames(combined_summary_table) == "DEff" ~ "LRT_DEff",
    colnames(combined_summary_table) == "df" ~ "LRT_df_numerator",
    colnames(combined_summary_table) == "ddf" ~ "LRT_df_denominator",
    colnames(combined_summary_table) == "name_coefficient" ~ "variable_category",
    TRUE ~ colnames(combined_summary_table)
  )
  result_columns <- c(
    "variable", "variable_level_p_value",
    "variable_category", "estimated_coefficient", "se_coefficient",
    "conf_intrvl_lower", "conf_intrvl_upper",
    "p_value_coefficient",
    "LRT_chisq_statistic", "LRT_DEff", "LRT_df_numerator", "LRT_df_denominator"
  )
  combined_summary_table <- combined_summary_table[result_columns]

  # Return result
  return(combined_summary_table)
}
