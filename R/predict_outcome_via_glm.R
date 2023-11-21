#' @title Fit a regression model to predict survey outcomes
#' @description A regression model is fit to the sample data to
#' predict outcomes measured by a survey.
#' This model can be used to identify auxiliary variables that are
#' predictive of survey outcomes and hence are potentially useful
#' for nonresponse bias analysis or weighting adjustments. \cr
#'
#' Only data from survey respondents will be used to fit the model,
#' since survey outcomes are only measured among respondents. \cr
#'
#' The function returns a summary of the model, including overall tests
#' for each variable of whether that variable improves the model's
#' ability to predict response status in the population of interest (not just in the random sample at hand). \cr
#'
#'
#' @param survey_design A survey design object created with the `survey` package.
#' @param outcome_variable Name of an outcome variable to use as the dependent variable in the model
#' The value of this variable is expected to be \code{NA} (i.e. missing)
#' for all cases other than eligible respondents.
#' @param outcome_type Either \code{"binary"} or \code{"continuous"}. For \code{"binary"},
#' a logistic regression model is used. For \code{"continuous"}, a generalized linear model
#' is fit using using an identity link function.
#' @param outcome_to_predict Only required if \code{outcome_type="binary"}.
#' Specify which category of \code{outcome_variable} is to be predicted.
#' @param numeric_predictors A list of names of numeric auxiliary variables to use for predicting response status.
#' @param categorical_predictors A list of names of categorical auxiliary variables to use for predicting response status.
#' @param model_selection A character string specifying how to select a model.
#' The default and recommended method is 'main-effects', which simply includes main effects
#' for each of the predictor variables. \cr
#' The method \code{'stepwise'} can be used to perform stepwise selection of variables for the model.
#' However, stepwise selection invalidates p-values, standard errors, and confidence intervals,
#' which are generally calculated under the assumption that model specification is predetermined.
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
#' @return A data frame summarizing the fitted regression model. \cr
#'
#' Each row in the data frame represents a coefficient in the model.
#' The column \code{variable} describes the underlying variable
#' for the coefficient. For categorical variables, the column \code{variable_category}
#' indicates the particular category of that variable for which a coefficient is estimated. \cr
#'
#' The columns \code{estimated_coefficient}, \code{se_coefficient},
#' \code{conf_intrvl_lower}, \code{conf_intrvl_upper}, and \code{p_value_coefficient}
#' are summary statistics for the estimated coefficient. Note that \code{p_value_coefficient}
#' is based on the Wald t-test for the coefficient. \cr
#'
#' The column \code{variable_level_p_value} gives the p-value of the
#' Rao-Scott Likelihood Ratio Test for including the variable in the model.
#' This likelihood ratio test has its test statistic given by the column
#' \code{LRT_chisq_statistic}, and the reference distribution
#' for this test is a linear combination of \code{p} F-distributions
#' with numerator degrees of freedom given by \code{LRT_df_numerator}
#' and denominator degrees of freedom given by \code{LRT_df_denominator},
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
#' survey_design <- svydesign(
#'   weights = ~BASE_WEIGHT,
#'   strata = ~SCHOOL_DISTRICT,
#'   id = ~ SCHOOL_ID + UNIQUE_ID,
#'   fpc = ~ N_SCHOOLS_IN_DISTRICT + N_STUDENTS_IN_SCHOOL,
#'   data = involvement_survey_str2s
#' )
#'
#' predict_outcome_via_glm(
#'   survey_design = survey_design,
#'   outcome_variable = "WHETHER_PARENT_AGREES",
#'   outcome_type = "binary",
#'   outcome_to_predict = "AGREE",
#'   model_selection = "main-effects",
#'   numeric_predictors = c("STUDENT_AGE"),
#'   categorical_predictors = c("STUDENT_DISABILITY_CATEGORY", "PARENT_HAS_EMAIL")
#' )
#'
predict_outcome_via_glm <- function(survey_design,
                                    outcome_variable,
                                    outcome_type = "continuous",
                                    outcome_to_predict = NULL,
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

  ## _ Outcome variable

  if (!is.character(outcome_variable) || length(outcome_variable) != 1) {
    stop("`outcome_variable` must be a single column name supplied as a character vector.")
  }

  if (!outcome_variable %in% colnames(survey_design[["variables"]])) {
    stop("`outcome_variable` is missing from the survey data.")
  }

  if (!is.character(outcome_type) || length(outcome_type) != 1 || !outcome_type %in% c("binary", "continuous")) {
    stop("`outcome_type` must be either 'binary' or 'continuous'.")
  }

  if (outcome_type == "binary") {
    if (is.factor(survey_design[["variables"]][[outcome_variable]])) {
      unique_outcomes <- levels(survey_design[["variables"]][[outcome_variable]])
    } else {
      unique_outcomes <- unique(survey_design[["variables"]][[outcome_variable]])
      unique_outcomes <- unique_outcomes[!is.na(unique_outcomes)]
    }

    if (length(unique_outcomes) != 2) {
      stop("When `outcome_type = 'binary'`, then `outcome_variable` must have two possible values.")
    }
    if (length(outcome_to_predict) != 1 || !outcome_to_predict %in% unique_outcomes) {
      stop(sprintf(
        "`outcome_to_predict` must be one of the values of `outcome_variable`: %s",
        sprintf("'%s' or '%s'", unique_outcomes[1], unique_outcomes[2])
      ))
    }
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

  if (outcome_type == "binary") {
    if (!is.factor(survey_design_for_model[["variables"]][[outcome_variable]])) {
      survey_design_for_model[["variables"]][[outcome_variable]] <- factor(
        x = survey_design_for_model[["variables"]][[outcome_variable]],
        levels = c(
          unique_outcomes[unique_outcomes != outcome_to_predict],
          outcome_to_predict
        )
      )
    }
    if (is.factor(survey_design_for_model[["variables"]][[outcome_variable]])) {
      survey_design_for_model[["variables"]][[outcome_variable]] <- relevel(
        x = survey_design_for_model[["variables"]][[outcome_variable]],
        ref = unique_outcomes[unique_outcomes != outcome_to_predict]
      )
    }
  }


  # Select the variables for the model ----

  regression_family <- switch(outcome_type,
    "binary" = quasibinomial("logit"),
    "continuous" = gaussian()
  )

  if (model_selection == "main-effects") {
    model_variables <- predictor_variables
    model_formula <- reformulate(
      response = as.name(outcome_variable),
      termlabels = model_variables
    )
  }

  if (model_selection == "stepwise") {
    message("Beginning stepwise model selection.")

    stepwise_model <- stepwise_model_selection(
      survey_design_for_model,
      outcome_variable = outcome_variable,
      predictor_variables = predictor_variables,
      model_type = switch(outcome_type,
        "binary" = "binary-logistic",
        "continuous" = "normal"
      ),
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
      response = as.name(outcome_variable),
      termlabels = model_variables
    )
  }

  final_model <- survey::svyglm(
    formula = model_formula,
    design = survey_design_for_model,
    family = regression_family
  )

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
    by = "name_coefficient",
    relationship = "many-to-many"
  )
  combined_summary_table <- dplyr::full_join(
    x = anova_table,
    y = coefs_table,
    by = "term",
    relationship = "many-to-many"
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

  # For categorical predictors, get a list of the reference levels
  categorical_predictor_levels <- lapply(names(final_model$xlevels), \(name) {
    data.frame(variable = name,
               variable_category = final_model$xlevels[[name]],
               stringsAsFactors = FALSE)
  }) |> do.call(what = rbind)

  reference_levels <- NULL
  if (!is.null(categorical_predictor_levels)) {
    reference_levels <- dplyr::anti_join(
      x = categorical_predictor_levels,
      y = combined_summary_table,
      by = c("variable", "variable_category")
    )
  }

  attr(combined_summary_table, 'reference_levels') <- reference_levels

  # Return result
  return(combined_summary_table)
}
