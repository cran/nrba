#' Select and fit a model using stepwise regression
#'
#' @param survey_design A survey design object created with the `survey` package.
#' @param outcome_variable The name of an outcome variable to use as the dependent variable.
#' @param predictor_variables A list of names of variables to consider as predictors for the model.
#' @param model_type A character string describing the type of model to fit.
#' \code{'binary-logistic'} for a binary logistic regression,
#' \code{'ordinal-logistic'} for an ordinal logistic regression (cumulative proportional-odds),
#' \code{'normal'} for the typical model which assumes residuals follow a Normal distribution.
#' @param max_iterations Maximum number of iterations to try adding new variables to the model.
#' @param alpha_enter The maximum p-value allowed for a variable to be added to the model.
#' Large values such as 0.5 or greater are recommended to reduce the bias
#' of estimates from the selected model.
#' @param alpha_remain The maximum p-value allowed for a variable to remain in the model.
#' Large values such as 0.5 or greater are recommended to reduce the bias
#' of estimates from the selected model.
#'
#' @return An object of class \code{\link[survey]{svyglm}} representing
#' a regression model fit using the 'survey' package.
#'
#' @description A regression model is selected by iteratively adding and removing variables based on the p-value from a
#' likelihood ratio rest. At each stage, a single variable is added to the model if
#' the p-value of the likelihood ratio test from adding the variable is below \code{alpha_enter}
#' and its p-value is less than that of all other variables not already in the model.
#' Next, of the variables already in the model, the variable with the largest p-value
#' is dropped if its p-value is greater than \code{alpha_remain}. This iterative process
#' continues until a maximum number of iterations is reached or until
#' either all variables have been added to the model or there are no variables
#' not yet in the model whose likelihood ratio test has a p-value below \code{alpha_enter}. \cr
#'
#' Stepwise model selection generally invalidates inferential statistics
#' such as p-values, standard errors, or confidence intervals and leads to
#' overestimation of the size of coefficients for variables included in the selected model.
#' This bias increases as the value of \code{alpha_enter} or \code{alpha_remain} decreases.
#' The use of stepwise model selection should be limited only to
#' reducing a large list of candidate variables for nonresponse adjustment.
#'
#' @details
#' See Lumley and Scott (2017) for details of how regression models are fit to survey data.
#' For overall tests of variables, a Rao-Scott Likelihood Ratio Test is conducted
#' (see section 4 of Lumley and Scott (2017) for statistical details)
#' using the function \code{regTermTest(method = "LRT", lrt.approximation = "saddlepoint")}
#' from the 'survey' package.
#'
#' See Sauerbrei et al. (2020) for a discussion of statistical issues with using stepwise model selection.
#'
#' @references
#' - Lumley, T., & Scott A. (2017). Fitting Regression Models to Survey Data. Statistical Science 32 (2) 265 - 278. https://doi.org/10.1214/16-STS605
#' - Sauerbrei, W., Perperoglou, A., Schmid, M. et al. (2020). State of the art in selection of variables and functional forms in multivariable analysis - outstanding issues. Diagnostic and Prognostic Research 4, 3. https://doi.org/10.1186/s41512-020-00074-3
#'
#' @export
#'
#' @examples
#' library(survey)
#'
#' # Load example data and prepare it for analysis
#' data(involvement_survey_str2s, package = 'nrba')
#'
#' involvement_survey <- svydesign(
#'   data = involvement_survey_str2s,
#'   ids = ~ SCHOOL_ID + UNIQUE_ID,
#'   fpc = ~ N_SCHOOLS_IN_DISTRICT + N_STUDENTS_IN_SCHOOL,
#'   strata = ~ SCHOOL_DISTRICT,
#'   weights = ~ BASE_WEIGHT
#' )
#'
#' involvement_survey <- involvement_survey |>
#'     transform(WHETHER_PARENT_AGREES = factor(WHETHER_PARENT_AGREES))
#'
#' # Fit a regression model using stepwise selection
#' selected_model <- stepwise_model_selection(
#'   survey_design = involvement_survey,
#'   outcome_variable = "WHETHER_PARENT_AGREES",
#'   predictor_variables = c("STUDENT_RACE", "STUDENT_DISABILITY_CATEGORY"),
#'   model_type = "binary-logistic",
#'   max_iterations = 100,
#'   alpha_enter = 0.5,
#'   alpha_remain = 0.5
#' )

stepwise_model_selection <- function(survey_design,
                                     outcome_variable,
                                     predictor_variables,
                                     model_type = "binary-logistic",
                                     max_iterations = 100L,
                                     alpha_enter = 0.5,
                                     alpha_remain = 0.5) {


  ##_ First determine initial variable to use ----
  one_way_model_pvalues <- vector('numeric',
                                  length = length(predictor_variables))
  names(one_way_model_pvalues) <- predictor_variables

  for (var_name in predictor_variables) {

    model_formula <- reformulate(response = as.name(outcome_variable),
                                 termlabels = var_name)

    if (model_type == "binary-logistic") {
      fitted_model <- survey::svyglm(formula = model_formula,
                                     design = survey_design,
                                     family = quasibinomial(link = 'logit'))
    }
    if (model_type == "normal") {
      fitted_model <- survey::svyglm(formula = model_formula,
                                     design = survey_design,
                                     family = gaussian())
    }
    if (model_type == "ordinal-logistic") {
      fitted_model <- survey::svyolr(formula = model_formula,
                                     design = survey_design,
                                     method = 'logistic')
    }

    one_way_model_pvalues[var_name] <- fitted_model |>
      survey::regTermTest(test.terms = var_name,
                          method = "LRT") |>
      getElement("p") |> as.vector()
  }

  min_oneway_pvalue <- one_way_model_pvalues[which.min(one_way_model_pvalues)]

  if (min_oneway_pvalue < alpha_enter) {
    model_variables <- names(min_oneway_pvalue)
    stop_criterion_reached <- FALSE
  } else {
    stop_criterion_reached <- TRUE
    model_variables <- vector('character', length = 0L)
  }

  ##_ Iteratively add/remove additional variables ----

  iteration_number <- 0L

  while (!stop_criterion_reached) {

    iteration_number <- iteration_number + 1L

    ###__ Determine additional variable to add ----

    potential_new_variables <- setdiff(predictor_variables, model_variables)

    multiple_reg_model_pvalues <- vector(
      'numeric',
      length = length(potential_new_variables)
    )
    names(multiple_reg_model_pvalues) <- potential_new_variables

    for (var_name in potential_new_variables) {

      model_formula <- reformulate(response = as.name(outcome_variable),
                                   termlabels = c(model_variables,
                                                  var_name))

      if (model_type == "binary-logistic") {
        fitted_model <- survey::svyglm(formula = model_formula,
                                       design = survey_design,
                                       family = quasibinomial(link = 'logit'))
      }
      if (model_type == "normal") {
        fitted_model <- survey::svyglm(formula = model_formula,
                                       design = survey_design,
                                       family = gaussian())
      }
      if (model_type == "ordinal-logistic") {
        fitted_model <- survey::svyolr(formula = model_formula,
                                       design = survey_design,
                                       method = 'logistic')
      }

      multiple_reg_model_pvalues[var_name] <- fitted_model |>
        survey::regTermTest(test.terms = var_name,
                            method = "LRT") |>
        getElement("p") |> as.vector()
    }

    min_pvalue <- multiple_reg_model_pvalues[which.min(multiple_reg_model_pvalues)]

    if (min_pvalue >= alpha_enter) {
      stop_criterion_reached <- TRUE
    } else if (min_pvalue < alpha_enter) {

      ###__ Add new variable to list of model variables ----
      new_variable <- names(min_pvalue)
      model_variables <- c(model_variables, new_variable)

      ###__ Now see if previous model variables need to be dropped ----
      model_formula <- reformulate(response = as.name(outcome_variable),
                                   termlabels = model_variables)

      if (model_type == "binary-logistic") {
        fitted_model <- survey::svyglm(formula = model_formula,
                                       design = survey_design,
                                       family = quasibinomial(link = 'logit'))
      }
      if (model_type == "normal") {
        fitted_model <- survey::svyglm(formula = model_formula,
                                       design = survey_design,
                                       family = gaussian())
      }
      if (model_type == "ordinal-logistic") {
        fitted_model <- survey::svyolr(formula = model_formula,
                                       design = survey_design,
                                       method = 'logistic')
      }

      pvalues_of_prev_variables <- sapply(
        setdiff(model_variables, new_variable), function(var_name) {
          survey::regTermTest(model = fitted_model,
                              test.terms = var_name) %>%
            getElement("p") %>% as.vector()
        }
      )

      variables_to_drop <- names(pvalues_of_prev_variables)[
        pvalues_of_prev_variables >= alpha_remain
      ]

      model_variables <- setdiff(model_variables, variables_to_drop)
    }

    ###__ Check whether to keep iterating ----
    if (!stop_criterion_reached) {
      stop_criterion_reached <- all(predictor_variables %in% model_variables)
    }
    if (!stop_criterion_reached) {
      stop_criterion_reached <- iteration_number == max_iterations
    }
  }

##_ Fit the final model ----
  if (length(model_variables) == 0) {
    model_variables <- "1"
  }
  model_formula <- reformulate(response = as.name(outcome_variable),
                               termlabels = model_variables)
  if (model_type == "binary-logistic") {
    final_model <- survey::svyglm(formula = model_formula, design = survey_design,
                                  family = quasibinomial(link = 'logit'))
  }
  if (model_type == "ordinal-logistic") {
    final_model <- survey::svyolr(formula = model_formula, design = survey_design,
                                  method = 'logistic')
  }
  if (model_type == "normal") {
    final_model <- survey::svyglm(formula = model_formula, design = survey_design,
                                  family = gaussian(link = 'identity'))
  }
  return(final_model)
}

