#' t-test of differences in means/percentages relative to external estimates
#'
#' @description Compare estimated means/percentages from the present survey to external estimates from a benchmark source.
#' A t-test is used to evaluate whether the survey's estimates differ from the external estimates.
#'
#' @param survey_design A survey design object created with the `survey` package.
#' @param y_var Name of dependent variable. For categorical variables, percentages of each category are tested.
#' @param ext_ests A numeric vector containing the external estimate of the mean for the dependent variable.
#' If \code{variable} is a categorical variable, a named vector of means must be provided.
#' @param ext_std_errors (Optional) The standard errors of the external estimates.
#' This is useful if the external data are estimated with an appreciable level of uncertainty,
#'  for instance if the external data come from a survey with a small-to-moderate sample size.
#' If supplied, the variance of the difference between the survey and external estimates
#' is estimated by adding the variance of the external estimates to the estimated variance
#' of the survey's estimates.
#' @param na.rm Whether to drop cases with missing values for \code{y_var}
#' @param null_difference The hypothesized difference between the estimate and the external mean. Default is \code{0}.
#' @param alternative Can be one of the following: \cr
#' - \code{'unequal'}: two-sided test of whether difference in means is equal to \code{null_difference} \cr
#' - \code{'less'}: one-sided test of whether difference is less than \code{null_difference} \cr
#' - \code{'greater'}: one-sided test of whether difference is greater than \code{null_difference}
#' @param degrees_of_freedom The degrees of freedom to use for the test's reference distribution.
#' Unless specified otherwise, the default is the design degrees of freedom minus one,
#' where the design degrees of freedom are estimated using the survey package's \code{degf} method.
#' @return A data frame describing the results of the t-tests, one row per mean being compared.
#' @references
#' See Brick and Bose (2001) for an example of this analysis method
#' and a discussion of its limitations.
#' - Brick, M., and Bose, J. (2001). \emph{Analysis of Potential Nonresponse Bias}. in
#'   Proceedings of the Section on Survey Research Methods. Alexandria, VA: American Statistical Association.
#'   http://www.asasrms.org/Proceedings/y2001/Proceed/00021.pdf
#'
#' @examples
#'
#'
#' library(survey)
#'
#' # Create a survey design ----
#' data("involvement_survey_str2s", package = 'nrba')
#'
#' involvement_survey_sample <- svydesign(
#'   data = involvement_survey_str2s,
#'   weights = ~ BASE_WEIGHT,
#'   strata =  ~ SCHOOL_DISTRICT,
#'   ids =     ~ SCHOOL_ID             + UNIQUE_ID,
#'   fpc =     ~ N_SCHOOLS_IN_DISTRICT + N_STUDENTS_IN_SCHOOL
#' )
#'
#' # Subset to only include survey respondents ----
#'
#' involvement_survey_respondents <- subset(involvement_survey_sample,
#'                                          RESPONSE_STATUS == "Respondent")
#'
#' # Test whether percentages of categorical variable differ from benchmark ----
#'
#' parent_email_benchmark <- c(
#'   'Has Email' = 0.85,
#'   'No Email' = 0.15
#' )
#'
#' t_test_vs_external_estimate(
#'   survey_design = involvement_survey_respondents,
#'   y_var = "PARENT_HAS_EMAIL",
#'   ext_ests = parent_email_benchmark
#' )
#'
#' # Test whether the sample mean differs from the population benchmark ----
#'
#' average_age_benchmark <- 11
#'
#' t_test_vs_external_estimate(
#'   survey_design = involvement_survey_respondents,
#'   y_var = "STUDENT_AGE",
#'   ext_ests = average_age_benchmark,
#'   null_difference = 0
#' )
#'
#' @export

t_test_vs_external_estimate <- function(survey_design, y_var,
                                        ext_ests, ext_std_errors = NULL,
                                        na.rm = TRUE,
                                        null_difference = 0, alternative = "unequal",
                                        degrees_of_freedom = survey::degf(survey_design) - 1) {

  # Input checks

  ##_ Dependent variables

    if (is.null(y_var) || !is.character(y_var) || length(y_var) != 1) {
      stop("`y_var` must be a character string giving the name of a variable in the data.")
    }

    if (!all(y_var %in% colnames(survey_design))) {
      missing_y_vars <- setdiff(y_var, colnames(survey_design))
      error_msg <- sprintf("The specified `y_var`, %s, is not one of the variables in the survey design object.",
                           y_var)
      stop(error_msg)
    }

  ##_ External estimate

    y_var_class <- class(survey_design[['variables']][[y_var]])
    y_var_is_categorical <- any(y_var_class %in% c("character", "factor"))
    if (y_var_is_categorical) {
      if (is.null(names(ext_ests))) {
        if (any(y_var_class == "factor")) {
          y_var_categories <- levels(survey_design[['variables']][[y_var]])
        } else if (any(y_var_class == "character")) {
          y_var_categories <- sort(unique(survey_design[['variables']][[y_var]]))
        }
        error_msg <- sprintf("The specified `y_var` is a categorical variable, of type '%s'.",
                             y_var_class)
        error_msg <- paste(error_msg,
                           "In this case, `ext_means` must be a named vector with names matching the categories:",
                           paste(sprintf('"%s"', y_var_categories),
                                 collapse = "; "),
                           sep = "\n")

        stop(error_msg)
      }
    }

    ###_ Potentially rescale external estimates to sum to 1 for categorical variables
    if (y_var_is_categorical) {
      sum_of_ext_ests <- sum(ext_ests)
      ext_ests_sum_to_one <- sum_of_ext_ests == 1
      need_to_rescale_ext_ests <- !ext_ests_sum_to_one

      if (need_to_rescale_ext_ests) {
        warning("Rescaling values of external estimates to sum to 1, since outcome is categorical.")
        ext_ests <- setNames(ext_ests / sum_of_ext_ests,
                             names(ext_ests))
      }
    }


  ##_ Degrees of freedom

    if (is.null(degrees_of_freedom)) {
      stop("Must specify degrees of freedom or use the default, `degf(survey_design)` - 1")
    }

    if (!is.numeric(degrees_of_freedom) || length(degrees_of_freedom) != 1) {
      stop("`degrees_of_freedom` must be a single number")
    }

    if (is.na(degrees_of_freedom)) {
      stop("`degrees_of_freedom` cannot be a missing value")
    }

  # Compute estimates and their standard error

    outcome_formula <- stats::reformulate(termlabels = y_var)

    estimate <- survey::svymean(x = outcome_formula, design = survey_design,
                                na.rm = na.rm)

    if (y_var_is_categorical) {
      estimate_names <- names(estimate)
      estimate_names <- sapply(estimate_names, function(var_nm) {
        substr(x = var_nm,
               start = nchar(y_var)+1,
               stop = nchar(var_nm))
      })
      names(estimate) <- estimate_names
      ext_ests <- ext_ests[estimate_names]
    }

    estimate_minus_ext_means <- estimate - ext_ests
    std_error <- survey::SE(estimate)

  # Account for variance of external estimates
  # (assuming independence)

    if (!is.null(ext_std_errors)) {
      if (y_var_is_categorical) {
        ext_std_errors <- ext_std_errors[estimate_names]

        if (need_to_rescale_ext_ests) {
          ext_std_errors <- setNames(ext_std_errors / sum_of_ext_ests,
                                     names(ext_std_errors))
        }
      }
      std_error <- sqrt(std_error^2 + ext_std_errors^2)
    }

  # Calculate statistics for t test
    t_statistic <- as.numeric(estimate_minus_ext_means - null_difference) / std_error

    if (alternative == "unequal") {
      p_value <- 2 * pt(q = abs(t_statistic),
                        df = degrees_of_freedom,
                        lower.tail = FALSE)
    }
    if (alternative == "less") {
      p_value <- pt(q = t_statistic,
                    df = degrees_of_freedom,
                    lower.tail = TRUE)
    }
    if (alternative == "greater") {
      p_value <- pt(q = t_statistic,
                    df = degrees_of_freedom,
                    lower.tail = FALSE)
    }

  # Prepare output

  result <- data.frame(
    'estimate' = as.numeric(estimate),
    'external_estimate' = as.numeric(ext_ests),
    'difference' = as.numeric(estimate_minus_ext_means),
    'std_error' = as.numeric(std_error),
    'p_value' = as.numeric(p_value),
    't_statistic' = as.numeric(t_statistic),
    'df' = degrees_of_freedom,
    row.names = NULL
  )

  if (y_var_is_categorical) {
    result <- cbind('category' = estimate_names,
                    result)
    rownames(result) <- NULL
  }

  return(result)
}
