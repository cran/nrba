#' Test of differences in survey percentages relative to external estimates
#'
#' @description Compare estimated percentages from the present survey to external estimates from a benchmark source.
#' A Chi-Square test with Rao-Scott's second-order adjustment is used to evaluate whether the survey's estimates differ from the external estimates.
#' @param survey_design A survey design object created with the `survey` package.
#' @param y_var Name of dependent categorical variable.
#' @param ext_ests A numeric vector containing the external estimate of the percentages for each category.
#' The vector must have names, each name corresponding to a given category.
#' @param na.rm Whether to drop cases with missing values
#'
#' @return A data frame containing the results of the Chi-Square test(s) of whether survey-based estimates systematically differ from external estimates.
#' \cr
#' \cr
#' The columns of the output dataset include: \cr
#' - \code{statistic}: The value of the test statistic \cr
#' - \code{df}: Degrees of freedom for the reference Chi-Squared distribution \cr
#' - \code{scale}: Estimated scale parameter.
#' - \code{p_value}: The p-value of the test of independence \cr
#' - \code{test_method}: Text giving the name of the statistical test
#' - \code{variance_method}: Text describing the method of variance estimation
#'
#' @details Please see \link[survey]{svygofchisq} for details of how the Rao-Scott second-order adjusted test is conducted.
#' The test statistic, \code{statistic} is obtained by calculating the Pearson Chi-squared statistic for the estimated table of population totals. The reference distribution is a Satterthwaite approximation. The p-value is obtained by comparing \code{statistic}/\code{scale} to a Chi-squared distribution with \code{df} degrees of freedom.
#'
#' @references
#' - Rao, JNK, Scott, AJ (1984) "On Chi-squared Tests For Multiway Contigency Tables with Proportions Estimated From Survey Data" Annals of Statistics 12:46-60. \cr
#'
#' @export
#'
#' @examples
#'
#' library(survey)
#'
#' # Create a survey design ----
#' data("involvement_survey_pop", package = "nrba")
#' data("involvement_survey_str2s", package = "nrba")
#'
#' involvement_survey_sample <- svydesign(
#'   data = involvement_survey_str2s,
#'   weights = ~BASE_WEIGHT,
#'   strata = ~SCHOOL_DISTRICT,
#'   ids = ~ SCHOOL_ID + UNIQUE_ID,
#'   fpc = ~ N_SCHOOLS_IN_DISTRICT + N_STUDENTS_IN_SCHOOL
#' )
#'
#' # Subset to only include survey respondents ----
#'
#' involvement_survey_respondents <- subset(
#'   involvement_survey_sample,
#'   RESPONSE_STATUS == "Respondent"
#' )
#'
#' # Test whether percentages of categorical variable differ from benchmark ----
#'
#' parent_email_benchmark <- c(
#'   "Has Email" = 0.85,
#'   "No Email" = 0.15
#' )
#'
#' chisq_test_vs_external_estimate(
#'   survey_design = involvement_survey_respondents,
#'   y_var = "PARENT_HAS_EMAIL",
#'   ext_ests = parent_email_benchmark
#' )
#'
chisq_test_vs_external_estimate <- function(survey_design,
                                            y_var,
                                            ext_ests,
                                            na.rm = TRUE) {

  # Parameter checks

  ## _ survey_design

  if (!any(c("survey.design", "svyrep.design") %in% class(survey_design))) {
    stop("`survey_design` should be a survey design object created using the `survey` or `srvyr` package.\nUse `help('svydesign', package = 'survey')` to see how.")
  }

  # Input checks

  ## _ Dependent variables

  if (is.null(y_var) || !is.character(y_var) || length(y_var) != 1) {
    stop("`y_var` must be a character string giving the name of a variable in the data.")
  }

  if (!all(y_var %in% colnames(survey_design))) {
    missing_y_vars <- setdiff(y_var, colnames(survey_design))
    error_msg <- sprintf(
      "The specified `y_var`, %s, is not one of the variables in the survey design object.",
      missing_y_vars
    )
    stop(error_msg)
  }

  ## _ External estimates

  y_var_class <- class(survey_design[["variables"]][[y_var]])
  if (any(y_var_class == "factor")) {
    y_var_categories <- levels(survey_design[["variables"]][[y_var]])
  } else if (any(y_var_class == "character")) {
    y_var_categories <- sort(unique(survey_design[["variables"]][[y_var]]))
  }

  ext_ests_missing_names <- is.null(names(ext_ests)) || any(!y_var_categories %in% names(ext_ests))
  if (ext_ests_missing_names || length(ext_ests) != length(y_var_categories)) {
    error_msg <- sprintf(
      "The specified `y_var` is a categorical variable, of type '%s'.",
      y_var_class
    )
    error_msg <- paste(error_msg,
      "In this case, `ext_means` must be a named vector with names matching the categories:",
      paste(sprintf('"%s"', y_var_categories),
        collapse = "; "
      ),
      sep = "\n"
    )

    stop(error_msg)
  }

  if (any(is.na(ext_ests)) || any(is.infinite(ext_ests))) {
    stop("Values for external estimates must be positive, non-zero numbers.")
  }

  ### _ Potentially rescale external estimates to sum to 1 for categorical variables

  sum_of_ext_ests <- sum(ext_ests)
  ext_ests_sum_to_one <- sum_of_ext_ests == 1

  if (!ext_ests_sum_to_one) {
    warning("Rescaling values of external estimates to sum to 1, since outcome is categorical.")
    ext_ests <- setNames(
      ext_ests / sum_of_ext_ests,
      names(ext_ests)
    )
  }

  null_proportions <- ext_ests[y_var_categories]

  # Calculate test statistics

  formula_string <- sprintf("~ %s", y_var)
  chisq_formula <- as.formula(formula_string)

  htest_output <- survey::svygofchisq(
    design = survey_design,
    formula = chisq_formula,
    p = null_proportions,
    na.rm = na.rm
  )

  # Get a text string describing method of variance estimation
  variance_method <- get_variance_method(survey_design)

  # Format results into a data frame
  result_df <- data.frame(
    "statistic" = htest_output[["statistic"]],
    "df" = htest_output[["parameter"]][["df"]],
    "scale" = htest_output[["parameter"]][["scale"]],
    "p_value" = htest_output[["p.value"]],
    "test_method" = "Rao-Scott Chi-Square goodness-of-fit test",
    "variance_method" = variance_method
  )
  rownames(result_df) <- NULL
  return(result_df)
}
