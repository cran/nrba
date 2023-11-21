#' Test the independence of survey response and auxiliary variables
#'
#' @description Tests whether response status among eligible sample cases is independent of categorical auxiliary variables,
#' using a Chi-Square test with Rao-Scott's second-order adjustment.
#' If the data include cases known to be ineligible or who have unknown eligibility status,
#' the data are subsetted to only include respondents and nonrespondents known to be eligible.
#' @param survey_design A survey design object created with the `survey` package.
#' @param status A character string giving the name of the variable representing response/eligibility status.
#' The \code{status} variable should have at most four categories,
#' representing eligible respondents (ER), eligible nonrespondents (EN),
#' known ineligible cases (IE), and cases whose eligibility is unknown (UE).
#' @param status_codes A named vector, with four entries named 'ER', 'EN', 'IE', and 'UE'. \cr
#' \code{status_codes} indicates how the values of the \code{status} variable are to be interpreted.
#' @param aux_vars A list of names of auxiliary variables.
#'
#' @return A data frame containing the results of the Chi-Square test(s) of independence between response status and each auxiliary variable.
#' If multiple auxiliary variables are specified, the output data contains one row per auxiliary variable.
#' \cr
#' \cr
#' The columns of the output dataset include: \cr
#' - \code{auxiliary_variable}: The name of the auxiliary variable tested \cr
#' - \code{statistic}: The value of the test statistic \cr
#' - \code{ndf}: Numerator degrees of freedom for the reference distribution \cr
#' - \code{ddf}: Denominator degrees of freedom for the reference distribution \cr
#' - \code{p_value}: The p-value of the test of independence \cr
#' - \code{test_method}: Text giving the name of the statistical test
#' - \code{variance_method}: Text describing the method of variance estimation
#'
#' @details Please see \link[survey]{svychisq} for details of how the Rao-Scott second-order adjusted test is conducted.
#'
#' @references
#' - Rao, JNK, Scott, AJ (1984) "On Chi-squared Tests For Multiway Contigency Tables with Proportions Estimated From Survey Data" Annals of Statistics 12:46-60. \cr
#'
#' @export
#'
#' @examples
#'
#' # Create a survey design object ----
#' library(survey)
#' data(involvement_survey_srs, package = "nrba")
#'
#' involvement_survey <- svydesign(
#'   weights = ~BASE_WEIGHT,
#'   id = ~UNIQUE_ID,
#'   data = involvement_survey_srs
#' )
#'
#'
#' # Test whether response status varies by race or by sex ----
#'
#' test_results <- chisq_test_ind_response(
#'   survey_design = involvement_survey,
#'   status = "RESPONSE_STATUS",
#'   status_codes = c(
#'     "ER" = "Respondent",
#'     "EN" = "Nonrespondent",
#'     "UE" = "Unknown",
#'     "IE" = "Ineligible"
#'   ),
#'   aux_vars = c("STUDENT_RACE", "STUDENT_SEX")
#' )
#'
#' print(test_results)
chisq_test_ind_response <- function(survey_design, status, status_codes = c("ER", "EN", "UE", "IE"), aux_vars) {

  # Parameter checks

  ## _ survey_design

  if (!any(c("survey.design", "svyrep.design") %in% class(survey_design))) {
    stop("`survey_design` should be a survey design object created using the `survey` or `srvyr` package.\nUse `help('svydesign', package = 'survey')` to see how.")
  }

  ## _ Status variable and codes

  if (missing(status)) {
    stop("A variable name must be supplied to the `status` parameter")
  }

  if (!is.character(status) || length(status) != 1) {
    stop("Must specify a single variable name for `status`")
  }
  if (!status %in% colnames(survey_design[["variables"]])) {
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

  ##_ Auxiliary variables

  if (missing(aux_vars) || is.null(aux_vars)) {
    stop("Must specify variable names for `aux_vars`.")
  } else {
    if (!all(aux_vars %in% colnames(survey_design[["variables"]]))) {
      stop("Some of the specified `aux_vars` do not appear in `survey_design`.")
    }
  }

  # Subset the survey design object if necessary

  survey_design$variables[["__STATUS__"]] <- survey_design$variables[[status]]

  orig_n_rows <- nrow(survey_design)

  survey_design <- subset(
    survey_design,
    survey_design$variables[["__STATUS__"]] %in% status_codes[c("EN", "ER")]
  )

  if (nrow(survey_design) != orig_n_rows) {
    data_subset <- sprintf(
      "`%s` in (%s)",
      status,
      paste(sprintf("'%s'", status_codes[c("ER", "EN")]),
        collapse = ","
      )
    )
    subsetting_message <- sprintf(
      "Subsetting to only compare eligible respondents to eligible nonrespondents: %s",
      data_subset
    )
    message(subsetting_message)
  }

  # Calculate test statistics

  test_results <- lapply(
    X = setNames(aux_vars, aux_vars),
    FUN = function(aux_var) {
      formula_string <- sprintf(
        "~ %s + %s",
        status,
        aux_var
      )
      chisq_formula <- as.formula(formula_string)

      result <- survey::svychisq(
        design = survey_design,
        formula = chisq_formula,
        statistic = "F" # Specifies the Rao-Scott second-order correction
      )

      return(result)
    }
  )

  # Get a text string describing method of variance estimation
  variance_method <- get_variance_method(survey_design)

  # Format results into a data frame
  test_results <- lapply(test_results, function(htest_obj) {
    result_df <- data.frame(
      "statistic" = htest_obj[["statistic"]],
      "ndf" = htest_obj[["parameter"]][["ndf"]],
      "ddf" = htest_obj[["parameter"]][["ddf"]],
      "p_value" = htest_obj[["p.value"]],
      "test_method" = "Rao-Scott Chi-Square test (second-order adjustment)",
      "variance_method" = variance_method
    )
    rownames(result_df) <- NULL
    return(result_df)
  })

  test_results <- dplyr::bind_rows(test_results, .id = "auxiliary_variable")

  return(test_results)
}
