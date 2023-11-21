#' @name t_test_by_response_status
#'
#' @title t-test of differences in means/percentages between responding sample and full sample, or between responding sample and eligible sample
#' @description The function \code{t_test_resp_vs_full} tests whether means of auxiliary variables differ between respondents and the full selected sample,
#' where the full sample consists of all cases regardless of response status or eligibility status. \cr
#' The function \code{t_test_resp_vs_elig} tests whether means differ between the responding sample and the eligible sample,
#' where the eligible sample consists of all cases known to be eligible, regardless of response status.
#'
#' See Lohr and Riddles (2016) for the statistical theory of this test.
#' @param survey_design A survey design object created with the `survey` package.
#' @param y_vars Names of dependent variables for tests. For categorical variables, percentages of each category are tested.
#' @param na.rm Whether to drop cases with missing values for a given dependent variable.
#' @param status The name of the variable representing response/eligibility status. \cr
#' The \code{status} variable should have at most four categories,
#' representing eligible respondents (ER), eligible nonrespondents (EN),
#' known ineligible cases (IE), and cases whose eligibility is unknown (UE).
#' @param status_codes A named vector, with four entries named 'ER', 'EN', 'IE', and 'UE'. \cr
#' \code{status_codes} indicates how the values of the \code{status} variable are to be interpreted.
#' @param null_difference The difference between the two means under the null hypothesis. Default is \code{0}.
#' @param alternative Can be one of the following: \cr
#' - \code{'unequal'} (the default): two-sided test of whether difference in means is equal to \code{null_difference} \cr
#' - \code{'less'}: one-sided test of whether difference is less than \code{null_difference} \cr
#' - \code{'greater'}: one-sided test of whether difference is greater than \code{null_difference}
#' @param degrees_of_freedom The degrees of freedom to use for the test's reference distribution.
#' Unless specified otherwise, the default is the design degrees of freedom minus one,
#' where the design degrees of freedom are estimated using the \code{survey} package's \code{degf} method.
#'
#' @return A data frame describing the results of the t-tests, one row per dependent variable.
#'
#' @section Statistical Details:
#' The t-statistic used for the test has as its numerator the difference in means between the two samples, minus the \code{null_difference}.
#' The denominator for the t-statistic is the estimated standard error of the difference in means.
#' Because the two means are based on overlapping groups and thus have correlated sampling errors, special care is taken to estimate the covariance of the two estimates.
#' For designs which use sets of replicate weights for variance estimation, the two means and their difference are estimated using each set of replicate weights;
#' the estimated differences from the sets of replicate weights are then used to estimate sampling error with a formula appropriate to the replication method (JKn, BRR, etc.).
#' For designs which use linearization methods for variance estimation, the covariance between the two means is estimated using the method of linearization based on influence functions implemented in the \code{survey} package.
#' See Osier (2009) for an overview of the method of linearization based on influence functions.
#' Eckman et al. (2023) showed in a simulation study that linearization and replication
#' performed similarly in estimating the variance of a difference in means for overlapping samples. \cr
#' \cr
#' Unless specified otherwise using the \code{degrees_of_freedom} parameter, the degrees of freedom for the test are set to the design degrees of freedom minus one.
#' Design degrees of freedom are estimated using the \code{survey} package's \code{degf} method. \cr
#' \cr
#' See Lohr and Riddles (2016) for the statistical details of this test.
#' See Van de Kerckhove et al. (2009) and  Amaya and Presser (2017)
#' for examples of a nonresponse bias analysis which uses t-tests to compare responding samples to eligible samples.
#'
#' @references
#' - Amaya, A., Presser, S. (2017). \emph{Nonresponse Bias for Univariate and Multivariate Estimates of Social Activities and Roles}. Public Opinion Quarterly, Volume 81, Issue 1, 1 March 2017, Pages 1–36, https://doi.org/10.1093/poq/nfw037
#' - Eckman, S., Unangst, J., Dever, J., Antoun, A. (2023). \emph{The Precision of Estimates of Nonresponse Bias in Means}. Journal of Survey Statistics and Methodology, 11(4), 758-783. https://doi.org/10.1093/jssam/smac019
#' - Lohr, S., Riddles, M. (2016). \emph{Tests for Evaluating Nonresponse Bias in Surveys}. Survey Methodology 42(2): 195-218. https://www150.statcan.gc.ca/n1/pub/12-001-x/2016002/article/14677-eng.pdf
#' - Osier, G. (2009). \emph{Variance estimation for complex indicators of poverty and inequality using linearization techniques}. Survey Research Methods, 3(3), 167-195. https://doi.org/10.18148/srm/2009.v3i3.369
#' - Van de Kerckhove, W., Krenzke, T., and Mohadjer, L. (2009). \emph{Adult Literacy and Lifeskills Survey (ALL) 2003: U.S. Nonresponse Bias Analysis (NCES 2009-063)}. National Center for Education Statistics, Institute of Education Sciences, U.S. Department of Education. Washington, DC.
#' @examples
#' library(survey)
#'
#' # Create a survey design ----
#' data(involvement_survey_srs, package = 'nrba')
#'
#' survey_design <- svydesign(weights = ~ BASE_WEIGHT,
#'                            id = ~ UNIQUE_ID,
#'                            fpc = ~ N_STUDENTS,
#'                            data = involvement_survey_srs)
#'
#' # Compare respondents' mean to the full sample mean ----
#'
#' t_test_resp_vs_full(survey_design = survey_design,
#'                     y_vars = c("STUDENT_AGE", "WHETHER_PARENT_AGREES"),
#'                     status = 'RESPONSE_STATUS',
#'                     status_codes = c('ER' = "Respondent",
#'                                      'EN' = "Nonrespondent",
#'                                      'IE' = "Ineligible",
#'                                      'UE' = "Unknown"))
#'
#' @rdname t_test_by_response_status
#' @export
t_test_resp_vs_full <- function(survey_design,
                                y_vars,
                                na.rm = TRUE,
                                status,
                                status_codes = c('ER', 'EN', 'IE', 'UE'),
                                null_difference = 0, alternative = "unequal",
                                degrees_of_freedom = survey::degf(survey_design) - 1) {
  # Parameter checks ----

    ##_ Status variable and codes

    if (missing(status)) {
      stop("A variable name must be supplied to the `status` parameter")
    }

    if (!is.character(status) || length(status) != 1) {
      stop("Must specify a single variable name for `status`")
    }
    if (!status %in% colnames(survey_design)) {
      stop(sprintf("The status variable '%s' does not appear in the supplied data",
                   status))
    }

    if (missing(status_codes) || is.null(status_codes)) {
      stop("Must supply status codes to the `status_codes` parameter.")
    }

    if (!((is.vector(status_codes) | is.factor(status_codes)) && !is.list(status_codes) )  || is.null(names(status_codes))) {
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

    valid_status_values <- all(survey_design[['variables']][[status]] %in% status_codes)
    if (!valid_status_values) {
      stop("The data contains values in the status variable which are not listed in `status_codes`.")
    }

  # Use an internal function to get the test statistics

  test_output <- t_test_overlap(survey_design = survey_design,
                                y_vars = y_vars,
                                status = status,
                                group_1 = status_codes['ER'],
                                group_2 = status_codes,
                                null_difference = null_difference,
                                alternative = alternative,
                                degrees_of_freedom = degrees_of_freedom)

  # Rename columns in output for ease-of-use
  colnames(test_output) <- gsub(x = colnames(test_output),
                                pattern = "group_1", replacement = "resp")
  colnames(test_output) <- gsub(x = colnames(test_output),
                                pattern = "group_2", replacement = "full_sample")

  return(test_output)
}

#' @rdname t_test_by_response_status
#' @export
#' @examples
#' # Compare respondents' mean to the mean of all eligible cases ----
#'
#' t_test_resp_vs_full(survey_design = survey_design,
#'                     y_vars = c("STUDENT_AGE", "WHETHER_PARENT_AGREES"),
#'                     status = 'RESPONSE_STATUS',
#'                     status_codes = c('ER' = "Respondent",
#'                                      'EN' = "Nonrespondent",
#'                                      'IE' = "Ineligible",
#'                                      'UE' = "Unknown"))
#' # One-sided tests ----
#'
#'   ## Null Hypothesis: Y_bar_resp - Y_bar_full <= 0.1
#'   ## Alt. Hypothesis: Y_bar_resp - Y_bar_full >  0.1
#'
#' t_test_resp_vs_full(survey_design = survey_design,
#'                     y_vars = c("STUDENT_AGE", "WHETHER_PARENT_AGREES"),
#'                     status = 'RESPONSE_STATUS',
#'                     status_codes = c('ER' = "Respondent",
#'                                      'EN' = "Nonrespondent",
#'                                      'IE' = "Ineligible",
#'                                      'UE' = "Unknown"),
#'                     null_difference = 0.1, alternative = 'greater')
#'
#'   ## Null Hypothesis: Y_bar_resp - Y_bar_full >= 0.1
#'   ## Alt. Hypothesis: Y_bar_resp - Y_bar_full <  0.1
#'
#' t_test_resp_vs_full(survey_design = survey_design,
#'                     y_vars = c("STUDENT_AGE", "WHETHER_PARENT_AGREES"),
#'                     status = 'RESPONSE_STATUS',
#'                     status_codes = c('ER' = "Respondent",
#'                                      'EN' = "Nonrespondent",
#'                                      'IE' = "Ineligible",
#'                                      'UE' = "Unknown"),
#'                     null_difference = 0.1, alternative = 'less')
t_test_resp_vs_elig <- function(survey_design,
                                y_vars,
                                na.rm = TRUE,
                                status,
                                status_codes = c('ER', 'EN', 'IE', 'UE'),
                                null_difference = 0, alternative = "unequal",
                                degrees_of_freedom = survey::degf(survey_design) - 1) {

  # Parameter checks ----

    ##_ Status variable and codes

    if (missing(status)) {
      stop("A variable name must be supplied to the `status` parameter")
    }

    if (!is.character(status) || length(status) != 1) {
      stop("Must specify a single variable name for `status`")
    }
    if (!status %in% colnames(survey_design)) {
      stop(sprintf("The status variable '%s' does not appear in the supplied data",
                   status))
    }

    if (missing(status_codes) || is.null(status_codes)) {
      stop("Must supply status codes to the `status_codes` parameter.")
    }

    if (!((is.vector(status_codes) | is.factor(status_codes)) && !is.list(status_codes) )  || is.null(names(status_codes))) {
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

    valid_status_values <- all(survey_design[['variables']][[status]] %in% status_codes)
    if (!valid_status_values) {
      stop("The data contains values in the status variable which are not listed in `status_codes`.")
    }

  # Use an internal function to get the test statistics

    test_output <- t_test_overlap(survey_design = survey_design,
                                  y_vars = y_vars,
                                  na.rm = na.rm,
                                  status = status,
                                  group_1 = status_codes['ER'],
                                  group_2 = status_codes[c('ER', 'EN')],
                                  null_difference = null_difference,
                                  alternative = alternative,
                                  degrees_of_freedom = degrees_of_freedom)

  # Rename columns in output for ease-of-use
    colnames(test_output) <- gsub(x = colnames(test_output),
                                  pattern = "group_1", replacement = "resp")
    colnames(test_output) <- gsub(x = colnames(test_output),
                                  pattern = "group_2", replacement = "elig")

  return(test_output)
}
