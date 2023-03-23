#' @name t_test_of_weight_adjustment
#'
#' @title t-test of differences in estimated means/percentages from two different sets of replicate weights.
#' @description Tests whether estimates of means/percentages differ systematically between two sets of replicate weights:
#' an original set of weights, and the weights after adjustment (e.g. post-stratification or nonresponse adjustments) and possibly subsetting (e.g. subsetting to only include respondents).
#' @param orig_design A replicate design object created with the \code{survey} package.
#' @param updated_design A potentially updated version of \code{orig_design},
#' for example where weights have been adjusted for nonresponse or updated using post-stratification.
#' The type and number of sets of replicate weights must match that of \code{orig_design}.
#' The number of rows may differ (e.g. if \code{orig_design} includes the full sample but \code{updated_design} only includes respondents).
#' @param y_vars Names of dependent variables for tests. For categorical variables, percentages of each category are tested.
#' @param na.rm Whether to drop cases with missing values for a given dependent variable.
#' @param null_difference The difference between the two means/percentages under the null hypothesis. Default is \code{0}.
#' @param alternative Can be one of the following: \cr
#' - \code{'unequal'} (the default): two-sided test of whether difference in means is equal to \code{null_difference} \cr
#' - \code{'less'}: one-sided test of whether difference is less than \code{null_difference} \cr
#' - \code{'greater'}: one-sided test of whether difference is greater than \code{null_difference}
#' @param degrees_of_freedom The degrees of freedom to use for the test's reference distribution.
#' Unless specified otherwise, the default is the design degrees of freedom minus one,
#' where the design degrees of freedom are estimated using the \code{survey} package's \code{degf} method
#' applied to the 'stacked' design formed by combining \code{orig_design} and \code{updated_design}.
#'
#' @return A data frame describing the results of the t-tests, one row per dependent variable.
#'
#' @section Statistical Details:
#' The t-statistic used for the test has as its numerator the difference in means/percentages between the two samples, minus the \code{null_difference}.
#' The denominator for the t-statistic is the estimated standard error of the difference in means.
#' Because the two means are based on overlapping groups and thus have correlated sampling errors, special care is taken to estimate the covariance of the two estimates.
#' For designs which use sets of replicate weights for variance estimation, the two means and their difference are estimated using each set of replicate weights;
#' the estimated differences from the sets of replicate weights are then used to estimate sampling error with a formula appropriate to the replication method (JKn, BRR, etc.). \cr
#' \cr
#' This analysis is not implemented for designs which use linearization methods for variance estimation.
#' \cr
#' Unless specified otherwise using the \code{degrees_of_freedom} parameter, the degrees of freedom for the test are set to the design degrees of freedom minus one.
#' Design degrees of freedom are estimated using the \code{survey} package's \code{degf} method. \cr
#' \cr
#' See Van de Kerckhove et al. (2009) for an example of this type of nonresponse bias analysis (among others).
#' See Lohr and Riddles (2016) for the statistical details of this test.
#' @references
#' - Lohr, S., Riddles, M. (2016). \emph{Tests for Evaluating Nonresponse Bias in Surveys}. Survey Methodology 42(2): 195-218. https://www150.statcan.gc.ca/n1/pub/12-001-x/2016002/article/14677-eng.pdf
#'
#' - Van de Kerckhove, W., Krenzke, T., and Mohadjer, L. (2009). \emph{Adult Literacy and Lifeskills Survey (ALL) 2003: U.S. Nonresponse Bias Analysis (NCES 2009-063)}. National Center for Education Statistics, Institute of Education Sciences, U.S. Department of Education. Washington, DC.
#' @examples
#' \donttest{
#' library(survey)
#'
#' # Create a survey design ----
#'
#' data(involvement_survey_srs, package = 'nrba')
#'
#' survey_design <- svydesign(weights = ~ BASE_WEIGHT,
#'                            id = ~ UNIQUE_ID,
#'                            fpc = ~ N_STUDENTS,
#'                            data = involvement_survey_srs)
#'
#' # Create replicate weights for the design ----
#' rep_svy_design <- as.svrepdesign(survey_design, type = "subbootstrap",
#'                                  replicates = 500)
#'
#' # Subset to only respondents (always subset *after* creating replicate weights)
#'
#' rep_svy_respondents <- subset(rep_svy_design,
#'                               RESPONSE_STATUS == "Respondent")
#'
#' # Apply raking adjustment ----
#'
#' raked_rep_svy_respondents <- rake_to_benchmarks(
#'   survey_design = rep_svy_respondents,
#'   group_vars = c("PARENT_HAS_EMAIL", "STUDENT_RACE"),
#'   group_benchmark_vars = c("PARENT_HAS_EMAIL_BENCHMARK",
#'                            "STUDENT_RACE_BENCHMARK"),
#' )
#'
#' # Compare estimates from respondents in original vs. adjusted design ----
#'
#' t_test_of_weight_adjustment(orig_design = rep_svy_respondents,
#'                             updated_design = raked_rep_svy_respondents,
#'                             y_vars = c('STUDENT_AGE', 'STUDENT_SEX'))
#'
#' t_test_of_weight_adjustment(orig_design = rep_svy_respondents,
#'                             updated_design = raked_rep_svy_respondents,
#'                             y_vars = c('WHETHER_PARENT_AGREES'))
#'
#' # Compare estimates to true population values ----
#'
#' data('involvement_survey_pop', package = 'nrba')
#'
#' mean(involvement_survey_pop$STUDENT_AGE)
#'
#' prop.table(table(involvement_survey_pop$STUDENT_SEX))
#' }
#' @rdname t_test_of_weight_adjustment
#' @export
t_test_of_weight_adjustment <- function(orig_design,
                                        updated_design,
                                        y_vars, na.rm = TRUE,
                                        null_difference = 0, alternative = "unequal",
                                        degrees_of_freedom = NULL) {

  if (orig_design$type %in% c("subbootstrap", "mrbbootstrap")) {
    orig_design$type <- 'bootstrap'
  }
  if (updated_design$type %in% c("subbootstrap", "mrbbootstrap")) {
    updated_design$type <- 'bootstrap'
  }

  stacked_design <- svrep::stack_replicate_designs(
    'Original' = orig_design,
    'Adjusted' = updated_design,
    .id = '_which_design_'
  )

  if (is.null(degrees_of_freedom)) {
    degrees_of_freedom <- survey::degf(stacked_design) - 1L
  }

  # Use an internal function to get the test statistics

  test_output <- t_test_overlap(survey_design = stacked_design,
                                y_vars = y_vars,
                                na.rm = na.rm,
                                status = "_which_design_",
                                group_1 = "Original",
                                group_2 = "Adjusted",
                                null_difference = null_difference,
                                alternative = alternative,
                                degrees_of_freedom = degrees_of_freedom)

  # Rename columns in output for ease-of-use
  colnames(test_output) <- gsub(x = colnames(test_output),
                                pattern = "group_1", replacement = "Original")
  colnames(test_output) <- gsub(x = colnames(test_output),
                                pattern = "group_2", replacement = "Adjusted")

  return(test_output)
}
