#' Re-weight data to match population benchmarks, using raking or post-stratification
#'
#' @description Adjusts weights in the data to ensure that estimated population totals for
#' grouping variables match known population benchmarks. If there is only one grouping variable,
#' simple post-stratification is used. If there are multiple grouping variables,
#' raking (also known as iterative post-stratification) is used.
#' @param survey_design A survey design object created with the `survey` package.
#' @param group_vars Names of grouping variables in the data dividing the sample
#' into groups for which benchmark data are available. These variables cannot have any missing values
#' @param group_benchmark_vars Names of group benchmark variables in the data corresponding to \code{group_vars}.
#' For each category of a grouping variable, the group benchmark variable gives the
#' population benchmark (i.e. population size) for that category.
#' @param max_iterations If there are multiple grouping variables,
#' then raking is used rather than post-stratification.
#' The parameter \code{max_iterations} controls the maximum number of iterations to
#' use in raking.
#' @param epsilon If raking is used, convergence for a given margin is declared
#' if the maximum change in a re-weighted total is less than \code{epsilon} times
#' the total sum of the original weights in the design.
#' @details Raking adjusts the weight assigned to each sample member
#' so that, after reweighting, the weighted sample percentages for population subgroups
#' match their known population percentages. In a sense, raking causes
#' the sample to more closely resemble the population in terms of variables
#' for which population sizes are known.
#' \cr
#' \cr
#' Raking can be useful to reduce nonresponse bias caused by
#' having groups which are overrepresented in the responding sample
#' relative to their population size.
#' If the population subgroups systematically differ in terms of outcome variables of interest,
#' then raking can also be helpful in terms of reduce sampling variances. However,
#' when population subgroups do not differ in terms of outcome variables of interest,
#' then raking may increase sampling variances.
#' \cr
#' \cr
#' There are two basic requirements for raking. \cr
#' \itemize{
#'   \item Basic Requirement 1 - Values of the grouping variable(s) must be known for all respondents.
#'   \item Basic Requirement 2 - The population size of each group must be known (or precisely estimated).
#' }
#' When there is effectively only one grouping variable
#' (though this variable can be defined as a combination of other variables),
#' raking amounts to simple post-stratification.
#' For example, simple post-stratification would be used if the grouping variable
#' is "Age x Sex x Race", and the population size of each combination of
#' age, sex, and race is known.
#' The method of "iterative poststratification" (also known as "iterative proportional fitting")
#' is used when there are multiple grouping variables,
#' and population sizes are known for each grouping variable
#' but not for combinations of grouping variables.
#' For example, iterative proportional fitting would be necessary
#' if population sizes are known for age groups and for gender categories
#' but not for combinations of age groups and gender categories.
#'
#' @return A survey design object with raked or post-stratified weights
#' @export
#'
#' @examples
#' # Load the survey data
#'
#' data(involvement_survey_srs, package = "nrba")
#'
#' # Calculate population benchmarks
#' population_benchmarks <- list(
#'   "PARENT_HAS_EMAIL" = data.frame(
#'     PARENT_HAS_EMAIL = c("Has Email", "No Email"),
#'     PARENT_HAS_EMAIL_POP_BENCHMARK = c(17036, 2964)
#'   ),
#'   "STUDENT_RACE" = data.frame(
#'     STUDENT_RACE = c(
#'       "AM7 (American Indian or Alaska Native)", "AS7 (Asian)",
#'       "BL7 (Black or African American)",
#'       "HI7 (Hispanic or Latino Ethnicity)", "MU7 (Two or More Races)",
#'       "PI7 (Native Hawaiian or Other Pacific Islander)",
#'       "WH7 (White)"
#'     ),
#'     STUDENT_RACE_POP_BENCHMARK = c(206, 258, 3227, 1097, 595, 153, 14464)
#'   )
#' )
#'
#' # Add the population benchmarks as variables in the data
#' involvement_survey_srs <- merge(
#'   x = involvement_survey_srs,
#'   y = population_benchmarks$PARENT_HAS_EMAIL,
#'   by = "PARENT_HAS_EMAIL"
#' )
#' involvement_survey_srs <- merge(
#'   x = involvement_survey_srs,
#'   y = population_benchmarks$STUDENT_RACE,
#'   by = "STUDENT_RACE"
#' )
#'
#' # Create a survey design object
#' library(survey)
#'
#' survey_design <- svydesign(
#'   weights = ~BASE_WEIGHT,
#'   id = ~UNIQUE_ID,
#'   fpc = ~N_STUDENTS,
#'   data = involvement_survey_srs
#' )
#'
#' # Subset data to only include respondents
#' survey_respondents <- subset(
#'   survey_design,
#'   RESPONSE_STATUS == "Respondent"
#' )
#'
#' # Rake to the benchmarks
#' raked_survey_design <- rake_to_benchmarks(
#'   survey_design = survey_respondents,
#'   group_vars = c("PARENT_HAS_EMAIL", "STUDENT_RACE"),
#'   group_benchmark_vars = c(
#'     "PARENT_HAS_EMAIL_POP_BENCHMARK",
#'     "STUDENT_RACE_POP_BENCHMARK"
#'   ),
#' )
#'
#' # Inspect estimates from respondents, before and after raking
#'
#' svymean(
#'   x = ~PARENT_HAS_EMAIL,
#'   design = survey_respondents
#' )
#' svymean(
#'   x = ~PARENT_HAS_EMAIL,
#'   design = raked_survey_design
#' )
#'
#' svymean(
#'   x = ~WHETHER_PARENT_AGREES,
#'   design = survey_respondents
#' )
#' svymean(
#'   x = ~WHETHER_PARENT_AGREES,
#'   design = raked_survey_design
#' )
#'
rake_to_benchmarks <- function(survey_design,
                               group_vars,
                               group_benchmark_vars,
                               max_iterations = 100,
                               epsilon = 5e-06) {

  # Checks on inputs

  ## Variable names specified correctly
  if (!is.character(group_vars) | !is.character(group_benchmark_vars)) {
    stop("`group_vars` and `group_benchmark_vars` must both be character vectors.")
  }

  if (length(group_benchmark_vars) != length(group_vars)) {
    stop("`group_benchmark_vars` must have the same number of variable names as `group_vars`")
  }

  if (!all(group_vars %in% colnames(survey_design))) {
    stop("Some of the variable names specified by `group_vars` are not in the data.")
  }
  if (!all(group_benchmark_vars %in% colnames(survey_design))) {
    stop("Some of the variable names specified by `group_benchmark_vars` are not in the data.")
  }

  ## No missing values
  n_nonmissing <- nrow(
    na.omit(survey_design$variables[, c(group_vars, group_benchmark_vars),
      drop = FALSE
    ])
  )
  n_total <- nrow(survey_design)

  if (n_total != n_nonmissing) {
    stop("The variables specified by `group_vars` and `group_benchmark_vars` should not have any missing values.")
  }

  ## Population benchmarks are numeric values
  all_benchmark_cols_numeric <- all(
    sapply(
      survey_design$variables[, group_benchmark_vars],
      is.numeric
    )
  )
  if (!all_benchmark_cols_numeric) {
    stop("All of the variables specified by `group_benchmark_vars` must be numeric.")
  }

  ## Each population group should only have one population size
  for (i in seq_along(group_vars)) {
    n_unique_groups <- nrow(
      unique(survey_design$variables[, group_vars[i], drop = FALSE])
    )
    n_unique_combos <- nrow(
      unique(
        survey_design$variables[, c(group_vars[i], group_benchmark_vars[i]), drop = FALSE]
      )
    )
    if (n_unique_groups != n_unique_combos) {
      error_msg <- sprintf(
        "Each category of `%s` should only be associated with one unique value of `%s`",
        group_vars[i], group_benchmark_vars[i]
      )
      stop(error_msg)
    }
  }

  # Count the number of raking/poststratifying variables
  n_margins <- length(group_vars)

  # Initialize list of dataframes givign population totals
  pop_margins <- vector("list", length = n_margins)
  names(pop_margins) <- group_vars

  # Create data frame for each margin's population totals
  # Each data frame has the grouping variable
  # and a variable named 'Freq' giving the population totals
  for (i in seq_len(n_margins)) {
    pop_margins[[i]] <- unique(
      survey_design$variables[, c(
        group_vars[i],
        group_benchmark_vars[i]
      )]
    )
    colnames(pop_margins[[i]])[2] <- "Freq"
  }

  # Create list of formulas, one for each grouping variable
  group_var_formulas <- lapply(group_vars, reformulate)

  # Check whether raking will fail due to zero observations for a group
  if (inherits(survey_design, "svyrep.design")) {
    group_var_has_zero_obs_in_replicate <- sapply(
      X = group_var_formulas,
      FUN = function(var_formula) {
        replicate_estimates <- survey::svytotal(
          x = var_formula, design = survey_design,
          na.rm = TRUE, return.replicates = TRUE
        ) |>
          getElement("replicates")

        any_replicate_has_zero_obs <- any(replicate_estimates == 0)
        return(any_replicate_has_zero_obs)
      }
    )
    group_vars_with_zero_obs_in_replicates <- group_vars[group_var_has_zero_obs_in_replicate]
    if (any(group_var_has_zero_obs_in_replicate)) {
      error_msg <- sprintf(
        "Unable to conduct weight adjustments due to small group sizes for the following variables:\n%s.\nConsider collapsing small categories together, or selecting a different type of replicate weight.",
        paste(sprintf('"%s"', group_vars_with_zero_obs_in_replicates), collapse = ", ")
      )
      stop(error_msg)
    }
  }

  # If there are multiple grouping variables, use survey::rake()
  # If there is only one grouping variable, use survey::postStratify()
  if (n_margins > 1) {
    calibrated_design <- survey::rake(
      design = survey_design,
      sample.margins = group_var_formulas,
      population.margins = pop_margins,
      control = list(
        iter = max_iterations,
        epsilon = epsilon,
        verbose = FALSE
      )
    )
  } else if (n_margins == 1) {
    poststratum_variable <- group_var_formulas[[1]]
    population_totals <- pop_margins[[1]]

    calibrated_design <- survey::postStratify(
      design = survey_design,
      strata = poststratum_variable,
      population = population_totals
    )
  }
  return(calibrated_design)
}
