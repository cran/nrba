#' Test for differences in means/percentages between two potentially overlapping groups
#'
#' @param survey_design A survey design object created with the `survey` package.
#' @param y_vars Names of dependent variables for tests. For categorical variables, percentages of each category are tested.
#' @param na.rm Whether to drop cases with missing values for a given dependent variable.
#' @param status The name of the variable representing response/eligibility status.
#' @param group_1 Vector of values of \code{status} variable representing the first group
#' @param group_2 Vector of values of \code{status} variable representing the second group
#' @param null_difference The hypothesized difference between the groups' means. Default is \code{0}.
#' @param alternative Can be one of the following: \cr
#' - \code{'unequal'}: two-sided test of whether difference in means is equal to \code{null_difference} \cr
#' - \code{'less'}: one-sided test of whether difference is less than \code{null_difference} \cr
#' - \code{'greater'}: one-sided test of whether difference is greater than \code{null_difference}
#' @param degrees_of_freedom The degrees of freedom to use for the test's reference distribution.
#' Unless specified otherwise, the default is the design degrees of freedom minus one,
#' where the design degrees of freedom are estimated using the \code{survey} package's \code{degf} method.
#'
#' @return A data frame describing the difference in group means/percentages and the statistics from the t-test
#' @keywords internal

t_test_overlap <- function(survey_design,
                           y_vars,
                           na.rm = TRUE,
                           status, group_1, group_2,
                           null_difference = 0, alternative = "unequal",
                           degrees_of_freedom = survey::degf(survey_design) - 1) {

  # Parameter checks

  ##_ Dependent variables

  if (is.null(y_vars) || !is.character(y_vars) || length(y_vars) == 0) {
    stop("`y_vars` must be a character vector with at least one name")
  }

  if (!all(y_vars %in% colnames(survey_design))) {
    missing_y_vars <- setdiff(y_vars, colnames(survey_design))
    error_msg <- paste0("`y_vars` contains some variable names not in the survey design object:",
                        "\n", paste(missing_y_vars, collapse = ", "))
    stop(error_msg)
  }

  ##_ Group variables

  if (missing(group_1) || missing(group_2)) {
    stop("A vector of values must be supplied to both the `group_1` and `group_2` arguments")
  }
  if (length(group_1) == 0 || length(group_2) == 0) {
    stop("Must supply values to the `group_1` and `group_2` arguments")
  }

  ##_ survey_design

  if (!any(c("survey.design", "svyrep.design") %in% class(survey_design))) {
    stop("`survey_design` should be a survey design object created using the `survey` or `srvyr` package.\nUse `help('svydesign', package = 'survey')` to see how.")
  }

  ##_ Status variable and codes

  if (missing(status)) {
    stop("A variable name must be supplied to the `status` parameter")
  }
  if (is.character(status)) {
    status_var <- status
  } else {
    stop("The value supplied to `status` must be a character value")
  }

  if (!is.character(status_var) || length(status_var) != 1) {
    stop("Must specify a single variable name for `status`")
  }
  if (!status_var %in% colnames(survey_design[['variables']])) {
    stop(sprintf("The status variable '%s' does not appear in the supplied data",
                 status_var))
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

  # Determine which dependent variables are categorical; if so, create indicator variables

  which_y_vars_categorical <- sapply(setNames(y_vars, y_vars), function(y_var) {
    any(c("character", "factor") %in% class(survey_design[['variables']][[y_var]]))
  })

  y_var_categories <- lapply(setNames(y_vars[which_y_vars_categorical],
                                      y_vars[which_y_vars_categorical]),
                                      function(y_var) {

    if (is.factor(survey_design[['variables']])) {
      y_var_levels <- levels(survey_design[['variables']][[y_var]])
    } else {
      y_var_levels <- sort(unique(survey_design[['variables']][[y_var]]))
    }
    return(y_var_levels)
  })

  indicator_var_names <- list()

  for (y_var in y_vars[which_y_vars_categorical]) {
    for (y_var_level in y_var_categories[[y_var]]) {

      var_name <- sprintf("_%s_%s_", y_var, y_var_level)
      indicator_var_names[[y_var]][[y_var_level]] <- var_name

      survey_design[['variables']][[var_name]] <- ifelse(
        survey_design[['variables']][[y_var]] == y_var_level,
        1L, 0L
      )

    }
  }

  y_vars_incl_indicators <- c(y_vars[!which_y_vars_categorical],
                              unname(unlist(indicator_var_names)))
  outcome_names <- c(
    y_vars[!which_y_vars_categorical],
    rep(names(indicator_var_names),
        times = sapply(indicator_var_names, length))
  )

  outcome_categories <- c(
    rep(NA, length(y_vars[!which_y_vars_categorical])),
    unname(unlist(lapply(indicator_var_names, names)))
  )

  # Calculate estimates and their covariance matrix

  estimates <- lapply(
    X = setNames(y_vars_incl_indicators,
                 y_vars_incl_indicators),
    FUN = function(y_var) {

      # Add necessary variables to the survey design
      survey_design[['variables']][['_y_var']] <- survey_design[['variables']][[y_var]]
      survey_design[['variables']][['_group_1']] <- survey_design[['variables']][[status_var]] %in% group_1
      survey_design[['variables']][['_group_2']] <- survey_design[['variables']][[status_var]] %in% group_2

      survey_design[['variables']][['_y_var_group_1']] <- survey_design[['variables']][['_y_var']] * survey_design[['variables']][['_group_1']]
      survey_design[['variables']][['_y_var_group_2']] <- survey_design[['variables']][['_y_var']] * survey_design[['variables']][['_group_2']]

      # Estimate the domain means as ratios
      ratio_estimates <- survey::svyratio(numerator = ~ `_y_var_group_1` + `_y_var_group_2`,
                                          denominator = ~ `_group_1` + `_group_2`,
                                          design = survey_design, covmat = TRUE,
                                          na.rm = na.rm)

      # Extract only the relevant output from the result
      domain_means <- c(
        ratio_estimates$ratio['_y_var_group_1','_group_1'],
        ratio_estimates$ratio['_y_var_group_2','_group_2']
      )

      vcov_of_domain_means <- vcov(ratio_estimates)[
        c('_y_var_group_1/_group_1', '_y_var_group_2/_group_2'),
        c('_y_var_group_1/_group_1', '_y_var_group_2/_group_2')
      ]

      names(domain_means) <- c("group_1", "group_2")
      rownames(vcov_of_domain_means) <- c("group_1", "group_2")
      colnames(vcov_of_domain_means) <- c("group_1", "group_2")

      output <- list('estimates' = domain_means,
                     'vcov_matrix' = vcov_of_domain_means)
      return(output)

    })

  # Calculate test statistics

  test_statistics <- lapply(
    X = setNames(names(estimates),
                 names(estimates)),
    FUN = function(y_var) {
      domain_means <- estimates[[y_var]][['estimates']]
      vcov_matrix <- estimates[[y_var]][['vcov_matrix']]

      ##_ Estimate difference and associated standard error
      estimated_difference <- unname(domain_means['group_1'] - domain_means['group_2'])

      est_var_of_diff <- as.numeric(
        t(c(1,-1)) %*% vcov_matrix %*% c(1,-1)
      )

      std_err_of_diff <- sqrt(est_var_of_diff)

      ##_ Calculate statistics for t test
      t_statistic <- (estimated_difference - null_difference) / std_err_of_diff

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

      result <- data.frame(
        'group_1_mean' = domain_means['group_1'],
        'group_2_mean' = domain_means['group_2'],
        'difference' = estimated_difference,
        'std_error' = std_err_of_diff,
        'p_value' = p_value,
        't_statistic' = t_statistic,
        'df' = degrees_of_freedom,
        'group_1_se' = sqrt(vcov_matrix['group_1', 'group_1']),
        'group_2_se' = sqrt(vcov_matrix['group_2', 'group_2']),
        'covariance' = vcov_matrix['group_1', 'group_2'],
        row.names = NULL
      )
      return(result)
    })

  # Compile results into a data frame

  test_results <- Reduce(rbind, test_statistics)
  test_results <- cbind(outcome = outcome_names,
                        outcome_category = outcome_categories,
                        test_results)
  row_order <- order(factor(outcome_names, levels = y_vars))
  test_results <- test_results[row_order,]

  return(test_results)
}
