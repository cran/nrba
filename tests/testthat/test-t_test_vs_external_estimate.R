suppressWarnings({
  suppressPackageStartupMessages({
    library(survey)
    library(nrba)
  })
})

# Create a survey design ----
  data("involvement_survey_pop", package = 'nrba')
  data("involvement_survey_str2s", package = 'nrba')
  data("involvement_survey_srs", package = 'nrba')

  survey_design <- survey_design <- svydesign(
    data = involvement_survey_str2s,
    weights = ~ BASE_WEIGHT,
    strata =  ~ SCHOOL_DISTRICT,
    ids =     ~ SCHOOL_ID             + UNIQUE_ID,
    fpc =     ~ N_SCHOOLS_IN_DISTRICT + N_STUDENTS_IN_SCHOOL
  )

  rep_design <- as.svrepdesign(survey_design, type = "mrbbootstrap")

# Obtain external benchmark estimates ----

  parent_email_benchmark <- table(involvement_survey_pop$PARENT_HAS_EMAIL)
  parent_email_benchmark <- prop.table(parent_email_benchmark)

  STUDENT_AGE_benchmark <- mean(involvement_survey_pop$STUDENT_AGE)

  STUDENT_AGE_estimated_benchmark <- survey::svymean(
    design = svydesign(data = involvement_survey_srs, ids = ~ 1,
                       weights = ~ BASE_WEIGHT),
    x = ~ STUDENT_AGE
  )

# Test the function ----

  ##_ Numeric variable ----

  test_result <- list(
    'two-sided' = t_test_vs_external_estimate(survey_design = survey_design,
                                              y_var = 'STUDENT_AGE',
                                              ext_ests = STUDENT_AGE_benchmark,
                                              alternative = 'unequal'),
    'less' = t_test_vs_external_estimate(survey_design = survey_design,
                                         y_var = 'STUDENT_AGE',
                                         ext_ests = STUDENT_AGE_benchmark,
                                         alternative = 'less',
                                         null_difference = 0.1,
                                         degrees_of_freedom = Inf),
    'greater' = t_test_vs_external_estimate(survey_design = survey_design,
                                            y_var = 'STUDENT_AGE',
                                            ext_ests = STUDENT_AGE_benchmark,
                                            alternative = 'greater',
                                            null_difference = -0.1,
                                            degrees_of_freedom = 100)
  )

    expected_ests <- svymean(
      design = survey_design,
      x = ~ STUDENT_AGE
    )

    se_diff <- SE(expected_ests)
    se_diff <- as.numeric(se_diff)

    expected_mean <- as.numeric(expected_ests)
    expected_diff <- unname(expected_mean - STUDENT_AGE_benchmark)

    expected_test_stats <- list(
      'two-sided' = list(
        'difference' = expected_diff,
        'std_error' = se_diff,
        't_statistic' = (expected_diff)/se_diff,
        'df' = degf(survey_design) - 1
      ),
      'less' = list(
        'difference' = expected_diff,
        'std_error' = se_diff,
        't_statistic' = (expected_diff - 0.1)/se_diff,
        'df' = Inf
      ),
      'greater' = list(
        'difference' = expected_diff,
        'std_error' = se_diff,
        't_statistic' = (expected_diff - (-0.1))/se_diff,
        'df' = 100
      )
    )

    expected_test_stats$`two-sided`$p_value <- 2*pt(
      abs(expected_test_stats$`two-sided`$t_statistic),
      df = expected_test_stats$`two-sided`$df,
      lower.tail = FALSE
    )

    expected_test_stats$`less`$p_value <- pt(
      expected_test_stats$`less`$t_statistic,
      df = expected_test_stats$less$df,
      lower.tail = TRUE
    )

    expected_test_stats$`greater`$p_value <- pt(
      expected_test_stats$`greater`$t_statistic,
      df = expected_test_stats$greater$df,
      lower.tail = FALSE
    )

    test_that(
      "Correct estimate of difference and its standard error", {
        expect_equal(expected_test_stats$`two-sided`[c("difference", "std_error")],
                     as.list(test_result$`two-sided`[,c("difference", "std_error")]))
      })

    test_that(
      "Correct t-statistic and p-value for two-sided test", {
        expect_equal(expected_test_stats$`two-sided`[c("t_statistic", "p_value")],
                     as.list(test_result$`two-sided`[,c("t_statistic", "p_value")]))
      })

    test_that(
      "Correct t-statistic and p-value for when using point null other than zero", {
        expect_equal(expected_test_stats$`less`[c("t_statistic", "p_value")],
                     as.list(test_result$`less`[,c("t_statistic", "p_value")]))
      })

    test_that(
      "Correct t-statistic and p-value for one-sided test with alternative of 'less-than'", {
        expect_equal(expected_test_stats$`less`[c("t_statistic", "p_value")],
                     as.list(test_result$`less`[,c("t_statistic", "p_value")]))
      })

    test_that(
      "Correct t-statistic and p-value for one-sided test with alternative of 'greater-than'", {
        expect_equal(expected_test_stats$`greater`[c("t_statistic", "p_value")],
                     as.list(test_result$`greater`[,c("t_statistic", "p_value")]))
      })

    test_that(
      "Correct default degrees of freedom for linearization variance", {
        expect_equal(expected_test_stats$`two-sided`[c("df", "p_value")],
                     as.list(test_result$`two-sided`[,c("df", "p_value")]))
      })

    test_that(
      "Correct results when manually specifying specific degrees of freedom", {
        expect_equal(expected_test_stats$greater[c("df", "p_value")],
                     as.list(test_result$greater[,c("df", "p_value")]))
      })

    test_that(
      "Correct results when manually specifying `Inf` degrees of freedom", {
        expect_equal(expected_test_stats$less[c("df", "p_value")],
                     as.list(test_result$less[,c("df", "p_value")]))
      })

    ##_ Categorical variables (i.e. percentages) ----

    test_result <- t_test_vs_external_estimate(
      survey_design = survey_design,
      y_var = "PARENT_HAS_EMAIL",
      ext_ests = parent_email_benchmark
    )

    expected_ests <- svymean(~ PARENT_HAS_EMAIL,
                             design = survey_design)

    expected_diffs <- c(
      expected_ests['PARENT_HAS_EMAILHas Email'] - parent_email_benchmark['Has Email'],
      expected_ests['PARENT_HAS_EMAILNo Email'] - parent_email_benchmark['No Email']
    )
    names(expected_diffs) <- c("Has Email", "No Email")


    expected_std_errors <- SE(expected_ests)[c('PARENT_HAS_EMAILHas Email',
                                               'PARENT_HAS_EMAILNo Email')]
    names(expected_std_errors) <- c("Has Email", "No Email")

    expected_stats <- list(
      'difference' = as.numeric(expected_diffs[c("Has Email", "No Email")]),
      'std_error' = unname(expected_std_errors[c("Has Email", "No Email")])
    )

    test_result_stats <- test_result %>%
      subset(category %in% c("Has Email", "No Email"))
    test_result_stats <- test_result_stats[,c("difference", "std_error")]


    test_that(
      "Correct results using categorical dependent variables", {
        expect_equal(expected_stats,
                     as.list(test_result_stats))
      })

    ##_ For categorical variables, rescaling external estimates to sum to 1

    test_that(
      "For categorical variables, rescales external estimates to sum to 1, if necessary.", {

        expect_equal(
          object = suppressWarnings(
            t_test_vs_external_estimate(
              survey_design = survey_design,
              y_var = "PARENT_HAS_EMAIL",
              ext_ests = 100 * parent_email_benchmark
            )
          ),
          expected = t_test_vs_external_estimate(
            survey_design = survey_design,
            y_var = "PARENT_HAS_EMAIL",
            ext_ests = parent_email_benchmark
          )
        )

        expect_warning(
          object = t_test_vs_external_estimate(
            survey_design = survey_design,
            y_var = "PARENT_HAS_EMAIL",
            ext_ests = 100 * parent_email_benchmark
          ),
          regexp = "Rescaling values of external estimates to sum to 1"
        )
      })

    ##_ External estimates have standard errors ----

    test_result <- t_test_vs_external_estimate(
      survey_design = survey_design,
      y_var = 'STUDENT_AGE',
      ext_ests = coef(STUDENT_AGE_estimated_benchmark),
      ext_std_errors = SE(STUDENT_AGE_estimated_benchmark),
      alternative = 'unequal',
      degrees_of_freedom = survey::degf(survey_design) - 1
    )

    expected_est <- svymean(design = survey_design,
                            x = ~ STUDENT_AGE)

    expected_diff <- coef(expected_est) - coef(STUDENT_AGE_estimated_benchmark)
    expected_std_error <- sqrt(
      diag(vcov(expected_est) + vcov(STUDENT_AGE_estimated_benchmark))
    )
    expected_t_stat <- expected_diff / expected_std_error
    expected_p_value <- 2 * pt(
      q = abs(expected_t_stat),
      lower.tail = FALSE,
      df = survey::degf(survey_design) - 1
    )
    expected_test_stats <- data.frame(
      'difference' = unname(expected_diff),
      'std_error' = unname(expected_std_error),
      't_statistic' = unname(expected_t_stat),
      'p_value' = unname(expected_p_value)
    )

    test_that(
      "Correct results when external estimate has standard error", {
      expect_equal(
        object = test_result[,c("difference", "std_error", "t_statistic", "p_value")],
        expected = expected_test_stats
      )
    })

    test_that(
      "If rescaling external estimates, external standard errors are rescaled too", {
        expect_equal(
          object = suppressWarnings(
            t_test_vs_external_estimate(
              survey_design = survey_design,
              y_var = "PARENT_HAS_EMAIL",
              ext_ests = 100 * parent_email_benchmark,
              ext_std_errors = (parent_email_benchmark) * (1 - parent_email_benchmark)
            )
          ),
          expected = t_test_vs_external_estimate(
            survey_design = survey_design,
            y_var = "PARENT_HAS_EMAIL",
            ext_ests = parent_email_benchmark,
            ext_std_errors = (parent_email_benchmark) * (1 - parent_email_benchmark) / 100
          )
        )
      })
