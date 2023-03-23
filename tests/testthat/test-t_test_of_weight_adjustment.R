suppressWarnings({
  suppressPackageStartupMessages({
    library(survey)
    library(nrba)
  })
})

# Create a survey design ----
  data("involvement_survey_str2s", package = 'nrba')

  involvement_survey_str2s <- transform(
    involvement_survey_str2s,
    response_status = sample(x = c(1, 2, 3, 4),
                             prob = c(0.3, 0.65, 0.025, 0.025),
                             size = nrow(involvement_survey_str2s),
                             replace = TRUE))

  survey_design <- survey_design <- svydesign(
    data = involvement_survey_str2s,
    weights = ~ BASE_WEIGHT,
    strata =  ~ SCHOOL_DISTRICT,
    ids =     ~ SCHOOL_ID             + UNIQUE_ID,
    fpc =     ~ N_SCHOOLS_IN_DISTRICT + N_STUDENTS_IN_SCHOOL
  )

  rep_design <- as.svrepdesign(survey_design, type = "mrbbootstrap")

# Post-stratify to frame totals ----

  data("involvement_survey_pop", package = "nrba")

  STUDENT_AGE_deciles <- quantile(involvement_survey_pop$STUDENT_AGE,
                                  probs = seq(0,1,0.1))

  frame_data <- involvement_survey_pop
  frame_data[['STUDENT_AGE_group']] <- cut(involvement_survey_pop$STUDENT_AGE,
                                           breaks = STUDENT_AGE_deciles,
                                           include.lowest = TRUE)
  rep_design[['variables']][['STUDENT_AGE_group']] <- cut(rep_design$variables$STUDENT_AGE,
                                                          breaks = STUDENT_AGE_deciles,
                                                          include.lowest = TRUE)

  calibrated_design <- postStratify(
    design = rep_design,
    strata = ~ PARENT_HAS_EMAIL + STUDENT_AGE_group,
    population = xtabs(~ PARENT_HAS_EMAIL + STUDENT_AGE_group, data = frame_data)
  )

# Test the function ----

  ##_ Numeric dependent variables ----

  test_result <- list(
    'two-sided' = t_test_of_weight_adjustment(orig_design = rep_design,
                                              updated_design = calibrated_design,
                                              y_vars = 'STUDENT_AGE'),
    'less' = t_test_of_weight_adjustment(orig_design = rep_design,
                                         updated_design = calibrated_design,
                                         y_vars = 'STUDENT_AGE',
                                         alternative = 'less',
                                         null_difference = 0.1,
                                         degrees_of_freedom = Inf),
    'greater' = t_test_of_weight_adjustment(orig_design = rep_design,
                                            updated_design = calibrated_design,
                                            y_vars = 'STUDENT_AGE',
                                            alternative = 'greater',
                                            null_difference = -0.1,
                                            degrees_of_freedom = 100)
  )

    if (rep_design$type %in% c("subbootstrap", "mrbbootstrap")) {
      rep_design$type <- 'bootstrap'
    }
    if (calibrated_design$type %in% c("subbootstrap", "mrbbootstrap")) {
      calibrated_design$type <- 'bootstrap'
    }

    expected_ests <- svyby(
      design = svrep::stack_replicate_designs('Original' = rep_design,
                                              'Adjusted' = calibrated_design,
                                              .id = "_which_design_"),
      FUN = svymean,
      formula = ~ STUDENT_AGE,
      by = ~ `_which_design_`,
      covmat = TRUE
    )

    expected_vcov <- vcov(expected_ests)

    se_diff <- sqrt(t(c(1,-1)) %*% expected_vcov %*% c(1,-1))
    se_diff <- as.numeric(se_diff)

    expected_means <- setNames(expected_ests$STUDENT_AGE,
                               rownames(expected_ests))
    expected_diff <- unname(expected_means['Original'] - expected_means['Adjusted'])

    expected_test_stats <- list(
      'two-sided' = list(
        'difference' = expected_diff,
        'std_error' = se_diff,
        't_statistic' = (expected_diff)/se_diff,
        'df' = degf(svrep::stack_replicate_designs(rep_design,
                                                   calibrated_design)) - 1
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
      "Before and after post-stratification: Correct estimate of difference and its standard error", {
        expect_equal(expected_test_stats$`two-sided`[c("difference", "std_error")],
                     as.list(test_result$`two-sided`[,c("difference", "std_error")]))
      })

    test_that(
      "Before and after post-stratification: Correct t-statistic and p-value for two-sided test", {
        expect_equal(expected_test_stats$`two-sided`[c("t_statistic", "p_value")],
                     as.list(test_result$`two-sided`[,c("t_statistic", "p_value")]))
      })

    test_that(
      "Before and after post-stratification: Correct t-statistic and p-value for when using point null other than zero", {
        expect_equal(expected_test_stats$`less`[c("t_statistic", "p_value")],
                     as.list(test_result$`less`[,c("t_statistic", "p_value")]))
      })

    test_that(
      "Before and after post-stratification: Correct t-statistic and p-value for one-sided test with alternative of 'less-than'", {
        expect_equal(expected_test_stats$`less`[c("t_statistic", "p_value")],
                     as.list(test_result$`less`[,c("t_statistic", "p_value")]))
      })

    test_that(
      "Before and after post-stratification: Correct t-statistic and p-value for one-sided test with alternative of 'greater-than'", {
        expect_equal(expected_test_stats$`greater`[c("t_statistic", "p_value")],
                     as.list(test_result$`greater`[,c("t_statistic", "p_value")]))
      })

    test_that(
      "Before and after post-stratification: Correct default degrees of freedom", {
        expect_equal(expected_test_stats$`two-sided`[c("df", "p_value")],
                     as.list(test_result$`two-sided`[,c("df", "p_value")]))
      })

    test_that(
      "Before and after post-stratification: Correct results when manually specifying specific degrees of freedom", {
        expect_equal(expected_test_stats$greater[c("df", "p_value")],
                     as.list(test_result$greater[,c("df", "p_value")]))
      })

    test_that(
      "Before and after post-stratification: Correct results when manually specifying `Inf` degrees of freedom", {
        expect_equal(expected_test_stats$less[c("df", "p_value")],
                     as.list(test_result$less[,c("df", "p_value")]))
      })

    ##_ Categorical dependent variables ----

    test_result <- t_test_of_weight_adjustment(orig_design = rep_design,
                                               updated_design = calibrated_design,
                                               y_vars = 'PARENT_HAS_EMAIL')

    if (rep_design$type %in% c("subbootstrap", "mrbbootstrap")) {
      rep_design$type <- 'bootstrap'
    }
    if (calibrated_design$type %in% c("subbootstrap", "mrbbootstrap")) {
      calibrated_design$type <- 'bootstrap'
    }

    stacked_design <- svrep::stack_replicate_designs('Original' = rep_design,
                                                     'Adjusted' = calibrated_design,
                                                     .id = "_which_design_")

    expected_ests <- svyby(
      design = stacked_design,
      FUN = svymean,
      formula = ~ PARENT_HAS_EMAIL,
      by = ~ `_which_design_`,
      covmat = TRUE
    )

    expected_vcov <- vcov(expected_ests)[c("Adjusted:PARENT_HAS_EMAILHas Email",
                                           "Original:PARENT_HAS_EMAILHas Email"),
                                         c("Adjusted:PARENT_HAS_EMAILHas Email",
                                           "Original:PARENT_HAS_EMAILHas Email")]

    colnames(expected_vcov) <- c("Adjusted", "Original")
    rownames(expected_vcov) <- c("Adjusted", "Original")

    se_diff <- sqrt(t(c(1,-1)) %*% expected_vcov %*% c(1,-1))
    se_diff <- as.numeric(se_diff)

    expected_means <- c(expected_ests["Original", "PARENT_HAS_EMAILHas Email"],
                        expected_ests["Adjusted", "PARENT_HAS_EMAILHas Email"])
    names(expected_means) <- c("Original", "Adjusted")

    expected_diff <- unname(expected_means['Original'] - expected_means['Adjusted'])

    expected_stats <- list('difference' = expected_diff,
                           'std_error' = se_diff)
    test_stats <- subset(test_result, outcome_category == "Has Email")
    test_stats <- as.list(
      test_stats[,c("difference", "std_error")]
    )

    test_that(
      "Before and after post-stratification: Correct results using categorical dependent variables", {
        expect_equal(expected_stats,
                     test_stats)
      })

    ##_ Correct handling of missing values in dependent variables ----

    test_result <- t_test_of_weight_adjustment(
      orig_design = rep_design |>
        transform(STUDENT_RACE = ifelse(
          STUDENT_GRADE == "PK", NA, STUDENT_RACE
        )),
      updated_design = calibrated_design |>
        transform(STUDENT_RACE = ifelse(
          STUDENT_GRADE == "PK", NA, STUDENT_RACE
        )),
      y_vars = 'STUDENT_RACE',
      na.rm = TRUE
    )

    test_that(
      "When `na.rm = TRUE`, drop missing values in dependent variable", {
        expect_equal(sum(test_result$Original_mean), 1)
        expect_equal(sum(test_result$Adjusted_mean), 1)
      })
