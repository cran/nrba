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

# Test the function ----

  ##_ Respondents vs eligible ----

  test_result <- list(
    'two-sided' = t_test_resp_vs_elig(survey_design = survey_design,
                                      y_vars = 'STUDENT_AGE',
                                      status = 'response_status',
                                      status_codes = c("ER" = 1,
                                                       "EN" = 2,
                                                       "IE" = 3,
                                                       "UE" = 4)),
    'less' = t_test_resp_vs_elig(survey_design = survey_design,
                                 y_vars = 'STUDENT_AGE',
                                 status = 'response_status',
                                 status_codes = c("ER" = 1,
                                                  "EN" = 2,
                                                  "IE" = 3,
                                                  "UE" = 4),
                                 alternative = 'less',
                                 null_difference = 0.1,
                                 degrees_of_freedom = Inf),
    'greater' = t_test_resp_vs_elig(survey_design = survey_design,
                                    y_vars = 'STUDENT_AGE',
                                    status = 'response_status',
                                    status_codes = c("ER" = 1,
                                                     "EN" = 2,
                                                     "IE" = 3,
                                                     "UE" = 4),
                                    alternative = 'greater',
                                    null_difference = -0.1,
                                    degrees_of_freedom = 100)
  )

    expected_ests <- svyratio(
      design = transform(survey_design,
                         resp = response_status == 1,
                         elig = response_status %in% c(1,2)) %>%
        transform(y_resp = STUDENT_AGE * resp,
                  y_elig = STUDENT_AGE * elig),
      numerator = ~ y_resp + y_elig,
      denominator = ~ resp + elig,
      covmat = TRUE
    )

    expected_vcov <- vcov(expected_ests)[c('y_resp/resp', 'y_elig/elig'),
                                         c('y_resp/resp', 'y_elig/elig')]

    se_diff <- sqrt(t(c(1,-1)) %*% expected_vcov %*% c(1,-1))
    se_diff <- as.numeric(se_diff)

    expected_means <- diag(expected_ests$ratio)
    expected_diff <- unname(expected_means[1] - expected_means[2])

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
      "Respondents vs. Eligible: Correct estimate of difference and its standard error", {
        expect_equal(expected_test_stats$`two-sided`[c("difference", "std_error")],
                     as.list(test_result$`two-sided`[,c("difference", "std_error")]))
      })

    test_that(
      "Respondents vs. Eligible: Correct t-statistic and p-value for two-sided test", {
        expect_equal(expected_test_stats$`two-sided`[c("t_statistic", "p_value")],
                     as.list(test_result$`two-sided`[,c("t_statistic", "p_value")]))
      })

    test_that(
      "Respondents vs. Eligible: Correct t-statistic and p-value for when using point null other than zero", {
        expect_equal(expected_test_stats$`less`[c("t_statistic", "p_value")],
                     as.list(test_result$`less`[,c("t_statistic", "p_value")]))
      })

    test_that(
      "Respondents vs. Eligible: Correct t-statistic and p-value for one-sided test with alternative of 'less-than'", {
        expect_equal(expected_test_stats$`less`[c("t_statistic", "p_value")],
                     as.list(test_result$`less`[,c("t_statistic", "p_value")]))
      })

    test_that(
      "Respondents vs. Eligible: Correct t-statistic and p-value for one-sided test with alternative of 'greater-than'", {
        expect_equal(expected_test_stats$`greater`[c("t_statistic", "p_value")],
                     as.list(test_result$`greater`[,c("t_statistic", "p_value")]))
      })

    test_that(
      "Respondents vs. Eligible: Correct default degrees of freedom for linearization variance", {
        expect_equal(expected_test_stats$`two-sided`[c("df", "p_value")],
                     as.list(test_result$`two-sided`[,c("df", "p_value")]))
      })

    test_that(
      "Respondents vs. Eligible: Correct results when manually specifying specific degrees of freedom", {
        expect_equal(expected_test_stats$greater[c("df", "p_value")],
                     as.list(test_result$greater[,c("df", "p_value")]))
      })

    test_that(
      "Respondents vs. Eligible: Correct results when manually specifying `Inf` degrees of freedom", {
        expect_equal(expected_test_stats$less[c("df", "p_value")],
                     as.list(test_result$less[,c("df", "p_value")]))
      })

    ##_ Respondents vs full ----

    test_result <- list(
      'two-sided' = t_test_resp_vs_full(survey_design = survey_design,
                                        y_vars = 'STUDENT_AGE',
                                        status = 'response_status',
                                        status_codes = c("ER" = 1,
                                                         "EN" = 2,
                                                         "IE" = 3,
                                                         "UE" = 4)),
      'less' = t_test_resp_vs_full(survey_design = survey_design,
                                   y_vars = 'STUDENT_AGE',
                                   status = 'response_status',
                                   status_codes = c("ER" = 1,
                                                    "EN" = 2,
                                                    "IE" = 3,
                                                    "UE" = 4),
                                   alternative = 'less',
                                   null_difference = 0.1,
                                   degrees_of_freedom = Inf),
      'greater' = t_test_resp_vs_full(survey_design = survey_design,
                                      y_vars = 'STUDENT_AGE',
                                      status = 'response_status',
                                      status_codes = c("ER" = 1,
                                                       "EN" = 2,
                                                       "IE" = 3,
                                                       "UE" = 4),
                                      alternative = 'greater',
                                      null_difference = -0.1,
                                      degrees_of_freedom = 100)
    )

    expected_ests <- svyratio(
      design = transform(survey_design,
                         resp = response_status == 1,
                         full = 1) %>%
        transform(y_resp = STUDENT_AGE * resp,
                  y_full = STUDENT_AGE * full),
      numerator = ~ y_resp + y_full,
      denominator = ~ resp + full,
      covmat = TRUE
    )

    expected_vcov <- vcov(expected_ests)[c('y_resp/resp', 'y_full/full'),
                                         c('y_resp/resp', 'y_full/full')]

    se_diff <- sqrt(t(c(1,-1)) %*% expected_vcov %*% c(1,-1))
    se_diff <- as.numeric(se_diff)

    expected_means <- diag(expected_ests$ratio)
    expected_diff <- unname(expected_means[1] - expected_means[2])

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
      "Respondents vs. Full: Correct estimate of difference and its standard error", {
        expect_equal(expected_test_stats$`two-sided`[c("difference", "std_error")],
                     as.list(test_result$`two-sided`[,c("difference", "std_error")]))
      })

    test_that(
      "Respondents vs. Full: Correct t-statistic and p-value for two-sided test", {
        expect_equal(expected_test_stats$`two-sided`[c("t_statistic", "p_value")],
                     as.list(test_result$`two-sided`[,c("t_statistic", "p_value")]))
      })

    test_that(
      "Respondents vs. Full: Correct t-statistic and p-value for when using point null other than zero", {
        expect_equal(expected_test_stats$`less`[c("t_statistic", "p_value")],
                     as.list(test_result$`less`[,c("t_statistic", "p_value")]))
      })

    test_that(
      "Respondents vs. Full: Correct t-statistic and p-value for one-sided test with alternative of 'less-than'", {
        expect_equal(expected_test_stats$`less`[c("t_statistic", "p_value")],
                     as.list(test_result$`less`[,c("t_statistic", "p_value")]))
      })

    test_that(
      "Respondents vs. Full: Correct t-statistic and p-value for one-sided test with alternative of 'greater-than'", {
        expect_equal(expected_test_stats$`greater`[c("t_statistic", "p_value")],
                     as.list(test_result$`greater`[,c("t_statistic", "p_value")]))
      })

    test_that(
      "Respondents vs. Full: Correct default degrees of freedom for linearization variance", {
        expect_equal(expected_test_stats$`two-sided`[c("df", "p_value")],
                     as.list(test_result$`two-sided`[,c("df", "p_value")]))
      })

    test_that(
      "Respondents vs. Full: Correct results when manually specifying specific degrees of freedom", {
        expect_equal(expected_test_stats$greater[c("df", "p_value")],
                     as.list(test_result$greater[,c("df", "p_value")]))
      })

    test_that(
      "Respondents vs. Full: Correct results when manually specifying `Inf` degrees of freedom", {
        expect_equal(expected_test_stats$less[c("df", "p_value")],
                     as.list(test_result$less[,c("df", "p_value")]))
      })

    ##_ Categorical variables (i.e. percentages) ----

    test_result <- t_test_resp_vs_full(
      survey_design = transform(survey_design,
                                PARENT_HAS_EMAIL = as.character(PARENT_HAS_EMAIL),
                                STUDENT_SEX = factor(STUDENT_SEX)),
      y_vars = c('PARENT_HAS_EMAIL', 'STUDENT_SEX'),
      status = 'response_status',
      status_codes = c("ER" = 1,
                       "EN" = 2,
                       "IE" = 3,
                       "UE" = 4)
    )

    svy_design_for_result <- survey_design
    svy_design_for_result[['variables']] <- svy_design_for_result[['variables']] %>%
      transform(resp_has_email = ifelse(response_status == 1 & PARENT_HAS_EMAIL == "Has Email", 1, 0),
                full_has_email = ifelse(PARENT_HAS_EMAIL == "Has Email", 1, 0),
                resp_male = ifelse(response_status == 1 & STUDENT_SEX == "Male", 1, 0),
                full_male = ifelse(STUDENT_SEX == "Male", 1, 0),
                full = 1, resp = ifelse(response_status == 1, 1, 0))

    expected_ratio_ests <- list(
      'Email' = svyratio(
        numerator = ~ resp_has_email + full_has_email,
        denominator = ~ full + resp,
        design = svy_design_for_result,
        covmat = TRUE),
      'Male' = svyratio(
        numerator = ~ resp_male + full_male,
        denominator = ~ full + resp,
        design = svy_design_for_result,
        covmat = TRUE)
    )

    expected_ests <- list(
      'Email' = c(
        'full' = expected_ratio_ests$Email$ratio['full_has_email', 'full'],
        'resp' = expected_ratio_ests$Email$ratio['resp_has_email', 'resp']
      ),
      'Male' = c(
        'full' = expected_ratio_ests$Male$ratio['full_male', 'full'],
        'resp' = expected_ratio_ests$Male$ratio['resp_male', 'resp']
      )
    )

    expected_diffs <- lapply(expected_ests,
                             function(x) unname(x['resp'] - x['full']))



    expected_vcovs <- list(
      'Email' = vcov(expected_ratio_ests$Email)[c('resp_has_email/resp', 'full_has_email/full'),
                                                c('resp_has_email/resp', 'full_has_email/full')],
      'Male' = vcov(expected_ratio_ests$Male)[c('resp_male/resp', 'full_male/full'),
                                              c('resp_male/resp', 'full_male/full')]
    ) %>% lapply(function(X) {
      colnames(X) <- c("resp", "full")
      rownames(X) <- c("resp", "full")
      X
    })

    expected_std_errors <- sapply(expected_vcovs,
                                  function(X) {
                                    sqrt(
                                      t(c(1,-1)) %*% X %*% c(1,-1)
                                    )
                                  })

    expected_stats <- list(
      'difference' = as.numeric(expected_diffs[c("Email", "Male")]),
      'std_error' = unname(expected_std_errors[c("Email", "Male")])
    )

    test_result_stats <- test_result %>%
      subset(outcome_category %in% c("Has Email", "Male"))
    test_result_stats <- test_result_stats[,c("difference", "std_error")]


    test_that(
      "Respondents vs. Full: Correct results using categorical dependent variables (factor or character)", {
        expect_equal(expected_stats,
                     as.list(test_result_stats))
      })

  ##_ Missing values in dependent variable ----

    test_result <- t_test_resp_vs_full(
      survey_design = transform(survey_design,
                                STUDENT_AGE = ifelse(
                                  STUDENT_GRADE == "PK", NA, STUDENT_AGE
                                )),
      y_vars = "STUDENT_AGE", na.rm = TRUE,
      status = 'response_status',
      status_codes = c("ER" = 1,
                       "EN" = 2,
                       "IE" = 3,
                       "UE" = 4)
    )

    svy_design_for_result <- survey_design |>
      transform(STUDENT_AGE = ifelse(
                  STUDENT_GRADE == "PK", NA, STUDENT_AGE
                ))
    svy_design_for_result[['variables']] <- svy_design_for_result[['variables']] %>%
      transform(resp_age = ifelse(response_status == 1 & !is.na(STUDENT_AGE),
                                  as.numeric(STUDENT_AGE), 0),
                full_age = ifelse(!is.na(STUDENT_AGE), as.numeric(STUDENT_AGE), 0),
                full_age_nonmissing = ifelse(!is.na(STUDENT_AGE), 1, 0),
                resp_age_nonmissing = ifelse(!is.na(STUDENT_AGE) & response_status == 1, 1, 0))


    expected_ratio_ests <- list(
      'STUDENT_AGE' = svyratio(
        numerator = ~ resp_age + full_age,
        denominator = ~ resp_age_nonmissing + full_age_nonmissing,
        design = svy_design_for_result,
        covmat = TRUE)
    )

    student_age_result <- subset(
      test_result,
      outcome == "STUDENT_AGE"
    )[,c("resp_mean", "full_sample_mean",
         "resp_se", "full_sample_se")]

    expected_student_age_result <- list(
      'resp_mean' = coef(expected_ratio_ests$STUDENT_AGE)['resp_age/resp_age_nonmissing'],
      'full_sample_mean' = coef(expected_ratio_ests$STUDENT_AGE)['full_age/full_age_nonmissing'],
      'resp_se' = SE(expected_ratio_ests$STUDENT_AGE)['resp_age/resp_age_nonmissing'],
      'full_sample_se' = SE(expected_ratio_ests$STUDENT_AGE)['full_age/full_age_nonmissing']
    ) %>% lapply(unname)


    test_that(
      "Respondents vs. Full: Correct handling of missing values in dependent variable", {
        expect_equal(expected_student_age_result,
                     as.list(student_age_result))
    })


