suppressWarnings({
  suppressPackageStartupMessages({
    library(survey)
    library(nrba)
  })
})

# Create a survey design ----
  data("involvement_survey_pop", package = 'nrba')
  data("involvement_survey_str2s", package = 'nrba')

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


# Test the function ----

  ##_ Basic test with linearization design ----

    test_result <- chisq_test_vs_external_estimate(
      survey_design = survey_design,
      y_var = "PARENT_HAS_EMAIL",
      ext_ests = parent_email_benchmark
    )

    expected_result <- svygofchisq(
      design = survey_design,
      formula = ~ PARENT_HAS_EMAIL,
      p = parent_email_benchmark
    )


    test_that("Correct result for linearization design", {
      expect_equal(object = test_result[,c("statistic", "df", "scale", "p_value")],
                   expected = data.frame(statistic = expected_result$statistic,
                                         df = expected_result$parameter['df'],
                                         scale = expected_result$parameter['scale'],
                                         p_value = expected_result$p.value,
                                         row.names = NULL))
    })

  ##_ Basic test with replicate designs ----

    test_result <- chisq_test_vs_external_estimate(
      survey_design = rep_design,
      y_var = "PARENT_HAS_EMAIL",
      ext_ests = parent_email_benchmark
    )

    expected_result <- svygofchisq(
      design = rep_design,
      formula = ~ PARENT_HAS_EMAIL,
      p = parent_email_benchmark
    )


    test_that("Correct result for replicate design", {
      expect_equal(object = test_result[,c("statistic", "df", "scale", "p_value")],
                   expected = data.frame(statistic = expected_result$statistic,
                                         df = expected_result$parameter['df'],
                                         scale = expected_result$parameter['scale'],
                                         p_value = expected_result$p.value,
                                         row.names = NULL))
    })

  ##_ Basic test with subset ----
    median_student_age <- median(involvement_survey_pop$STUDENT_AGE)
    older_pop <- subset(involvement_survey_pop, STUDENT_AGE > median_student_age)
    older_benchmark <- table(older_pop$PARENT_HAS_EMAIL)
    older_benchmark <- prop.table(older_benchmark)

    test_result <- chisq_test_vs_external_estimate(
      survey_design = subset(survey_design, STUDENT_AGE > median_student_age),
      y_var = "PARENT_HAS_EMAIL",
      ext_ests = older_benchmark
    )

    expected_result <- svygofchisq(
      design = subset(survey_design, STUDENT_AGE > median_student_age),
      formula = ~ PARENT_HAS_EMAIL,
      p = older_benchmark
    )


    test_that("Correct result for subset design", {
      expect_equal(object = test_result[,c("statistic", "df", "scale", "p_value")],
                   expected = data.frame(statistic = expected_result$statistic,
                                         df = expected_result$parameter['df'],
                                         scale = expected_result$parameter['scale'],
                                         p_value = expected_result$p.value,
                                         row.names = NULL))
    })

  ##_ Rescaling external estimates to sum to 1, if necessary

    test_that(
      "Rescales external estimates to sum to 1, if necessary.", {
        expect_equal(
          object = suppressWarnings(
            chisq_test_vs_external_estimate(
              survey_design = rep_design,
              y_var = "PARENT_HAS_EMAIL",
              ext_ests = 100*parent_email_benchmark
            )
          ),
          expected = chisq_test_vs_external_estimate(
            survey_design = rep_design,
            y_var = "PARENT_HAS_EMAIL",
            ext_ests = parent_email_benchmark
          )
        )

        expect_warning(
          object = chisq_test_vs_external_estimate(
            survey_design = rep_design,
            y_var = "PARENT_HAS_EMAIL",
            ext_ests = 100*parent_email_benchmark
          ),
          regexp = "Rescaling values of external estimates to sum to 1"
        )
      })

  ##_ Check for error messages ----

    test_that("Informative errors messages for bad inputs", {
      expect_error(
        chisq_test_vs_external_estimate(
          survey_design = survey_design,
          y_var = "PARENT_HAS_EMAIL",
          ext_ests = parent_email_benchmark[1]
        ), regexp = "names matching the categories"
      )
      expect_error(
        chisq_test_vs_external_estimate(
          survey_design = survey_design,
          y_var = "PARENT_HAS_EMAIL",
          ext_ests = setNames(c(NA, NA), names(parent_email_benchmark))
        ), regexp = "external estimates must be"
      )
    })
