suppressWarnings({
  suppressPackageStartupMessages({
    library(survey)
    library(nrba)
  })
})

# Create a survey design ----
data("involvement_survey_str2s", package = 'nrba')

involvement_survey_str2s <- transform(involvement_survey_str2s,
                                      response_status = sample(x = c(1, 2, 3, 4),
                                                               size = nrow(involvement_survey_str2s),
                                                               replace = TRUE))
survey_design <- svydesign(
  data = involvement_survey_str2s,
  weights = ~ BASE_WEIGHT,
  strata =  ~ SCHOOL_DISTRICT,
  ids =     ~ SCHOOL_ID             + UNIQUE_ID,
  fpc =     ~ N_SCHOOLS_IN_DISTRICT + N_STUDENTS_IN_SCHOOL
)


rep_design <- as.svrepdesign(survey_design, type = "mrbbootstrap")

# Test the function ----

  ##_ Single auxililary variable ----

    suppressMessages({
      test_result <- chisq_test_ind_response(survey_design,
                                             status = "response_status",
                                             status_codes = c("ER" = 1, "EN" = 2,
                                                              "IE" = 3, "UE" = 4),
                                             aux_vars = "PARENT_HAS_EMAIL")
    })

    expected_result <- svychisq(
      design = subset(survey_design, response_status %in% c(1,2)),
      formula = ~ response_status + PARENT_HAS_EMAIL,
      statistic = "F"
    )


    test_that("Correct result for single auxiliary variable", {
      expect_equal(object = test_result[,c("statistic", "ndf", "ddf", "p_value")],
                   expected = data.frame(statistic = expected_result$statistic,
                                         ndf = expected_result$parameter['ndf'],
                                         ddf = expected_result$parameter['ddf'],
                                         p_value = expected_result$p.value,
                                         row.names = NULL))
    })

  ##_ Multiple auxiliary variables ----

    suppressMessages({
      test_result <- chisq_test_ind_response(survey_design,
                                             status = "response_status",
                                             status_codes = c("ER" = 1, "EN" = 2,
                                                              "IE" = 3, "UE" = 4),
                                             aux_vars = c("STUDENT_RACE", "PARENT_HAS_EMAIL"))
    })

    expected_result <- list(
      'student_urm' = svychisq(
        design = subset(survey_design, response_status %in% c(1,2)),
        formula = ~ response_status + STUDENT_RACE,
        statistic = "F"
      ),
      'parent_has_email' = svychisq(
        design = subset(survey_design, response_status %in% c(1,2)),
        formula = ~ response_status + PARENT_HAS_EMAIL,
        statistic = "F"
      )
    )

    expected_result <- lapply(expected_result, function(output) {
      data.frame(statistic = output$statistic,
                 ndf = output$parameter['ndf'],
                 ddf = output$parameter['ddf'],
                 p_value = output$p.value,
                 row.names = NULL)
    })

    expected_result <- cbind(
      "auxiliary_variable" = c("STUDENT_RACE", "PARENT_HAS_EMAIL"),
      Reduce(rbind, expected_result)
    )


    test_that("Correct result for multiple auxiliary variables", {
      expect_equal(object = test_result[,c("auxiliary_variable", "statistic",
                                           "ndf", "ddf", "p_value")],
                   expected = expected_result)
    })

  ##_ Works with replicate designs ----

    suppressMessages({
      test_result <- chisq_test_ind_response(rep_design,
                                             status = "response_status",
                                             status_codes = c("ER" = 1, "EN" = 2,
                                                              "IE" = 3, "UE" = 4),
                                             aux_vars = "PARENT_HAS_EMAIL")
    })

    expected_result <- svychisq(
      design = subset(rep_design, response_status %in% c(1,2)),
      formula = ~ response_status + PARENT_HAS_EMAIL,
      statistic = "F"
    )


    test_that("Correct result for replicate design", {
      expect_equal(object = test_result[,c("statistic", "ndf", "ddf", "p_value")],
                   expected = data.frame(statistic = expected_result$statistic,
                                         ndf = expected_result$parameter['ndf'],
                                         ddf = expected_result$parameter['ddf'],
                                         p_value = expected_result$p.value,
                                         row.names = NULL))
    })

  ##_ Displays informative message if subsetting ----

    test_that("Displays message if subsetting", {
      expect_message({
        chisq_test_ind_response(survey_design = survey_design,
                                status = "response_status",
                                status_codes = c("ER" = 1, "EN" = 2,
                                                 "IE" = 3, "UE" = 4),
                                aux_vars = "PARENT_HAS_EMAIL")
      }, regexp = "Subsetting to only compare")
    })

# Expected error messages ----

    test_that("Informative error messages for bad inputs", {
      expect_error({
        chisq_test_ind_response(survey_design = survey_design |>
                                  subset(response_status %in% c(1,2)),
                                status = "response_status",
                                status_codes = c("ER" = 1, "EN" = 2,
                                                 "IE" = 3, "UE" = 4),
                                aux_vars = NULL)
      }, regexp = "Must specify variable names")

      expect_error({
        chisq_test_ind_response(survey_design = survey_design |>
                                  subset(response_status %in% c(1,2)),
                                status = "response_status",
                                status_codes = c("ER" = 1, "EN" = 2,
                                                 "IE" = 3, "UE" = 4),
                                aux_vars = "made_up_name")
      }, regexp = "do not appear in")
    })
