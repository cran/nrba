suppressWarnings({
  suppressPackageStartupMessages({
    library(dplyr)
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

status_codes_to_use <- c(
  "ER" = 1,
  "EN" = 2,
  "IE" = 3,
  "UE" = 4
)

survey_design <- survey_design <- svydesign(
  data = involvement_survey_str2s,
  weights = ~ BASE_WEIGHT,
  strata =  ~ SCHOOL_DISTRICT,
  ids =     ~ SCHOOL_ID             + UNIQUE_ID,
  fpc =     ~ N_SCHOOLS_IN_DISTRICT + N_STUDENTS_IN_SCHOOL
)

rep_design <- as.svrepdesign(survey_design, type = "mrbbootstrap")

# Test a basic example ----

basic_numeric_outcome_output <- assess_range_of_bias(
  rep_design,
  y_var = "STUDENT_AGE",
  status = "response_status",
  status_codes = status_codes_to_use,
  assumed_multiple = c(0.5, 1, 1.5)
)

stats_for_analysis <- bind_cols(
  rep_design$variables |>
    calculate_response_rates(
      status = "response_status",
      status_codes = status_codes_to_use,
      rr_formula = "RR5",
      weights = "BASE_WEIGHT"
    ) |> select(wtd_rr = RR5_Weighted, Nhat_ER, Nhat_EN),
  rep_design |>
    srvyr::as_survey() |>
    filter(response_status == status_codes_to_use[['ER']]) |>
    summarise(
      resp_mean = survey_mean(x = STUDENT_AGE, vartype = NULL)
    )
)

expected_results <- lapply(X = c(0.5, 1, 1.5), FUN = function(mult) {
  stats_for_analysis |>
    mutate(
      assumed_mult = mult,
      nonresp_mean = mult * resp_mean,
      full_samp_mean = (wtd_rr*resp_mean) + ((1 - wtd_rr)*nonresp_mean),
    ) |>
    mutate(
      bias_of_unadj_estimate = resp_mean - full_samp_mean,
      bias_of_adj_estimate = resp_mean - full_samp_mean
    )
}) |> dplyr::bind_rows()

test_that(
  "Correct results for basic example with assumed multiples and no cells", {

    expect_equal(
      object = basic_numeric_outcome_output$wtd_rr,
      expected = expected_results$wtd_rr
    )
    expect_equal(
      object = basic_numeric_outcome_output$respondent_mean,
      expected = expected_results$resp_mean
    )

    expect_equal(
      object = basic_numeric_outcome_output$bias_of_adj_estimate,
      expected = expected_results$bias_of_adj_estimate
    )

    expect_equal(
      object = basic_numeric_outcome_output$bias_of_unadj_estimate,
      expected = expected_results$bias_of_unadj_estimate
    )

  }
)

# Test a basic example with cells and percentiles ----

nrba_output <- assess_range_of_bias(
  rep_design,
  y_var = "STUDENT_AGE",
  status = "response_status",
  comparison_cell = "STUDENT_SEX",
  status_codes = status_codes_to_use,
  assumed_percentile = c(0.234, 0.8)
)

stats_for_analysis <- bind_cols(
  rep_design$variables |>
    group_by(STUDENT_SEX) |>
    calculate_response_rates(
      status = "response_status",
      status_codes = status_codes_to_use,
      rr_formula = "RR5",
      weights = "BASE_WEIGHT"
    ) |> select(wtd_rr = RR5_Weighted, Nhat_ER, Nhat_EN),
  rep_design |>
    srvyr::as_survey() |>
    filter(response_status == status_codes_to_use[['ER']]) |>
    group_by(STUDENT_SEX) |>
    summarise(
      resp_mean = survey_mean(x = STUDENT_AGE, vartype = NULL),
      nonresp_mean = survey_quantile(x = STUDENT_AGE,
                                     quantiles = c(0.234, 0.8),
                                     na.rm = TRUE,
                                     vartype = NULL, qrule = "hf2")
    )
)

expected_results <- stats_for_analysis |>
  tidyr::pivot_longer(
    cols = matches("nonresp_mean"),
    names_to = "assumed_percentile",
    values_to = "nonresp_mean"
  ) |>
  mutate(assumed_percentile = gsub(x = assumed_percentile,
                                   pattern = "nonresp_mean_q",
                                   replacement = "")) |>
  mutate(assumed_percentile = paste0("0.", assumed_percentile) |>
           as.numeric()) |>
  mutate(Nhat_ELIG = (Nhat_ER + Nhat_EN)) |>
  group_by(assumed_percentile) |>
  summarise(
    full_samp_mean = sum(Nhat_ELIG * ((wtd_rr * resp_mean) + ((1 - wtd_rr)*nonresp_mean))) / sum(Nhat_ELIG),
    nr_adjusted_mean = sum(Nhat_ELIG * resp_mean) / sum(Nhat_ELIG),
    resp_mean = sum(Nhat_ER * resp_mean) / sum(Nhat_ER),
    wtd_rr = sum(Nhat_ER) / sum(Nhat_ELIG)
  ) |>
  mutate(
    bias_of_unadj_estimate = resp_mean - full_samp_mean,
    bias_of_adj_estimate = nr_adjusted_mean - full_samp_mean
  ) |>
  arrange(assumed_percentile)

test_that(
  "Correct results for basic example with assumed percentiles and comparison cells", {

    expect_equal(
      object = nrba_output$wtd_rr,
      expected = expected_results$wtd_rr
    )
    expect_equal(
      object = nrba_output$respondent_mean,
      expected = expected_results$resp_mean
    )

    expect_equal(
      object = nrba_output$bias_of_adj_estimate,
      expected = expected_results$bias_of_adj_estimate
    )

    expect_equal(
      object = nrba_output$bias_of_unadj_estimate,
      expected = expected_results$bias_of_unadj_estimate
    )

  }
)
