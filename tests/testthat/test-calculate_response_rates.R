suppressWarnings({
  suppressPackageStartupMessages(library(dplyr, quietly = TRUE))
})

# Create example data ----
sample_dispositions <- data.frame(
  'Response_Status' = c(rep(1, 20), rep(2,20), rep(3,4), rep(4,6)),
  'Region' = c(rep("North", times = 15), rep("South", times = 5),
               rep("North", times = 5), rep("South", times = 15),
               c("North", "North", "North", "South"),
               rep("North", 4), rep("South", 2)),
  'Age' = rep(c("Young", "Old"), each = 25)
)
sample_dispositions[['Base_Weight']] <- ifelse(1:50 %% 2 == 0, 0.5, 2) + as.numeric(sample_dispositions[['Region']] == "North")
sample_dispositions[['e_region']] <- ifelse(sample_dispositions[['Region']] == 'North', 0.75, 0.25)

# Calculate expected results for example data ----

expected_overall_elig_rate_unwtd <- sum(sample_dispositions[['Response_Status']] %in% c(1,2)) / sum(sample_dispositions[['Response_Status']] %in% c(1,2,3))
expected_overall_elig_rate_wtd <- sum(sample_dispositions[['Base_Weight']] * (sample_dispositions[['Response_Status']] %in% c(1,2))) / sum(sample_dispositions[['Base_Weight']] * (sample_dispositions[['Response_Status']] %in% c(1,2,3)))

expected_region_results <- list()

expected_region_results[['Basic']] <- sample_dispositions %>%
  dplyr::mutate(Response_Type = dplyr::case_when(Response_Status == 1 ~ "ER",
                                          Response_Status == 2 ~ "EN",
                                          Response_Status == 3 ~ "IE",
                                          Response_Status == 4 ~ "UE")) %>%
  dplyr::group_by(Region) %>%
  dplyr::summarize(n = dplyr::n(),
                   Nhat = sum(Base_Weight),
                   e_region = unique(e_region),
                   n_ER = sum(Response_Type == "ER"),
                   Nhat_ER = sum(Base_Weight * (Response_Type == "ER")),
                   n_EN = sum(Response_Type == "EN"),
                   Nhat_EN = sum(Base_Weight * (Response_Type == "EN")),
                   n_IE = sum(Response_Type == "IE"),
                   Nhat_IE = sum(Base_Weight * (Response_Type == "IE")),
                   n_UE = sum(Response_Type == "UE"),
                   Nhat_UE = sum(Base_Weight * (Response_Type == "UE"))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(e_unwtd_subgroup = (n_ER + n_EN)/(n_ER + n_EN + n_IE),
                e_wtd_subgroup = (Nhat_ER + Nhat_EN)/(Nhat_ER + Nhat_EN + Nhat_IE))

  expected_region_results[['CASRO-overall']] <- expected_region_results[['Basic']] %>%
    dplyr::mutate(e_wtd = expected_overall_elig_rate_wtd,
                  e_unwtd = expected_overall_elig_rate_unwtd) %>%
    dplyr::mutate(
      RR1_Unweighted = n_ER / (n_ER + n_EN + n_UE),
      RR1_Weighted = Nhat_ER / (Nhat_ER + Nhat_EN + Nhat_UE),
      RR3_Unweighted = n_ER / (n_ER + n_EN + e_unwtd * n_UE),
      RR3_Weighted = Nhat_ER / (Nhat_ER + Nhat_EN + e_wtd * Nhat_UE),
      RR5_Unweighted = n_ER / (n_ER + n_EN),
      RR5_Weighted = Nhat_ER / (Nhat_ER + Nhat_EN)
    ) %>%
    dplyr::select(-e_region, -e_wtd_subgroup, -e_unwtd_subgroup)

  expected_region_results[['CASRO-subgroup']] <- expected_region_results[['Basic']] %>%
    dplyr::mutate(e_wtd = e_wtd_subgroup, e_unwtd = e_unwtd_subgroup) %>%
    dplyr::mutate(
      RR1_Unweighted = n_ER / (n_ER + n_EN + n_UE),
      RR1_Weighted = Nhat_ER / (Nhat_ER + Nhat_EN + Nhat_UE),
      RR3_Unweighted = n_ER / (n_ER + n_EN + e_unwtd * n_UE),
      RR3_Weighted = Nhat_ER / (Nhat_ER + Nhat_EN + e_wtd * Nhat_UE),
      RR5_Unweighted = n_ER / (n_ER + n_EN),
      RR5_Weighted = Nhat_ER / (Nhat_ER + Nhat_EN)
    ) %>%
    dplyr::select(-e_region, -e_wtd_subgroup, -e_unwtd_subgroup)

  expected_region_results[['specified-value']] <- expected_region_results[['Basic']] %>%
    dplyr::mutate(e_wtd = 0.5, e_unwtd = 0.5) %>%
    dplyr::mutate(
      RR1_Unweighted = n_ER / (n_ER + n_EN + n_UE),
      RR1_Weighted = Nhat_ER / (Nhat_ER + Nhat_EN + Nhat_UE),
      RR3_Unweighted = n_ER / (n_ER + n_EN + e_unwtd * n_UE),
      RR3_Weighted = Nhat_ER / (Nhat_ER + Nhat_EN + e_wtd * Nhat_UE),
      RR5_Unweighted = n_ER / (n_ER + n_EN),
      RR5_Weighted = Nhat_ER / (Nhat_ER + Nhat_EN)
    ) %>%
    dplyr::select(-e_region, -e_wtd_subgroup, -e_unwtd_subgroup)

  expected_region_results[['specified-variable']] <- expected_region_results[['Basic']] %>%
    dplyr::mutate(e_wtd = e_region, e_unwtd = e_region) %>%
    dplyr::mutate(
      RR1_Unweighted = n_ER / (n_ER + n_EN + n_UE),
      RR1_Weighted = Nhat_ER / (Nhat_ER + Nhat_EN + Nhat_UE),
      RR3_Unweighted = n_ER / (n_ER + n_EN + e_unwtd * n_UE),
      RR3_Weighted = Nhat_ER / (Nhat_ER + Nhat_EN + e_wtd * Nhat_UE),
      RR5_Unweighted = n_ER / (n_ER + n_EN),
      RR5_Weighted = Nhat_ER / (Nhat_ER + Nhat_EN)
    ) %>%
    dplyr::select(-e_region, -e_wtd_subgroup, -e_unwtd_subgroup)


# Get results from package function ----
result <- list()

result[['specified-value']] <- sample_dispositions %>%
  group_by(Region) %>%
  calculate_response_rates(status = "Response_Status",
                           status_codes = c("ER" = 1, "EN" = 2, "IE" = 3, "UE" = 4),
                           weights = "Base_Weight",
                           rr_formula = c("RR1", "RR3", "RR5"),
                           elig_method = 'specified',
                           e = 0.5)

result[['specified-variable']] <- sample_dispositions %>%
  group_by(Region) %>%
  calculate_response_rates(status = "Response_Status",
                           status_codes = c("ER" = 1, "EN" = 2, "IE" = 3, "UE" = 4),
                           weights = "Base_Weight",
                           rr_formula = c("RR1", "RR3", "RR5"),
                           elig_method = 'specified',
                           e = "e_region")

result[['CASRO-overall']] <- sample_dispositions %>%
  group_by(Region) %>%
  calculate_response_rates(status = "Response_Status",
                           status_codes = c("ER" = 1, "EN" = 2, "IE" = 3, "UE" = 4),
                           weights = "Base_Weight",
                           rr_formula = c("RR1", "RR3", "RR5"),
                           elig_method = "CASRO-overall")

result[['CASRO-subgroup']] <- sample_dispositions %>%
  group_by(Region) %>%
  calculate_response_rates(status = "Response_Status",
                           status_codes = c("ER" = 1, "EN" = 2, "IE" = 3, "UE" = 4),
                           weights = "Base_Weight",
                           rr_formula = c("RR1", "RR3", "RR5"),
                           elig_method = "CASRO-subgroup")

result <- lapply(result, as.data.frame)
expected_region_results <- lapply(expected_region_results, as.data.frame)

# Basic tests of calculations ----
  test_that("Calculations correct when eligibility rate is a single specified value", {
    expect_equal(object = result[['specified-value']][names(expected_region_results$`specified-value`)],
                 expected = expected_region_results$`specified-value`)
  })
  test_that("Calculations correct when eligibility rate of subgroups given by variable in the data", {
    expect_equal(object = result[['specified-variable']][names(expected_region_results$`specified-variable`)],
                 expected = expected_region_results$`specified-variable`)
  })
  test_that("Calculations correct when eligibility rate method set to `CASRO-overall`", {
    expect_equal(object = result[['CASRO-overall']][names(expected_region_results$`CASRO-overall`)],
                 expected = expected_region_results$`CASRO-overall`)
  })
  test_that("Calculations correct when eligibility rate method set to `CASRO-subgroup`", {
    expect_equal(object = result[['CASRO-subgroup']][names(expected_region_results$`CASRO-subgroup`)],
                 expected = expected_region_results$`CASRO-subgroup`)
  })

# Test that different kinds of inputs will work correctly ----

  disp_data_w_factor_status <- sample_dispositions %>%
    dplyr::mutate(
      Factor_Status = factor(Response_Status, levels = 1:4, labels = c("R", "NR", "Ineligible", "Unknown"))
    )

  status_factor_result <- disp_data_w_factor_status %>%
    group_by(Region) %>%
    calculate_response_rates(status = "Factor_Status",
                             status_codes = c("ER" = "R",
                                              "EN" = "NR",
                                              "IE" = "Ineligible",
                                              "UE" = "Unknown"),
                             weights = "Base_Weight",
                             rr_formula = c("RR1", "RR3", "RR5"),
                             elig_method = 'specified',
                             e = 0.5) %>%
    as.data.frame()

    test_that("The `status` variable can be a factor", {
      expect_equal(object = status_factor_result[names(expected_region_results$`specified-value`)],
                   expected = expected_region_results$`specified-value`)
    })
