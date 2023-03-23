suppressWarnings({
  suppressPackageStartupMessages({
    library(survey)
    library(nrba)
  })
})

# Load the survey data

data("involvement_survey_str2s", package = 'nrba')
data("involvement_survey_pop", package = 'nrba')

# Calculate population benchmarks
population_benchmarks <- list(
  'PARENT_HAS_EMAIL' = NULL,
  'STUDENT_DISABILITY_CATEGORY' = NULL
)

population_benchmarks <- lapply(
  setNames(names(population_benchmarks),
           names(population_benchmarks)),
  function(nm) {
    pop_xtabs <- xtabs(data = involvement_survey_pop,
                       reformulate(nm))
    df <- as.data.frame(pop_xtabs)
    colnames(df) <- c(nm, paste0(nm, "_benchmark"))
    return(df)
  })

# Add the population benchmarks as variables in the data
involvement_survey_str2s <- merge(
  x = involvement_survey_str2s,
  y = population_benchmarks$PARENT_HAS_EMAIL,
  by = "PARENT_HAS_EMAIL"
)
involvement_survey_str2s <- merge(
  x = involvement_survey_str2s,
  y = population_benchmarks$STUDENT_DISABILITY_CATEGORY,
  by = "STUDENT_DISABILITY_CATEGORY"
)

# Create a survey design object
  survey_design <- survey_design <- svydesign(
    data = involvement_survey_str2s,
    weights = ~ BASE_WEIGHT,
    strata =  ~ SCHOOL_DISTRICT,
    ids =     ~ SCHOOL_ID             + UNIQUE_ID,
    fpc =     ~ N_SCHOOLS_IN_DISTRICT + N_STUDENTS_IN_SCHOOL
  )
# Subset data to only include respondents
survey_respondents <- subset(
  survey_design,
  RESPONSE_STATUS == "Respondent"
)

# Rake to the benchmarks using nrba package
raked_survey_design <- rake_to_benchmarks(
  survey_design = survey_respondents,
  group_vars = c("PARENT_HAS_EMAIL", "STUDENT_DISABILITY_CATEGORY"),
  group_benchmark_vars = c("PARENT_HAS_EMAIL_benchmark",
                           "STUDENT_DISABILITY_CATEGORY_benchmark"),
)

# Rake to the benchmarks using the survey package
survey_pkg_raked <- survey::rake(
  design = survey_respondents,
  sample.margins = list(~ PARENT_HAS_EMAIL, ~ STUDENT_DISABILITY_CATEGORY),
  population.margins = population_benchmarks,
  control = list(maxit = formals(rake_to_benchmarks)$max_iterations,
                 epsilon = formals(rake_to_benchmarks)$epsilon,
                 verbose = FALSE)
)

# Post-stratify to a benchmark using nrba package
poststratified_survey_design <- rake_to_benchmarks(
  survey_design = survey_respondents,
  group_vars = c("PARENT_HAS_EMAIL"),
  group_benchmark_vars = c("PARENT_HAS_EMAIL_benchmark"),
)

# Post-stratify to a benchmark using the survey package
survey_pkg_poststratified <- survey::postStratify(
  design = survey_respondents,
  strata = ~ PARENT_HAS_EMAIL,
  population = population_benchmarks$PARENT_HAS_EMAIL
)

# Compare post estimates and standard errors

test_that(
  "Point estimate and standard errors from raking match survey package", {
    expect_equal(
      object = svytotal(x = ~ WHETHER_PARENT_AGREES,
                        design = raked_survey_design),
      expected = svytotal(x = ~ WHETHER_PARENT_AGREES,
                          design = survey_pkg_raked)
    )
  })

test_that(
  "Point estimate and standard errors from poststratifying match survey package", {
    expect_equal(
      object = svytotal(x = ~ WHETHER_PARENT_AGREES,
                        design = poststratified_survey_design),
      expected = svytotal(x = ~ WHETHER_PARENT_AGREES,
                          design = survey_pkg_poststratified)
    )
  })

# Informative error messages for bad inputs

bad_survey_data <- survey_respondents
bad_survey_data$variables$STUDENT_DISABILITY_CATEGORY[10] <- NA
test_that(
  "Informative error message for missing values in grouping variable", {
  expect_error(
    object = {rake_to_benchmarks(
      survey_design = bad_survey_data,
      group_vars = c("PARENT_HAS_EMAIL", "STUDENT_DISABILITY_CATEGORY"),
      group_benchmark_vars = c("PARENT_HAS_EMAIL_benchmark",
                               "STUDENT_DISABILITY_CATEGORY_benchmark"),
    )},
    regexp = "should not have any missing values"
  )
})

bad_survey_data <- survey_respondents
bad_survey_data$variables$STUDENT_DISABILITY_CATEGORY_benchmark[10] <- NA
test_that(
  "Informative error message for missing values in benchmark variable", {
    expect_error(
      object = {rake_to_benchmarks(
        survey_design = bad_survey_data,
        group_vars = c("PARENT_HAS_EMAIL", "STUDENT_DISABILITY_CATEGORY"),
        group_benchmark_vars = c("PARENT_HAS_EMAIL_benchmark",
                                 "STUDENT_DISABILITY_CATEGORY_benchmark"),
      )},
      regexp = "should not have any missing values"
    )
  })

bad_survey_data <- survey_respondents
bad_survey_data$variables$STUDENT_DISABILITY_CATEGORY_benchmark[10] <- 18
test_that(
  "Informative error message for when group has multiple values of benchmark variable", {
    expect_error(
      object = {rake_to_benchmarks(
        survey_design = bad_survey_data,
        group_vars = c("PARENT_HAS_EMAIL", "STUDENT_DISABILITY_CATEGORY"),
        group_benchmark_vars = c("PARENT_HAS_EMAIL_benchmark",
                                 "STUDENT_DISABILITY_CATEGORY_benchmark"),
      )},
      regexp = "should only be associated with one unique value"
    )
  })

# Informative error when raking dimension too small ----
suppressWarnings({
  rep_design <- survey_design |> as.svrepdesign(type = "JKn")
})
rep_design$variables[['bad_raking_variable']] <- ifelse(
  rep_design$variables$SCHOOL_ID == rep_design$variables$SCHOOL_ID[1],
  "SCHOOL_1", "OTHER_SCHOOLS"
)
rep_design[['variables']][['RAKING_TARGET']] <- ifelse(
  rep_design$variables[['bad_raking_variable']] == "SCHOOL_1",
  5000, 10000
)
test_that(
  "Informative error message when a raking dimension has zero observations in replicate", {
    expect_error(
      object = {rake_to_benchmarks(
        survey_design = rep_design,
        group_vars = c("bad_raking_variable"),
        group_benchmark_vars = c("RAKING_TARGET"),
      )},
      regexp = "Unable to conduct"
    )
  })

