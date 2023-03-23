
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nrba <img src="man/figures/logo.png" align="right" height="50" />

The ‘nrba’ package is developed and maintained by [Westat,
Inc.](https://www.westat.com/capability/statistical-sciences/survey-statistics-methods-research)
If this package is used in publications, please cite the package as
follows:

> Schneider B, Green J, Brock S, Krenzke T, Jones M, Van de Kerckhove W,
> Ferraro D, Alvarez-Rojas L, Hubbell K (2023). “nrba: Methods for
> Conducting Nonresponse Bias Analysis.” R package version 0.1.0.
> Copyright: Westat, Inc.

The ‘nrba’ package facilitates nonresponse bias analysis (NRBA) for
survey data. Such data may arise from a complex sampling design with
features such as stratification, clustering, or unequal probabilities of
selection. Multiple types of analyses may be conducted:

- Comparisons of response rates across subgroups
- Tests of independence between response status and covariates
- Tests of systematic differences in covariate means between respondents
  and the entire sample
- Comparisons of estimates before and after weighting adjustments
- Comparisons of sample-based estimates to external population benchmark
  data
- Modeling of outcomes and response status as a function of covariates
- Calculating differences in estimates

Extensive documentation and references are provided for each type of
analysis. Variance estimation and weighting methods are implemented
using functionality from the ‘survey’ package. Outputs from each
analysis are generally represented as data frames, so that they can be
used as inputs to functions from ‘tidyverse’ packages such as ‘dplyr’ or
‘ggplot2’.

## Installation

You can install the released version of ‘nrba’ from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("nrba")
```

## Example Usage

To illustrate the usage of this package, we’ll consider example
(simulated) data from a survey of 5,000 parents of students with
disabilities.

``` r
library(nrba)
library(dplyr)
library(survey)
data("involvement_survey_srs", package = "nrba")
```

### Analyzing Response Rates

Of the 5,000 parents sampled for the survey, there are only 2,961 who
were eligible respondents to the survey. To understand the level of
nonresponse for the survey, we can calculate response rates using the
response rate formulas promulgated by the American Association of Public
Opinion Research (AAPOR).

``` r
overall_response_rates <- involvement_survey_srs |>
  calculate_response_rates(
    status = "RESPONSE_STATUS",
    status_codes = c(
      "ER" = "Respondent",
      "EN" = "Nonrespondent",
      "IE" = "Ineligible",
      "UE" = "Unknown"
    )
  )

print(overall_response_rates)
#>   RR3_Unweighted    n n_ER n_EN n_IE n_UE   e_unwtd
#> 1      0.6238782 5000 2961 1563  242  234 0.9492237
```

To understand how response rates vary across different subpopulations,
we can calculate response rates for different groups.

``` r
 involvement_survey_srs |>
  group_by(PARENT_HAS_EMAIL, STUDENT_RACE) |>
  calculate_response_rates(
    status = "RESPONSE_STATUS",
    status_codes = c(
      "ER" = "Respondent",
      "EN" = "Nonrespondent",
      "IE" = "Ineligible",
      "UE" = "Unknown"
    )
  )
```

    #> # A tibble: 14 × 9
    #>    PARENT_HAS_EMAIL STUDENT_RACE   RR3_U…¹     n  n_ER  n_EN  n_IE  n_UE e_unwtd
    #>    <chr>            <chr>            <dbl> <int> <int> <int> <int> <int>   <dbl>
    #>  1 Has Email        AM7 (American…   0.758    40    28     8     3     1   0.923
    #>  2 Has Email        AS7 (Asian)      0.712    39    27     9     1     2   0.973
    #>  3 Has Email        BL7 (Black or…   0.693   575   378   139    28    30   0.949
    #>  4 Has Email        HI7 (Hispanic…   0.356   807   273   457    38    39   0.951
    #>  5 Has Email        MU7 (Two or M…   0.662   114    72    31     5     6   0.954
    #>  6 Has Email        PI7 (Native H…   0.682    25    15     7     3     0   0.88 
    #>  7 Has Email        WH7 (White)      0.688  2662  1740   668   126   128   0.950
    #>  8 No Email         AM7 (American…   0.667     7     4     2     1     0   0.857
    #>  9 No Email         AS7 (Asian)      0.4       5     2     3     0     0   1    
    #> 10 No Email         BL7 (Black or…   0.676   119    79    30     2     8   0.982
    #> 11 No Email         HI7 (Hispanic…   0.318   141    41    85    12     3   0.913
    #> 12 No Email         MU7 (Two or M…   0.667    24    16     8     0     0   1    
    #> 13 No Email         PI7 (Native H…   0.667    10     6     3     1     0   0.9  
    #> 14 No Email         WH7 (White)      0.684   432   280   113    22    17   0.947
    #> # … with abbreviated variable name ¹​RR3_Unweighted

### Testing for Systematic Differences in Response Propensities

Since differences in response rates may be simply attributable to
sampling variability, a statistical test can be used to evaluate whether
observed differences in nonresponse among groups are statistically
significant.

``` r
library(survey)

involvement_survey <- svydesign(
  data = involvement_survey_srs,
  ids = ~ 1, weights = ~ BASE_WEIGHT
)

chisq_test_ind_response(
  survey_design = involvement_survey,
  status = "RESPONSE_STATUS",
  status_codes = c(
    "ER" = "Respondent",
    "EN" = "Nonrespondent",
    "IE" = "Ineligible",
    "UE" = "Unknown"
  ),
  aux_vars = c("PARENT_HAS_EMAIL", "STUDENT_RACE")
)
#>   auxiliary_variable statistic ndf   ddf      p_value
#> 1   PARENT_HAS_EMAIL  1.081471   1  4523 2.984242e-01
#> 2       STUDENT_RACE 64.600969   6 27138 4.941136e-80
#>                                           test_method variance_method
#> 1 Rao-Scott Chi-Square test (second-order adjustment)   linearization
#> 2 Rao-Scott Chi-Square test (second-order adjustment)   linearization
```

### Comparing Estimates from Respondents to Estimates from the Full Sample

To evaluate whether there are systematic differences between respondents
and the full sample, we can compare means and percentages estimated from
the respondent sample to means and percentages calculated using data
from the entire sample.

``` r
comparison_of_respondent_sample_to_full_eligible_sample <- t_test_resp_vs_elig(
  survey_design = involvement_survey,
  y_vars = c("PARENT_HAS_EMAIL"),
  status = "RESPONSE_STATUS",
  status_codes = c(
    "ER" = "Respondent",
    "EN" = "Nonrespondent",
    "IE" = "Ineligible",
    "UE" = "Unknown"
  )
)

comparison_of_respondent_sample_to_full_eligible_sample |>
  select(
    outcome, outcome_category,
    resp_mean, elig_mean,
    difference, std_error, p_value
  )
#>            outcome outcome_category resp_mean elig_mean   difference
#> 1 PARENT_HAS_EMAIL        Has Email 0.8554542 0.8514589  0.003995352
#> 2 PARENT_HAS_EMAIL         No Email 0.1445458 0.1485411 -0.003995352
#>     std_error   p_value
#> 1 0.003880093 0.3031981
#> 2 0.003880093 0.3031981
```

### Comparing Characteristics of Respondents to External Benchmark Data

A common method of nonresponse bias analysis is to compare the
characteristics of respondents to external benchmark data. Typically,
these external benchmarks come from large reference surveys (such as
those produced by the U.S. Census Bureau) or from administrative data.
If there are large discrepancies between the characteristics of
respondents and the external benchmarks, this may be indicative of
nonresponse bias or other forms of nonsampling error (such as coverage
error).

The function `t_test_vs_external_estimate()` allows the user to compare
the observed distribution of respondents against external benchmark
estimates. A t-test is used to evaluate whether differences are simply
attributable to sampling error.

``` r
# Subset the survey data to only include respondents
# NOTE: This should generally be done
#       *after* creating a survey design
involvement_survey_respondents <- involvement_survey |>
  subset(RESPONSE_STATUS == "Respondent")

# Set benchmark values to use for comparison
parent_email_benchmark <- c(
  "Has Email" = 0.83,
  "No Email" = 0.17
)

# Compare the respondents' characteristics to the benchmark values
t_test_vs_external_estimate(
  survey_design = involvement_survey_respondents,
  y_var = "PARENT_HAS_EMAIL",
  ext_ests = parent_email_benchmark
)
#>    category  estimate external_estimate  difference   std_error     p_value
#> 1 Has Email 0.8554542              0.83  0.02545424 0.006462868 8.38725e-05
#> 2  No Email 0.1445458              0.17 -0.02545424 0.006462868 8.38725e-05
#>   t_statistic   df
#> 1    3.938536 2959
#> 2   -3.938536 2959
```

### Comparing Estimates from Respondents, Before and After Weighting Adjustments

Since raking/calibration may potentially reduce nonresponse bias, it can
be informative to compare estimates before and after weighting
adjustments. For this purpose, it is helpful to use replicate weights,
which can easily be created using the ‘survey’ package.

``` r
# Create bootstrap replicate weights
replicate_design <- as.svrepdesign(involvement_survey, type = "bootstrap",
                                   replicates = 500)

# Subset to only respondents (always subset *after* creating replicate weights)
rep_svy_respondents <- subset(replicate_design,
                              RESPONSE_STATUS == "Respondent")
```

After creating the replicate weights, we can rake the survey weights
using population benchmark data.

``` r
raked_rep_svy_respondents <- rake_to_benchmarks(
  survey_design = rep_svy_respondents,
  group_vars = c("PARENT_HAS_EMAIL", "STUDENT_RACE"),
  group_benchmark_vars = c("PARENT_HAS_EMAIL_BENCHMARK",
                           "STUDENT_RACE_BENCHMARK"),
)
```

Now we can compare estimates from before and after raking. In this
example, we can see that raking produced a large, statistically
significant difference in the estimated proportions for the outcome
variable.

``` r
comparison_before_and_after_raking <- t_test_of_weight_adjustment(
  orig_design = rep_svy_respondents,
  updated_design = raked_rep_svy_respondents,
  y_vars = "WHETHER_PARENT_AGREES"
)

comparison_before_and_after_raking |>
  select(outcome, outcome_category,
         Original_mean, Adjusted_mean,
         difference, p_value)
#>                 outcome outcome_category Original_mean Adjusted_mean
#> 1 WHETHER_PARENT_AGREES            AGREE     0.5417089     0.5181013
#> 2 WHETHER_PARENT_AGREES         DISAGREE     0.4582911     0.4818987
#>    difference      p_value
#> 1  0.02360761 2.106909e-15
#> 2 -0.02360761 2.106909e-15
```
