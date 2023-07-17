
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nrba <img src="man/figures/logo.png" align="right" height="50" />

The ‘nrba’ package is developed and maintained by [Westat,
Inc.](https://www.westat.com/capability/statistical-sciences/survey-statistics-methods-research)
If this package is used in publications, please cite the package as
follows:

> Schneider B, Green J, Brock S, Krenzke T, Jones M, Van de Kerckhove W,
> Ferraro D, Alvarez-Rojas L, Hubbell K (2023). “nrba: Methods for
> Conducting Nonresponse Bias Analysis.” R package version 0.2.0.
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

Of the 5,000 parents sampled for the survey, there are only 3,011 who
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
#> 1      0.6331604 5000 3011 1521  233  235 0.9511018
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
    #>    PARENT_HAS_EMAIL STUDENT_RACE    RR3_Unweighted     n  n_ER  n_EN  n_IE  n_UE
    #>    <chr>            <chr>                    <dbl> <int> <int> <int> <int> <int>
    #>  1 Has Email        AM7 (American …          0.690    29    20     8     0     1
    #>  2 Has Email        AS7 (Asian)              0.771    41    30     7     2     2
    #>  3 Has Email        BL7 (Black or …          0.696   592   387   141    34    30
    #>  4 Has Email        HI7 (Hispanic …          0.346   771   253   446    38    34
    #>  5 Has Email        MU7 (Two or Mo…          0.713    89    57    22     9     1
    #>  6 Has Email        PI7 (Native Ha…          0.847    27    22     3     1     1
    #>  7 Has Email        WH7 (White)              0.701  2685  1795   644   117   129
    #>  8 No Email         AM7 (American …          0.875     8     7     1     0     0
    #>  9 No Email         AS7 (Asian)              0.455    11     4     4     2     1
    #> 10 No Email         BL7 (Black or …          0.632   131    78    37     7     9
    #> 11 No Email         HI7 (Hispanic …          0.317   125    38    77     5     5
    #> 12 No Email         MU7 (Two or Mo…          0.696    23    16     6     0     1
    #> 13 No Email         PI7 (Native Ha…          0.556    10     5     4     1     0
    #> 14 No Email         WH7 (White)              0.679   458   299   121    17    21
    #> # ℹ 1 more variable: e_unwtd <dbl>

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
#> 1   PARENT_HAS_EMAIL  1.965074   1  4531 1.610401e-01
#> 2       STUDENT_RACE 70.005249   6 27186 6.654778e-87
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
#> 1 PARENT_HAS_EMAIL        Has Email 0.8515443 0.8462048  0.005339571
#> 2 PARENT_HAS_EMAIL         No Email 0.1484557 0.1537952 -0.005339571
#>     std_error  p_value
#> 1 0.003862094 0.166862
#> 2 0.003862094 0.166862
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
#>    category  estimate external_estimate  difference   std_error      p_value
#> 1 Has Email 0.8515443              0.83  0.02154434 0.006480225 0.0008959752
#> 2  No Email 0.1484557              0.17 -0.02154434 0.006480225 0.0008959752
#>   t_statistic   df
#> 1    3.324628 3009
#> 2   -3.324628 3009
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
#> 1 WHETHER_PARENT_AGREES            AGREE     0.5562936     0.5280613
#> 2 WHETHER_PARENT_AGREES         DISAGREE     0.4437064     0.4719387
#>    difference      p_value
#> 1  0.02823232 8.153992e-17
#> 2 -0.02823232 8.153992e-17
```
