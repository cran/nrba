% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_response_status_via_glm.R
\name{predict_response_status_via_glm}
\alias{predict_response_status_via_glm}
\title{Fit a logistic regression model to predict response to the survey.}
\usage{
predict_response_status_via_glm(
  survey_design,
  status,
  status_codes = c("ER", "EN", "IE", "UE"),
  numeric_predictors = NULL,
  categorical_predictors = NULL,
  model_selection = "main-effects",
  selection_controls = list(alpha_enter = 0.5, alpha_remain = 0.5, max_iterations = 100L)
)
}
\arguments{
\item{survey_design}{A survey design object created with the \code{survey} package.}

\item{status}{A character string giving the name of the variable representing response/eligibility status.
The \code{status} variable should have at most four categories,
representing eligible respondents (ER), eligible nonrespondents (EN),
known ineligible cases (IE), and cases whose eligibility is unknown (UE).}

\item{status_codes}{A named vector, with two entries named 'ER' and 'EN'
indicating which values of the \code{status} variable represent
eligible respondents (ER) and eligible nonrespondents (EN).}

\item{numeric_predictors}{A list of names of numeric auxiliary variables to use for predicting response status.}

\item{categorical_predictors}{A list of names of categorical auxiliary variables to use for predicting response status.}

\item{model_selection}{A character string specifying how to select a model.
The default and recommended method is 'main-effects', which simply includes main effects
for each of the predictor variables. \cr
The method \code{'stepwise'} can be used to perform stepwise selection of variables for the model.
However, stepwise selection invalidates p-values, standard errors, and confidence intervals,
which are generally calculated under the assumption that model specification is predetermined.}

\item{selection_controls}{Only required if \code{model-selection} isn't set to \code{"main-effects"}.
Otherwise, a list of parameters for model selection to pass on to \code{\link{stepwise_model_selection}},
with elements \code{alpha_enter}, \code{alpha_remain}, and \code{max_iterations}.}
}
\value{
A data frame summarizing the fitted logistic regression model. \cr

Each row in the data frame represents a coefficient in the model.
The column \code{variable} describes the underlying variable
for the coefficient. For categorical variables, the column \code{variable_category} indicates
the particular category of that variable for which a coefficient is estimated. \cr

The columns \code{estimated_coefficient}, \code{se_coefficient}, \code{conf_intrvl_lower}, \code{conf_intrvl_upper},
and \code{p_value_coefficient} are summary statistics for
the estimated coefficient. Note that \code{p_value_coefficient} is based on the Wald t-test for the coefficient. \cr

The column \code{variable_level_p_value} gives the p-value of the
Rao-Scott Likelihood Ratio Test for including the variable in the model.
This likelihood ratio test has its test statistic given by the column
\code{LRT_chisq_statistic}, and the reference distribution
for this test is a linear combination of \code{p} F-distributions
with numerator degrees of freedom given by \code{LRT_df_numerator} and
denominator degrees of freedom given by \code{LRT_df_denominator},
where \code{p} is the number of coefficients in the model corresponding to
the variable being tested.
}
\description{
A logistic regression model is fit to the sample data to
predict whether an individual responds to the survey (i.e. is an eligible respondent)
rather than a nonrespondent. Ineligible cases and cases with unknown eligibility status
are not included in this model. \cr

The function returns a summary of the model, including overall tests
for each variable of whether that variable improves the model's
ability to predict response status in the population of interest (not just in the random sample at hand). \cr

This model can be used to identify auxiliary variables associated with response status
and compare multiple auxiliary variables in terms of their ability to predict response status.
}
\details{
See Lumley and Scott (2017) for details of how regression models are fit to survey data.
For overall tests of variables, a Rao-Scott Likelihood Ratio Test is conducted
(see section 4 of Lumley and Scott (2017) for statistical details)
using the function \code{regTermTest(method = "LRT", lrt.approximation = "saddlepoint")}
from the 'survey' package. \cr

If the user specifies \code{model_selection = "stepwise"}, a regression model
is selected by adding and removing variables based on the p-value from a
likelihood ratio rest. At each stage, a single variable is added to the model if
the p-value of the likelihood ratio test from adding the variable is below \code{alpha_enter}
and its p-value is less than that of all other variables not already in the model.
Next, of the variables already in the model, the variable with the largest p-value
is dropped if its p-value is greater than \code{alpha_remain}. This iterative process
continues until a maximum number of iterations is reached or until
either all variables have been added to the model or there are no unadded variables
for which the likelihood ratio test has a p-value below \code{alpha_enter}.
}
\examples{
library(survey)

# Create a survey design ----
data(involvement_survey_str2s, package = "nrba")

survey_design <- survey_design <- svydesign(
  data = involvement_survey_str2s,
  weights = ~BASE_WEIGHT,
  strata = ~SCHOOL_DISTRICT,
  ids = ~ SCHOOL_ID + UNIQUE_ID,
  fpc = ~ N_SCHOOLS_IN_DISTRICT + N_STUDENTS_IN_SCHOOL
)

predict_response_status_via_glm(
  survey_design = survey_design,
  status = "RESPONSE_STATUS",
  status_codes = c(
    "ER" = "Respondent",
    "EN" = "Nonrespondent",
    "IE" = "Ineligible",
    "UE" = "Unknown"
  ),
  model_selection = "main-effects",
  numeric_predictors = c("STUDENT_AGE"),
  categorical_predictors = c("PARENT_HAS_EMAIL", "STUDENT_GRADE")
)

}
\references{
\itemize{
\item Lumley, T., & Scott A. (2017). Fitting Regression Models to Survey Data. Statistical Science 32 (2) 265 - 278. https://doi.org/10.1214/16-STS605
}
}
