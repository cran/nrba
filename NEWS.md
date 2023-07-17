# nrba 0.2.0

* The regression analysis types (`predict_response_status_via_glm()` and `predict_outcome_via_glm()`) now invisibly return an attribute named `reference_levels`. It can be retrieved using `attr(x, 'reference_levels')`. This is a data frame which lists, for each categorical predictor, the category used as a reference level in the regression. It is mainly useful for supporting the `idcnrba` package.

* Minor updates:

  * The `chisq_test_ind_response()` function now returns an informative error if you do not supply any variables to the `aux_vars` argument. Previously it returned a data frame with zero rows and columns.

  * Minor code updates required to keep up with new `dplyr` deprecations and new warnings in `*_join()` functions.
  
  * Added a variable `CONTACT_ATTEMPTS` to the example datasets, for use in level of effort analysis.
  
  * Made the involvement survey data examples more realistic by setting values of the outcome (`WHETHER_PARENT_AGREES`) equal to `NA` for all cases whose response status isn't equal to `"Respondent"`.
  
  * Simplified the `involvement_survey_str2s` dataset by removing columns of jackknife and bootstrap replicate weights.

# nrba 0.1.0

* Initial version of the package. This package is still undergoing active development, so please be prepared for potential breaking changes until version 1.0 of the package is released.
