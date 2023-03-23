#' Adjust weights in a replicate design for nonresponse or unknown eligibility status, using weighting classes
#'
#' @description Updates weights in a survey design object to adjust for nonresponse and/or unknown eligibility
#' using the method of weighting class adjustment. For unknown eligibility adjustments, the weight in each class
#' is set to zero for cases with unknown eligibility, and the weight of all other cases in the class is
#' increased so that the total weight is unchanged. For nonresponse adjustments, the weight in each class
#' is set to zero for cases classified as eligible nonrespondents, and the weight of eligible respondent cases
#' in the class is increased so that the total weight is unchanged. \cr
#' \cr
#' This function currently only works for survey designs with replicate weights,
#' since the linearization-based estimators included in the \code{survey} package (or Stata or SAS for that matter)
#' are unable to fully reflect the impact of nonresponse adjustment.
#' Adjustments are made to both the full-sample weights and all of the sets of replicate weights. \cr
#'
#' @param survey_design A replicate survey design object created with the `survey` package.
#' @param status A character string giving the name of the variable representing response/eligibility status. \cr
#' The \code{status} variable should have at most four categories,
#' representing eligible respondents (ER), eligible nonrespondents (EN),
#' known ineligible cases (IE), and cases whose eligibility is unknown (UE).
#' @param status_codes A named vector, with four entries named 'ER', 'EN', 'IE', and 'UE'. \cr
#' \code{status_codes} indicates how the values of the \code{status} variable are to be interpreted.
#' @param wt_class (Optional) A character string giving the name of the variable which divides sample cases into weighting classes. \cr
#' If \code{wt_class=NULL} (the default), adjustment is done using the entire sample.
#' @param type A character vector including one or more of the following options: \cr
#' - \code{'UE'}: Adjust for unknown eligibility. \cr
#' - \code{'NR'}: Adjust for nonresponse.
#' \cr
#' To sequentially adjust for unknown eligibility and then nonresponse, set \code{type=c('UE', 'NR')}.
#' @details See the vignette "Nonresponse Adjustments" from the svrep package for a step-by-step walkthrough of
#' nonresponse weighting adjustments in R: \cr
#' \code{vignette(topic = "nonresponse-adjustments", package = "svrep")}
#'
#' @references
#' See Chapter 2 of Heeringa, West, and Berglund (2017) or Chapter 13 of Valliant, Dever, and Kreuter (2018)
#' for an overview of nonresponse adjustment methods based on redistributing weights.
#'
#' - Heeringa, S., West, B., Berglund, P. (2017). Applied Survey Data Analysis, 2nd edition. Boca Raton, FL: CRC Press.
#' "Applied Survey Data Analysis, 2nd edition." Boca Raton, FL: CRC Press.
#'
#' - Valliant, R., Dever, J., Kreuter, F. (2018).
#'  "Practical Tools for Designing and Weighting Survey Samples, 2nd edition." New York: Springer.
#' @seealso [svrep::redistribute_weights()], `vignette(topic = "nonresponse-adjustments", package = "svrep")`
#' @return A replicate survey design object, with adjusted full-sample and replicate weights
#' @export
#'
#' @examples
#' library(survey)
#' # Load an example dataset
#' data("involvement_survey_str2s", package = "nrba")
#'
#' # Create a survey design object
#'
#' involvement_survey_sample <- svydesign(
#'   data = involvement_survey_str2s,
#'   weights = ~BASE_WEIGHT,
#'   strata = ~SCHOOL_DISTRICT,
#'   ids = ~ SCHOOL_ID + UNIQUE_ID,
#'   fpc = ~ N_SCHOOLS_IN_DISTRICT + N_STUDENTS_IN_SCHOOL
#' )
#'
#' rep_design <- as.svrepdesign(involvement_survey_sample, type = "mrbbootstrap")
#'
#' # Adjust weights for nonresponse within weighting classes
#' nr_adjusted_design <- wt_class_adjust(
#'   survey_design = rep_design,
#'   status = "RESPONSE_STATUS",
#'   status_codes = c(
#'     "ER" = "Respondent",
#'     "EN" = "Nonrespondent",
#'     "IE" = "Ineligible",
#'     "UE" = "Unknown"
#'   ),
#'   wt_class = "PARENT_HAS_EMAIL",
#'   type = "NR"
#' )
wt_class_adjust <- function(survey_design, status, status_codes, wt_class = NULL, type = c("UE", "NR")) {

  # Parameter checks

  ## _ Survey design object
  if (!any(c("svyrep.design") %in% class(survey_design))) {
    error_msg <- paste(
      "`survey_design` should be a replicate survey design object created using the `survey` or `srvyr` package.",
      "\nSee `help('svrepdesign', package = 'survey')` for instructions on how to set up an object if the replicate weights are already available.",
      "\nSee `help('as.svrepdesign', package = 'survey')` for instructions on how to create replicate weights for an existing survey design object.",
      "\n"
    )
    stop(error_msg)
  }

  ## _ Status variable and codes

  if (missing(status)) {
    stop("A variable name must be supplied to the `status` parameter")
  }

  if (!is.character(status) || length(status) != 1) {
    stop("Must specify a single variable name for `status`")
  }
  if (!status %in% colnames(survey_design)) {
    stop(sprintf(
      "The status variable '%s' does not appear in the supplied data",
      status
    ))
  }

  if (missing(status_codes) || is.null(status_codes)) {
    stop("Must supply status codes to the `status_codes` parameter.")
  }

  if (!((is.vector(status_codes) || is.factor(status_codes)) && !is.list(status_codes)) || is.null(names(status_codes))) {
    stop("Must supply a named vector for `status_codes`.")
  }

  valid_status_code_names <- !any(is.na(names(status_codes)))
  valid_status_code_names <- valid_status_code_names && is.logical(all.equal(sort(names(status_codes)), c("EN", "ER", "IE", "UE")))
  if (length(status_codes) != 4 || !valid_status_code_names) {
    stop("`status_codes` must be a vector with four values, with names: 'ER', 'EN', 'IE', and 'UE'.")
  }

  if (length(unique(status_codes)) != 4) {
    stop("Must use distinct values for the status codes supplied to `status_codes`.")
  }

  valid_status_values <- all(survey_design$variables[[status]] %in% status_codes)
  if (!valid_status_values) {
    stop("The data contains values in the status variable which are not listed in `status_codes`.")
  }

  ## _ Weighting class variable

  if (is.null(wt_class)) {
    survey_design[["variables"]][["_wt_class_"]] <- rep("Full sample", nrow(survey_design))
    wt_class <- "_wt_class_"
  }

  if (!is.null(wt_class) && (!is.character(wt_class) || length(wt_class) == 0)) {
    stop("`wt_class` must be a single variable name (i.e. a length-one character vector).")
  }

  if (!all(wt_class %in% colnames(survey_design))) {
    error_msg <- sprintf(
      "The specified `wt_class` variable, `%s`, is not included in the survey design object.",
      wt_class
    )
    stop(error_msg)
  }

  if (any(is.na(survey_design$variables[[wt_class]]))) {
    error_msg <- sprintf(
      "The specified `wt_class` variable, `%s`, should not have any missing values.",
      wt_class
    )
    stop(error_msg)
  }

  ## _ Adjustment type

  if (missing(type) || is.null(type)) {
    stop("Must supply at least one value for the `type` parameter.")
  }

  if (!is.character(type)) {
    stop("Must supply a vector for the argument `type`.")
  }

  if (length(type) == 0 || !all(type %in% c("UE", "NR"))) {
    stop("The only possible values for `type` are: 'UE', 'NR', or c('UE', 'NR').")
  }

  # Iterate over adjustment types in order: UE -> NR
  rep_design <- survey_design

  ordered_adj_types <- intersect(c("UE", "NR"), toupper(type))

  for (adj_type in ordered_adj_types) {
    if (adj_type == "UE") {
      in_group_to_upweight <- rep_design$variables[[status]] %in% status_codes[c("ER", "EN", "IE")]
      in_group_to_downweight <- rep_design$variables[[status]] %in% status_codes[c("UE")]
    }

    if (adj_type == "NR") {
      in_group_to_upweight <- rep_design$variables[[status]] %in% status_codes[c("ER")]
      in_group_to_downweight <- rep_design$variables[[status]] %in% status_codes[c("EN")]
    }

    rep_design <- svrep::redistribute_weights(
      design = rep_design,
      increase_if = in_group_to_upweight,
      reduce_if = in_group_to_downweight,
      by = wt_class
    )
  }

  if ("_wt_class_" %in% colnames(rep_design)) {
    rep_design[["variables"]][["_wt_class_"]] <- NULL
  }

  return(rep_design)
}
