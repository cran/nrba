#' Summarize the variance estimation method for the survey design
#'
#' @param survey_design A survey design object created with the \code{survey} package.
#'
#' @return A text string describing the method used for variance estimation
#' @keywords internal
#'
#' @details For replicate designs, the type of replicates will be determined
#' based on the \code{'type'} element of the survey design object.
#' If \code{type = 'bootstrap'}, this can correspond to any of various
#' types of bootstrap replication (Canty-Davison bootstrap, Rao-Wu's (n-1) bootstrap, etc.).
#'
#' For designs which use linearization-based variance estimation, the summary
#' only indicates that linearization is used for variance estimation and, if a special
#' method is used for PPS variance estimation (e.g. Overton's approximation),
#' that PPS variance estimation method will be described.
#'
#'
# #' library(survey)
# #'
# #' data(election, package = 'survey')
# #' data(scd, package = 'survey')
# #'
# #' # Create several different types of survey design objects
# #'   dpps <- svydesign(id=~1, fpc=~p, data=election_pps, pps="brewer")
# #'   scddes <- svydesign(data=scd, prob=~1, id=~ambulance,
# #'                       strata=~ESA, nest=TRUE, fpc=rep(5,6))
# #'   scdnofpc <- svydesign(data=scd, prob=~1, id=~ambulance,
# #'                         strata=~ESA, nest=TRUE)
# #'   scd2brr <- as.svrepdesign(scdnofpc, type="BRR")
# #'   scd2fay <- as.svrepdesign(scdnofpc, type="Fay",fay.rho=0.3)
# #'   scd2jkn <- as.svrepdesign(scdnofpc, type="JKn")
# #'   scdmbb <- as.svrepdesign(scddes, type = "mrbbootstrap")
# #'
# #' # Get variance methods for each object
# #'   get_variance_method(dpps)
# #'   get_variance_method(scdnofpc)
# #'   get_variance_method(scd2brr)
# #'   get_variance_method(scd2fay)
# #'   get_variance_method(scd2jkn)
# #'
get_variance_method <- function(survey_design) {
  if ("svyrep.design" %in% class(survey_design)) {
    rep_type <- survey_design[['type']]
    variance_method <- switch(rep_type,
                              "mrbbootstrap" = "Preston's multistage rescaled bootstrap",
                              "subbootstrap" = "Rao and Wu's (n-1) bootstrap",
                              "bootstrap" = "Canty and Davison's bootstrap",
                              "JKn" = "Jackknife: JKn",
                              "JK1" = "Jackknife: JK1",
                              "JK2" = "Jackknife: JK2",
                              "BRR" = "Balanced Repeated Replication (BRR)",
                              "Fay" = "Fay's Method (Fay's BRR)",
                              "ACS" = "Successive differences replication",
                              "successive-difference" = "Successive differences replication")
  } else {
    is_pps_design <- FALSE
    if (!is.null(survey_design$pps)) {
      if (survey_design$pps != FALSE) {
        is_pps_design <- TRUE
      }
    }
    if (is_pps_design) {
      pps_string <- survey_design$call$pps
      if (!is.character(pps_string)) {
        pps_string <- deparse(pps_string)
      }
      pps_string <- tolower(trimws(pps_string))
      if (!pps_string %in% c("brewer", "overton")) {
        pps_string <- survey_design$variance
      }
      pps_string <- switch(pps_string,
                           "brewer" = "Brewer's approximation",
                           "overton" = "Overton's approximation",
                           "HT" = "Horvitz-Thompson",
                           "YG" = "Yates-Grundy")
      pps_string <- sprintf(", PPS (%s)", pps_string)
    } else {
      pps_string <- ""
    }
    variance_method <- sprintf("linearization%s", pps_string)
  }

  return(variance_method)
}
