#' Calculate Response Rates
#'
#' @description Calculates response rates using one of the response rate formulas
#' defined by AAPOR (American Association of Public Opinion Research).
#' @param data A data frame containing the selected sample, one row per case.
#' @param status A character string giving the name of the variable representing response/eligibility status.
#' The \code{status} variable should have at most four categories,
#' representing eligible respondents (ER), eligible nonrespondents (EN),
#' known ineligible cases (IE), and cases whose eligibility is unknown (UE).
#' @param status_codes A named vector, with four entries named 'ER', 'EN', 'IE', and 'UE'.
#' \code{status_codes} indicates how the values of the \code{status} variable are to be interpreted.
#' @param weights (Optional) A character string giving the name of a variable representing weights in the data
#' to use for calculating weighted response rates
#' @param rr_formula A character vector including any of the following: 'RR1', 'RR3', and 'RR5'. \cr
#' These are the names of formulas defined by AAPOR. See the \emph{Formulas} section below for formulas.
#' @param elig_method If \code{rr_formula='RR3'}, this specifies how to estimate
#' an eligibility rate for cases with unknown eligibility. Must be one of the following: \cr
#' \cr
#' \code{'CASRO-overall'} \cr
#' Estimates an eligibility rate using the overall sample.
#' If response rates are calculated for subgroups, the single overall sample estimate
#' will be used as the estimated eligibility rate for subgroups as well. \cr
#' \cr
#' \code{'CASRO-subgroup'} \cr
#' Estimates eligibility rates separately for each subgroup. \cr
#' \cr
#' \code{'specified'} \cr
#' With this option, a numeric value is supplied by the user to the parameter \code{e}. \cr
#' \cr
#' For \code{elig_method='CASRO-overall'} or \code{elig_method='CASRO-subgroup'},
#' the eligibility rate is estimated as \eqn{(ER)/(ER + NR + IE)}.
#' @param e (Required if \code{elig_method='specified'}). A numeric value between 0 and 1 specifying the estimated eligibility rate for cases with unknown eligibility.
#' A character string giving the name of a numeric variable may also be supplied; in that case, the eligibility rate must be constant for all cases in a subgroup.
#'
#' @return Output consists of a data frame giving weighted and unweighted response rates. The following columns may be included, depending on the arguments supplied:
#' \itemize{
#'  \item{\code{RR1_Unweighted}}
#'  \item{\code{RR1_Weighted}}
#'  \item{\code{RR3_Unweighted}}
#'  \item{\code{RR3_Weighted}}
#'  \item{\code{RR5_Unweighted}}
#'  \item{\code{RR5_Weighted}}
#'  \item{\code{n}: Total sample size}
#'  \item{\code{Nhat}: Sum of weights for the total sample}
#'  \item{\code{n_ER}: Number of eligible respondents}
#'  \item{\code{Nhat_ER}: Sum of weights for eligible respondents}
#'  \item{\code{n_EN}: Number of eligible nonrespondents}
#'  \item{\code{Nhat_EN}: Sum of weights for eligible nonrespondents}
#'  \item{\code{n_IE}: Number of ineligible cases}
#'  \item{\code{Nhat_IE}: Sum of weights for ineligible cases}
#'  \item{\code{n_UE}: Number of cases whose eligibility is unknown}
#'  \item{\code{Nhat_UE}: Sum of weights for cases whose eligibility is unknown}
#'  \item{\code{e_unwtd}: If \emph{RR3} is calculated, the eligibility rate estimate \emph{e} used for the unweighted response rate.}
#'  \item{\code{e_wtd}: If \emph{RR3} is calculated, the eligibility rate estimate \emph{e} used for the weighted response rate.}
#' }
#'
#' If the data frame is grouped (i.e. by using \code{df %>% group_by(Region)}),
#' then the output contains one row per subgroup.
#'
#' @section Formulas:
#' Denote the sample totals as follows:
#' \itemize{
#'  \item{\strong{ER}: Total number of eligible respondents}
#'  \item{\strong{EN}: Total number of eligible non-respondents}
#'  \item{\strong{IE}: Total number of ineligible cases}
#'  \item{\strong{UE}: Total number of cases whose eligibility is unknown}
#' }
#'
#' For weighted response rates, these totals are calculated using weights.
#'
#' The response rate formulas are then as follows:
#'
#' \deqn{RR1 = ER / ( ER + EN + UE )}
#'
#' RR1 essentially assumes that all cases with unknown eligibility are in fact eligible.
#'
#' \deqn{RR3 = ER / ( ER + EN + (e * UE) )}
#'
#' RR3 uses an estimate, \emph{e}, of the eligibility rate among cases with unknown eligibility.
#'
#' \deqn{RR5 = ER / ( ER + EN )}
#'
#' RR5 essentially assumes that all cases with unknown eligibility are in fact ineligible. \cr
#'
#'
#' For \emph{RR3}, an estimate, \code{e}, of the eligibility rate among cases with unknown eligibility must be used.
#' AAPOR strongly recommends that the basis for the estimate should be explicitly stated and detailed.
#'
#' The CASRO methods, which might be appropriate for the design, use the formula \eqn{e = 1 - ( IE / (ER + EN + IE) )}.
#'
#' - For \code{elig_method='CASRO-overall'}, an estimate is calculated for the overall sample
#' and this single estimate is used when calculating response rates for subgroups.
#'
#' - For \code{elig_method='CASRO-subgroup'}, estimates are calculated separately for each subgroup.
#'
#'
#' Please consult AAPOR's current \emph{Standard Definitions} for in-depth explanations.
#' @export
#'
#' @examples
#' # Load example data
#' data(involvement_survey_srs, package = "nrba")
#'
#' involvement_survey_srs[["RESPONSE_STATUS"]] <- sample(1:4, size = 5000, replace = TRUE)
#'
#' # Calculate overall response rates
#'
#' involvement_survey_srs %>%
#'   calculate_response_rates(
#'     status = "RESPONSE_STATUS",
#'     status_codes = c("ER" = 1, "EN" = 2, "IE" = 3, "UE" = 4),
#'     weights = "BASE_WEIGHT",
#'     rr_formula = "RR3",
#'     elig_method = "CASRO-overall"
#'   )
#'
#' # Calculate response rates by subgroup
#'
#' library(dplyr)
#'
#' involvement_survey_srs %>%
#'   group_by(STUDENT_RACE, STUDENT_SEX) %>%
#'   calculate_response_rates(
#'     status = "RESPONSE_STATUS",
#'     status_codes = c("ER" = 1, "EN" = 2, "IE" = 3, "UE" = 4),
#'     weights = "BASE_WEIGHT",
#'     rr_formula = "RR3",
#'     elig_method = "CASRO-overall"
#'   )
#'
#' # Compare alternative approaches for handling of cases with unknown eligiblity
#'
#' involvement_survey_srs %>%
#'   group_by(STUDENT_RACE) %>%
#'   calculate_response_rates(
#'     status = "RESPONSE_STATUS",
#'     status_codes = c("ER" = 1, "EN" = 2, "IE" = 3, "UE" = 4),
#'     rr_formula = "RR3",
#'     elig_method = "CASRO-overall"
#'   )
#'
#' involvement_survey_srs %>%
#'   group_by(STUDENT_RACE) %>%
#'   calculate_response_rates(
#'     status = "RESPONSE_STATUS",
#'     status_codes = c("ER" = 1, "EN" = 2, "IE" = 3, "UE" = 4),
#'     rr_formula = "RR3",
#'     elig_method = "CASRO-subgroup"
#'   )
#'
#' involvement_survey_srs %>%
#'   group_by(STUDENT_RACE) %>%
#'   calculate_response_rates(
#'     status = "RESPONSE_STATUS",
#'     status_codes = c("ER" = 1, "EN" = 2, "IE" = 3, "UE" = 4),
#'     rr_formula = "RR3",
#'     elig_method = "specified",
#'     e = 0.5
#'   )
#'
#' involvement_survey_srs %>%
#'   transform(e_by_email = ifelse(PARENT_HAS_EMAIL == "Has Email", 0.75, 0.25)) %>%
#'   group_by(PARENT_HAS_EMAIL) %>%
#'   calculate_response_rates(
#'     status = "RESPONSE_STATUS",
#'     status_codes = c("ER" = 1, "EN" = 2, "IE" = 3, "UE" = 4),
#'     rr_formula = "RR3",
#'     elig_method = "specified",
#'     e = "e_by_email"
#'   )
#'
#' @references The American Association for Public Opinion Research. 2016. \emph{Standard Definitions:
#' Final Dispositions of Case Codes and Outcome Rates for Surveys. 9th edition.} AAPOR.
#'
calculate_response_rates <- function(data, status, status_codes = c("ER", "EN", "IE", "UE"), weights, rr_formula = "RR3", elig_method = "CASRO-subgroup", e = NULL) {

  # Parameter checks

  ## _ Data

  if (!"data.frame" %in% class(data)) {
    stop("`data` should be a data frame.")
  }

  ## _ Status variable and codes

  if (missing(status)) {
    stop("A variable name must be supplied to the `status` parameter")
  }

  if (!is.character(status) || length(status) != 1) {
    stop("Must specify a single variable name for `status`")
  }
  if (!status %in% colnames(data)) {
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

  valid_status_values <- all(data[[status]] %in% status_codes)
  if (!valid_status_values) {
    stop("The data contains values in the status variable which are not listed in `status_codes`.")
  }

  ## _ Weights

  if (!missing(weights)) {
    if (length(weights) == 0) {
      using_weights <- FALSE
    } else {
      using_weights <- TRUE
    }
  } else {
    using_weights <- FALSE
  }

  if (using_weights) {
    if (!is.character(weights) || length(weights) != 1) {
      stop("If supplied, the `weights` variable must be a string giving the name of a variable in the data.")
    }

    if (!weights %in% colnames(data)) {
      stop(sprintf(
        "The weights variable '%s' does not appear in the supplied data",
        weights
      ))
    }
    if (any(is.na(data[[weights]]))) {
      stop(sprintf(
        "The weights variable '%s' should not have any missing values",
        weights
      ))
    }
  }

  ## _ Response rate formula
  valid_rr_formula <- is.character(rr_formula)
  valid_rr_formula <- valid_rr_formula && length(rr_formula) %in% c(1, 2, 3)
  rr_formula <- toupper(rr_formula)
  valid_rr_formula <- valid_rr_formula && all(rr_formula %in% c("RR1", "RR3", "RR5"))

  if (!valid_rr_formula) {
    stop("The `rr_formula` parameter must be a character vector with any combination of: 'RR1', 'RR3', 'RR5'.")
  }

  ## _ Eligibility rate method

  valid_elig_method <- is.character(elig_method)
  valid_elig_method <- valid_elig_method && length(valid_elig_method) == 1
  elig_method <- tolower(elig_method)
  valid_elig_method <- valid_elig_method && elig_method %in% c("casro-overall", "casro-subgroup", "specified")

  if (!valid_elig_method) {
    stop("The `elig_method` parameter must be a single character string from: 'CASRO-overall', 'CASRO-subgroup', or 'specified'.")
  }

  if (elig_method != "specified" && (!missing(e) && !is.null(e))) {
    stop("A value should only be supplied to `e` if `elig_method = 'specified'`.")
  }

  if (elig_method == "specified") {
    if (missing(e)) {
      stop("If `elig_method = 'specified'`, then the `e` parameter must be used. See the help page for details with the command:\n help('calculate_response_rates', package = 'nrba')")
    }
    if (!is.null(e) && is.character(e)) {
      if ((length(e) != 1) || (!e %in% colnames(data))) {
        e_valid <- FALSE
      } else {
        e_valid <- TRUE
      }
      e_type <- "variable"
    } else {
      e_valid <- !is.null(e)
      e_valid <- e_valid && is.numeric(e) && !any(is.na(e))
      e_valid <- e_valid && length(e) %in% c(1L, nrow(data))
      e_type <- "single-value"
    }

    if (!e_valid) {
      stop("For `e`, must specify either a single numeric value or a string giving the name of a numeric variable in `data` with no missing values.")
    }

    e <- switch(e_type,
      "single-value" = e,
      "variable" = data[[e]]
    )
  }

  # Assemble underlying totals for response rates

  ## _ Overall
  overall_totals <- as.data.frame(rbind(vector("numeric")))
  for (status_type in c("ER" = "ER", "EN" = "EN", "IE" = "IE", "UE" = "UE")) {
    varname <- sprintf("%s_UNWTD_TOTAL", status_type)
    overall_totals[1, varname] <- sum(data[[status]] == status_codes[status_type])
    if (using_weights) {
      varname <- sprintf("%s_WTD_TOTAL", status_type)
      overall_totals[1, varname] <- sum(data[[weights]] * (data[[status]] == status_codes[status_type]))
    }
  }

  ## _ By Group
  is_data_grouped <- !is.null(attributes(data)$groups)
  if (!is_data_grouped) {
    group_totals <- overall_totals
    group_totals[[".rows"]] <- list(seq_len(nrow(data)))
  } else {
    group_totals <- attributes(data)$groups
  }

  group_ids <- seq_len(nrow(group_totals))
  for (group_index in group_ids) {
    data_of_group <- data[group_totals[[".rows"]][[group_index]], ]

    for (status_type in c("ER" = "ER", "EN" = "EN", "IE" = "IE", "UE" = "UE")) {
      varname <- sprintf("%s_UNWTD_TOTAL", status_type)
      group_totals[group_index, varname] <- sum(data_of_group[[status]] == status_codes[status_type])
      if (using_weights) {
        varname <- sprintf("%s_WTD_TOTAL", status_type)
        group_totals[group_index, varname] <- sum(data_of_group[[weights]] * (data_of_group[[status]] == status_codes[status_type]))
      }
    }
    group_totals[group_index, "n"] <- nrow(data_of_group)
    if (using_weights) {
      group_totals[group_index, "Nhat"] <- sum(data_of_group[[weights]])
    }
  }



  # Get estimated eligibility rate for cases of unknown eligibility
  if ("RR3" %in% toupper(rr_formula)) {
    if (elig_method == "casro-overall") {
      group_totals[["e_unwtd"]] <- sum(overall_totals[["ER_UNWTD_TOTAL"]] + overall_totals[["EN_UNWTD_TOTAL"]]) / sum(overall_totals[c("ER_UNWTD_TOTAL", "EN_UNWTD_TOTAL", "IE_UNWTD_TOTAL")])

      if (using_weights) {
        group_totals[["e_wtd"]] <- sum(overall_totals[["ER_WTD_TOTAL"]] + overall_totals[["EN_WTD_TOTAL"]]) / sum(overall_totals[c("ER_WTD_TOTAL", "EN_WTD_TOTAL", "IE_WTD_TOTAL")])
      }
    }

    if (elig_method == "casro-subgroup") {
      group_totals[["e_unwtd"]] <- (group_totals[["ER_UNWTD_TOTAL"]] + group_totals[["EN_UNWTD_TOTAL"]]) / (group_totals[["ER_UNWTD_TOTAL"]] + group_totals[["EN_UNWTD_TOTAL"]] + group_totals[["IE_UNWTD_TOTAL"]])

      if (using_weights) {
        group_totals[["e_wtd"]] <- (group_totals[["ER_WTD_TOTAL"]] + group_totals[["EN_WTD_TOTAL"]]) / (group_totals[["ER_WTD_TOTAL"]] + group_totals[["EN_WTD_TOTAL"]] + group_totals[["IE_WTD_TOTAL"]])
      }
    }
    if (elig_method == "specified") {
      for (group_index in group_ids) {
        if (e_type == "variable") {
          e_of_group <- unique(e[group_totals[[".rows"]][[group_index]]])
          if (length(e_of_group) > 1) {
            stop("If a variable is specified to use for `e`, each subgroup must only have one unique value for that variable.")
          }
          group_totals[group_index, "e"] <- e_of_group
        } else if (e_type == "single-value") {
          group_totals[group_index, "e"] <- e
        }
      }
    }
  }

  # Calculate the response rates

  result <- group_totals
  result[[".rows"]] <- NULL

  if ("RR1" %in% toupper(rr_formula)) {
    result[["RR1_Unweighted"]] <- result[["ER_UNWTD_TOTAL"]] / (result[["ER_UNWTD_TOTAL"]] + result[["EN_UNWTD_TOTAL"]] + result[["UE_UNWTD_TOTAL"]])
    if (using_weights) {
      result[["RR1_Weighted"]] <- result[["ER_WTD_TOTAL"]] / (result[["ER_WTD_TOTAL"]] + result[["EN_WTD_TOTAL"]] + result[["UE_WTD_TOTAL"]])
    }
  }

  if ("RR3" %in% toupper(rr_formula)) {
    if (elig_method == "specified") {
      result[["e_unwtd"]] <- result[["e"]]
      result[["RR3_Unweighted"]] <- result[["ER_UNWTD_TOTAL"]] / (result[["ER_UNWTD_TOTAL"]] + result[["EN_UNWTD_TOTAL"]] + result[["e"]] * result[["UE_UNWTD_TOTAL"]])
      if (using_weights) {
        result[["e_wtd"]] <- result[["e"]]
        result[["RR3_Weighted"]] <- result[["ER_WTD_TOTAL"]] / (result[["ER_WTD_TOTAL"]] + result[["EN_WTD_TOTAL"]] + result[["e"]] * result[["UE_WTD_TOTAL"]])
      }
    }
    if (elig_method != "specified") {
      result[["RR3_Unweighted"]] <- result[["ER_UNWTD_TOTAL"]] / (result[["ER_UNWTD_TOTAL"]] + result[["EN_UNWTD_TOTAL"]] + result[["e_unwtd"]] * result[["UE_UNWTD_TOTAL"]])
      if (using_weights) {
        result[["RR3_Weighted"]] <- result[["ER_WTD_TOTAL"]] / (result[["ER_WTD_TOTAL"]] + result[["EN_WTD_TOTAL"]] + result[["e_wtd"]] * result[["UE_WTD_TOTAL"]])
      }
    }
  }

  if ("RR5" %in% toupper(rr_formula)) {
    result[["RR5_Unweighted"]] <- result[["ER_UNWTD_TOTAL"]] / (result[["ER_UNWTD_TOTAL"]] + result[["EN_UNWTD_TOTAL"]])
    if (using_weights) {
      result[["RR5_Weighted"]] <- result[["ER_WTD_TOTAL"]] / (result[["ER_WTD_TOTAL"]] + result[["EN_WTD_TOTAL"]])
    }
  }

  colnames(result) <- ifelse(grepl(x = colnames(result), "_WTD_TOTAL"),
    paste0("Nhat_", colnames(result)),
    colnames(result)
  )
  colnames(result) <- ifelse(grepl(x = colnames(result), "_UNWTD_TOTAL"),
    paste0("n_", colnames(result)),
    colnames(result)
  )
  colnames(result) <- gsub(
    x = colnames(result),
    pattern = "_(WTD|UNWTD)_TOTAL",
    replacement = ""
  )

  colnames_to_keep <- c(
    dplyr::group_vars(data),
    colnames(result)[grepl(x = colnames(result), pattern = "RR\\d_")],
    intersect(colnames(result), c("n", "Nhat")),
    colnames(result)[grepl(x = colnames(result), pattern = "^(n|Nhat)_ER$")],
    colnames(result)[grepl(x = colnames(result), pattern = "^(n|Nhat)_EN$")],
    colnames(result)[grepl(x = colnames(result), pattern = "^(n|Nhat)_IE$")],
    colnames(result)[grepl(x = colnames(result), pattern = "^(n|Nhat)_UE$")],
    intersect(colnames(result), c("e_unwtd", "e_wtd"))
  )
  result <- result[, colnames_to_keep]

  return(result)
}
