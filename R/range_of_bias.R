#' @title Assess the range of possible bias based on specified assumptions
#' about how nonrespondents differ from respondents
#' @description
#' This range-of-bias analysis assesses the range of possible nonresponse bias
#' under varying assumptions about how nonrespondents differ from respondents.
#' The range of potential bias is calculated for both unadjusted estimates (i.e., from using base weights)
#' and nonresponse-adjusted estimates (i.e., based on nonresponse-adjusted weights).
#' @param survey_design A survey design object created with the 'survey' package
#' @param y_var Name of a variable whose mean or proportion is to be estimated
#' @param comparison_cell (Optional) The name of a variable in the data
#' dividing the sample into cells. If supplied, then the analysis is based on
#' assumptions about differences between respondents and nonrespondents
#' within the same cell. Typically, the variable used is a nonresponse adjustment cell
#' or post-stratification variable.
#' @param status A character string giving the name of the variable representing response/eligibility status.
#' The status variable should have at most four categories,
#' representing eligible respondents (ER),
#' eligible nonrespondents (EN),
#' known ineligible cases (IE),
#' and cases whose eligibility is unknown (UE).
#' @param status_codes A named vector,
#' with four entries named 'ER', 'EN', 'IE', and 'UE'.
#' \code{status_codes} indicates how the values of the status variable are to be interpreted.
#' @param assumed_multiple One or more numeric values.
#' Within each nonresponse adjustment cell,
#' the mean for nonrespondents is assumed to be a specified multiple
#' of the mean for respondents. If \code{y_var} is a categorical variable,
#' then the assumed nonrespondent mean (i.e., the proportion) in each cell is capped at 1.
#' @param assumed_percentile One or more numeric values, ranging from 0 to 1.
#' Within each nonresponse adjustment cell,
#' the mean of a continuous variable among nonrespondents is
#' assumed to equal a specified percentile of the variable among respondents.
#' The \code{assumed_percentile} parameter should be used only when the \code{y_var}
#' variable is numeric. Quantiles are estimated with weights,
#' using the function
#' \code{\link[survey]{svyquantile}(..., qrule = "hf2")}.
#'
#'
#' @return A data frame summarizing the range of bias under each assumption.
#' For a numeric outcome variable, there is one row per value of
#' \code{assumed_multiple} or \code{assumed_percentile}. For a categorical
#' outcome variable, there is one row per combination of category
#' and \code{assumed_multiple} or \code{assumed_percentile}.
#'
#' The column \code{bias_of_unadj_estimate} is the nonresponse bias
#' of the estimate from respondents produced using the unadjusted weights.
#' The column \code{bias_of_adj_estimate} is the nonresponse bias
#' of the estimate from respondents produced
#' using nonresponse-adjusted weights, based on a weighting-class
#' adjustment with \code{comparison_cell} as the weighting class variable.
#' If no \code{comparison_cell} is specified, the two bias estimates
#' will be the same.
#' @export
#' @references
#'
#' See Petraglia et al. (2016) for an example of a range-of-bias analysis
#' using these methods.
#'
#' - Petraglia, E., Van de Kerckhove, W., and Krenzke, T. (2016).
#' \emph{Review of the Potential for Nonresponse Bias in FoodAPS 2012}.
#' Prepared for the Economic Research Service,
#' U.S. Department of Agriculture. Washington, D.C.
#'
#' @examples
#' # Load example data
#'
#' suppressPackageStartupMessages(library(survey))
#' data(api)
#'
#' base_weights_design <- svydesign(
#'   data    = apiclus1,
#'   id      = ~dnum,
#'   weights = ~pw,
#'   fpc     = ~fpc
#' ) |> as.svrepdesign(type = "JK1")
#'
#' base_weights_design$variables$response_status <- sample(
#'   x = c("Respondent", "Nonrespondent"),
#'   prob = c(0.75, 0.25),
#'   size = nrow(base_weights_design),
#'   replace = TRUE
#' )
#'
#' # Assess range of bias for mean of `api00`
#' # based on assuming nonrespondent means
#' # are equal to the 25th percentile or 75th percentile
#' # among respondents, within nonresponse adjustment cells
#'
#'   assess_range_of_bias(
#'     survey_design = base_weights_design,
#'     y_var = "api00",
#'     comparison_cell = "stype",
#'     status = "response_status",
#'     status_codes = c("ER" = "Respondent",
#'                      "EN" = "Nonrespondent",
#'                      "IE" = "Ineligible",
#'                      "UE" = "Unknown"),
#'     assumed_percentile = c(0.25, 0.75)
#'   )
#'
#' # Assess range of bias for proportions of `sch.wide`
#' # based on assuming nonrespondent proportions
#' # are equal to some multiple of respondent proportions,
#' # within nonresponse adjustment cells
#'
#'   assess_range_of_bias(
#'     survey_design = base_weights_design,
#'     y_var = "sch.wide",
#'     comparison_cell = "stype",
#'     status = "response_status",
#'     status_codes = c("ER" = "Respondent",
#'                      "EN" = "Nonrespondent",
#'                      "IE" = "Ineligible",
#'                      "UE" = "Unknown"),
#'     assumed_multiple = c(0.25, 0.75)
#'   )

assess_range_of_bias <- function(
    survey_design,
    y_var,
    comparison_cell,
    status,
    status_codes,
    assumed_multiple = c(0.5, 0.75, 0.9, 1.1, 1.25, 1.5),
    assumed_percentile = NULL
) {

  # Parameter checks

  ## _ survey_design

  if (!any(c("survey.design", "svyrep.design") %in% class(survey_design))) {
    stop("`survey_design` should be a survey design object created using the `survey` or `srvyr` package.\nUse `help('svydesign', package = 'survey')` to see how.")
  }

  ## _ Status variable and codes

  if (missing(status)) {
    stop("A variable name must be supplied to the `status` parameter")
  }

  if (!is.character(status) || length(status) != 1) {
    stop("Must specify a single variable name for `status`")
  }
  if (!status %in% colnames(survey_design[["variables"]])) {
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

  valid_status_values <- all(survey_design[["variables"]][[status]] %in% status_codes)
  if (!valid_status_values) {
    stop("The data contains values in the status variable which are not listed in `status_codes`.")
  }

  ## _ assumptions

  using_assumed_multiple <- !(missing(assumed_multiple) || is.null(assumed_multiple))
  using_assumed_percentile <- !(missing(assumed_percentile) || is.null(assumed_percentile))
  if (!xor(using_assumed_multiple, using_assumed_percentile)) {
    stop("Cannot use both `assumed_multiple` and `assumed_percentile`.")
  }
  if (using_assumed_multiple) {
    if (any(is.na(assumed_multiple)) || (length(assumed_multiple) == 0)) {
      stop("`assumed_multiple` must be at least one number, and cannot include a missing value.")
    }
  }
  if (using_assumed_percentile) {
    if (any(is.na(assumed_percentile)) || (length(assumed_percentile) == 0)) {
      stop("`assumed_percentile` must be at least one number, and cannot include a missing value.")
    }
    if (any(assumed_percentile < 0) || any(assumed_percentile > 1)) {
      stop("`assumed_percentile` can only include values between 0 and 1, inclusive.")
    }
  }

  ## _ outcome variable

  y_var_type <- typeof(survey_design[['variables']][[y_var]])
  if (is.factor(survey_design[['variables']][[y_var]])) {
    y_var_type <- 'factor'
  }

  if (using_assumed_percentile) {
    if (y_var_type %in% c("factor", "character")) {
      stop("When using `assumed_percentile`, `y_var` should not be a character or factor variable.")
    }
  }

  ## _ comparison_cell

  if (!missing(comparison_cell) && !is.null(comparison_cell)) {
    if ((length(comparison_cell) != 1) || (!comparison_cell %in% colnames(survey_design[['variables']]))) {
      stop("`comparison_cell` must be either `NULL` or the name of a variable in `survey_design`.")
    }
  } else {
    survey_design[['variables']][['_COMPARISON_CELL_']] <- rep(1, times = nrow(survey_design[['variables']]))
    comparison_cell <- "_COMPARISON_CELL_"
  }

  # Identify which cases are respondents or nonrespondents
  respondent_cases <- (survey_design[['variables']][[status]] %in% status_codes[['ER']])
  nonrespondent_cases <- (survey_design[['variables']][[status]] %in% status_codes[['EN']])

  # Get the sum of weights for nonrespondents in each cell

  cell_nonrespondent_weight_sums <- survey_design[nonrespondent_cases,] |>
    srvyr::as_survey() |>
    srvyr::group_by(dplyr::across(dplyr::all_of(comparison_cell))) |>
    srvyr::summarise(nonrespondent_weight_sum = sum(srvyr::cur_svy_wts(), na.rm = TRUE)) |>
    srvyr::ungroup()

  # Estimate means for respondents and nonrespondents within cells
  if (using_assumed_multiple) {

    if (y_var_type %in% c("factor", "character")) {
      cell_means <- survey_design[respondent_cases,] |>
        srvyr::as_survey() |>
        dplyr::group_by(dplyr::across(dplyr::all_of(c(comparison_cell, y_var)))) |>
        dplyr::summarise(respondent_mean = srvyr::survey_mean(vartype = NULL, na.rm = TRUE),
                         respondent_weight_sum = sum(srvyr::cur_svy_wts(), na.rm = TRUE)) |>
        dplyr::ungroup()

      colnames(cell_means)[colnames(cell_means) == y_var] <- "outcome_category"

      cell_max <- 1

    } else {
      cell_means <- survey_design[respondent_cases,] |>
        srvyr::as_survey() |>
        dplyr::group_by(dplyr::across(dplyr::all_of(c(comparison_cell)))) |>
        dplyr::summarise(respondent_mean = srvyr::survey_mean(x = !!sym(y_var), vartype = NULL),
                         respondent_weight_sum = sum(srvyr::cur_svy_wts(), na.rm = TRUE)) |>
        dplyr::ungroup()

      cell_max <- Inf

    }

    cell_means <- lapply(X = assumed_multiple, FUN = function(mult) {
      cell_means |>
        dplyr::mutate(
          assumed_multiple = mult,
          nonrespondent_mean = pmin(
            mult * .data[['respondent_mean']],
            cell_max
          )
        )
    }) |> do.call(what = rbind)

  }

  if (using_assumed_percentile) {

    cell_means <- survey_design[respondent_cases,] |>
      srvyr::as_survey() |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(comparison_cell)))) |>
      dplyr::summarise(
        respondent_mean = srvyr::survey_mean(
          !!rlang::sym(y_var), vartype = NULL
        ),
        nonrespondent_mean = srvyr::survey_quantile(
          !!rlang::sym(y_var), vartype = NULL,
          quantiles = assumed_percentile,
          qrule = "hf2"
        ),
        respondent_weight_sum = sum(srvyr::cur_svy_wts(), na.rm = TRUE)
      ) |>
      dplyr::ungroup()

    cell_means <- cell_means |>
      tidyr::pivot_longer(
        cols = dplyr::starts_with("nonrespondent_mean_"),
        names_to = "assumed_percentile",
        values_to = "nonrespondent_mean"
      ) |>
      dplyr::mutate(
        assumed_percentile = gsub(x = .data[['assumed_percentile']],
                                  pattern = "nonrespondent_mean_q",
                                  replacement = "")
      ) |>
      dplyr::mutate(
        assumed_percentile = as.numeric(
          paste0("0.", .data[['assumed_percentile']])
        )
      )
  }

  # Estimate the overall mean within cells
  cell_summaries <- dplyr::full_join(
    x = cell_means,
    y = cell_nonrespondent_weight_sums,
    by = comparison_cell
  )

  cell_summaries[['eligibles_weight_sum']] <- (
    cell_summaries[['respondent_weight_sum']] +
      cell_summaries[['nonrespondent_weight_sum']]
  )

  cell_summaries[['combined_mean']] <- (
    (cell_summaries[['respondent_mean']] * cell_summaries[['respondent_weight_sum']]) +
      (cell_summaries[['nonrespondent_mean']]) * cell_summaries[['nonrespondent_weight_sum']]
  ) / (cell_summaries[['eligibles_weight_sum']])

  # Calculate mean for different assumed multiples

  if (using_assumed_multiple) {
    grouping_vars <- "assumed_multiple"
    if (y_var_type %in% c("factor", "character")) {
      grouping_vars <- c(grouping_vars, "outcome_category")
    }
  }
  if (using_assumed_percentile) {
    grouping_vars <- "assumed_percentile"
  }

  bias_estimates <- cell_summaries |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) |>
    dplyr::summarise(
      nr_adjusted_mean = weighted.mean(x = .data[['respondent_mean']],
                                       w = .data[['eligibles_weight_sum']]),
      nonrespondent_mean = weighted.mean(x = .data[['nonrespondent_mean']],
                                         w = .data[['nonrespondent_weight_sum']]),
      full_sample_mean = weighted.mean(x = .data[['combined_mean']],
                                       w = .data[['eligibles_weight_sum']]),
      respondent_mean = weighted.mean(x = .data[['respondent_mean']],
                                      w = .data[['respondent_weight_sum']]),
      wtd_rr = sum(.data[['respondent_weight_sum']])/sum(.data[['eligibles_weight_sum']])
    ) |>
    dplyr::mutate(
      bias_of_adj_estimate = .data[['nr_adjusted_mean']] - .data[['full_sample_mean']],
      bias_of_unadj_estimate = .data[['respondent_mean']] - .data[['full_sample_mean']]
    )

  # Format the output
  if (comparison_cell == "_COMPARISON_CELL_") {
    comparison_cell <- NA_character_
  }

  bias_estimates <- cbind(
    outcome = rep(y_var, times = nrow(bias_estimates)),
    comparison_cell = rep(comparison_cell, times = nrow(bias_estimates)),
    bias_estimates
  )
  bias_estimates <- bias_estimates |>
    dplyr::select(dplyr::any_of(c("comparison_cell",
                                  "outcome", "outcome_category",
                                  "assumed_multiple", "assumed_percentile",
                                  "bias_of_adj_estimate", "bias_of_unadj_estimate",
                                  "full_sample_mean", "nr_adjusted_mean", "respondent_mean",
                                  "nonrespondent_mean", "wtd_rr")))

  return(bias_estimates)
}
