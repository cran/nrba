#' Calculate cumulative estimates of a mean/proportion
#' @description Calculates estimates of a mean/proportion which are cumulative with respect
#' to a predictor variable, such as week of data collection or number of contact attempts.
#' This can be useful for examining whether estimates are affected by decisions such as
#' whether to extend the data collection period or make additional contact attempts.
#' @param survey_design 	A survey design object created with the \code{survey} package.
#' @param y_var Name of a variable whose mean or proportion is to be estimated.
#' @param y_var_type Either \code{NULL}, \code{"categorical"} or \code{"numeric"}.
#' For \code{"categorical"}, proportions are estimated. For \code{"numeric"}, means are estimated.
#' For \code{NULL} (the default), then proportions are estimated if \code{y_var} is a factor or character variable.
#' Otherwise, means are estimated. The data will be subset to remove any missing values in this variable.
#' @param predictor_variable Name of a variable for which cumulative estimates of \code{y_var}
#' will be calculated. This variable should either be numeric or have categories which when sorted
#' by their label are arranged in ascending order. The data will be subset to remove
#' any missing values of the predictor variable.
#' @return
#' A dataframe of cumulative estimates. The first column--whose name matches \code{predictor_variable}--gives
#' describes the values of \code{predictor_variable} for which a given estimate was computed.
#' The other columns of the result include the following:
#' \itemize{
#'  \item{"outcome"}{: The name of the variable for which estimates are computed}
#'  \item{"outcome_category"}{: For a categorical variable, the category of that variable}
#'  \item{"estimate"}{: The estimated mean or proportion.}
#'  \item{"std_error"}{: The estimated standard error}
#'  \item{"respondent_sample_size"}{: The number of cases used to produce the estimate (excluding missing values)}
#' }
#'
#' @export
#' @references
#'
#' See Maitland et al. (2017) for an example of a level-of-effort analysis
#' based on this method.
#'
#' - Maitland, A. et al. (2017). \emph{A Nonresponse Bias Analysis
#' of the Health Information National Trends Survey (HINTS)}.
#' Journal of Health Communication 22, 545-553.
#' doi:10.1080/10810730.2017.1324539
#'
#' @examples
#'
#' # Create an example survey design
#' # with a variable representing number of contact attempts
#' library(survey)
#' data(involvement_survey_srs, package = "nrba")
#'
#' involvement_survey_srs[["CONTACT_ATTEMPTS"]] <- runif(
#'   n = nrow(involvement_survey_srs),
#'   min = 1, max = 8
#' ) |> round()
#'
#' survey_design <- svydesign(
#'   weights = ~BASE_WEIGHT,
#'   id = ~UNIQUE_ID,
#'   fpc = ~N_STUDENTS,
#'   data = involvement_survey_srs
#' )
#'
#' # Cumulative estimates from respondents for average student age ----
#' get_cumulative_estimates(
#'   survey_design = survey_design |>
#'     subset(RESPONSE_STATUS == "Respondent"),
#'   y_var = "STUDENT_AGE",
#'   y_var_type = "numeric",
#'   predictor_variable = "CONTACT_ATTEMPTS"
#' )
#'
#' # Cumulative estimates from respondents for proportions of categorical variable ----
#' get_cumulative_estimates(
#'   survey_design = survey_design |>
#'     subset(RESPONSE_STATUS == "Respondent"),
#'   y_var = "WHETHER_PARENT_AGREES",
#'   y_var_type = "categorical",
#'   predictor_variable = "CONTACT_ATTEMPTS"
#' )
#'
get_cumulative_estimates <- function(survey_design,
                                     y_var,
                                     y_var_type = NULL,
                                     predictor_variable) {

  # Check inputs
  if (is.null(y_var) || !is.character(y_var) || length(y_var) != 1) {
    stop("`y_var` must be a single character string giving the name of a variable.")
  }
  if (is.null(predictor_variable) || !is.character(predictor_variable) || length(predictor_variable) != 1) {
    stop("`predictor_variable` must be a single character string giving the name of a variable.")
  }
  if (!is.null(y_var_type)) {
    if ((!is.character(y_var_type) || length(y_var_type) != 1)) {
      stop("If `y_var_type` is specified, then it must be either 'categorical' or 'numeric'.")
    }
    if (y_var_type == "categorical") {
      if (!is.factor(survey_design[["variables"]][[y_var]]) && !is.character(survey_design[["variables"]][[y_var]])) {
        survey_design[["variables"]][["y_var"]] <- factor(
          x = survey_design[["variables"]][[y_var]],
          levels = survey_design[["variables"]][[y_var]] |>
            unique() |> sort()
        )
      }
    }
    if (y_var_type == "numeric") {
      if (is.factor(survey_design[["variables"]][[y_var]]) || is.character(survey_design[["variables"]][[y_var]])) {
        numeric_y_var <- as.numeric(survey_design[["variables"]][[y_var]])
        if (any(xor(is.na(numeric_y_var), is.na(survey_design[["variables"]][[y_var]])))) {
          error_msg <- sprintf(
            "The outcome variable `%s` contains non-numeric variables.",
            y_var
          )
          stop(error_msg)
        }
      }
    }
  }
  if (is.null(y_var_type)) {
    if (is.factor(survey_design[["variables"]][[y_var]]) || is.character(survey_design[["variables"]][[y_var]])) {
      y_var_type <- "categorical"
    } else {
      y_var_type <- "numeric"
    }
  }

  # Get list of unique values for the predictor variable
  unique_predictor_values <- unique(survey_design[["variables"]][[predictor_variable]])
  unique_predictor_values <- sort(unique_predictor_values)
  if (is.character(unique_predictor_values)) {
    should_be_numeric <- all(suppressWarnings(!is.na(as.numeric(unique_predictor_values))))
    if (should_be_numeric) {
      unique_predictor_values <- unique_predictor_values[
        order(as.numeric(unique_predictor_values))
      ]
    }
  }

  survey_design[["variables"]][["_PRED_VAR_"]] <- survey_design[["variables"]][[predictor_variable]]
  result <- NULL

  for (value_i in seq_along(unique_predictor_values)) {
    predictor_values <- unique_predictor_values[seq(from = 1, to = value_i)]
    predictor_values_string <- paste(
      paste0("'", predictor_values[unique(c(1,value_i))], "'"),
      collapse = " to "
    )

    subsetted_design <- survey_design |>
      subset(survey_design[["variables"]][["_PRED_VAR_"]] %in% predictor_values)

    nonmissing_indices <- !is.na(subsetted_design[["variables"]][[y_var]])
    respondent_sample_size <- sum(nonmissing_indices)

    subsetted_design <- subsetted_design[nonmissing_indices, ]

    y_var_formula <- reformulate(termlabels = y_var)

    estimate <- subsetted_design |>
      survey::svymean(x = y_var_formula)

    estimates_data_frame <- data.frame(
      level_of_effort = rep(
        predictor_values_string,
        length(coef(estimate))
      ),
      outcome = rep(y_var, length(coef(estimate))),
      outcome_category = names(coef(estimate)),
      estimate = unname(coef(estimate)),
      std_error = unname(survey::SE(estimate)),
      respondent_sample_size = respondent_sample_size
    )

    result <- dplyr::bind_rows(result, estimates_data_frame)
  }
  colnames(result)[1] <- predictor_variable

  # For categorical outcome variables, improve the formatting
  # of the variable which gives the category for which an estimate was made
  if (y_var_type == "categorical") {
    result[["outcome_category"]] <- sapply(
      X = result[["outcome_category"]],
      FUN = function(category_string) {
        substr(
          x = category_string,
          start = nchar(y_var) + 1,
          stop = nchar(category_string)
        )
      }
    )
  }
  if (y_var_type == "numeric") {
    result[["outcome_category"]] <- NULL
  }

  return(result)
}
