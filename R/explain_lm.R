#' Explain an OLS model in teaching-oriented language
#'
#' Translate the main parts of an \code{lm} object into an educational summary
#'
#' This function is designed for students who are still learning how to read
#' ordinary least squares output. It keeps the original numerical results from
#' \code{stats::lm()} but adds short plain-language interpretations for the
#' coefficients and the overall model fit.
#'
#' The returned object is a simple list with two data frames:
#' \itemize{
#'   \item \code{model_summary}: overall fit quantities such as \eqn{R^2} and
#'     the residual standard deviation.
#'   \item \code{coefficient_summary}: one row per coefficient including a
#'     plain-language explanation.
#' }
#'
#' @param model An object produced by \code{stats::lm()}.
#' @param confidence_level Confidence level used for the coefficient confidence
#'   intervals.
#' @param digits Number of digits used when printing numeric values.
#' @param print Logical, should the summary be printed to the console?
#'
#' @return Invisibly returns a list with components \code{model_summary} and
#'   \code{coefficient_summary}.
#' @export
#'
#' @examples
#' x <- stats::rnorm(150)
#' z <- stats::rnorm(150)
#' y <- 4 + 1.2 * x - 0.7 * z + stats::rnorm(150, sd = 0.8)
#' fit <- stats::lm(y ~ x + z)
#'
#' explanation <- bootcamp::explain_lm(fit, print = FALSE)
#' explanation$coefficient_summary
explain_lm <- function(model,
                       confidence_level = 0.95,
                       digits = 3,
                       print = TRUE) {
  validate_bootcamp_lm(model)
  validate_confidence_level(confidence_level)

  model_summary_object <- summary(model)
  model_frame <- stats::model.frame(model)
  response_name <- names(model_frame)[1]

  coefficients_table <- model_summary_object$coefficients
  coefficient_names <- rownames(coefficients_table)
  confidence_intervals <- stats::confint(model, level = confidence_level)

  coefficient_summary <- data.frame(
    term = coefficient_names,
    estimate = unname(coefficients_table[, 1]),
    std_error = unname(coefficients_table[, 2]),
    statistic = unname(coefficients_table[, 3]),
    p_value = unname(coefficients_table[, 4]),
    conf_low = unname(confidence_intervals[, 1]),
    conf_high = unname(confidence_intervals[, 2]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  coefficient_summary$interpretation <- vapply(
    seq_len(nrow(coefficient_summary)),
    function(i) {
      build_explain_lm_interpretation(
        term = coefficient_summary$term[i],
        estimate = coefficient_summary$estimate[i],
        response_name = response_name,
        digits = digits
      )
    },
    character(1)
  )

  f_statistic <- model_summary_object$fstatistic
  if (is.null(f_statistic)) {
    overall_p_value_label <- "Not available"
  } else {
    overall_p_value <- stats::pf(
      q = unname(f_statistic[1]),
      df1 = unname(f_statistic[2]),
      df2 = unname(f_statistic[3]),
      lower.tail = FALSE
    )
    overall_p_value_label <- format.pval(overall_p_value, digits = digits)
  }

  model_summary <- data.frame(
    metric = c(
      "Response",
      "Observations",
      "Predictors",
      "R-squared",
      "Adjusted R-squared",
      "Residual standard deviation",
      "Overall model p-value"
    ),
    value = c(
      response_name,
      stats::nobs(model),
      ncol(stats::model.matrix(model)) - 1L,
      formatC(unname(model_summary_object$r.squared), digits = digits, format = "f"),
      formatC(unname(model_summary_object$adj.r.squared), digits = digits, format = "f"),
      formatC(unname(model_summary_object$sigma), digits = digits, format = "f"),
      overall_p_value_label
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  result <- list(
    model_summary = model_summary,
    coefficient_summary = coefficient_summary
  )

  if (print) {
    print_explain_lm_result(result, digits = digits)
  }

  invisible(result)
}


#' @keywords internal
build_explain_lm_interpretation <- function(term, estimate, response_name, digits) {
  estimate_label <- formatC(estimate, digits = digits, format = "f")

  if (identical(term, "(Intercept)")) {
    return(
      paste0(
        "When all predictors equal zero, the expected value of ",
        response_name,
        " is ",
        estimate_label,
        "."
      )
    )
  }

  direction <- if (estimate >= 0) "increases" else "decreases"
  magnitude_label <- formatC(abs(estimate), digits = digits, format = "f")

  paste0(
    "Holding the other predictors constant, a one-unit increase in ",
    term,
    " is associated with a ",
    direction,
    " of about ",
    magnitude_label,
    " in ",
    response_name,
    "."
  )
}


#' @keywords internal
print_explain_lm_result <- function(result, digits) {
  cat("Model summary\n")
  print(result$model_summary, row.names = FALSE, right = FALSE)
  cat("\nCoefficient summary\n")

  display_table <- result$coefficient_summary
  numeric_columns <- c("estimate", "std_error", "statistic", "conf_low", "conf_high")
  for (column_name in numeric_columns) {
    display_table[[column_name]] <- formatC(display_table[[column_name]], digits = digits, format = "f")
  }
  display_table$p_value <- format.pval(display_table$p_value, digits = digits)

  print(display_table, row.names = FALSE, right = FALSE)
  invisible(NULL)
}


#' @keywords internal
validate_bootcamp_lm <- function(model) {
  if (!inherits(model, "lm")) {
    stop("'model' should be an object produced by stats::lm()")
  }

  invisible(TRUE)
}


#' @keywords internal
validate_confidence_level <- function(confidence_level) {
  if (!is.numeric(confidence_level) || length(confidence_level) != 1L || is.na(confidence_level) ||
      confidence_level <= 0 || confidence_level >= 1) {
    stop("'confidence_level' should be a single number between 0 and 1")
  }

  invisible(TRUE)
}
