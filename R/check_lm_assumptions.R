#' Check common OLS assumptions in an educational summary
#'
#' Summarise several regression diagnostics in a compact teaching-friendly table
#'
#' This function is intended for classroom use and for introductory model
#' checking. It does not try to be a comprehensive diagnostics package. Instead,
#' it gathers a small set of familiar checks into one table so students can link
#' patterns in diagnostic plots to concrete indicators.
#'
#' The function is especially useful alongside \code{bootcamp::demo_residuals()},
#' because students can first see the visual pattern in the demo and then see
#' how a simple rule-based summary flags similar issues in a fitted model.
#'
#' The function currently reports on:
#' \itemize{
#'   \item possible non-linearity, based on the gain from adding a quadratic
#'     term in fitted values;
#'   \item heteroscedasticity, based on the association between fitted values
#'     and the size of residuals;
#'   \item normality of residuals, based on the Shapiro-Wilk test when that test
#'     is defined;
#'   \item outlying residuals, based on standardized residuals;
#'   \item influential cases, based on Cook's distance;
#'   \item independence, which is flagged as not assessed because it usually
#'     requires design knowledge or time-order information.
#' }
#'
#' @param model An object produced by \code{stats::lm()}.
#' @param linearity_threshold Threshold for the increase in \eqn{R^2} from a
#'   quadratic fit to the fitted values. Larger values suggest non-linearity.
#' @param heteroscedasticity_threshold Threshold for the absolute correlation
#'   between fitted values and the absolute standardized residuals.
#' @param outlier_threshold Threshold used for absolute standardized residuals.
#' @param digits Number of digits used when printing numeric values.
#' @param print Logical, should the table be printed to the console?
#'
#' @return Invisibly returns a data frame with one row per assumption-related
#'   check.
#' @export
#'
#' @examples
#' x <- stats::runif(120, -2, 2)
#' y <- 3 + 1.5 * x + stats::rnorm(120, sd = 0.7)
#' fit <- stats::lm(y ~ x)
#'
#' bootcamp::check_lm_assumptions(fit)
#'
#' curved_y <- 3 + 1.5 * x + 1.2 * x^2 + stats::rnorm(120, sd = 0.7)
#' curved_fit <- stats::lm(curved_y ~ x)
#' bootcamp::check_lm_assumptions(curved_fit, print = FALSE)
#'
#' hetero_y <- 3 + 1.5 * x + stats::rnorm(120, sd = 0.3 + abs(x))
#' hetero_fit <- stats::lm(hetero_y ~ x)
#' bootcamp::check_lm_assumptions(
#'   hetero_fit,
#'   heteroscedasticity_threshold = 0.20
#' )
check_lm_assumptions <- function(model,
                                 linearity_threshold = 0.02,
                                 heteroscedasticity_threshold = 0.25,
                                 outlier_threshold = 2.5,
                                 digits = 3,
                                 print = TRUE) {
  validate_bootcamp_lm(model)

  residuals <- stats::residuals(model)
  fitted_values <- stats::fitted(model)
  standardized_residuals <- stats::rstandard(model)
  cooks_distance <- stats::cooks.distance(model)
  observation_count <- stats::nobs(model)
  response <- stats::model.response(stats::model.frame(model))

  linearity_gain <- compute_lm_linearity_gain(
    response = response,
    fitted_values = fitted_values
  )
  if (isTRUE(all.equal(stats::sd(fitted_values), 0))) {
    heteroscedasticity_metric <- 0
  } else {
    heteroscedasticity_metric <- abs(stats::cor(abs(standardized_residuals), fitted_values))
  }
  outlier_count <- sum(abs(standardized_residuals) > outlier_threshold, na.rm = TRUE)
  cooks_cutoff <- 4 / observation_count
  influential_count <- sum(cooks_distance > cooks_cutoff, na.rm = TRUE)
  normality_check <- compute_lm_normality_check(residuals)

  result <- data.frame(
    assumption = c(
      "Linearity",
      "Equal variance",
      "Normal residuals",
      "Outlying residuals",
      "Influential cases",
      "Independence"
    ),
    metric = c(
      "Quadratic gain in R-squared",
      "Correlation between |standardized residuals| and fitted values",
      "Shapiro-Wilk p-value",
      "Count with |standardized residual| above threshold",
      "Count with Cook's distance above 4 / n",
      "Not assessed from the model object alone"
    ),
    value = c(
      linearity_gain,
      heteroscedasticity_metric,
      normality_check$value,
      outlier_count,
      influential_count,
      NA_real_
    ),
    guideline = c(
      paste0("Smaller is better; values above ", linearity_threshold, " suggest non-linearity"),
      paste0("Smaller is better; values above ", heteroscedasticity_threshold, " suggest unequal spread"),
      "Small p-values suggest non-normal residuals",
      paste0("Lower is better; threshold = ", outlier_threshold),
      paste0("Lower is better; cutoff = ", formatC(cooks_cutoff, digits = digits, format = "f")),
      "Check design, time structure, or dependence between observations"
    ),
    status = c(
      if (linearity_gain > linearity_threshold) "check" else "ok",
      if (heteroscedasticity_metric > heteroscedasticity_threshold) "check" else "ok",
      normality_check$status,
      if (outlier_count > 0L) "check" else "ok",
      if (influential_count > 0L) "check" else "ok",
      "not_assessed"
    ),
    interpretation = c(
      if (linearity_gain > linearity_threshold) {
        "A curved pattern may explain the data better than a straight line."
      } else {
        "There is little evidence here that a curved relationship would improve fit a lot."
      },
      if (heteroscedasticity_metric > heteroscedasticity_threshold) {
        "Residual spread appears to change across fitted values."
      } else {
        "Residual spread looks fairly stable across fitted values."
      },
      normality_check$interpretation,
      if (outlier_count > 0L) {
        "At least one case has an unusually large residual."
      } else {
        "No unusually large residuals were flagged by this rule."
      },
      if (influential_count > 0L) {
        "One or more cases may have strong influence on the fitted coefficients."
      } else {
        "No cases were flagged as especially influential by Cook's distance."
      },
      "Independence usually depends on how the data were collected, so it is not diagnosed here."
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (print) {
    display_result <- result
    numeric_rows <- !is.na(display_result$value)
    display_result$value[numeric_rows] <- formatC(
      as.numeric(display_result$value[numeric_rows]),
      digits = digits,
      format = "f"
    )
    print(display_result, row.names = FALSE, right = FALSE)
  }

  invisible(result)
}


#' @keywords internal
compute_lm_linearity_gain <- function(response, fitted_values) {
  if (isTRUE(all.equal(stats::sd(fitted_values), 0))) {
    return(0)
  }

  linear_fit <- stats::lm(response ~ fitted_values)
  quadratic_fit <- stats::lm(response ~ fitted_values + I(fitted_values^2))

  unname(summary(quadratic_fit)$r.squared - summary(linear_fit)$r.squared)
}


#' @keywords internal
compute_lm_normality_check <- function(residuals) {
  clean_residuals <- residuals[!is.na(residuals)]

  if (length(clean_residuals) < 3L || length(clean_residuals) > 5000L ||
      isTRUE(all.equal(stats::sd(clean_residuals), 0))) {
    return(list(
      value = NA_real_,
      status = "not_assessed",
      interpretation = "The Shapiro-Wilk test is not defined for this residual vector."
    ))
  }

  shapiro_result <- stats::shapiro.test(clean_residuals)
  p_value <- unname(shapiro_result$p.value)

  list(
    value = p_value,
    status = if (p_value < 0.05) "check" else "ok",
    interpretation = if (p_value < 0.05) {
      "The residual distribution differs noticeably from normality under this test."
    } else {
      "The residuals are not strongly inconsistent with normality under this test."
    }
  )
}
