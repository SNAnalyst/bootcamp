#' Annotated scatterplot for teaching association and regression
#'
#' Draw a scatterplot with optional correlation and OLS annotations
#'
#' This helper is meant for teaching relationships between two continuous
#' variables. It can overlay an ordinary least squares line, report a
#' correlation coefficient, and optionally label a small number of unusual
#' observations so students can discuss influence and fit.
#'
#' The function stays intentionally lightweight. It does not try to replace a
#' full plotting package; instead it provides a consistent base R visual that is
#' easy to reuse in tutorials and class demonstrations.
#'
#' @param x Numeric vector shown on the x-axis.
#' @param y Numeric vector shown on the y-axis.
#' @param na.rm Logical, should incomplete pairs be removed?
#' @param add_lm Logical, should an OLS line be added when possible?
#' @param add_correlation Logical, should a correlation coefficient be shown in
#'   a legend?
#' @param correlation_method Correlation method passed to \code{stats::cor()}.
#' @param highlight_n Number of observations to label. When a regression line is
#'   available, the observations with the largest absolute standardized
#'   residuals are selected.
#' @param point_labels Optional labels for the observations.
#' @param point_col Colour of the points.
#' @param point_pch Plotting symbol for the points.
#' @param line_col Colour of the regression line.
#' @param main Main title. When \code{NULL}, a default title is used.
#' @param xlab Label for the x-axis. When \code{NULL}, the expression supplied
#'   to \code{x} is used.
#' @param ylab Label for the y-axis. When \code{NULL}, the expression supplied
#'   to \code{y} is used.
#' @param label_digits Number of digits used for the legend annotation.
#' @param legend_position Position passed to \code{graphics::legend()}.
#' @param ... Additional arguments passed to \code{graphics::plot()}.
#'
#' @return Invisibly returns a list containing the plotted data, the correlation
#'   coefficient, and regression summary information when available.
#' @export
#'
#' @examples
#' x <- stats::rnorm(120)
#' y <- 2 + 0.8 * x + stats::rnorm(120, sd = 0.6)
#'
#' bootcamp::annotated_scatterplot(x, y)
#' bootcamp::annotated_scatterplot(x, y, highlight_n = 2L)
annotated_scatterplot <- function(x,
                                  y,
                                  na.rm = TRUE,
                                  add_lm = TRUE,
                                  add_correlation = TRUE,
                                  correlation_method = c("pearson", "spearman"),
                                  highlight_n = 0L,
                                  point_labels = NULL,
                                  point_col = "#1F77B4",
                                  point_pch = 19,
                                  line_col = "#D55E00",
                                  main = NULL,
                                  xlab = NULL,
                                  ylab = NULL,
                                  label_digits = 2,
                                  legend_position = "topleft",
                                  ...) {
  correlation_method <- match.arg(correlation_method)
  validated <- validate_annotated_scatterplot_input(
    x = x,
    y = y,
    na.rm = na.rm,
    point_labels = point_labels,
    highlight_n = highlight_n
  )

  x_values <- validated$x
  y_values <- validated$y
  point_labels <- validated$point_labels

  if (is.null(main)) {
    main <- "Scatterplot with teaching annotations"
  }
  if (is.null(xlab)) {
    xlab <- deparse(substitute(x))
  }
  if (is.null(ylab)) {
    ylab <- deparse(substitute(y))
  }

  graphics::plot(
    x = x_values,
    y = y_values,
    col = point_col,
    pch = point_pch,
    main = main,
    xlab = xlab,
    ylab = ylab,
    ...
  )
  graphics::grid(col = "grey90")

  correlation_value <- NA_real_
  if (add_correlation && stats::sd(x_values) > 0 && stats::sd(y_values) > 0) {
    correlation_value <- stats::cor(x_values, y_values, method = correlation_method)
  }

  fit_summary <- list(
    can_fit = FALSE,
    intercept = NA_real_,
    slope = NA_real_,
    r_squared = NA_real_,
    highlighted = integer(0)
  )

  if (add_lm && length(x_values) >= 2L && diff(range(x_values)) > 0) {
    fit <- stats::lm(y_values ~ x_values)
    coefficients <- stats::coef(fit)

    graphics::abline(fit, col = line_col, lwd = 2.5)
    fit_summary$can_fit <- TRUE
    fit_summary$intercept <- unname(coefficients[1])
    fit_summary$slope <- unname(coefficients[2])
    fit_summary$r_squared <- unname(summary(fit)$r.squared)

    if (highlight_n > 0L) {
      highlight_n <- min(highlight_n, length(x_values))
      fit_summary$highlighted <- order(abs(stats::rstandard(fit)), decreasing = TRUE)[seq_len(highlight_n)]
    }
  } else if (highlight_n > 0L) {
    distance_from_center <- (x_values - mean(x_values))^2 + (y_values - mean(y_values))^2
    highlight_n <- min(highlight_n, length(x_values))
    fit_summary$highlighted <- order(distance_from_center, decreasing = TRUE)[seq_len(highlight_n)]
  }

  if (length(fit_summary$highlighted) > 0L) {
    graphics::points(
      x = x_values[fit_summary$highlighted],
      y = y_values[fit_summary$highlighted],
      pch = 21,
      bg = "#D55E00",
      col = "#D55E00",
      cex = 1.5
    )
    graphics::text(
      x = x_values[fit_summary$highlighted],
      y = y_values[fit_summary$highlighted],
      labels = point_labels[fit_summary$highlighted],
      pos = 3,
      cex = 0.8,
      col = "#D55E00"
    )
  }

  legend_text <- character(0)
  legend_colours <- character(0)
  legend_lines <- integer(0)
  legend_points <- integer(0)

  if (!is.na(correlation_value)) {
    legend_text <- c(
      legend_text,
      paste0(
        toupper(substr(correlation_method, 1L, 1L)),
        substr(correlation_method, 2L, nchar(correlation_method)),
        " correlation = ",
        formatC(correlation_value, digits = label_digits, format = "f")
      )
    )
    legend_colours <- c(legend_colours, point_col)
    legend_lines <- c(legend_lines, 0L)
    legend_points <- c(legend_points, point_pch)
  }

  if (isTRUE(fit_summary$can_fit)) {
    legend_text <- c(
      legend_text,
      paste0("OLS slope = ", formatC(fit_summary$slope, digits = label_digits, format = "f"))
    )
    legend_colours <- c(legend_colours, line_col)
    legend_lines <- c(legend_lines, 1L)
    legend_points <- c(legend_points, 0L)
  }

  if (length(legend_text) > 0L) {
    graphics::legend(
      x = legend_position,
      legend = legend_text,
      col = legend_colours,
      lwd = 2,
      lty = legend_lines,
      pch = legend_points,
      bty = "n"
    )
  }

  invisible(list(
    x = x_values,
    y = y_values,
    correlation = correlation_value,
    correlation_method = correlation_method,
    slope = fit_summary$slope,
    intercept = fit_summary$intercept,
    r_squared = fit_summary$r_squared,
    highlighted = fit_summary$highlighted
  ))
}


#' @keywords internal
validate_annotated_scatterplot_input <- function(x, y, na.rm, point_labels, highlight_n) {
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("'x' and 'y' should be numeric vectors")
  }
  if (length(x) != length(y)) {
    stop("'x' and 'y' should have the same length")
  }
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm)) {
    stop("'na.rm' should be TRUE or FALSE")
  }
  if (!is.null(point_labels) && length(point_labels) != length(x)) {
    stop("'point_labels' should have the same length as 'x' and 'y'")
  }
  if (!is.numeric(highlight_n) || length(highlight_n) != 1L || is.na(highlight_n) ||
      highlight_n < 0 || highlight_n != as.integer(highlight_n)) {
    stop("'highlight_n' should be a single non-negative whole number")
  }

  complete <- !(is.na(x) | is.na(y))
  if (!is.null(point_labels)) {
    point_labels <- as.character(point_labels)
  } else {
    point_labels <- as.character(seq_along(x))
  }

  if (na.rm) {
    x <- x[complete]
    y <- y[complete]
    point_labels <- point_labels[complete]
  } else if (!all(complete)) {
    stop("'x' and 'y' contain missing values; set 'na.rm = TRUE' to remove them")
  }

  finite <- is.finite(x) & is.finite(y)
  x <- x[finite]
  y <- y[finite]
  point_labels <- point_labels[finite]

  if (length(x) == 0L) {
    stop("'x' and 'y' should contain at least one complete finite pair")
  }

  list(x = x, y = y, point_labels = point_labels)
}
