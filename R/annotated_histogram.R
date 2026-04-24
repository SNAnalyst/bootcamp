#' Annotated histogram for teaching descriptives
#'
#' Draw a histogram and annotate it with common descriptive statistics
#'
#' This helper is designed for teaching situations in which students should see
#' how a histogram relates to numerical summaries such as the mean, median, and
#' standard deviation. The function deliberately stays close to base R graphics
#' so that the visual elements remain transparent and easy to explain in class.
#'
#' By default the plot adds vertical reference lines for the mean and median.
#' Optional standard deviation markers can be added to show how spread around
#' the mean translates into the horizontal scale of the histogram.
#'
#' @param x A numeric vector.
#' @param breaks Passed to \code{graphics::hist()} to define the binning.
#' @param na.rm Logical, should missing values be removed before plotting?
#' @param show_mean Logical, should the sample mean be drawn?
#' @param show_median Logical, should the sample median be drawn?
#' @param show_sd Logical, should one-standard-deviation reference lines be
#'   drawn around the mean?
#' @param show_rug Logical, should a rug be added to show individual
#'   observations?
#' @param col Fill colour for the histogram bars.
#' @param border Border colour for the histogram bars.
#' @param main Main title. When \code{NULL}, a default educational title is
#'   used.
#' @param xlab Label for the x-axis. When \code{NULL}, the deparsed expression
#'   supplied to \code{x} is used.
#' @param ylab Label for the y-axis.
#' @param label_digits Number of digits used in the legend labels.
#' @param legend_position Position passed to \code{graphics::legend()}.
#' @param ... Additional arguments passed to \code{graphics::hist()}.
#'
#' @return Invisibly returns a list containing the cleaned data, sample size,
#'   mean, median, standard deviation, and histogram break points.
#' @export
#'
#' @examples
#' x <- stats::rnorm(200, mean = 10, sd = 2)
#' bootcamp::annotated_histogram(x)
#' bootcamp::annotated_histogram(x, show_sd = TRUE, show_rug = TRUE)
#'
#' skewed_x <- stats::rexp(250, rate = 0.4)
#' bootcamp::annotated_histogram(
#'   skewed_x,
#'   breaks = 20,
#'   col = "#F3D9B1",
#'   main = "Right-skewed distribution"
#' )
#'
#' values_with_missing <- c(stats::rnorm(120, mean = 5), NA, NA)
#' bootcamp::annotated_histogram(
#'   values_with_missing,
#'   na.rm = TRUE,
#'   show_mean = TRUE,
#'   show_median = TRUE,
#'   show_sd = TRUE
#' )
annotated_histogram <- function(x,
                                breaks = "Sturges",
                                na.rm = TRUE,
                                show_mean = TRUE,
                                show_median = TRUE,
                                show_sd = FALSE,
                                show_rug = FALSE,
                                col = "#C7E9C0",
                                border = "white",
                                main = NULL,
                                xlab = NULL,
                                ylab = "Frequency",
                                label_digits = 2,
                                legend_position = "topright",
                                ...) {
  values <- validate_annotated_histogram_input(x = x, na.rm = na.rm)

  if (is.null(main)) {
    main <- "Histogram with descriptive markers"
  }
  if (is.null(xlab)) {
    xlab <- deparse(substitute(x))
  }

  summary_values <- list(
    n = length(values),
    mean = mean(values),
    median = stats::median(values),
    sd = stats::sd(values)
  )

  histogram <- graphics::hist(
    x = values,
    breaks = breaks,
    col = col,
    border = border,
    main = main,
    xlab = xlab,
    ylab = ylab,
    ...
  )

  legend_labels <- character(0)
  legend_colours <- character(0)
  legend_lines <- integer(0)

  if (show_mean) {
    graphics::abline(v = summary_values$mean, col = "#D55E00", lwd = 2)
    legend_labels <- c(
      legend_labels,
      paste0("Mean = ", formatC(summary_values$mean, digits = label_digits, format = "f"))
    )
    legend_colours <- c(legend_colours, "#D55E00")
    legend_lines <- c(legend_lines, 1L)
  }

  if (show_median) {
    graphics::abline(v = summary_values$median, col = "#1F77B4", lwd = 2, lty = 2)
    legend_labels <- c(
      legend_labels,
      paste0("Median = ", formatC(summary_values$median, digits = label_digits, format = "f"))
    )
    legend_colours <- c(legend_colours, "#1F77B4")
    legend_lines <- c(legend_lines, 2L)
  }

  if (show_sd && !is.na(summary_values$sd) && summary_values$sd > 0) {
    sd_limits <- summary_values$mean + c(-1, 1) * summary_values$sd

    # These dashed lines help students connect the abstract SD concept to the
    # horizontal spread of the observed distribution.
    graphics::abline(v = sd_limits, col = "#009E73", lwd = 2, lty = 3)
    legend_labels <- c(
      legend_labels,
      paste0("Mean \u00b1 1 SD = ", formatC(summary_values$sd, digits = label_digits, format = "f"))
    )
    legend_colours <- c(legend_colours, "#009E73")
    legend_lines <- c(legend_lines, 3L)
  }

  if (show_rug) {
    graphics::rug(values, col = "grey35")
  }

  if (length(legend_labels) > 0L) {
    graphics::legend(
      x = legend_position,
      legend = legend_labels,
      col = legend_colours,
      lwd = 2,
      lty = legend_lines,
      bty = "n"
    )
  }

  invisible(list(
    data = values,
    n = summary_values$n,
    mean = summary_values$mean,
    median = summary_values$median,
    sd = summary_values$sd,
    breaks = histogram$breaks
  ))
}


#' @keywords internal
validate_annotated_histogram_input <- function(x, na.rm) {
  if (!is.numeric(x)) {
    stop("'x' should be a numeric vector")
  }
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm)) {
    stop("'na.rm' should be TRUE or FALSE")
  }

  values <- x
  if (na.rm) {
    values <- values[!is.na(values)]
  } else if (anyNA(values)) {
    stop("'x' contains missing values; set 'na.rm = TRUE' to remove them")
  }

  values <- values[is.finite(values)]
  if (length(values) == 0L) {
    stop("'x' should contain at least one finite value")
  }

  values
}
