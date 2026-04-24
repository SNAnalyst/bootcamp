#' Annotated boxplot for teaching spread and outliers
#'
#' Draw a boxplot with optional raw data and mean markers
#'
#' This helper is intended for teaching descriptive statistics. It combines a
#' standard base R boxplot with optional jittered raw observations and mean
#' markers so students can connect the summary representation to the underlying
#' data points.
#'
#' The function accepts a single numeric vector or multiple groups supplied as a
#' list, data frame, or matrix. Non-numeric groups are not allowed because the
#' goal is to keep the visual interpretation straightforward for beginners.
#'
#' @param x A numeric vector, list of numeric vectors, numeric matrix, or
#'   numeric data frame.
#' @param na.rm Logical, should missing values be removed within each group?
#' @param horizontal Logical, should the boxes be drawn horizontally?
#' @param show_points Logical, should raw observations be added with jitter?
#' @param show_mean Logical, should the group mean be shown as a diamond?
#' @param col Fill colour for the boxes.
#' @param border Border colour for the boxes.
#' @param point_col Colour for the raw data points.
#' @param mean_col Colour for the mean markers.
#' @param main Main title. When \code{NULL}, a default title is used.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param label_digits Number of digits used for returned summaries.
#' @param ... Additional arguments passed to \code{graphics::boxplot()}.
#'
#' @return Invisibly returns a data frame with one row per group and columns for
#'   the group name, sample size, mean, median, interquartile range, and number
#'   of outliers flagged by the boxplot rule.
#' @export
#'
#' @examples
#' values <- stats::rnorm(100)
#' bootcamp::annotated_boxplot(values)
#'
#' grouped_values <- list(
#'   Group_A = stats::rnorm(80, mean = 0),
#'   Group_B = stats::rnorm(80, mean = 1)
#' )
#' bootcamp::annotated_boxplot(grouped_values, horizontal = FALSE)
#'
#' matrix_values <- cbind(
#'   Before = stats::rnorm(60, mean = 10, sd = 2),
#'   After = stats::rnorm(60, mean = 11.5, sd = 2.2)
#' )
#' bootcamp::annotated_boxplot(
#'   matrix_values,
#'   horizontal = TRUE,
#'   show_points = TRUE,
#'   show_mean = TRUE
#' )
#'
#' values_with_missing <- list(
#'   Group_A = c(stats::rnorm(40), NA, NA),
#'   Group_B = c(stats::rnorm(40, mean = 0.5), NA)
#' )
#' bootcamp::annotated_boxplot(values_with_missing, na.rm = TRUE)
annotated_boxplot <- function(x,
                              na.rm = TRUE,
                              horizontal = TRUE,
                              show_points = TRUE,
                              show_mean = TRUE,
                              col = "#F3D9B1",
                              border = "#8C6D31",
                              point_col = "#4D4D4D",
                              mean_col = "#D55E00",
                              main = NULL,
                              xlab = NULL,
                              ylab = NULL,
                              label_digits = 2,
                              ...) {
  prepared <- prepare_annotated_boxplot_data(x = x, na.rm = na.rm)
  group_values <- prepared$values
  group_names <- prepared$names

  if (is.null(main)) {
    main <- "Boxplot with raw data and mean markers"
  }
  if (is.null(xlab)) {
    xlab <- if (horizontal) "Value" else ""
  }
  if (is.null(ylab)) {
    ylab <- if (horizontal) "" else "Value"
  }

  boxplot_stats <- graphics::boxplot(
    x = group_values,
    horizontal = horizontal,
    col = col,
    border = border,
    main = main,
    xlab = xlab,
    ylab = ylab,
    names = group_names,
    ...
  )

  positions <- seq_along(group_values)
  if (show_points) {
    graphics::stripchart(
      x = group_values,
      method = "jitter",
      vertical = !horizontal,
      add = TRUE,
      pch = 16,
      cex = 0.65,
      col = point_col
    )
  }

  if (show_mean) {
    group_means <- vapply(group_values, mean, numeric(1))
    if (horizontal) {
      graphics::points(group_means, positions, pch = 18, cex = 1.5, col = mean_col)
    } else {
      graphics::points(positions, group_means, pch = 18, cex = 1.5, col = mean_col)
    }
    graphics::legend(
      "topright",
      legend = "Mean",
      pch = 18,
      col = mean_col,
      bty = "n"
    )
  }

  group_summary <- data.frame(
    group = group_names,
    n = vapply(group_values, length, integer(1)),
    mean = round(vapply(group_values, mean, numeric(1)), digits = label_digits),
    median = round(vapply(group_values, stats::median, numeric(1)), digits = label_digits),
    iqr = round(vapply(group_values, stats::IQR, numeric(1)), digits = label_digits),
    stringsAsFactors = FALSE
  )

  outlier_counts <- integer(length(group_values))
  if (length(boxplot_stats$out) > 0L && length(boxplot_stats$group) > 0L) {
    outlier_counts <- tabulate(boxplot_stats$group, nbins = length(group_values))
  }
  group_summary$outliers <- outlier_counts

  invisible(group_summary)
}


#' @keywords internal
prepare_annotated_boxplot_data <- function(x, na.rm) {
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm)) {
    stop("'na.rm' should be TRUE or FALSE")
  }

  if (is.numeric(x) && is.null(dim(x))) {
    values <- list(x)
    names(values) <- deparse(substitute(x))
  } else if (is.list(x)) {
    values <- x
    if (is.null(names(values))) {
      names(values) <- paste0("Group_", seq_along(values))
    }
  } else if (is.matrix(x) || is.data.frame(x)) {
    values <- lapply(seq_len(ncol(x)), function(i) x[, i])
    names(values) <- colnames(x)
    if (is.null(names(values))) {
      names(values) <- paste0("Group_", seq_along(values))
    }
  } else {
    stop("'x' should be a numeric vector, list, matrix, or data.frame")
  }

  values <- lapply(values, function(group) {
    if (!is.numeric(group)) {
      stop("All groups supplied to 'x' should be numeric")
    }
    cleaned_group <- group
    if (na.rm) {
      cleaned_group <- cleaned_group[!is.na(cleaned_group)]
    } else if (anyNA(cleaned_group)) {
      stop("One or more groups contain missing values; set 'na.rm = TRUE' to remove them")
    }
    cleaned_group <- cleaned_group[is.finite(cleaned_group)]
    if (length(cleaned_group) == 0L) {
      stop("Each group should contain at least one finite value")
    }
    cleaned_group
  })

  list(values = values, names = names(values))
}
