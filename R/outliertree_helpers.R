#' Work with flagged cases from an outliertree model
#'
#' Inspect, select, print, or subset the training rows flagged by
#' \code{outliertree}
#'
#' This function brings the most common follow-up steps after
#' \code{outliertree::outlier.tree()} together in one place. Instead of switching
#' between separate helper functions, you can use the same function to:
#'
#' \itemize{
#'   \item retrieve a tidy summary of all flagged training cases;
#'   \item return only the original row numbers of the flagged cases;
#'   \item subset the original training data to those rows; or
#'   \item print the detailed \code{outliertree} explanation for selected cases.
#' }
#'
#' The \code{outlier_tree_model} must have been fitted with
#' \code{save_outliers = TRUE}. Otherwise \code{outliertree} does not retain the
#' detailed per-row annotations needed for inspection after model fitting.
#'
#' When you want to focus on only part of the detected outliers, you can select
#' rows in two ways. Use \code{which_data_row} to refer to the original row
#' numbers from the training data. Use \code{which_number} to refer to the
#' position among the detected outliers after sorting them by original row
#' number. If both are supplied, only cases matching both selections are kept.
#'
#' @param outlier_tree_model An object returned by
#'   \code{outliertree::outlier.tree()} and fitted with
#'   \code{save_outliers = TRUE}.
#' @param x Optional original training data used to fit
#'   \code{outlier_tree_model}. This argument is required when
#'   \code{output = "subset"} and must be a \code{data.frame} or \code{matrix}.
#' @param output Character string indicating what should be returned. Must be one
#'   of \code{"summary"}, \code{"rows"}, \code{"subset"}, or \code{"print"}.
#'   The default \code{"summary"} returns a flat data frame with one row per
#'   detected outlier.
#' @param which_data_row Optional integer vector with original row numbers from
#'   the training data. Only flagged rows matching these row numbers are kept.
#' @param which_number Optional integer vector with positions among the detected
#'   outliers after sorting by original row number. For example,
#'   \code{which_number = 1} returns the first detected outlier in that sorted
#'   list.
#' @param keep_summary Logical, only used when \code{output = "subset"}. If
#'   \code{TRUE}, helper columns with the outlier row number, suspicious column,
#'   and outlier score are prepended to the returned subset.
#'
#' @return Depending on \code{output}, this function returns one of four values:
#'   \describe{
#'     \item{\code{"summary"}}{A data frame with one row per selected flagged
#'       case. The columns are \code{row}, \code{suspicious_column},
#'       \code{suspicious_value}, \code{decimals}, \code{tree_depth},
#'       \code{uses_NA_branch}, and \code{outlier_score}.}
#'     \item{\code{"rows"}}{An integer vector with the selected original row
#'       numbers.}
#'     \item{\code{"subset"}}{A subset of \code{x} containing only the selected
#'       flagged rows. If \code{keep_summary = TRUE}, the columns
#'       \code{.outlier_row}, \code{.outlier_column}, and
#'       \code{.outlier_score} are prepended.}
#'     \item{\code{"print"}}{The selected outliers are printed in the
#'       human-readable \code{outliertree} format. Invisibly, the selected
#'       summary data frame is returned.}
#'   }
#'
#'   If no rows are flagged, or no flagged rows match the selection, an empty
#'   object of the requested type is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' demo_data <- data.frame(
#'   a = c(stats::rnorm(100), 10, 11),
#'   b = c(stats::rnorm(100), 10, 11),
#'   c = c(sample(c("x", "y"), 100, replace = TRUE), "x", "y"),
#'   stringsAsFactors = FALSE
#' )
#'
#' tree_model <- outliertree::outlier.tree(demo_data, save_outliers = TRUE)
#'
#' outliertree_outliers(tree_model)
#' outliertree_outliers(tree_model, output = "rows")
#' outliertree_outliers(tree_model, x = demo_data, output = "subset")
#' outliertree_outliers(tree_model, output = "print", which_number = 1)
#' }
outliertree_outliers <- function(outlier_tree_model,
                                 x = NULL,
                                 output = c("summary", "rows", "subset", "print"),
                                 which_data_row = NULL,
                                 which_number = NULL,
                                 keep_summary = TRUE) {
  output <- match.arg(output)

  if (!is.logical(keep_summary) || length(keep_summary) != 1L || is.na(keep_summary)) {
    stop("'keep_summary' should be TRUE or FALSE")
  }

  outlier_outputs <- get_saved_outliertree_outputs(outlier_tree_model)
  outlier_summary <- build_outliertree_outlier_summary(outlier_outputs)
  selected_rows <- select_outliertree_rows(
    flagged_rows = outlier_summary$row,
    which_data_row = which_data_row,
    which_number = which_number
  )
  selected_summary <- subset_outliertree_summary(outlier_summary, selected_rows)

  if (identical(output, "summary")) {
    return(selected_summary)
  }

  if (identical(output, "rows")) {
    return(selected_summary$row)
  }

  if (identical(output, "subset")) {
    return(outliertree_subset_data(x, selected_summary, keep_summary))
  }

  if (nrow(selected_summary) == 0L) {
    message("No flagged outliers matched the requested selection.")
    return(invisible(selected_summary))
  }

  # We reuse the public print method from outliertree so users see the same
  # descriptive output they already know from the package itself.
  print(outlier_outputs[selected_summary$row])
  invisible(selected_summary)
}




#' @keywords internal
get_saved_outliertree_outputs <- function(outlier_tree_model) {
  if (!base::requireNamespace("outliertree", quietly = TRUE)) {
    stop("Package 'outliertree' is required. Install it with utils::install.packages('outliertree').")
  }
  if (!inherits(outlier_tree_model, "outliertree")) {
    stop("'outlier_tree_model' should be an object returned by outliertree::outlier.tree()")
  }
  if (!"outliers_data" %in% names(outlier_tree_model)) {
    stop(
      "Could not extract saved outlier annotations. Fit the model with save_outliers = TRUE before calling this function.",
      call. = FALSE
    )
  }

  outlier_tree_model$outliers_data
}


#' @keywords internal
build_outliertree_outlier_summary <- function(outlier_outputs) {
  flagged_rows <- which(vapply(outlier_outputs, is_flagged_outliertree_case, logical(1)))
  if (length(flagged_rows) == 0L) {
    return(empty_outliertree_outlier_summary())
  }

  # We keep the summary intentionally flat so it can be inspected directly or
  # joined back to the original data without unpacking nested list columns.
  rows_as_data_frames <- lapply(flagged_rows, function(i) {
    entry <- outlier_outputs[[i]]
    data.frame(
      row = i,
      suspicious_column = entry$suspicous_value$column,
      suspicious_value = entry$suspicous_value$value,
      decimals = entry$suspicous_value$decimals,
      tree_depth = entry$tree_depth,
      uses_NA_branch = entry$uses_NA_branch,
      outlier_score = entry$outlier_score,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows_as_data_frames)
}


#' @keywords internal
select_outliertree_rows <- function(flagged_rows, which_data_row = NULL, which_number = NULL) {
  selected_rows <- sort(unique(as.integer(flagged_rows)))

  if (!is.null(which_data_row)) {
    selected_rows_by_data <- validate_requested_outliertree_rows(which_data_row, flagged_rows)
    missing_rows <- setdiff(selected_rows_by_data, flagged_rows)
    if (length(missing_rows) > 0L) {
      warning(
        "Some requested rows were not flagged as outliers and were ignored: ",
        paste(missing_rows, collapse = ", "),
        call. = FALSE
      )
    }

    selected_rows <- intersect(selected_rows, selected_rows_by_data)
  }

  if (!is.null(which_number)) {
    selected_rows_by_number <- rows_from_outlier_positions(which_number, flagged_rows)
    selected_rows <- intersect(selected_rows, selected_rows_by_number)
  }

  sort(unique(as.integer(selected_rows)))
}


#' @keywords internal
validate_requested_outliertree_rows <- function(which_data_row, flagged_rows) {
  if (!is.numeric(which_data_row) || anyNA(which_data_row)) {
    stop("'which_data_row' should contain row numbers without missing values")
  }
  if (any(which_data_row < 1) || any(which_data_row != as.integer(which_data_row))) {
    stop("'which_data_row' should contain positive whole numbers")
  }

  sort(unique(as.integer(which_data_row)))
}


#' @keywords internal
rows_from_outlier_positions <- function(which_number, flagged_rows) {
  if (!is.numeric(which_number) || anyNA(which_number)) {
    stop("'which_number' should contain outlier positions without missing values")
  }
  if (any(which_number < 1) || any(which_number != as.integer(which_number))) {
    stop("'which_number' should contain positive whole numbers")
  }

  which_number <- sort(unique(as.integer(which_number)))
  if (length(which_number) == 0L || length(flagged_rows) == 0L) {
    return(integer(0))
  }
  if (any(which_number > length(flagged_rows))) {
    stop(
      "'which_number' refers to positions beyond the number of detected outliers",
      call. = FALSE
    )
  }

  flagged_rows[which_number]
}


#' @keywords internal
subset_outliertree_summary <- function(outlier_summary, selected_rows) {
  if (nrow(outlier_summary) == 0L || length(selected_rows) == 0L) {
    return(outlier_summary[0, , drop = FALSE])
  }

  outlier_summary[match(selected_rows, outlier_summary$row), , drop = FALSE]
}


#' @keywords internal
outliertree_subset_data <- function(x, selected_summary, keep_summary) {
  if (is.null(x)) {
    stop("'x' should contain the original training data when output = 'subset'")
  }
  if (!is.data.frame(x) && !is.matrix(x)) {
    stop("'x' should be a data.frame or a matrix")
  }

  subsetted <- x[selected_summary$row, , drop = FALSE]
  if (!keep_summary || nrow(selected_summary) == 0L) {
    return(subsetted)
  }

  data.frame(
    .outlier_row = selected_summary$row,
    .outlier_column = selected_summary$suspicious_column,
    .outlier_score = selected_summary$outlier_score,
    subsetted,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}


#' @keywords internal
is_flagged_outliertree_case <- function(entry) {
  !is.null(entry$suspicous_value$column) &&
    length(entry$suspicous_value$column) == 1L &&
    !is.na(entry$outlier_score)
}


#' @keywords internal
empty_outliertree_outlier_summary <- function() {
  data.frame(
    row = integer(0),
    suspicious_column = character(0),
    suspicious_value = numeric(0),
    decimals = integer(0),
    tree_depth = integer(0),
    uses_NA_branch = logical(0),
    outlier_score = numeric(0),
    stringsAsFactors = FALSE
  )
}
