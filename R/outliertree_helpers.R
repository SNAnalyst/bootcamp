#' Extract flagged cases from an outliertree model
#'
#' Summarize the training rows that were flagged by \code{outliertree}
#'
#' This function provides a tidy summary of all training rows that were flagged
#' as outliers by an \code{outliertree::outlier.tree()} model. The model must
#' have been fitted with \code{save_outliers = TRUE}; otherwise the original
#' outlier annotations are not retained by \code{outliertree}.
#'
#' The returned data frame contains one row per flagged training case and keeps
#' the row number from the original training data in the \code{row} column.
#' This makes it easy to inspect which variable triggered the flag and to link
#' the result back to the original dataset.
#'
#' @param outlier_tree_model An object returned by
#'   \code{outliertree::outlier.tree()}.
#'
#' @return A data frame with one row per flagged case. The columns are:
#'   \describe{
#'     \item{row}{Row number in the original training data.}
#'     \item{suspicious_column}{Variable for which the case was flagged.}
#'     \item{suspicious_value}{Observed suspicious value.}
#'     \item{decimals}{Suggested number of decimals for printing the suspicious value.}
#'     \item{tree_depth}{Depth in the outlier tree at which the case was flagged.}
#'     \item{uses_NA_branch}{Logical, whether the detected path used an \code{NA} branch.}
#'     \item{outlier_score}{Outlier score returned by \code{outliertree}.}
#'   }
#'   If no cases are flagged, a zero-row data frame with these columns is
#'   returned.
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' demo_data <- data.frame(
#'   a = c(stats::rnorm(100), 10, 11),
#'   b = c(stats::rnorm(100), 10, 11),
#'   c = c(sample(c("x", "y"), 100, replace = TRUE), "x", "y")
#' )
#'
#' tree_model <- outliertree::outlier.tree(demo_data, save_outliers = TRUE)
#' extract_outliertree_outliers(tree_model)
#' }
extract_outliertree_outliers <- function(outlier_tree_model) {
  if (!base::requireNamespace("outliertree", quietly = TRUE)) {
    stop("Package 'outliertree' is required. Install it with utils::install.packages('outliertree').")
  }

  raw_outliers <- tryCatch(
    outliertree::extract.training.outliers(outlier_tree_model),
    error = function(e) {
      stop(
        "Could not extract saved outlier annotations. Fit the model with save_outliers = TRUE before calling this function.",
        call. = FALSE
      )
    }
  )

  flagged_rows <- which(vapply(raw_outliers, is_flagged_outliertree_case, logical(1)))
  if (length(flagged_rows) == 0L) {
    return(empty_outliertree_outlier_summary())
  }

  # We keep the summary intentionally flat so it can be joined back to the
  # original data without unpacking nested list columns first.
  rows_as_data_frames <- lapply(flagged_rows, function(i) {
    entry <- raw_outliers[[i]]
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




#' Subset the original data to the rows flagged by outliertree
#'
#' Return only the training rows that were flagged by \code{outliertree}
#'
#' This function subsets the original training data to the rows that were flagged
#' by an \code{outliertree::outlier.tree()} model. It is designed to pair with
#' \code{extract_outliertree_outliers()}, which provides the corresponding row
#' numbers and summary information.
#'
#' By default, the function prepends a few helper columns so the returned subset
#' remains self-explanatory: the original row number, the suspicious variable,
#' and the outlier score.
#'
#' @param x The original training data that were used to fit
#'   \code{outlier_tree_model}. Must be a \code{data.frame} or \code{matrix}.
#' @param outlier_tree_model An object returned by
#'   \code{outliertree::outlier.tree()} and fitted with
#'   \code{save_outliers = TRUE}.
#' @param keep_summary Logical, should summary columns describing the detected
#'   outlier be included in the returned subset? Defaults to \code{TRUE}.
#'
#' @return A subset of \code{x} containing only the flagged rows. If
#'   \code{keep_summary = TRUE}, the columns \code{.outlier_row},
#'   \code{.outlier_column}, and \code{.outlier_score} are prepended. If no rows
#'   are flagged, a zero-row subset of \code{x} is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' demo_data <- data.frame(
#'   a = c(stats::rnorm(100), 10, 11),
#'   b = c(stats::rnorm(100), 10, 11),
#'   c = c(sample(c("x", "y"), 100, replace = TRUE), "x", "y")
#' )
#'
#' tree_model <- outliertree::outlier.tree(demo_data, save_outliers = TRUE)
#' subset_outliertree_outliers(demo_data, tree_model)
#' }
subset_outliertree_outliers <- function(x, outlier_tree_model, keep_summary = TRUE) {
  if (!base::is.data.frame(x) && !base::is.matrix(x)) {
    stop("'x' should be a data.frame or a matrix")
  }
  if (!base::is.logical(keep_summary) || length(keep_summary) != 1L || is.na(keep_summary)) {
    stop("'keep_summary' should be TRUE or FALSE")
  }

  outlier_summary <- extract_outliertree_outliers(outlier_tree_model)
  flagged_rows <- outlier_summary$row

  subsetted <- x[flagged_rows, , drop = FALSE]
  if (!keep_summary || nrow(outlier_summary) == 0L) {
    return(subsetted)
  }

  data.frame(
    .outlier_row = outlier_summary$row,
    .outlier_column = outlier_summary$suspicious_column,
    .outlier_score = outlier_summary$outlier_score,
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
