
#' Partial correlation matrix
#' 
#' Determine a partial correlation matrix
#' 
#' \code{partial_corr} computes the pairwise *partial correlation* coefficients 
#' from either a correlation matrix or a data matrix.
#' 
#' The approach is to find the complete partial correlation matrix 
#' (that is, partial all the other variables out of each variable). 
#' This may be done by simply specifying the raw data or correlation matrix. 
#' (In the case of raw data, correlations will be found according to \code{use} 
#' and \code{method}.) In this case, just specify the data matrix.
#' This is useful in the case of multiple regression. 
#'
#' \code{partial_corr()} computes the matrix from the inverse of the
#' correlation matrix. The companion function
#' \code{partial_corr_regression()} computes the same quantity by
#' residualizing each pair of variables against the remaining variables and then
#' correlating the residuals. The regression-based variant is helpful when the
#' inversion-based approach becomes numerically unstable.
#'
#' @param x correlation matrix or dataset in the form of a \code{matrix} or \code{data.frame}
#' @param use an optional character string giving a method for computing 
#' covariances in the presence of missing values. This must be (an abbreviation of) 
#' one of the strings \code{"everything"}, \code{"all.obs"}, \code{"complete.obs"}, 
#' \code{"na.or.complete"}, or \code{"pairwise.complete.obs"}
#' @param method a character string indicating which correlation coefficient 
#' (or covariance) is to be computed. One of \code{"pearson"} (default), 
#' \code{"kendall"}, or \code{"spearman"}: can be abbreviated 
#'
#' @return the matrix or partial correlations
#' @export
#'
#' @examples
#' partial_corr(bootcamp::movie[, -3])
#' partial_corr(bootcamp::centrality)
partial_corr <- function(x, use = "everything", 
                         method = c("pearson", "kendall", "spearman")) {
  if (inherits(x, "data.frame") + inherits(x, "matrix") == 0) {
    stop("'x' should be a matrix or a data.frame")
  }
  
  # check if x might be a correlation matrix
  appears_cor <- FALSE
  if (NROW(x) == NCOL(x)) {
    if (is.data.frame(x)) {
      if (isSymmetric(unclass(unname(as.matrix(x))))) {
        appears_cor <- TRUE
      }
    }
    else {
      if (isSymmetric(unclass(unname(x)))) {
        appears_cor <- TRUE
      }
    }
  }
  appears_cor <- appears_cor && isTRUE(all.equal(prod(diag(as.matrix(x))), 1))
  appears_cor <- appears_cor && isTRUE((min(x, na.rm = TRUE) >= -1) & 
                                         (max(x, na.rm = TRUE) <= 1))
  
  if (!appears_cor) { # the correlation needs to be created still
    res <- tryCatch(stats::cor(x = x, use = use, method = method), 
                    silent = TRUE, error = function(e) e)
    # check if it worked
    if (inherits(res, "error") | inherits(res, "simpleError")) {
      klassen <- sapply(x, class)
      klasse_factor <- colnames(x)[which(klassen == "factor")]
      klasse_karakter <- colnames(x)[which(klassen == "character")]
      cat("\nError message:", res$message, "\n\n")
      if (length(klasse_factor) > 0) {
        cat("The following variables are factors: ", klasse_factor, "\n\n")
      }
      if (length(klasse_karakter) > 0) {
        cat("The following variables are strings: ", klasse_karakter, "\n\n")
      }
      if ((length(klasse_factor) > 0) + (length(klasse_karakter) > 0) > 0) {
        cat(">>> Please remove these variables from 'x'\n\n")
      }
      return(invisible(NULL))
    }
  } else { # x is already a correlation matrix
    res <-  x
  }
  
  partial_from_cor(res)
}




#' Partial correlation matrix via regression
#'
#' Determine a partial correlation matrix using regression residuals
#'
#' \code{partial_corr_regression()} computes each pairwise partial correlation by
#' regressing both variables on all remaining variables and correlating the
#' resulting residuals. This offers a second implementation route when
#' \code{partial_corr()} is not desirable or fails numerically.
#'
#' Missing values are handled on a complete-case basis for the regression-based
#' approach. When \code{use} requests complete cases
#' (\code{"complete.obs"}, \code{"na.or.complete"}, or
#' \code{"pairwise.complete.obs"}), rows with any missing values are removed
#' before the regressions are fit. For \code{"everything"} and
#' \code{"all.obs"}, missing values trigger an error because regression cannot be
#' fit with unresolved \code{NA} values.
#'
#' For \code{method = "spearman"}, the variables are rank-transformed before the
#' regressions are fit. \code{method = "kendall"} is not supported in the
#' regression implementation.
#'
#' @inheritParams partial_corr
#'
#' @return the matrix of partial correlations
#' @export
#'
#' @examples
#' partial_corr_regression(bootcamp::movie[, -3])
#' partial_corr_regression(bootcamp::centrality, method = "spearman")
partial_corr_regression <- function(x, use = "everything",
                                    method = c("pearson", "kendall", "spearman")) {
  if (!base::inherits(x, "data.frame") && !base::inherits(x, "matrix")) {
    stop("'x' should be a matrix or a data.frame")
  }

  method <- base::match.arg(method)
  if (identical(method, "kendall")) {
    stop("Regression-based partial correlations currently support only 'pearson' and 'spearman'.")
  }

  prepared <- prepare_partial_corr_regression_data(x = x, use = use, method = method)
  x <- prepared$data

  p <- base::ncol(x)
  if (p == 0L) {
    stop("'x' should contain at least one numeric variable")
  }
  if (p == 1L) {
    single <- matrix(1, nrow = 1, ncol = 1)
    rownames(single) <- colnames(x)
    colnames(single) <- colnames(x)
    return(single)
  }

  result <- matrix(1, nrow = p, ncol = p)
  rownames(result) <- colnames(x)
  colnames(result) <- colnames(x)

  for (i in seq_len(p - 1L)) {
    for (j in seq.int(i + 1L, p)) {
      partial_value <- compute_pairwise_partial_corr_regression(x, i = i, j = j)
      result[i, j] <- partial_value
      result[j, i] <- partial_value
    }
  }

  result
}






partial_from_cor <- function(x) {
  # check if x looks like a corr matrix
  appears_cor <- FALSE
  if (NROW(x) == NCOL(x)) {
    if (is.data.frame(x)) {
      if (isSymmetric(unclass(unname(as.matrix(x))))) {
        appears_cor <- TRUE
      }
    } else {
      if (isSymmetric(unclass(unname(x)))) {
        appears_cor <- TRUE
      }
    }
  }
  appears_cor <- appears_cor && isTRUE(all.equal(prod(diag(as.matrix(x))), 1))
  appears_cor <- appears_cor && isTRUE((min(x, na.rm = TRUE) >= -1) & 
                                         (max(x, na.rm = TRUE) <= 1))
  if (!appears_cor) stop("Please provide an accurate correlation matrix")
  
  # determine pseudo inverse
  msvd = svd(x)
  if (length(msvd$d) == 0) {
    m <- array(0, dim(m)[2:1])
  } else {
    m <- msvd$v %*% (1/msvd$d * t(msvd$u))
  }
  
  # invert, then negate off-diagonal entries
  m = -m
  diag(m) = -diag(m)
  
  # standardize and return 
  m <- stats::cov2cor(m)
  rownames(m) <- rownames(x)
  colnames(m) <- colnames(x)
  m
}




#' @keywords internal
prepare_partial_corr_regression_data <- function(x, use, method) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  numeric_columns <- vapply(x, is.numeric, logical(1))
  if (!all(numeric_columns)) {
    stop("Regression-based partial correlations require numeric columns only.")
  }

  if (anyNA(x)) {
    if (use %in% c("complete.obs", "na.or.complete", "pairwise.complete.obs")) {
      # The regression implementation needs one common analysis set across all
      # variables, so we use complete cases throughout.
      x <- stats::na.omit(x)
    } else {
      stop("Missing values detected. Use 'complete.obs', 'na.or.complete', or 'pairwise.complete.obs'.")
    }
  }

  if (identical(method, "spearman")) {
    x[] <- lapply(x, base::rank, ties.method = "average")
  }

  list(data = x)
}


#' @keywords internal
compute_pairwise_partial_corr_regression <- function(x, i, j) {
  control_index <- setdiff(seq_len(ncol(x)), c(i, j))

  if (length(control_index) == 0L) {
    return(stats::cor(x[, i], x[, j]))
  }

  resid_i <- regression_residuals_from_controls(target = x[, i], controls = x[, control_index, drop = FALSE])
  resid_j <- regression_residuals_from_controls(target = x[, j], controls = x[, control_index, drop = FALSE])

  stats::cor(resid_i, resid_j)
}


#' @keywords internal
regression_residuals_from_controls <- function(target, controls) {
  fit <- stats::lm(target ~ ., data = as.data.frame(controls))
  stats::residuals(fit)
}

