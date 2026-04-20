
#' Plot a correlation matrix
#' 
#' Plot a correlation matrix
#' 
#' This is a useful wrapper around \code{\link[DescTools]{PlotCorr}}. 
#' For a provided matrix or data.frame \code{x}, the appropriate correlations 
#' are calculated by the \code{\link[stats]{cor}} function 
#' and this matrix is then passed to the \code{\link[DescTools]{PlotCorr}} 
#' function, which handles the plotting.
#' 
#' In addition to the regular \code{\link[DescTools]{PlotCorr}} plot, this 
#' function also adds the correlation values to the upper diagonal of the 
#' matrix.
#' 
#' If \code{x} includes a column that is a \code{character} or \code{factor}, 
#' it can be impossible to calculate a meaningful correlation. In this case, 
#' the function will provide the appropriate error and will try to provide the 
#' user with the info as to which variables are problematic. Removing these 
#' from the dataset should generally lead to a working result.
#' 
#' Several defaults have been set, but can all be overridden.
#'
#' @param x matrix or data frame
#' @param use an optional character string giving a method for computing 
#' covariances in the presence of missing values. This must be (an abbreviation of) 
#' one of the strings \code{"everything"}, \code{"all.obs"}, \code{"complete.obs"}, 
#' \code{"na.or.complete"}, or \code{"pairwise.complete.obs"}
#' @param method a character string indicating which correlation coefficient 
#' (or covariance) is to be computed. One of \code{"pearson"} (default), 
#' \code{"kendall"}, or \code{"spearman"}: can be abbreviated
#' @param show_values logical, should the correlation values be printed in the matrix
#' @param digits the number of decimals to show if \code{show_values} is \code{TRUE}
#' @param cex.cell the character expansion used for the correlation values shown
#' inside the matrix cells when \code{show_values = TRUE}.
#' @param cols the colors for shading the matrix. Uses the package's option
#' \code{"col1"} and \code{"col2"} as default.
#' @param breaks a set of breakpoints for the colours: must give one more
#' breakpoint than colour. These are passed to \code{image()} function.  If
#' breaks is specified then the algorithm used follows \code{\link{cut}}, so
#' intervals are closed on the right and open on the left except for the lowest
#' interval.
#' @param border color for borders. The default is \code{grey}. Set this
#' argument to \code{NA} if borders should be omitted.
#' @param lwd line width for borders. Default is 1.
#' @param args.colorlegend list of arguments for the \code{\link{ColorLegend}}.
#' Use \code{NA} if no color legend should be painted.
#' @param xaxt parameter to define, whether to draw an x-axis, defaults to
#' \code{"n"}
#' @param yaxt parameter to define, whether to draw an y-axis, defaults to
#' \code{"n"}.
#' @param cex.axis character extension for the axis labels.
#' @param las the style of axis labels.
#' @param mar sets the margins, defaults to mar = c(3, 8, 8, 8) as we need a
#' bit more room on the right.
#' @param mincor numeric value between 0 and 1, defining the smallest
#' correlation that is to be displayed. If this is >0 then all correlations
#' with a lower value are suppressed.
#' @param main character, the main title.
#' @param clust logical. If set to \code{TRUE}, the correlations will be
#' clustered in order to aggregate similar values. The default is
#' \code{FALSE}, which preserves the order of the columns in \code{x}.
#' @param stamp text or expression to be placed in the right bottom corner of the plot. 
#' This can be useful, if some author or date information should automatically 
#' be inserted by default. Any text can be set as option, but also dynamic 
#' expressions can be used. The default would use an expression as 
#' <username>/<date>, which will use the username from the system and the 
#' current date. See \code{\link[DescTools]{DescToolsOptions}} for details. 
#' The default is \code{FALSE}, which does not print anything.
#' @param ... the dots are passed to the function \code{\link[graphics]{image}}, which
#' produces the plot or to \code{\link[DescTools]{Format}} (for the formatting) 
#' of the correlation values printed inside the matrix cells.
#'
#' @return If an error occurs when computing the correlations, this is returned. 
#' If the error is the result of the use of character or factor variables, the 
#' function will report which variables are delinquent.
#' 
#' The correlation matrix is invisibly returned.
#' 
#' @export
#'
#' @examples
#' corrMat(bootcamp::movie[, -3])
#' corrMat(bootcamp::movie[, -3], digits = 3)
#' 
#' corrMat(bootcamp::centrality, show_values = FALSE)
#' 
#' \dontrun{
#' # graceful error, the data contain a factor variable
#' corrMat(bootcamp::movie)
#' }
corrMat <- function(x,
        use = "everything",
        method = c("pearson", "kendall", "spearman"),
        show_values = TRUE,
        digits = 2,
        cex.cell = 0.8,
        cols = grDevices::colorRampPalette(c(DescTools::Pal()[2], "white", 
                                             DescTools::Pal()[1]), space = "rgb")(20),
        breaks = seq(-1, 1, length = length(cols) + 1),
        border = "grey",
        lwd = 1,
        args.colorlegend = NULL,
        xaxt = graphics::par("xaxt"),
        yaxt = graphics::par("yaxt"),
        cex.axis = 0.8,
        las = 2,
        mar = c(3, 8, 8, 8),
        mincor = 0,
        main = "",
        clust = FALSE,
        stamp = NULL,
        ...) {
  
  oldpar <- DescTools::DescToolsOptions()
  DescTools::DescToolsOptions("stamp" = stamp)
  on.exit(DescTools::DescToolsOptions(oldpar))
  
  if (inherits(x, "data.frame") + inherits(x, "matrix") == 0) {
    stop("'x' should be a matrix or a data.frame")
  }
  
  res <- tryCatch(stats::cor(x = x, use = use, method = method), 
                  silent = TRUE, error = function(e) e)
  
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
  
  DescTools::PlotCorr(x = res, cols = cols, breaks = breaks,
                      border = border, lwd = lwd, args.colorlegend = args.colorlegend, 
                      xaxt = xaxt, yaxt = yaxt, cex.axis = cex.axis, 
                      las = las, mar = mar, 
                      mincor = mincor, main = main, 
                      clust = clust, 
                      ...)
  
  if (show_values) {
    idx <- get_corrmat_display_order(res, clust = clust)
    x <- matrix(rep(1:ncol(res), each = ncol(res), ncol = ncol(res)))
    y <- matrix(rep(ncol(res):1, ncol(res)), ncol = ncol(res))
    txt <- DescTools::Format(res[idx, idx], d = digits, na.form = "n.s.", ...)
    idx <- upper.tri(matrix(x, ncol = ncol(res)), diag = FALSE)
    graphics::text(x = x[idx], y = y[idx], label = txt[idx], cex = cex.cell, xpd = TRUE)
  }
  return(invisible(res))
}




#' @keywords internal
get_corrmat_display_order <- function(correlation_matrix, clust) {
  if (!isTRUE(clust)) {
    return(seq_len(ncol(correlation_matrix)))
  }

  stats::order.dendrogram(stats::as.dendrogram(
    stats::hclust(stats::dist(correlation_matrix), method = "mcquitty")
  ))
}

