
#' Network of correlations
#' 
#' Plot a network of correlations
#' 
#' Provide this function with a dataframe, matrix, or two vectors. 
#' It will calculate the appropriate correlation matrix and then plot it as 
#' a graph.
#' 
#' The correlation matrix is calculated by the \code{\link[stats]{cor}} function 
#' and the plotting is performed by the \code{\link[DescTools]{PlotWeb}} function.
#' 
#' Several useful defaults have been set, but can all be overridden.
#' 
#' Of particular interest is the argument \code{remove_below_abs}. This allows 
#' a researcher to create a "frugal" graph: it removes from the plot all 
#' correlations that are (in absolute value) smaller than \code{remove_below_abs}. 
#' The result is that only correlations are drawn that are larger or equal (in
#' absolute value) than \code{remove_below_abs}, which can make the plot show 
#' only correlations that are above a threshold of interest.
#' 
#' Further, the \code{remove_above_p numeric} makes it easy to remove correlations 
#' from the plot that are not statistically significant. If a value lower than 1 is set, 
#' all correlations with a p-value above \code{remove_above_p} will be removed 
#' from the plot. So, if only statistically significant correlations should be 
#' shown, one would set \code{remove_above_p numeric} to .05 or .01 or so.
#' 
#' The plot can be customized by adding arguments that are passed to the base 
#' R \code{\link[DescTools]{Canvas}} function. 
#' In practice, it will rarely be needed to set additional 
#' arguments yourself. Potentially useful arguments include \code{xlim}, 
#' \code{ylim}, \code{xpd} (expand drawing area, defaults to 
#' \code{graphics::par("xpd"))}, 
#' \code{asp} (numeric, giving the aspect ratio y/x, default is 1), 
#' \code{bg} (the background color of the entire plot, defaults to 
#' \code{graphics::par("bg")}, which usually will be "white), 
#' \code{usrbg} (sets the background for the rectangle that includes the graph). 
#' Any further \code{...} arguments are passed to \code{\link[graphics]{plot}}, 
#' but note that this can sometimes cause errors, since 
#' \code{\link[DescTools]{PlotWeb}} tries to set some of these arguments 
#' itself, which then causes a conflict.
#' 
#' If \code{x} includes a column that is a \code{character} or \code{factor}, 
#' it can be impossible to calculate a meaningful correlation. In this case, 
#' the function will provide the appropriate error and will try to provide the 
#' user with the info as to which variables are problematic. Removing these 
#' from the dataset should generally lead to a working result.
#'
#' @param x matrix or data frame
#' @param use an optional character string giving a method for computing 
#' covariances in the presence of missing values. This must be (an abbreviation of) 
#' one of the strings \code{"everything"}, \code{"all.obs"}, \code{"complete.obs"}, 
#' \code{"na.or.complete"}, or \code{"pairwise.complete.obs"}
#' @param method a character string indicating which correlation coefficient 
#' (or covariance) is to be computed. One of \code{"pearson"} (default), 
#' \code{"kendall"}, or \code{"spearman"}: can be abbreviated
#' @param use_partial_corr logical. If \code{TRUE}, the network is based on 
#' *partial correlations*, if \code{FALSE} (the default) the network is 
#' based on regular correlations
#' @param remove_below_abs numeric, correlations smaller than this value 
#' (in absolute value) will not be shown in the plot
#' @param remove_above_p numeric between 0 and 1. If a value lower than 1 is set, 
#' all correlations with a p-value above \code{remove_above_p} will be removed 
#' from the plot.
#' @param col the color for the connecting lines
#' @param lty the line type for the connecting lines, the default will be 
#' \code{graphics::par("lty")}
#' @param lwd the line widths for the connecting lines. If left to \code{NULL} it will 
#' be linearly scaled between the minimum and maximum value of \code{m}
#' @param args.legend list of additional arguments to be passed to the 
#' \code{legend} function. Use \code{args.legend = NA} if no legend should be added
#' @param pch the plotting symbols appearing in the plot, as a non-negative 
#' numeric vector (see \code{\link[graphics]{points}}, but unlike there 
#' negative values are omitted) or a vector of 1-character strings, or one 
#' multi-character string
#' @param pt.cex expansion factor(s) for the points
#' @param pt.col the foreground color for the points, corresponding to its 
#' argument \code{col}
#' @param pt.bg the background color for the points, corresponding to its argument \code{bg}
#' @param cex.lab the character extension for the labels
#' @param las alignment of the labels, 1 means horizontal, 2 radial and 3 vertical
#' @param adj adjustments for the labels. (Left: 0, Right: 1, Mid: 0.5)
#' @param dist gives the distance of the labels from the outer circle. Default is 2
#' @param mar set margins. Defaults to c(1, 1, 1, 1)
#' @param stamp text or expression to be placed in the right bottom corner of the plot. 
#' This can be useful, if some author or date information should automatically 
#' be inserted by default. Any text can be set as option, but also dynamic 
#' expressions can be used. The default would use an expression as 
#' <username>/<date>, which will use the username from the system and the 
#' current date. See \code{\link[DescTools]{DescToolsOptions}} for details. 
#' The default is \code{FALSE}, which does not print anything.
#' @param main title for the plot. If not given, an automatic title is generated.
#' @param ... dots are passed to \code{\link[DescTools]{Canvas}}
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
#' # runs fine, because the factor is not included
#' corrNetwork(bootcamp::movie[, -3])
#' 
#' corrNetwork(bootcamp::movie[, -3], use_partial_corr = TRUE)
#' 
#' # only show correlation > .6 or < -.6
#' corrNetwork(bootcamp::movie[, -3], remove_below_abs = .6)
#' 
#' # only show correlations that are significant at .05
#' corrNetwork(bootcamp::movie[, -3], remove_above_p = .05)
#' 
#' \dontrun{
#' # An informative error is provided, because the dataframe contains a factor
#' corrNetwork(bootcamp::movie)
#' }
corrNetwork <- function(x, use = "everything", 
                       method = c("pearson", "kendall", "spearman"),
                       use_partial_corr = FALSE,
                       remove_below_abs = NULL,
                       remove_above_p = 1,
                       col = c("#9A0941", "#8296C4"), lty = NULL, lwd = NULL, 
                       args.legend = NULL, pch = 21, pt.cex = 2,
                       pt.col = "black", pt.bg = "darkgrey", cex.lab = 1.0,
                       las = 1, adj = NULL, dist = 0.5, mar = c(1, 1, 1, 1), 
                       stamp = NULL, main = NULL,
                       ...) {
  
  oldpar <- DescTools::DescToolsOptions()
  DescTools::DescToolsOptions("stamp" = stamp)
  on.exit(DescTools::DescToolsOptions(oldpar))
  
  if (use_partial_corr) {
    res <- partial_corr(x, use = use, method = method)
    if (is.null(res)) {
      return(invisible(NULL))
    }
  } else {  # regular correlations
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
  }
  
  if (is.numeric(remove_below_abs)) {
    if (length(remove_below_abs) > 1) stop("'remove_below_abs' should be a single number")
    res[abs(res) < remove_below_abs] <- NA
  }

  if (remove_above_p < 1) {
    pvals <- DescTools::PairApply(x, function(x, y) stats::cor.test(x, y)$p.value, symmetric = TRUE)
    res[pvals > remove_above_p] <- NA
  }
  
  if (is.null(main)) {
    if (use_partial_corr) {
      main <- paste0("Partial correlation network of ", deparse(substitute(x)))
    } else {
      main <- paste0("Correlation network of ", deparse(substitute(x)))
    }
  }
  
  suppressWarnings(DescTools::PlotWeb(m = res, col = col, lty = lty, lwd = lwd, 
                     args.legend = args.legend, pch = pch, pt.cex = pt.cex,
                     pt.col = pt.col, pt.bg = pt.bg, cex.lab = cex.lab,
                     las = las, adj = adj, dist = dist, main = main, ...))
  
  return(invisible(res))
}


