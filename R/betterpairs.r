#' Pimped scatterplot matrix
#' 
#' Makes a pimped scatterplot matrix
#' 
#' This is a pimped version of the standard \code{\link[graphics]{pairs}} plot. 
#' If you want the original, more boring version, go there. 
#' This function is a lot cooler, though.
#' 
#' For datasets with lots of variables, it is usually best to look at 
#' the pairs plots for subsets of variables, unless you have a really large 
#' monitor...
#' 
#' @param x a matrix or dataframe
#' @param histogram logical, Should a histogram be plotted on the diagonal (or only variable names)
#' @param density logical, Should a histogram+density plot be plotted on the diagonal (or only variable names). 
#' If \code{histogram=TRUE} the value for \code{density} is not used, a histogram is plotted
#' @param smoother logical, should 'smoother' be used below the diagonal (default is TRUE). Otherwise \code{panel.smooth} is used.
#' @param rug logical, should \code{rug} be included in the histogram? Default = TRUE.
#' @param hist.col color of the histogram
#' @param method method for calculating the correlation (default = \code{pearson})
#' @param ... additional arguments, to be passed to \code{\link[graphics]{pairs}}
#' @param cex.cor (optioneel) numeriek, schaalt de grootte van de text waarmee de correlatie wordt weergegeven. 
#' Heeft geen effect op de grootte van de asterisks. Indien \code{NULL} (de default) kan het verschil in grootte erg groot worden.
#' @return Scatterplot matrix with histograms on the diagonal, 
#' scatterplots with smoothers below the diagonal and
#' pairwise signed correlations (with signif. stars) above the diagonal
#'
#' @export betterpairs
#' @examples
#' t <- matrix(runif(200), 40, 5)
#' colnames(t) <- LETTERS[1:5]
#' betterpairs(t)
#' 
#' \dontrun{
#' data(movie, package = "bootcamp")
#' }
betterpairs <- function(x, histogram = FALSE, density = TRUE, rug = TRUE, smoother = TRUE, 
                         hist.col = "red", method = "pearson", cex.cor = NULL, ...) 
{
  # x = as.matrix(x)
  # if (mode(x) != "numeric") 
  #   stop("Must pass in only numeric values")
  panel.cor <- function(x, y, digits = 2, prefix = "", use = "pairwise.complete.obs", 
                        ...) {
    #                         cex.cor = cex.cor, ...) {
    usr <- graphics::par("usr")
    on.exit(graphics::par(usr))
    graphics::par(usr = c(0, 1, 0, 1))
    correlation_info <- format_betterpairs_correlation(x = x, y = y, digits = digits, use = use)
    r <- correlation_info$correlation
    txt <- paste(prefix, correlation_info$label, sep = "")
    if (is.null(cex.cor)) 
      cex <- 0.8/graphics::strwidth(txt)
    if (!is.null(cex.cor)) 
      cex <- cex.cor
    test <- stats::cor.test(x, y)
    Signif <- stats::symnum(test$p.value, corr = FALSE, na = FALSE, 
                     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", 
                                                                              "**", "*", ".", " "))
    # Text size follows the magnitude, but the displayed label keeps the sign.
    graphics::text(0.5, 0.5, txt, cex = cex * sqrt(abs(r))/2)
    #     graphics::text(0.5, 0.5, txt, cex = cex * r)  ## oorspronkelijke schaling van de textgrootte van de correlaties
    #     graphics::text(0.8, 0.8, Signif, cex = cex, col = 2) # oospronkelijke grootte van de sterretjes
    graphics::text(0.8, 0.8, Signif, cex = 0.8/graphics::strwidth(txt), col = 2) #aangepast, zodat ze niet mee veranderen met de hoogte van de correlatie
  }
  
  f <- function(t) stats::dnorm(t, mean = mean(x), sd = stats::sd(x))
  panel.hist <- function(x, ...) {
    usr <- graphics::par("usr")
    on.exit(graphics::par(usr))
    graphics::par(usr = c(usr[1:2], 0, 1.5))
    h <- graphics::hist(x, plot = FALSE, probability = TRUE)
    graphics::lines(density(x, na.rm = TRUE), col = "red", lwd = 1)
    breaks <- h$breaks
    nB <- length(breaks)
    y <- h$counts
    y <- y/max(y)
    graphics::rect(breaks[-nB], 0, breaks[-1], y, col = hist.col)
    if (rug) 
      rug(x)
  }
  panel.hist.density <- function(x, ...) {
    usr <- graphics::par("usr")
    on.exit(graphics::par(usr))
    graphics::par(usr = c(usr[1:2], 0, 1.5))
    h <- graphics::hist(x, plot = FALSE)
    breaks <- h$breaks
    nB <- length(breaks)
    y <- h$counts
    y <- y/max(y)
    graphics::rect(breaks[-nB], 0, breaks[-1], y, col = hist.col)
    tryd <- try(d <- density(x, na.rm = TRUE, bw = "nrd", 
                             adjust = 1.2), silent = TRUE)
    if (class(tryd) != "try-error") {
      d$y <- d$y/max(d$y)
      graphics::lines(d)
      if (rug) 
        rug(x)
    }
  }
  panel.smoother <- function(x, y, pch = graphics::par("pch"), col.smooth = "red", 
                             span = 2/3, iter = 3, ...) {
    xm <- mean(x, na.rm = TRUE)
    ym <- mean(y, na.rm = TRUE)
    xs <- stats::sd(x, na.rm = TRUE)
    ys <- stats::sd(y, na.rm = TRUE)
    r = stats::cor(x, y, use = "pairwise", method = method)
    graphics::points(x, y, pch = pch, ...)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) 
      graphics::lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
            col = col.smooth, ...)
    panel.ellipse1(xm, ym, xs, ys, r, col.smooth = col.smooth, 
                   ...)
  }
  panel.ellipse1 <- function(x = 0, y = 0, xs = 1, ys = 1, 
                             r = 0, col.smooth, add = TRUE, segments = 51, ...) {
    angles <- (0:segments) * 2 * pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    if (!is.na(r)) {
      if (abs(r) > 0) 
        theta <- sign(r)/sqrt(2)
      else theta = 1/sqrt(2)
      shape <- diag(c(sqrt(1 + r), sqrt(1 - r))) %*% matrix(c(theta, 
                                                              theta, -theta, theta), ncol = 2, byrow = TRUE)
      ellipse <- unit.circle %*% shape
      ellipse[, 1] <- ellipse[, 1] * xs + x
      ellipse[, 2] <- ellipse[, 2] * ys + y
      graphics::points(x, y, pch = 19, col = col.smooth, cex = 1.5)
      graphics::lines(ellipse, ...)
    }
  }
  if (density) {
    if (smoother) 
      graphics::pairs(x, gap = 0, lower.panel = panel.smoother, upper.panel = panel.cor, 
            diag.panel = panel.hist.density, ...)
    else graphics::pairs(x, gap = 0, lower.panel = graphics::panel.smooth, upper.panel = panel.cor, 
               diag.panel = panel.hist.density, ...)
  }
  if (histogram) {
    if (smoother) 
      graphics::pairs(x, gap = 0, lower.panel = panel.smoother, upper.panel = panel.cor, 
            diag.panel = panel.hist, ...)
    else graphics::pairs(x, gap = 0, lower.panel = graphics::panel.smooth, upper.panel = panel.cor, 
               diag.panel = panel.hist, ...)
  }
  if (!density && !histogram) {
    if (smoother) 
      graphics::pairs(x, gap = 0, lower.panel = panel.smoother, upper.panel = panel.cor, 
            ...)
    else graphics::pairs(x, gap = 0, lower.panel = graphics::panel.smooth, upper.panel = panel.cor, 
               ...)
  }
  invisible()
}




#' @keywords internal
format_betterpairs_correlation <- function(x, y, digits = 2, use = "pairwise.complete.obs") {
  correlation <- stats::cor(x, y, use = use)
  label <- format(c(correlation, 0.123456789), digits = digits)[1]

  list(
    correlation = correlation,
    label = label
  )
}
