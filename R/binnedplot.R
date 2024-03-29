
#' @title Binned Residual Plot
#'
#' @description A function that plots averages of y versus averages of x and can be useful to plot residuals for logistic #'regression.
#'
#' @usage
#'           binnedplot(x ,y, nclass=NULL,
#'           xlab="Expected Values", ylab="Average residual",
#'           main="Binned residual plot",
#'           cex.pts=0.8, col.pts=1, col.int="gray", ...)
#' @param x	 The expected values from the logistic regression.
#' @param y   The residuals values from logistic regression (observed values minus expected values).
#' @param nclass	 Number of categories (bins) based on their fitted values in which the data are divided. Default=NULL and #' will take the value of nclass according to the $n$ such that if $n >=100$, nclass=floor(sqrt(length(x))); if $10<n<100$, #' nclass=10; if $n<10$, nclass=floor(n/2).
#'
#' @param xlab a label for the x axis, default is "Expected Values".
#'
#' @param ylab	a label for the y axis, default is "Average residual".
#'
#' @param main	 a main title for the plot, default is "Binned residual plot". See also title.
#'
#' @param cex.pts	 The size of points, default=0.8.
#'
#' @param col.pts	 color of points, default is black
#'
#' @param col.int	 color of intervals, default is gray
#'
#' @param ...	 Graphical parameters to be passed to methods
#'
#' @details In logistic regression, as with linear regression, the residuals can be defined as observed minus expected
#' values. The data are discrete and so are the residuals. As a result, plots of raw residuals from logistic regression are
#' generally not useful. The binned residuals plot instead, after dividing the data into categories (bins) based on their
#' fitted values, plots the average residual versus the average fitted value for each bin.
#'
#' Value
#' A plot in which the gray lines indicate plus and minus 2 standard-error bounds, within which one would expect about 95% #' of the binned residuals to fall, if the model were actually true.
#'
#' Note
#' There is typically some arbitrariness in choosing the number of bins: each bin should contain enough points so that the #' averaged residuals are not too noisy, but it helps to have also many bins so as to see more local patterns in the
#' residuals (see Gelman and Hill, Data Analysis Using Regression and Multilevel/Hierarchical Models, pag 97).
#'
#' @author M. Grazia Pittau grazia@stat.columbia.edu; Yu-Sung Su suyusung@tsinghua.edu.cn
#'
#' @references \url{Andrew Gelman and Jennifer Hill, Data Analysis Using Regression and Multilevel/Hierarchical Models,
#' Cambridge University Press, 2006.}
#' @return a binned plot
#'
#'
#' @examples
#' old.par <- par(no.readonly = TRUE)
#' data(lalonde)
#' attach(lalonde)
#' fit <- glm(treat ~ re74 + re75 + educ + black + hisp + married
#'            + nodegr + u74 + u75, family=binomial(link="logit"))
#' x <- predict(fit)
#' y <- resid(fit)
#' binnedplot(x,y)
#' par(old.par)
#'
#' @export
binnedplot <- function (x, y, nclass = NULL, xlab = "Expected Values",
                        ylab = "Average residual", main = "Binned residual plot",
                        cex.pts = 0.8, col.pts = 1, col.int = "gray", ...)
{
  n <- length(x)
  if (is.null(nclass)) {
    if (n >= 100) {
      nclass = floor(sqrt(length(x)))
    }
    if (n > 10 & n < 100) {
      nclass = 10
    }
    if (n <= 10) {
      nclass = floor(n/2)
    }
  }
  aa <- data.frame(binned.resids(x, y, nclass)$binned)
  plot(range(aa$xbar), range(aa$ybar, aa$X2se, -aa$X2se, na.rm = TRUE),
       xlab = xlab, ylab = ylab, type = "n", main = main,
       ...)
  graphics::abline(0, 0, lty = 2)
  graphics::lines(aa$xbar, aa$X2se, col = col.int)
  graphics::lines(aa$xbar, -aa$X2se, col = col.int)
  graphics::points(aa$xbar, aa$ybar, pch = 19, cex = cex.pts, col = col.pts)
}


binned.resids <- function (x, y, nclass = floor(sqrt(length(x)))) {
  breaks.index <- floor(length(x) * (1:(nclass - 1))/nclass)
  if (any(breaks.index == 0))
    nclass <- 1
  x.sort <- sort(x)
  breaks <- -Inf
  if (nclass > 1) {
    for (i in 1:(nclass - 1)) {
      x.lo <- x.sort[breaks.index[i]]
      x.hi <- x.sort[breaks.index[i] + 1]
      if (x.lo == x.hi) {
        if (x.lo == min(x)) {
          x.lo <- -Inf
        }
        else {
          x.lo <- max(x[x < x.lo])
        }
      }
      breaks <- c(breaks, (x.lo + x.hi)/2)
    }
  }
  else if (nclass == 1) {
    x.lo <- min(x)
    x.hi <- max(x)
    breaks <- c(breaks, (x.lo + x.hi)/2)
  }
  breaks <- c(breaks, Inf)
  breaks <- unique(breaks)
  nclass <- length(breaks) - 1
  output <- NULL
  xbreaks <- NULL
  x.binned <- as.numeric(cut(x, breaks))
  for (i in 1:nclass) {
    items <- (1:length(x))[x.binned == i]
    x.range <- range(x[items])
    xbar <- mean(x[items])
    ybar <- mean(y[items])
    n <- length(items)
    sdev <- if (length(y[items]) > 1)
      stats::sd(y[items])
    else 0
    output <- rbind(output, c(xbar, ybar, n, x.range, 2 *
                                sdev/sqrt(n)))
  }
  colnames(output) <- c("xbar", "ybar", "n",
                        "x.lo", "x.hi", "2se")
  return(list(binned = output, xbreaks = xbreaks))
}
