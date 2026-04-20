#' @title Compare fit statistics for lm models
#'
#' @description Produces a table of fit statistics for multiple lm models.
#' 
#' @param fits A series of model object names, separated by commas.
#' @param ... Other arguments passed to \code{list}.
#' 
#' @details  Produces a table of fit statistics for multiple lm models: 
#'           AIC, AICc, BIC, p-value (for the F-test), R-squared, and adjusted R-squared.
#'           
#'           Smaller values for AIC, AICc, and BIC indicate a better balance
#'           of goodness-of-fit of the model and the complexity of the
#'           model. The goal is to find a model that adequately explains the 
#'           data without having too many terms.
#'           
#'           BIC tends to choose models with fewer parameters relative to AIC.
#'           
#'           In the table, \code{anderson.p} refers to the
#'           p-value for the Anderson-Darling test of 
#'           Normality of the residuals of the model. 
#'           The value of the test statistic itself is not reported, since that 
#'           isn't informative by itself. If this is wanted, run \link{ad_test} 
#'           directly.
#'           
#'           For comparisons with AIC, etc., to be valid, both models must
#'           have the same data, without transformations, use the same 
#'           dependent variable, and be fit with the same method.
#'           They do not need to be nested.
#'
#' @author Salvatore Mangiafico, \email{mangiafico@njaes.rutgers.edu}
#' @references \url{http://rcompanion.org/handbook/I_10.html}
#'             \url{http://rcompanion.org/rcompanion/e_05.html}
#' @return A list of two objects: The series of model calls, and a data 
#'         frame of statistics for each model.
#'         
#' @examples
#' ### Compare among polynomial models
#' data(BrendonSmall, package = "bootcamp")
#' BrendonSmall$Calories = as.numeric(BrendonSmall$Calories)
#' model.1 = lm(Sodium ~ Calories, data = BrendonSmall)
#' model.2 = lm(Sodium ~ poly(Calories, 2), data = BrendonSmall)
#' model.3 = lm(Sodium ~ poly(Calories, 3), data = BrendonSmall)
#' model.4 = lm(Sodium ~ poly(Calories, 4), data = BrendonSmall)
#' compareLM(model.1, model.2, model.3, model.4)
#' 
#' @export
compareLM <- function (fits, ...) {
 fits <- list(fits, ...)
 n <- length(fits)
 Y <- matrix(rep("A", n),
             ncol = 1)
 colnames(Y) <- "Formula"
 rownames(Y) <- seq_len(n)

  for (i in seq_len(n)) {
    # `deparse()` can return multiple strings for long formulas; collapsing them
    # keeps the full model formula intact instead of failing on long calls.
    Y[i, ] <- paste(deparse(stats::formula(fits[[i]]), width.cutoff = 500L), collapse = "")
  }
   
 Z <- data.frame(Rank = rep(NA, n),
                 Df.res = rep(NA, n),
                 AIC = rep(NA, n),
                 AICc = rep(NA, n),
                 BIC = rep(NA, n),
                 R.squared = rep(NA, n),
                 Adj.R.sq = rep(NA, n),
                 f.p.value = rep(NA, n),
                 Anderson.p = rep(NA, n),
                 stringsAsFactors = FALSE)   
   for (i in seq_len(n)) {
     k <- length(fits[[i]]$coefficients) + 1
     L <- as.numeric(stats::logLik(fits[[i]]))
     N <- stats::nobs(fits[[i]])
     aic <- -2 * L + 2 * k
     aicc <- aic + 2 * (k * (k + 1)) / (N - k - 1)
     bic <- -2 * L + log(N) * k
    
     Z[i, ] <- c(signif(fits[[i]]$rank, digits = 4),
                 signif(fits[[i]]$df.residual, digits = 4),
                 signif(aic, digits = 4),
                 signif(aicc, digits = 4),
                 signif(bic, digits = 4),
                 signif(summary(fits[[i]])$r.squared, digits = 4),
                 signif(summary(fits[[i]])$adj.r.squared, digits = 4),
                 signif(stats::pf(summary(fits[[i]])$fstatistic[1], 
                                  summary(fits[[i]])$fstatistic[2], 
                                  summary(fits[[i]])$fstatistic[3],
                                  lower.tail = FALSE), digits = 4),
                 signif(bootcamp::ad_test(fits[[i]]$residuals)$p.value,
                        digits = 4)
                 )
     }     
   
 W <- list(Y, Z)
 names(W) <- c("Models",
               "Fit.criteria")
 return(W)            
} 




#' Anderson-Darling test for Normality
#' 
#' Anderson-Darling test for Normality
#' 
#' Performs the Anderson-Darling test for Normality. Missing values 
#' are allowed.
#' 
#' This implementation is based on the Anderson-Darling normality test as
#' provided by the \code{nortest} package.
#'
#' @param x a numeric vector of data values, the number of which must be 
#' greater than 7. Missing values are allowed.
#'
#' @return
#' A list with class “htest” containing the following components:
#' \describe{
#'   \item{statistic}{the value of the Anderson-Darling statistic}
#'   \item{p.value}{the p-value for the test}
#'   \item{method}{the character string “Anderson-Darling normality test”}
#'   \item{data.name}{a character string giving the name(s) of the data}
#' }
#' @export
#' @references Juergen Gross, author of the \code{ad.test()} implementation in
#' the \code{nortest} package.
#' @examples 
#' data(BrendonSmall, package = "bootcamp")
#' model.1 = lm(Sodium ~ Calories, data = BrendonSmall)
#' model.2 = lm(Sodium ~ poly(Calories, 2), BrendonSmall)
#' ad_test(model.1$residuals)
#' ad_test(model.2$residuals)
ad_test <- function (x) {
  DNAME <- deparse(substitute(x))
  x <- sort(x[stats::complete.cases(x)])
  n <- length(x)
  if (n < 8) 
    stop("sample size must be greater than 7")
  logp1 <- stats::pnorm((x - mean(x))/stats::sd(x), log.p = TRUE)
  logp2 <- stats::pnorm(-(x - mean(x))/stats::sd(x), log.p = TRUE)
  h <- (2 * seq(1:n) - 1) * (logp1 + rev(logp2))
  A <- -n - mean(h)
  AA <- (1 + 0.75/n + 2.25/n^2) * A
  if (AA < 0.2) {
    pval <- 1 - exp(-13.436 + 101.14 * AA - 223.73 * AA^2)
  }
  else if (AA < 0.34) {
    pval <- 1 - exp(-8.318 + 42.796 * AA - 59.938 * AA^2)
  }
  else if (AA < 0.6) {
    pval <- exp(0.9177 - 4.279 * AA - 1.38 * AA^2)
  }
  else if (AA < 10) {
    pval <- exp(1.2937 - 5.709 * AA + 0.0186 * AA^2)
  }
  else pval <- 3.7e-24
  RVAL <- list(statistic = c(A = A), p.value = pval, method = "Anderson-Darling normality test", 
               data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
}

