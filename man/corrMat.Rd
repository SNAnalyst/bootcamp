% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corrMat.R
\name{corrMat}
\alias{corrMat}
\title{Plot a correlation matrix}
\usage{
corrMat(
  x,
  use = "everything",
  method = c("pearson", "kendall", "spearman"),
  show_values = TRUE,
  digits = 2,
  cols = (grDevices::colorRampPalette(c(DescTools::Pal()[2], "white",
    DescTools::Pal()[1]), space = "rgb"))(20),
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
  clust = TRUE,
  stamp = NULL,
  ...
)
}
\arguments{
\item{x}{matrix or data frame}

\item{use}{an optional character string giving a method for computing 
covariances in the presence of missing values. This must be (an abbreviation of) 
one of the strings \code{"everything"}, \code{"all.obs"}, \code{"complete.obs"}, 
\code{"na.or.complete"}, or \code{"pairwise.complete.obs"}}

\item{method}{a character string indicating which correlation coefficient 
(or covariance) is to be computed. One of \code{"pearson"} (default), 
\code{"kendall"}, or \code{"spearman"}: can be abbreviated}

\item{show_values}{logical, should the correlation values be printed in the matrix}

\item{digits}{the number of decimals to show if \code{show_values} is \code{TRUE}}

\item{cols}{the colors for shading the matrix. Uses the package's option
\code{"col1"} and \code{"col2"} as default.}

\item{breaks}{a set of breakpoints for the colours: must give one more
breakpoint than colour. These are passed to \code{image()} function.  If
breaks is specified then the algorithm used follows \code{\link{cut}}, so
intervals are closed on the right and open on the left except for the lowest
interval.}

\item{border}{color for borders. The default is \code{grey}. Set this
argument to \code{NA} if borders should be omitted.}

\item{lwd}{line width for borders. Default is 1.}

\item{args.colorlegend}{list of arguments for the \code{\link{ColorLegend}}.
Use \code{NA} if no color legend should be painted.}

\item{xaxt}{parameter to define, whether to draw an x-axis, defaults to
\code{"n"}}

\item{yaxt}{parameter to define, whether to draw an y-axis, defaults to
\code{"n"}.}

\item{cex.axis}{character extension for the axis labels.}

\item{las}{the style of axis labels.}

\item{mar}{sets the margins, defaults to mar = c(3, 8, 8, 8) as we need a
bit more room on the right.}

\item{mincor}{numeric value between 0 and 1, defining the smallest
correlation that is to be displayed. If this is >0 then all correlations
with a lower value are suppressed.}

\item{main}{character, the main title.}

\item{clust}{logical. If set to \code{TRUE}, the correlations will be
clustered in order to aggregate similar values.}

\item{stamp}{text or expression to be placed in the right bottom corner of the plot. 
This can be useful, if some author or date information should automatically 
be inserted by default. Any text can be set as option, but also dynamic 
expressions can be used. The default would use an expression as 
<username>/<date>, which will use the username from the system and the 
current date. See \code{\link[DescTools]{DescToolsOptions}} for details. 
The default is \code{FALSE}, which does not print anything.}

\item{...}{the dots are passed to the function \code{\link[graphics]{image}}, which
produces the plot or to \code{\link[DescTools]{Format}} (for the formatting) 
of the correlation values printed inside the matrix cells.}
}
\value{
If an error occurs when computing the correlations, this is returned. 
If the error is the result of the use of character or factor variables, the 
function will report which variables are delinquent.

The correlation matrix is invisibly returned.
}
\description{
Plot a correlation matrix
}
\details{
This is a useful wrapper around \code{\link[DescTools]{PlotCorr}}. 
For a provided matrix or data.frame \code{x}, the appropriate correlations 
are calculated by the \code{\link[stats]{cor}} function 
and this matrix is then passed to the \code{\link[DescTools]{PlotCorr}} 
function, which handles the plotting.

In addition to the regular \code{\link[DescTools]{PlotCorr}} plot, this 
function also adds the correlation values to the upper diagonal of the 
matrix.

If \code{x} includes a column that is a \code{character} or \code{factor}, 
it can be impossible to calculate a meaningful correlation. In this case, 
the function will provide the appropriate error and will try to provide the 
user with the info as to which variables are problematic. Removing these 
from the dataset should generally lead to a working result.

Several defaults have been set, but can all be overridden.
}
\examples{
corrMat(bootcamp::movie[, -3])
corrMat(bootcamp::movie[, -3], digits = 3)

corrMat(bootcamp::centrality, show_values = FALSE)

\dontrun{
# graceful error, the data contain a factor variable
corrMat(bootcamp::movie)
}
}
