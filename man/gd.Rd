% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gd.R
\name{gd}
\alias{gd}
\alias{print.gd}
\alias{plot.gd}
\title{Geographical detectors: factor detector.}
\usage{
gd(formula, data = NULL)
\method{print}{gd}(x, ...)
\method{plot}{gd}(x, sig = TRUE, ...)
}
\arguments{
\item{formula}{A formula of response and explanatory variables}

\item{data}{A data.frame includes response and explanatory variables}

\item{x}{A list of factor detector results}

\item{sig}{If TRUE, only spatial associations that are significant at the 0.05 level will be plotted;
If FALSE, all spatial associations will be plotted.}

\item{...}{Ignore}
}
\description{
Function for calculating power determinant using factor detector
of geographical detectors and visualization.
}
\examples{
g1 <- gd(NDVIchange ~ Climatezone + Mining, data = ndvi_40)
g1
plot(g1)

}
