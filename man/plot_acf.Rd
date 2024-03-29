% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tseries-plots.R
\name{plot_acf}
\alias{plot_acf}
\alias{plot_acf.data.frame}
\alias{plot_pacf}
\alias{plot_pacf.data.frame}
\title{Plot Autocorrelation Function (ACF) or Partial Autocorrelation Function
(PACF) Values}
\usage{
plot_acf(
  .data,
  .color = "black",
  .conf_color = "blue",
  .title = "ACF Plot",
  .origin_width = 1,
  .conf_width = 0.5
)

\method{plot_acf}{data.frame}(
  .data,
  .color = "black",
  .conf_color = "blue",
  .title = "ACF Plot",
  .origin_width = 1,
  .conf_width = 0.5
)

plot_pacf(
  .data,
  .color = "black",
  .conf_color = "blue",
  .title = "PACF Plot",
  .origin_width = 1,
  .conf_width = 0.5
)

\method{plot_pacf}{data.frame}(
  .data,
  .color = "black",
  .conf_color = "blue",
  .title = "PACF Plot",
  .origin_width = 1,
  .conf_width = 0.5
)
}
\arguments{
\item{.data}{The output of \code{\link[=calculate_acf]{calculate_acf()}} or \code{\link[=calculate_pacf]{calculate_pacf()}}.}

\item{.color}{(Optional) The color of the plot's bars. Default is black.}

\item{.conf_color}{(Optional) The color of the confidence interval lines.
Default is blue.}

\item{.title}{(Optional) The title for the plot.}

\item{.origin_width}{(Optional) The width of the line at \code{y = 0}.}

\item{.conf_width}{(Optional) The width of the confidence interval lines.}
}
\value{
A \code{\link[ggplot2:ggplot]{ggplot}} object.
}
\description{
Plot Autocorrelation Function (ACF) or Partial Autocorrelation Function
(PACF) Values
}
\examples{
library(tidytest)

x <- rnorm(100)

#> ACF
acf_tbl <- calculate_acf(x)
plot_acf(acf_tbl)

#> PACF
pacf_tbl <- calculate_pacf(x)
plot_pacf(pacf_tbl)

}
\seealso{
\code{\link[=calculate_acf]{calculate_acf()}} \code{\link[=calculate_pacf]{calculate_pacf()}}
}
